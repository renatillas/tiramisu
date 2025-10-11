import gleam/bit_array
import gleam/bool
import gleam/bytes_tree.{type BytesTree}
import gleam/crypto
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gramps/websocket/compression.{
  type Context, type ContextTakeover, ContextTakeover,
}

pub type DataFrame {
  TextFrame(payload: BitArray)
  BinaryFrame(payload: BitArray)
}

pub type CloseReason {
  NotProvided
  Normal(body: BitArray)
  GoingAway(body: BitArray)
  ProtocolError(body: BitArray)
  UnexpectedDataType(body: BitArray)
  InconsistentDataType(body: BitArray)
  PolicyViolation(body: BitArray)
  MessageTooBig(body: BitArray)
  MissingExtensions(body: BitArray)
  UnexpectedCondition(body: BitArray)
  /// Usually used for `4000` codes.
  CustomCloseReason(
    /// If `code >= 5000`, it will be the same as a `Normal` close reason.
    code: Int,
    body: BitArray,
  )
}

pub type ControlFrame {
  CloseFrame(reason: CloseReason)
  PingFrame(payload: BitArray)
  PongFrame(payload: BitArray)
}

pub type Frame {
  Data(DataFrame)
  Control(ControlFrame)
  Continuation(length: Int, payload: BitArray)
}

@external(erlang, "crypto", "exor")
fn crypto_exor(a a: BitArray, b b: BitArray) -> BitArray

fn mask_data(
  data: BitArray,
  masks: List(BitArray),
  index: Int,
  resp: BitArray,
) -> BitArray {
  case data {
    <<masked:bits-size(8), rest:bits>> -> {
      let assert [one, two, three, four] = masks
      let mask_value = case index % 4 {
        0 -> one
        1 -> two
        2 -> three
        3 -> four
        _ -> panic as "Somehow a value mod 4 is not 0, 1, 2, or 3"
      }
      let unmasked = crypto_exor(mask_value, masked)
      mask_data(rest, masks, index + 1, <<resp:bits, unmasked:bits>>)
    }
    _ -> resp
  }
}

pub type FrameParseError {
  NeedMoreData(BitArray)
  InvalidFrame
}

pub type ParsedFrame {
  Complete(Frame)
  Incomplete(Frame)
}

pub fn decode_frame(
  message: BitArray,
  context: Option(Context),
) -> Result(#(ParsedFrame, BitArray), FrameParseError) {
  case message {
    <<
      complete:1,
      compressed:1,
      _reserved:2,
      opcode:int-size(4),
      masked:1,
      payload_length:int-size(7),
      rest:bits,
    >> -> {
      let compressed = compressed == 1
      let masked = masked == 1
      use <- bool.guard(
        when: compressed && option.is_none(context),
        return: Error(InvalidFrame),
      )
      let payload_size = case payload_length {
        126 -> 16
        127 -> 64
        _ -> 0
      }
      let maybe_pair = case masked, rest {
        True,
          <<
            length:int-size(payload_size),
            mask1:bytes-size(1),
            mask2:bytes-size(1),
            mask3:bytes-size(1),
            mask4:bytes-size(1),
            rest:bits,
          >>
        -> {
          let payload_byte_size = case length {
            0 -> payload_length
            n -> n
          }
          case rest {
            <<payload:bytes-size(payload_byte_size), rest:bits>> -> {
              let data =
                mask_data(payload, [mask1, mask2, mask3, mask4], 0, <<>>)
              Ok(#(data, rest))
            }
            _ -> {
              Error(NeedMoreData(message))
            }
          }
        }
        False, <<length:int-size(payload_size), rest:bits>> -> {
          let payload_byte_size = case length {
            0 -> payload_length
            n -> n
          }
          case rest {
            <<payload:bytes-size(payload_byte_size), rest:bits>> -> {
              Ok(#(payload, rest))
            }
            _ -> {
              Error(NeedMoreData(message))
            }
          }
        }
        _, _ -> Error(InvalidFrame)
      }

      use #(data, rest) <- result.try(maybe_pair)
      case opcode {
        0 -> {
          data
          |> inflate(compressed, context, _)
          |> result.map(fn(decompressed) {
            Continuation(payload_size, decompressed)
          })
        }
        1 -> {
          data
          |> inflate(compressed, context, _)
          |> result.map(fn(decompressed) { Data(TextFrame(decompressed)) })
        }
        2 -> {
          data
          |> inflate(compressed, context, _)
          |> result.map(fn(decompressed) { Data(BinaryFrame(decompressed)) })
        }
        8 -> {
          case data {
            <<1000:16, rest:bits>> -> Ok(Control(CloseFrame(Normal(rest))))
            <<1001:16, rest:bits>> -> Ok(Control(CloseFrame(GoingAway(rest))))
            <<1002:16, rest:bits>> ->
              Ok(Control(CloseFrame(ProtocolError(rest))))
            <<1003:16, rest:bits>> ->
              Ok(Control(CloseFrame(UnexpectedDataType(rest))))
            <<1007:16, rest:bits>> ->
              Ok(Control(CloseFrame(InconsistentDataType(rest))))
            <<1008:16, rest:bits>> ->
              Ok(Control(CloseFrame(PolicyViolation(rest))))
            <<1009:16, rest:bits>> ->
              Ok(Control(CloseFrame(MessageTooBig(rest))))
            <<1010:16, rest:bits>> ->
              Ok(Control(CloseFrame(MissingExtensions(rest))))
            <<1011:16, rest:bits>> ->
              Ok(Control(CloseFrame(UnexpectedCondition(rest))))
            <<code:16, rest:bits>> ->
              Ok(Control(CloseFrame(CustomCloseReason(code, rest))))
            _ -> Ok(Control(CloseFrame(NotProvided)))
          }
        }
        9 -> Ok(Control(PingFrame(data)))
        10 -> Ok(Control(PongFrame(data)))
        _ -> Error(InvalidFrame)
      }
      |> result.try(fn(frame) {
        case complete {
          1 -> Ok(#(Complete(frame), rest))
          0 -> Ok(#(Incomplete(frame), rest))
          _ -> Error(InvalidFrame)
        }
      })
    }
    _ -> Error(InvalidFrame)
  }
}

pub fn encode_text_frame(
  data: String,
  context: Option(Context),
  mask: Option(BitArray),
) -> BytesTree {
  to_frame(bit_array.from_string(data), context, mask, TextFrame, Data)
}

pub fn encode_binary_frame(
  data: BitArray,
  context: Option(Context),
  mask: Option(BitArray),
) -> BytesTree {
  to_frame(data, context, mask, BinaryFrame, Data)
}

pub fn encode_close_frame(
  reason: CloseReason,
  mask: Option(BitArray),
) -> BytesTree {
  encode_frame(Control(CloseFrame(reason)), Uncompressed, mask)
}

pub fn encode_ping_frame(data: BitArray, mask: Option(BitArray)) -> BytesTree {
  to_frame(data, None, mask, PingFrame, Control)
}

pub fn encode_pong_frame(data: BitArray, mask: Option(BitArray)) -> BytesTree {
  to_frame(data, None, mask, PongFrame, Control)
}

pub fn encode_continuation_frame(
  data: BitArray,
  total_size: Int,
  mask: Option(BitArray),
) -> BytesTree {
  let payload = apply_mask(data, mask)
  encode_frame(Continuation(total_size, payload), Uncompressed, mask)
}

fn encode_frame(
  frame: Frame,
  compressed: Compression,
  mask: Option(BitArray),
) -> BytesTree {
  case frame {
    Data(TextFrame(payload)) -> {
      let payload_length = bit_array.byte_size(payload)
      make_frame(1, payload_length, payload, compressed, mask)
    }
    Control(CloseFrame(reason)) -> {
      let #(payload_length, payload) = case reason {
        NotProvided -> #(0, <<>>)
        GoingAway(body:) -> {
          let payload_size = bit_array.byte_size(body) + 2
          #(payload_size, <<1001:16, body:bits>>)
        }
        InconsistentDataType(body:) -> {
          let payload_size = bit_array.byte_size(body) + 2
          #(payload_size, <<1007:16, body:bits>>)
        }
        MessageTooBig(body:) -> {
          let payload_size = bit_array.byte_size(body) + 2
          #(payload_size, <<1009:16, body:bits>>)
        }
        MissingExtensions(body:) -> {
          let payload_size = bit_array.byte_size(body) + 2
          #(payload_size, <<1010:16, body:bits>>)
        }
        Normal(body:) -> {
          let payload_size = bit_array.byte_size(body) + 2
          #(payload_size, <<1000:16, body:bits>>)
        }
        PolicyViolation(body:) -> {
          let payload_size = bit_array.byte_size(body) + 2
          #(payload_size, <<1008:16, body:bits>>)
        }
        ProtocolError(body:) -> {
          let payload_size = bit_array.byte_size(body) + 2
          #(payload_size, <<1002:16, body:bits>>)
        }
        UnexpectedCondition(body:) -> {
          let payload_size = bit_array.byte_size(body) + 2
          #(payload_size, <<1011:16, body:bits>>)
        }
        UnexpectedDataType(body:) -> {
          let payload_size = bit_array.byte_size(body) + 2
          #(payload_size, <<1003:16, body:bits>>)
        }
        CustomCloseReason(code:, body:) -> {
          let payload_size = bit_array.byte_size(body) + 2
          // Prevents integer overflow and changes the status code to `Normal` for invalid codes.
          let code = case code < 5000 {
            True -> code
            False -> 1000
          }
          #(payload_size, <<code:16, body:bits>>)
        }
      }
      make_frame(8, payload_length, apply_mask(payload, mask), compressed, mask)
    }
    Data(BinaryFrame(payload)) -> {
      let payload_length = bit_array.byte_size(payload)
      make_frame(2, payload_length, payload, compressed, mask)
    }
    Control(PongFrame(payload)) -> {
      let payload_length = bit_array.byte_size(payload)
      make_frame(10, payload_length, payload, compressed, mask)
    }
    Control(PingFrame(payload)) -> {
      let payload_length = bit_array.byte_size(payload)
      make_frame(9, payload_length, payload, compressed, mask)
    }
    Continuation(length, payload) ->
      make_frame(0, length, payload, compressed, mask)
  }
}

fn make_length(length: Int) -> BitArray {
  case length {
    length if length > 65_535 -> <<127:7, length:int-size(64)>>
    length if length >= 126 -> <<126:7, length:int-size(16)>>
    _length -> <<length:7>>
  }
}

type Compression {
  Compressed
  Uncompressed
}

fn make_frame(
  opcode: Int,
  length: Int,
  payload: BitArray,
  compressed: Compression,
  mask: Option(BitArray),
) -> BytesTree {
  let length_section = make_length(length)

  let masked = case option.is_some(mask) {
    True -> 1
    False -> 0
  }

  let mask_key = option.unwrap(mask, <<>>)

  let compressed = case compressed {
    Compressed -> 1
    Uncompressed -> 0
  }

  <<
    1:1,
    compressed:1,
    0:2,
    opcode:4,
    masked:1,
    length_section:bits,
    mask_key:bits,
    payload:bits,
  >>
  |> bytes_tree.from_bit_array
}

pub fn apply_mask(data: BitArray, mask: Option(BitArray)) -> BitArray {
  case mask {
    Some(mask) -> {
      let assert <<
        mask1:bytes-size(1),
        mask2:bytes-size(1),
        mask3:bytes-size(1),
        mask4:bytes-size(1),
      >> = mask
      mask_data(data, [mask1, mask2, mask3, mask4], 0, <<>>)
    }
    None -> data
  }
}

pub fn apply_deflate(data: BitArray, context: Option(Context)) -> BitArray {
  case context {
    Some(context) -> compression.deflate(context, data)
    _ -> data
  }
}

pub fn apply_inflate(data: BitArray, context: Option(Context)) -> BitArray {
  case context {
    Some(context) -> compression.deflate(context, data)
    _ -> data
  }
}

fn to_frame(
  data: BitArray,
  context: Option(Context),
  mask: Option(BitArray),
  create_inner_frame: fn(BitArray) -> a,
  create_frame: fn(a) -> Frame,
) -> BytesTree {
  let frame =
    data
    |> apply_deflate(context)
    |> apply_mask(mask)
    |> create_inner_frame
    |> create_frame
  let compress = case context {
    Some(_context) -> Compressed
    _ -> Uncompressed
  }
  encode_frame(frame, compress, mask)
}

pub fn decode_many_frames(
  data: BitArray,
  context: Option(Context),
  frames: List(ParsedFrame),
) -> #(List(ParsedFrame), BitArray) {
  case decode_frame(data, context) {
    Ok(#(frame, <<>>)) -> #(list.reverse([frame, ..frames]), <<>>)
    Ok(#(frame, rest)) -> decode_many_frames(rest, context, [frame, ..frames])
    Error(NeedMoreData(rest)) -> #(list.reverse(frames), rest)
    Error(InvalidFrame) -> #(list.reverse(frames), data)
  }
}

fn append_frame(left: Frame, data: BitArray) -> Frame {
  case left {
    Data(TextFrame(payload)) -> Data(TextFrame(<<payload:bits, data:bits>>))
    Data(BinaryFrame(payload)) -> Data(BinaryFrame(<<payload:bits, data:bits>>))
    Control(CloseFrame(..)) -> left
    Control(PingFrame(payload)) ->
      Control(PingFrame(<<payload:bits, data:bits>>))
    Control(PongFrame(payload)) ->
      Control(PongFrame(<<payload:bits, data:bits>>))
    Continuation(..) -> left
  }
}

pub fn aggregate_frames(
  frames: List(ParsedFrame),
  previous: Option(Frame),
  joined: List(Frame),
) -> Result(List(Frame), Nil) {
  case frames, previous {
    [], _ -> Ok(list.reverse(joined))
    [Complete(Continuation(payload: data, ..)), ..rest], Some(prev) -> {
      let next = append_frame(prev, data)
      aggregate_frames(rest, None, [next, ..joined])
    }
    [Incomplete(Continuation(payload: data, ..)), ..rest], Some(prev) -> {
      let next = append_frame(prev, data)
      aggregate_frames(rest, Some(next), joined)
    }
    [Incomplete(frame), ..rest], None -> {
      aggregate_frames(rest, Some(frame), joined)
    }
    [Complete(frame), ..rest], None -> {
      aggregate_frames(rest, None, [frame, ..joined])
    }
    _, _ -> Error(Nil)
  }
}

const websocket_key = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

pub fn make_client_key() -> String {
  let bytes = crypto.strong_random_bytes(16)
  bit_array.base64_encode(bytes, True)
}

type ShaHash {
  Sha
}

pub fn parse_websocket_key(key: String) -> String {
  key
  |> string.append(websocket_key)
  |> crypto_hash(Sha, _)
  |> base64_encode
}

@external(erlang, "crypto", "hash")
fn crypto_hash(hash hash: ShaHash, data data: String) -> String

@external(erlang, "base64", "encode")
fn base64_encode(data data: String) -> String

fn inflate(
  compressed: Bool,
  context: Option(Context),
  data: BitArray,
) -> Result(BitArray, FrameParseError) {
  case compressed, context {
    True, Some(context) -> {
      Ok(compression.inflate(context, data))
    }
    True, None -> Error(InvalidFrame)
    _, _ -> Ok(data)
  }
}

pub fn has_deflate(extensions: List(String)) -> Bool {
  list.any(extensions, fn(str) { str == "permessage-deflate" })
}

pub fn get_context_takeovers(extensions: List(String)) -> ContextTakeover {
  let no_client_context_takeover =
    list.any(extensions, fn(str) { str == "client_no_context_takeover" })
  let no_server_context_takeover =
    list.any(extensions, fn(str) { str == "server_no_context_takeover" })
  ContextTakeover(
    no_client: no_client_context_takeover,
    no_server: no_server_context_takeover,
  )
}
