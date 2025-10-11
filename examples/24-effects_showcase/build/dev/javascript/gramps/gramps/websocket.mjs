import * as $crypto from "../../gleam_crypto/gleam/crypto.mjs";
import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $bytes_tree from "../../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  toBitArray,
  sizedInt,
} from "../gleam.mjs";
import * as $compression from "../gramps/websocket/compression.mjs";
import { ContextTakeover } from "../gramps/websocket/compression.mjs";

export class TextFrame extends $CustomType {
  constructor(payload) {
    super();
    this.payload = payload;
  }
}

export class BinaryFrame extends $CustomType {
  constructor(payload) {
    super();
    this.payload = payload;
  }
}

export class NotProvided extends $CustomType {}

export class Normal extends $CustomType {
  constructor(body) {
    super();
    this.body = body;
  }
}

export class GoingAway extends $CustomType {
  constructor(body) {
    super();
    this.body = body;
  }
}

export class ProtocolError extends $CustomType {
  constructor(body) {
    super();
    this.body = body;
  }
}

export class UnexpectedDataType extends $CustomType {
  constructor(body) {
    super();
    this.body = body;
  }
}

export class InconsistentDataType extends $CustomType {
  constructor(body) {
    super();
    this.body = body;
  }
}

export class PolicyViolation extends $CustomType {
  constructor(body) {
    super();
    this.body = body;
  }
}

export class MessageTooBig extends $CustomType {
  constructor(body) {
    super();
    this.body = body;
  }
}

export class MissingExtensions extends $CustomType {
  constructor(body) {
    super();
    this.body = body;
  }
}

export class UnexpectedCondition extends $CustomType {
  constructor(body) {
    super();
    this.body = body;
  }
}

/**
 * Usually used for `4000` codes.
 */
export class CustomCloseReason extends $CustomType {
  constructor(code, body) {
    super();
    this.code = code;
    this.body = body;
  }
}

export class CloseFrame extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class PingFrame extends $CustomType {
  constructor(payload) {
    super();
    this.payload = payload;
  }
}

export class PongFrame extends $CustomType {
  constructor(payload) {
    super();
    this.payload = payload;
  }
}

export class Data extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Control extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Continuation extends $CustomType {
  constructor(length, payload) {
    super();
    this.length = length;
    this.payload = payload;
  }
}

export class NeedMoreData extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class InvalidFrame extends $CustomType {}

export class Complete extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Incomplete extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Compressed extends $CustomType {}

class Uncompressed extends $CustomType {}

class Sha extends $CustomType {}

function make_length(length) {
  let length$1 = length;
  if (length$1 > 65_535) {
    return toBitArray([sizedInt(127, 7, true), sizedInt(length$1, 64, true)]);
  } else {
    let length$2 = length;
    if (length$2 >= 126) {
      return toBitArray([sizedInt(126, 7, true), sizedInt(length$2, 16, true)]);
    } else {
      return toBitArray([sizedInt(length$2, 7, true)]);
    }
  }
}

function make_frame(opcode, length, payload, compressed, mask) {
  let length_section = make_length(length);
  let _block;
  let $ = $option.is_some(mask);
  if ($) {
    _block = 1;
  } else {
    _block = 0;
  }
  let masked = _block;
  let mask_key = $option.unwrap(mask, toBitArray([]));
  let _block$1;
  if (compressed instanceof Compressed) {
    _block$1 = 1;
  } else {
    _block$1 = 0;
  }
  let compressed$1 = _block$1;
  let _pipe = toBitArray([
    sizedInt(1, 1, true),
    sizedInt(compressed$1, 1, true),
    sizedInt(0, 2, true),
    sizedInt(opcode, 4, true),
    sizedInt(masked, 1, true),
    length_section,
    mask_key,
    payload,
  ]);
  return $bytes_tree.from_bit_array(_pipe);
}

function append_frame(left, data) {
  if (left instanceof Data) {
    let $ = left[0];
    if ($ instanceof TextFrame) {
      let payload = $.payload;
      return new Data(new TextFrame(toBitArray([payload, data])));
    } else {
      let payload = $.payload;
      return new Data(new BinaryFrame(toBitArray([payload, data])));
    }
  } else if (left instanceof Control) {
    let $ = left[0];
    if ($ instanceof CloseFrame) {
      return left;
    } else if ($ instanceof PingFrame) {
      let payload = $.payload;
      return new Control(new PingFrame(toBitArray([payload, data])));
    } else {
      let payload = $.payload;
      return new Control(new PongFrame(toBitArray([payload, data])));
    }
  } else {
    return left;
  }
}

export function aggregate_frames(loop$frames, loop$previous, loop$joined) {
  while (true) {
    let frames = loop$frames;
    let previous = loop$previous;
    let joined = loop$joined;
    if (frames instanceof $Empty) {
      return new Ok($list.reverse(joined));
    } else {
      let $ = frames.head;
      if ($ instanceof Complete) {
        if (previous instanceof Some) {
          let $1 = $[0];
          if ($1 instanceof Continuation) {
            let rest = frames.tail;
            let prev = previous[0];
            let data = $1.payload;
            let next = append_frame(prev, data);
            loop$frames = rest;
            loop$previous = new None();
            loop$joined = listPrepend(next, joined);
          } else {
            return new Error(undefined);
          }
        } else {
          let rest = frames.tail;
          let frame = $[0];
          loop$frames = rest;
          loop$previous = new None();
          loop$joined = listPrepend(frame, joined);
        }
      } else if (previous instanceof Some) {
        let $1 = $[0];
        if ($1 instanceof Continuation) {
          let rest = frames.tail;
          let prev = previous[0];
          let data = $1.payload;
          let next = append_frame(prev, data);
          loop$frames = rest;
          loop$previous = new Some(next);
          loop$joined = joined;
        } else {
          return new Error(undefined);
        }
      } else {
        let rest = frames.tail;
        let frame = $[0];
        loop$frames = rest;
        loop$previous = new Some(frame);
        loop$joined = joined;
      }
    }
  }
}

export function make_client_key() {
  let bytes = $crypto.strong_random_bytes(16);
  return $bit_array.base64_encode(bytes, true);
}

export function has_deflate(extensions) {
  return $list.any(
    extensions,
    (str) => { return str === "permessage-deflate"; },
  );
}

export function get_context_takeovers(extensions) {
  let no_client_context_takeover = $list.any(
    extensions,
    (str) => { return str === "client_no_context_takeover"; },
  );
  let no_server_context_takeover = $list.any(
    extensions,
    (str) => { return str === "server_no_context_takeover"; },
  );
  return new ContextTakeover(
    no_client_context_takeover,
    no_server_context_takeover,
  );
}

const websocket_key = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
