import gleam/bit_array
import gleam/bytes_tree.{type BytesTree}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}

pub type CompressionContext

pub type Context {
  Context(context: CompressionContext, no_takeover: Bool)
}

type Flush {
  Sync
}

type Deflated {
  Deflated
}

type Default {
  Default
}

pub type ContextTakeover {
  ContextTakeover(no_client: Bool, no_server: Bool)
}

pub type Compression {
  Compression(inflate: Context, deflate: Context)
}

pub fn init(takeover: ContextTakeover) -> Compression {
  let inflate = open()
  let inflate_context =
    Context(context: inflate, no_takeover: takeover.no_client)

  inflate_init(inflate, -15)
  let deflate = open()
  let deflate_context =
    Context(context: deflate, no_takeover: takeover.no_server)
  deflate_init(deflate, Default, Deflated, -15, 8, Default)

  Compression(inflate: inflate_context, deflate: deflate_context)
}

@external(erlang, "zlib", "inflateInit")
fn inflate_init(context: CompressionContext, bits: Int) -> Atom

@external(erlang, "zlib", "deflateInit")
fn deflate_init(
  context: CompressionContext,
  level: Default,
  deflated: Deflated,
  bits: Int,
  mem_level: Int,
  strategy: Default,
) -> Atom

@external(erlang, "zlib", "open")
fn open() -> CompressionContext

@external(erlang, "zlib", "inflate")
fn do_inflate(context: CompressionContext, data: BitArray) -> BytesTree

pub fn inflate(context: Context, data: BitArray) -> BitArray {
  let output =
    context.context
    |> do_inflate(<<data:bits, 0x00, 0x00, 0xFF, 0xFF>>)
    |> bytes_tree.to_bit_array

  let _ = case context.no_takeover {
    True -> inflate_reset(context.context)
    False -> Nil
  }

  output
}

@external(erlang, "zlib", "deflate")
fn do_deflate(
  context: CompressionContext,
  data: BitArray,
  flush: Flush,
) -> BytesTree

pub fn deflate(context: Context, data: BitArray) -> BitArray {
  let data =
    context.context
    |> do_deflate(data, Sync)
    |> bytes_tree.to_bit_array

  let size = bit_array.byte_size(data) - 4

  let return = case data {
    <<value:bytes-size(size), 0x00, 0x00, 0xFF, 0xFF>> -> value
    _ -> data
  }

  let _ = case context.no_takeover {
    True -> deflate_reset(context.context)
    False -> Nil
  }

  return
}

@external(erlang, "zlib", "set_controlling_process")
pub fn set_controlling_process(context: Context, pid: Pid) -> Atom

pub fn close(context: Context) -> Nil {
  do_close(context.context)
}

@external(erlang, "zlib", "close")
fn do_close(context: CompressionContext) -> Nil

@external(erlang, "zlib", "inflateReset")
fn inflate_reset(context: CompressionContext) -> Nil

@external(erlang, "zlib", "deflateReset")
fn deflate_reset(context: CompressionContext) -> Nil
