import gleam/bytes_tree.{type BytesTree}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import glisten/socket.{type ListenSocket, type Socket, type SocketReason}
import glisten/socket/options.{type TcpOption}

@external(erlang, "glisten_tcp_ffi", "controlling_process")
pub fn controlling_process(socket: Socket, pid: Pid) -> Result(Nil, Atom)

@external(erlang, "gen_tcp", "listen")
fn do_listen_tcp(
  port: Int,
  options: List(TcpOption),
) -> Result(ListenSocket, SocketReason)

@external(erlang, "gen_tcp", "accept")
pub fn accept_timeout(
  socket: ListenSocket,
  timeout: Int,
) -> Result(Socket, SocketReason)

@external(erlang, "gen_tcp", "accept")
pub fn accept(socket: ListenSocket) -> Result(Socket, SocketReason)

@external(erlang, "gen_tcp", "recv")
pub fn receive_timeout(
  socket: Socket,
  length: Int,
  timeout: Int,
) -> Result(BitArray, SocketReason)

@external(erlang, "gen_tcp", "recv")
pub fn receive(socket: Socket, length: Int) -> Result(BitArray, SocketReason)

@external(erlang, "glisten_tcp_ffi", "send")
pub fn send(socket: Socket, packet: BytesTree) -> Result(Nil, SocketReason)

@external(erlang, "socket", "info")
pub fn socket_info(socket: Socket) -> Dict(a, b)

@external(erlang, "glisten_tcp_ffi", "close")
pub fn close(socket: a) -> Result(Nil, SocketReason)

@external(erlang, "glisten_tcp_ffi", "shutdown")
pub fn do_shutdown(socket: Socket, write: Atom) -> Result(Nil, SocketReason)

pub fn shutdown(socket: Socket) -> Result(Nil, SocketReason) {
  do_shutdown(socket, atom.create("write"))
}

@external(erlang, "glisten_tcp_ffi", "set_opts")
fn do_set_opts(socket: Socket, opts: List(Dynamic)) -> Result(Nil, Nil)

@external(erlang, "gleam@function", "identity")
fn from(value: a) -> Dynamic

/// Update the optons for a socket (mutates the socket)
pub fn set_opts(socket: Socket, opts: List(TcpOption)) -> Result(Nil, Nil) {
  opts
  |> options.to_dict
  |> dict.to_list
  |> list.map(from)
  |> do_set_opts(socket, _)
}

/// Start listening over TCP on a port with the given options
pub fn listen(
  port: Int,
  opts: List(TcpOption),
) -> Result(ListenSocket, SocketReason) {
  opts
  |> options.merge_with_defaults
  |> do_listen_tcp(port, _)
}

pub fn handshake(socket: Socket) -> Result(Socket, Nil) {
  Ok(socket)
}

@external(erlang, "tcp", "negotiated_protocol")
pub fn negotiated_protocol(socket: Socket) -> a

@external(erlang, "inet", "peername")
pub fn peername(socket: Socket) -> Result(#(Dynamic, Int), Nil)

@external(erlang, "inet", "getopts")
pub fn get_socket_opts(
  socket: Socket,
  opts: List(Atom),
) -> Result(List(#(Atom, Dynamic)), Nil)

@external(erlang, "inet", "sockname")
pub fn sockname(socket: ListenSocket) -> Result(#(Dynamic, Int), SocketReason)
