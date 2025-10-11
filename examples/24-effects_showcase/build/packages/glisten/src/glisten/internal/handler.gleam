import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Selector, type Subject}
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import glisten/socket.{type Socket}
import glisten/socket/options.{type IpAddress}
import glisten/transport.{type Transport}
import logging

@external(erlang, "glisten_ffi", "rescue")
fn rescue(func: fn() -> anything) -> Result(anything, Dynamic)

/// All message types that the handler will receive, or that you can
/// send to the handler process
pub type InternalMessage {
  Close
  Ready
  ReceiveMessage(BitArray)
  SslClosed
  TcpClosed
}

pub type Message(user_message) {
  Internal(InternalMessage)
  User(user_message)
}

pub type LoopMessage(user_message) {
  Packet(BitArray)
  Custom(user_message)
}

pub type ClientIp =
  Result(#(IpAddress, Int), Nil)

pub type LoopState(state, user_message) {
  LoopState(
    client_ip: ClientIp,
    socket: Socket,
    sender: Subject(Message(user_message)),
    transport: Transport,
    state: state,
  )
}

pub type Connection(user_message) {
  Connection(
    client_ip: ClientIp,
    socket: Socket,
    transport: Transport,
    sender: Subject(Message(user_message)),
  )
}

pub type Next(user_state, user_message) {
  Continue(state: user_state, selector: Option(Selector(user_message)))
  NormalStop
  AbnormalStop(reason: String)
}

pub fn continue(state: user_state) -> Next(user_state, user_message) {
  Continue(state, None)
}

pub fn with_selector(
  next: Next(user_state, user_message),
  selector: Selector(user_message),
) -> Next(user_state, user_message) {
  case next {
    Continue(state, _) -> Continue(state, Some(selector))
    stop -> stop
  }
}

pub fn stop() -> Next(user_state, user_message) {
  NormalStop
}

pub fn stop_abnormal(reason: String) -> Next(user_state, user_message) {
  AbnormalStop(reason)
}

pub type Loop(state, user_message) =
  fn(state, LoopMessage(user_message), Connection(user_message)) ->
    Next(state, LoopMessage(user_message))

pub type Handler(state, user_message) {
  Handler(
    socket: Socket,
    loop: Loop(state, user_message),
    on_init: fn(Connection(user_message)) ->
      #(state, Option(Selector(user_message))),
    on_close: Option(fn(state) -> Nil),
    transport: Transport,
  )
}

/// Starts an actor for the TCP connection
pub fn start(
  handler: Handler(state, user_message),
) -> Result(actor.Started(Subject(Message(user_message))), actor.StartError) {
  actor.new_with_initialiser(1000, fn(subject) {
    let client_ip =
      transport.peername(handler.transport, handler.socket)
      |> result.replace_error(Nil)
    let connection =
      Connection(
        socket: handler.socket,
        client_ip: client_ip,
        transport: handler.transport,
        sender: subject,
      )
    let #(initial_state, user_selector) = handler.on_init(connection)
    let base_selector = process.new_selector() |> process.select(subject)
    let selector =
      process.new_selector()
      |> process.select_record(atom.create("tcp"), 2, fn(record) {
        {
          use data <- decode.field(2, decode.bit_array)
          decode.success(ReceiveMessage(data))
        }
        |> decode.run(record, _)
        |> result.unwrap(ReceiveMessage(<<>>))
      })
      |> process.select_record(atom.create("ssl"), 2, fn(record) {
        {
          use data <- decode.field(2, decode.bit_array)
          decode.success(ReceiveMessage(data))
        }
        |> decode.run(record, _)
        |> result.unwrap(ReceiveMessage(<<>>))
      })
      |> process.select_record(atom.create("ssl_closed"), 1, fn(_nil) {
        SslClosed
      })
      |> process.select_record(atom.create("tcp_closed"), 1, fn(_nil) {
        TcpClosed
      })
      |> process.map_selector(Internal)
      |> process.merge_selector(base_selector)
    let selector = case user_selector {
      Some(sel) ->
        sel
        |> process.map_selector(User)
        |> process.merge_selector(selector, _)
      _ -> selector
    }
    LoopState(
      client_ip: client_ip,
      socket: handler.socket,
      sender: subject,
      transport: handler.transport,
      state: initial_state,
    )
    |> actor.initialised()
    |> actor.selecting(selector)
    |> actor.returning(subject)
    |> Ok
  })
  |> actor.on_message(fn(state, msg) {
    let connection =
      Connection(
        socket: state.socket,
        client_ip: state.client_ip,
        transport: state.transport,
        sender: state.sender,
      )
    case msg {
      Internal(TcpClosed) | Internal(SslClosed) | Internal(Close) ->
        case transport.close(state.transport, state.socket) {
          Ok(Nil) -> {
            let _ = case handler.on_close {
              Some(on_close) -> on_close(state.state)
              _ -> Nil
            }
            actor.stop()
          }
          Error(err) -> actor.stop_abnormal(string.inspect(err))
        }
      Internal(Ready) ->
        state.socket
        |> transport.handshake(state.transport, _)
        |> result.replace_error("Failed to handshake socket")
        |> result.try(fn(_ok) {
          let _ =
            transport.set_buffer_size(state.transport, state.socket)
            |> result.map_error(fn(err) {
              logging.log(
                logging.Warning,
                "Failed to read `recbuf` size, using default: "
                  <> string.inspect(err),
              )
            })
          Ok(Nil)
        })
        |> result.try(fn(_ok) {
          transport.set_opts(state.transport, state.socket, [
            options.ActiveMode(options.Once),
          ])
          |> result.replace_error("Failed to set socket active")
        })
        |> result.replace(actor.continue(state))
        |> result.map_error(actor.stop_abnormal)
        |> result.unwrap_both
      User(msg) -> {
        let msg = Custom(msg)
        let res = rescue(fn() { handler.loop(state.state, msg, connection) })
        case res {
          Ok(Continue(next_state, _selector)) -> {
            let assert Ok(Nil) =
              transport.set_opts(state.transport, state.socket, [
                options.ActiveMode(options.Once),
              ])
            actor.continue(LoopState(..state, state: next_state))
          }
          Ok(NormalStop) -> actor.stop()
          Ok(AbnormalStop(reason)) -> actor.stop_abnormal(reason)
          Error(reason) -> {
            logging.log(
              logging.Error,
              "Caught error in user handler: " <> string.inspect(reason),
            )
            actor.continue(state)
          }
        }
      }
      Internal(ReceiveMessage(msg)) -> {
        let msg = Packet(msg)
        let res = rescue(fn() { handler.loop(state.state, msg, connection) })
        case res {
          Ok(Continue(next_state, _selector)) -> {
            let assert Ok(Nil) =
              transport.set_opts(state.transport, state.socket, [
                options.ActiveMode(options.Once),
              ])
            actor.continue(LoopState(..state, state: next_state))
          }
          Ok(NormalStop) -> actor.stop()
          Ok(AbnormalStop(reason)) -> actor.stop_abnormal(reason)
          Error(reason) -> {
            logging.log(
              logging.Error,
              "Caught error in user handler: " <> string.inspect(reason),
            )
            actor.continue(state)
          }
        }
      }
    }
  })
  |> actor.start()
}
