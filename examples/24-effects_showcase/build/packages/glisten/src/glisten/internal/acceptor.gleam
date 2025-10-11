import gleam/erlang/process.{type Selector, type Subject}
import gleam/list
import gleam/option.{type Option, None}
import gleam/otp/actor
import gleam/otp/static_supervisor as supervisor
import gleam/otp/supervision
import gleam/result
import gleam/string
import glisten/internal/handler.{
  type Connection, type Loop, Handler, Internal, Ready,
}
import glisten/internal/listener
import glisten/socket.{type ListenSocket, type Socket}
import glisten/socket/options.{type TcpOption}
import glisten/transport.{type Transport}
import logging

pub type AcceptorMessage {
  AcceptConnection(ListenSocket)
}

pub type AcceptorError {
  AcceptError
  HandlerError
  ControlError
}

pub type AcceptorState {
  AcceptorState(
    sender: Subject(AcceptorMessage),
    socket: Option(Socket),
    transport: Transport,
  )
}

/// Worker process that handles `accept`ing connections and starts a new process
/// which receives the messages from the socket
pub fn start(
  pool: Pool(data, user_message),
  listener_name: process.Name(listener.Message),
) -> Result(actor.Started(Subject(AcceptorMessage)), actor.StartError) {
  actor.new_with_initialiser(1000, fn(subject) {
    let listener = process.named_subject(listener_name)

    let state = process.call(listener, 750, listener.Info)
    process.send(subject, AcceptConnection(state.listen_socket))

    AcceptorState(subject, None, pool.transport)
    |> actor.initialised
    |> actor.returning(subject)
    |> actor.selecting(
      process.new_selector()
      |> process.select(subject),
    )
    |> Ok
  })
  |> actor.on_message(fn(state, msg) {
    let AcceptorState(sender, ..) = state
    case msg {
      AcceptConnection(listener) -> {
        let res = {
          use sock <- result.try(
            transport.accept(state.transport, listener)
            |> result.replace_error(AcceptError),
          )
          use start <- result.try(
            Handler(
              socket: sock,
              loop: pool.handler,
              on_init: pool.on_init,
              on_close: pool.on_close,
              transport: pool.transport,
            )
            |> handler.start
            |> result.replace_error(HandlerError),
          )
          transport.controlling_process(state.transport, sock, start.pid)
          |> result.replace_error(ControlError)
          |> result.map(fn(_) { process.send(start.data, Internal(Ready)) })
        }
        case res {
          Error(reason) -> {
            logging.log(
              logging.Error,
              "Failed to accept/start handler: " <> string.inspect(reason),
            )
            actor.stop_abnormal("Failed to accept/start handler")
          }
          _val -> {
            actor.send(sender, AcceptConnection(listener))
            actor.continue(state)
          }
        }
      }
    }
  })
  |> actor.start
}

pub type Pool(data, user_message) {
  Pool(
    handler: Loop(data, user_message),
    pool_count: Int,
    on_init: fn(Connection(user_message)) ->
      #(data, Option(Selector(user_message))),
    on_close: Option(fn(data) -> Nil),
    transport: Transport,
  )
}

/// Starts a pool of acceptors of size `pool_count`.
///
/// Runs `loop_fn` on ever message received
pub fn start_pool(
  pool: Pool(data, user_message),
  transport: Transport,
  port: Int,
  options: List(TcpOption),
  listener_name: process.Name(listener.Message),
) -> Result(actor.Started(supervisor.Supervisor), actor.StartError) {
  let acceptors = list.range(from: 0, to: pool.pool_count)
  supervisor.new(supervisor.OneForOne)
  |> supervisor.add(
    supervision.worker(fn() {
      listener.start(port, transport, options, listener_name)
    }),
  )
  |> supervisor.add(
    supervision.supervisor(fn() {
      supervisor.new(supervisor.OneForOne)
      |> list.fold(acceptors, _, fn(sup, _index) {
        supervisor.add(
          sup,
          supervision.worker(fn() { start(pool, listener_name) }),
        )
      })
      |> supervisor.start
    }),
  )
  |> supervisor.start
}
