import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/result
import gleam/string
import glisten/socket.{type ListenSocket}
import glisten/socket/options.{type IpAddress, type TcpOption}
import glisten/transport.{type Transport}

pub type Message {
  Info(caller: Subject(State))
}

pub type State {
  State(listen_socket: ListenSocket, port: Int, ip_address: IpAddress)
}

pub fn start(
  port: Int,
  transport: Transport,
  options: List(TcpOption),
  name: process.Name(Message),
) -> Result(actor.Started(Subject(Message)), actor.StartError) {
  actor.new_with_initialiser(5000, fn(subject) {
    transport.listen(transport, port, options)
    |> result.try(fn(socket) {
      transport.sockname(transport, socket)
      |> result.map(fn(info) {
        State(listen_socket: socket, ip_address: info.0, port: info.1)
      })
    })
    |> result.map(fn(state) {
      state
      |> actor.initialised
      |> actor.selecting(
        process.new_selector()
        |> process.select(subject),
      )
      |> actor.returning(subject)
    })
    |> result.map_error(fn(err) {
      "Failed to start socket listener: " <> string.inspect(err)
    })
  })
  |> actor.on_message(fn(state, msg) {
    case msg {
      Info(caller) -> {
        process.send(caller, state)
        actor.continue(state)
      }
    }
  })
  |> actor.named(name)
  |> actor.start
}
