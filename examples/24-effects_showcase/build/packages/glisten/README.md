# glisten

[![Package Version](https://img.shields.io/hexpm/v/glisten)](https://hex.pm/packages/glisten)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glisten/)

A Gleam TCP server library.  Built on top of `gleam_otp`, it provides a
supervisor over a pool of socket acceptors.  Each acceptor will block on `accept`
until a connection is opened.  The acceptor will then spawn a handler process
and then block again on `accept`.

Below is a simple example that will echo received messages.  You can get this
up and running with the following:

```sh
$ gleam new <your_project>
$ cd <your_project>
$ gleam add glisten gleam_erlang gleam_otp
```

Then place this code in `src/<your_project>.gleam`:

```gleam
import gleam/bytes_tree
import gleam/erlang/process
import gleam/option.{None}
import glisten.{Packet}

pub fn main() {
  let assert Ok(_) =
    glisten.new(fn(_conn) { #(Nil, None) }, fn(state, msg, conn) {
      let assert Packet(msg) = msg
      let assert Ok(_) = glisten.send(conn, bytes_tree.from_bit_array(msg))
      glisten.continue(state)
    })
    // NOTE:  By default, `glisten` will listen on the loopback interface.  If
    // you want to listen on all interfaces, pass the following.  You can also
    // specify other interface values, including IPv6 addresses.
    // |> glisten.bind("0.0.0.0")
    |> glisten.start(3000)

  process.sleep_forever()
}
```

TLS is also handled using the `glisten.with_tls` method.  This requires a
certificate and key file path.  The rest of the handler flow remains unchanged.

`glisten` doesn't provide a public API for connected clients.  In order to hook
into the socket lifecycle, you can establish some functions which are called
for the opening and closing of the socket.  An example is provided below.

To serve over TLS:

```gleam
// ...
pub fn main() {
  let assert Ok(_) =
    glisten.new(
      // omitted
    )
    |> glisten.with_tls(
      certfile: "/path/to/server.crt",
      keyfile: "/path/to/server.key",
    )
    |> glisten.start(port: 8080)
  process.sleep_forever()
}
```

But you can also drop down to the lower level listen/accept flow if you'd prefer
to manage connections yourself, or only handle a small number at a time.

```gleam
import gleam/result
import glisten/socket/options.{ActiveMode, Passive}
import glisten/tcp

pub fn main() {
  use listener <- result.try(tcp.listen(8000, [ActiveMode(Passive)]))
  use socket <- result.try(tcp.accept(listener))
  use msg <- result.try(tcp.receive(socket, 0))
  echo #("got a msg", msg)

  Ok(Nil)
}
```

See [mist](https://github.com/rawhat/mist) for HTTP support built on top of this library.
