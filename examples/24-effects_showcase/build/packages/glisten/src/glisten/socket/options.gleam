import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/list

/// Mode for the socket.  Currently `list` is not supported
pub type SocketMode {
  Binary
}

/// Mapping to the `{active, _}` option
pub type ActiveState {
  Once
  Passive
  Count(Int)
  Active
}

pub type Interface {
  Address(IpAddress)
  Any
  Loopback
}

pub type TlsCerts {
  CertKeyFiles(certfile: String, keyfile: String)
}

/// Options for the TCP socket
pub type TcpOption {
  Backlog(Int)
  Nodelay(Bool)
  Linger(#(Bool, Int))
  SendTimeout(Int)
  SendTimeoutClose(Bool)
  Reuseaddr(Bool)
  ActiveMode(ActiveState)
  Mode(SocketMode)
  // TODO:  Probably should adjust the type here to only allow this for TLS
  CertKeyConfig(TlsCerts)
  AlpnPreferredProtocols(List(String))
  Ipv6
  Buffer(Int)
  Ip(Interface)
}

@external(erlang, "gleam@function", "identity")
fn from(value: a) -> Dynamic

pub fn to_dict(options: List(TcpOption)) -> Dict(Dynamic, Dynamic) {
  let opt_decoder = {
    use opt <- decode.field(0, decode.dynamic)
    use value <- decode.field(1, decode.dynamic)
    decode.success(#(opt, value))
  }

  let active = atom.create("active")
  let ip = atom.create("ip")

  options
  |> list.map(fn(opt) {
    case opt {
      ActiveMode(Passive) -> from(#(active, False))
      ActiveMode(Active) -> from(#(active, True))
      ActiveMode(Count(n)) -> from(#(active, n))
      ActiveMode(Once) -> from(#(active, atom.create("once")))
      Ip(Address(IpV4(a, b, c, d))) -> from(#(ip, from(#(a, b, c, d))))
      Ip(Address(IpV6(a, b, c, d, e, f, g, h))) ->
        from(#(ip, from(#(a, b, c, d, e, f, g, h))))
      Ip(Any) -> from(#(ip, atom.create("any")))
      Ip(Loopback) -> from(#(ip, atom.create("loopback")))
      Ipv6 -> from(atom.create("inet6"))
      CertKeyConfig(CertKeyFiles(certfile, keyfile)) -> {
        from(
          #(atom.create("certs_keys"), [
            dict.from_list([
              #(atom.create("certfile"), certfile),
              #(atom.create("keyfile"), keyfile),
            ]),
          ]),
        )
      }
      other -> from(other)
    }
  })
  |> list.filter_map(decode.run(_, opt_decoder))
  |> dict.from_list
}

pub const default_options = [
  Backlog(1024),
  Nodelay(True),
  SendTimeout(30_000),
  SendTimeoutClose(True),
  Reuseaddr(True),
  Mode(Binary),
  ActiveMode(Passive),
]

pub fn merge_with_defaults(options: List(TcpOption)) -> List(TcpOption) {
  let overrides = to_dict(options)

  let has_ipv6 = list.contains(options, Ipv6)

  default_options
  |> to_dict
  |> dict.merge(overrides)
  |> dict.to_list
  |> list.map(from)
  |> fn(opts) {
    case has_ipv6 {
      True -> [from(atom.create("inet6")), ..opts]
      _ -> opts
    }
  }
  |> unsafe_coerce
}

@external(erlang, "gleam@function", "identity")
fn unsafe_coerce(value: a) -> b

pub type IpAddress {
  IpV4(Int, Int, Int, Int)
  IpV6(Int, Int, Int, Int, Int, Int, Int, Int)
}
