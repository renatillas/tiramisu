# platform
Zero dependency platform detection library for gleam

[![Package Version](https://img.shields.io/hexpm/v/platform)](https://hex.pm/packages/platform)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/platform/)

```sh
gleam add platform@1
```
```gleam
import gleam/io
import platform

pub fn main() {
  io.debug(platform.runtime()) // Erlang, Node, Deno, Bun, Browser
  io.debug(platform.arch()) // X86, X64, Arm, Arm64
  io.debug(platform.os()) // Win32, Linux, Darwin, FreeBsd, OpenBsd
}
```

Further documentation can be found at <https://hexdocs.pm/platform>.
