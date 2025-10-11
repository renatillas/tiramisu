# filepath

Work with file paths in Gleam!

[![Package Version](https://img.shields.io/hexpm/v/filepath)](https://hex.pm/packages/filepath)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/filepath/)

This package works on both Unix and Windows systems, and can run on both Erlang
or JavaScript runtimes.

```sh
gleam add filepath
```
```gleam
import filepath

pub fn main() {
  let path = filepath.join("/home/lucy", "pokemon-cards")
  // -> "/home/lucy/pokemon-cards"
}
```

Documentation can be found here: <https://hexdocs.pm/filepath>.
