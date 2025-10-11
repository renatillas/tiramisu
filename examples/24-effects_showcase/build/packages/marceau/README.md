# marceau

[![Package Version](https://img.shields.io/hexpm/v/marceau)](https://hex.pm/packages/marceau)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/marceau/)

A MIME types library for Gleam.

```sh
gleam add marceau
```
```gleam
import marceau

pub fn main() {
  marceau.extension_to_mime_type("json")
  // -> "application/json"

  marceau.mime_type_to_extensions("application/json")
  // -> ["json"]
}
```

This library is based off of the [mimerl](https://hex.pm/packages/mimerl) Erlang
library but ported to pure Gleam so it can be used on any target and platform.
