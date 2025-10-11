# houdini

[![Package Version](https://img.shields.io/hexpm/v/houdini)](https://hex.pm/packages/houdini)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/houdini/)

🪄 Fast HTML escaping

```sh
gleam add houdini@1
```

Use `houdini.escape` to escape any string to be safely used inside an HTML
document:

```gleam
import houdini

pub fn main() -> Nil {
  assert houdini.escape("wibble & wobble") == "wibble &amp; wobble"
  assert houdini.escape("wibble > wobble") == "wibble &gt; wobble"
}
```
