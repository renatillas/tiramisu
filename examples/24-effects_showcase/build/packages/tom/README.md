# tom

A Gleam TOML parser!

[![Package Version](https://img.shields.io/hexpm/v/tom)](https://hex.pm/packages/tom)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/tom/)


```sh
gleam add tom
```
```gleam
import tom

const config = "
  [person]
  name = \"Lucy\"
  is_cool = true
"

pub fn main() {
  // Parse a string of TOML
  let assert Ok(parsed) = tom.parse(config)

  // Now you can work with the data directly, or you can use the `get_*`
  // functions to retrieve values.

  tom.get_string(parsed, ["person", "name"])
  // -> Ok("Lucy")

  let is_cool = tom.get_bool(parsed, ["person", "is_cool"])
  // -> Ok(True)
}
```

Further documentation can be found at <https://hexdocs.pm/tom>.

## Status

The following string escape sequences are not supported yet:

- `\xHH`
- `\uHHHH`
- `\UHHHHHHHH`
