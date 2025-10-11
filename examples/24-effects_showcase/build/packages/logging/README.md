# logging

Configuration for the Erlang logger.

[![Package Version](https://img.shields.io/hexpm/v/logging)](https://hex.pm/packages/logging)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/logging/)

```sh
gleam add logging
```

```gleam
import logging.{Info}

pub fn main() {
  // Run this once at the start of your program
  logging.configure()

  // And get logging!
  logging.log(Info, "Hello, Joe!")
}
```

## Disabling the colored output

When using some logger services, colored output can be superfluous, because
they're not processed at all, and appears as real characters. You can set
the `NO_COLOUR` or `NO_COLOR` environment variable to any string that _is not_
`"false"` or the empty string to disable the colored output from the logger.

Further documentation can be found at <https://hexdocs.pm/logging>.
