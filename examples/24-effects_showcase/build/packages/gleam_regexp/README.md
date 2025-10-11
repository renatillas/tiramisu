# Regexp

Regular expressions in Gleam!

[![Package Version](https://img.shields.io/hexpm/v/gleam_regexp)](https://hex.pm/packages/gleam_regexp)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_regexp/)

```sh
gleam add gleam_regexp@1
```
```gleam
import gleam/regexp

pub fn main() {
  let assert Ok(re) = regexp.from_string("[0-9]")

  regexp.check(re, "abc123")
  // -> True

  regexp.check(re, "abcxyz")
  // -> False
}
```

This package uses the regular expression engine of the underlying platform.
Regular expressions in Erlang and JavaScript largely share the same syntax, but
there are some differences and have different performance characteristics. Be
sure to thoroughly test your code on all platforms that you support when using
this library.

Further documentation can be found at <https://hexdocs.pm/gleam_regexp>.
