# envoy

A zero dependency cross platform Gleam package for reading environment variables.

[![Package Version](https://img.shields.io/hexpm/v/envoy)](https://hex.pm/packages/envoy)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/envoy/)

```sh
gleam add envoy
```

```gleam
import envoy

pub fn main() {
  // Get an environment variable
  envoy.get("PORT")

  // Set an environment variable
  envoy.set("PORT", "8080")

  // Unset an environment variable
  envoy.unset("PORT")

  // Get all environment variables
  envoy.all()
}
```

That's pretty much it, but documentation can be found at
<https://hexdocs.pm/envoy>.

(OK I lied, it has one dep. The standard library so we can use `Dict` for `all`).
