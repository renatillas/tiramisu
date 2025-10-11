# Gleam JavaScript 🌼

Work with JavaScript types and values in Gleam, including promises!

```shell
gleam add gleam_javascript@1
```
```gleam
import gleam/io
import gleam/javascript/promise

pub fn main() {
  use data <- promise.await(some_async_function())
  io.println(data)
}
```

Documentation can be found at <https://hexdocs.pm/gleam_javascript>.
