# booklet

[![Package Version](https://img.shields.io/hexpm/v/booklet)](https://hex.pm/packages/booklet)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/booklet/)

Booklet is a simple, global cache optimised for concurrent accesses.

Booklets are designed for caching values that persist throughout your program's
lifetime. Booklets can never be deleted, but offer thread-safe, atomic updates
and fast concurrent reads.

Booklet is implemented using atomic compare-and-swap operations on top of a
managed ETS table on Erlang, or a simple mutable reference on Javascript.

### Installation

```sh
gleam add booklet@1
```

### Usage

```gleam
import booklet
import gleam/io

pub fn main() -> Nil {
  // Create a globally unique, global cache
  let booklet = booklet.new("Hello, World!")

  // Query the current value
  io.println(booklet.get(booklet))
  // --> Hello, World!

  // Update the value
  io.println(booklet.update(booklet, fn(msg) {
    msg <> "!!"
  }))
  // --> "Hello, World!!!"
}
```

### Concurrency

```gleam
import booklet
import gleam/io
import gleam/int
import gleam/list
import gleam/erlang/process

pub fn main() -> Nil {
  let booklet = booklet.new(0)

  list.range(1, 10_000)
  |> list.each(fn(n) {
    use <- process.spawn
    booklet.update(booklet, fn(x) { x + 1})
  })

  // wait some time to allow processes to exit
  process.sleep(2000)

  // guaranteed to print 10_000 (if all processes exited)
  io.println(int.to_string(booklet.get(booklet)))
}
```

Further documentation can be found at <https://hexdocs.pm/booklet>.
