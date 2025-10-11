# Yielder

Unfold values on-demand from a function.

[![Package Version](https://img.shields.io/hexpm/v/gleam_yielder)](https://hex.pm/packages/gleam_yielder)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_yielder/)


You may want to use this library when you have a function that can generate a
sequence of values but the sequence is too large to fit into memory, or it would
be wasteful to do so. You could instead create a `Yielder` for it and only have
the section you are working on in memory, and halt evaluation when you have
consumed as much of the sequence as you need for your program.
```sh
gleam add gleam_yielder@1
```
```gleam
import gleam/yielder

pub fn main() {
  yielder.unfold(2, fn(acc) { yielder.Next(acc, acc * 2) })
  |> yielder.take(5)
  |> yielder.to_list
  // -> [2, 4, 8, 16, 32]
}
```

Further documentation can be found at <https://hexdocs.pm/gleam_yielder>.
