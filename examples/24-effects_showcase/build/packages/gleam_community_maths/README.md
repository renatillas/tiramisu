# gleam-community/maths

[![Package Version](https://img.shields.io/hexpm/v/gleam_community_maths)](https://hex.pm/packages/gleam_community_maths)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_community_maths/)

A basic mathematics library that contains some of the most fundamental mathematics functions and utilities.

The library supports both targets: Erlang and JavaScript.

## Quickstart

```gleam
import gleam/float
import gleam/list
import gleam/yielder
import gleam_community/maths
import gleeunit/should

pub fn example() {
  // Evaluate the sine function
  let result = maths.sin(maths.pi())
  // Set the relative and absolute tolerance
  let assert Ok(absolute_tol) = float.power(10.0, -6.0)
  let relative_tol = 0.0
  // Check that the value is very close to 0.0
  // That is, if 'result' is within +/- 10^(-6)
  maths.is_close(result, 0.0, relative_tol, absolute_tol)
  |> should.be_true()

  // Find the greatest common divisor
  maths.gcd(54, 24)
  |> should.equal(6)

  // Determine if 999983 is a prime number
  maths.is_prime(999_983)
  |> should.equal(True)

  // Find the minimum and maximum of a list
  maths.extrema([10.0, 3.0, 50.0, 20.0, 3.0], float.compare)
  |> should.equal(Ok(#(3.0, 50.0)))

  // Determine if a number is fractional
  maths.is_fractional(0.3333)
  |> should.equal(True)

  // Generate all k = 2 combinations of [1, 2, 3]
  let assert Ok(combinations) = maths.list_combination([1, 2, 3], 2)
  combinations
  |> yielder.to_list()
  |> should.equal([[1, 2], [1, 3], [2, 3]])

  // Compute the Cosine Similarity between two (orthogonal) "vectors"
  maths.cosine_similarity([#(-1.0, 1.0), #(1.0, 1.0), #(0.0, -1.0)])
  |> should.equal(Ok(0.0))

  // Generate a list of 3 logarithmically-spaced points over a specified 
  // interval, i.e., [10^1, 10^3]
  let assert Ok(logspace) = maths.logarithmic_space(1.0, 3.0, 3, True, 10.0)
  let pairs = logspace |> list.zip([10.0, 100.0, 1000.0])
  pairs
  |> list.all(fn(x) { x.0 == x.1 })
  |> should.be_true()
}

```

## Installation

`gleam_community` packages are published to [hex.pm](https://hex.pm/packages/gleam_community_maths)
with the prefix `gleam_community_`. You can add them to your Gleam projects directly:

```sh
gleam add gleam_community_maths
```
