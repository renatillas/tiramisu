////<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css" integrity="sha384-nB0miv6/jRmo5UMMR1wu3Gz6NLsoTkbqJghGIsx//Rlm+ZU03BU6SQNC66uf4l5+" crossorigin="anonymous">
////<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js" integrity="sha384-7zkQWkzuo3B5mTepMUcHkMB5jZaolc2xDwL6VFqjFALcbeS9Ggm/Yr2r3Dy4lfFg" crossorigin="anonymous"></script>
////<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js" integrity="sha384-43gviWU0YVjaDtb/GhzOouOXtZMP/7XUzwPTstBeZFe/+rCMvRwr4yROQP43s0Xk" crossorigin="anonymous"></script>
////<script>
////    document.addEventListener("DOMContentLoaded", function() {
////        renderMathInElement(document.body, {
////          // customised options
////          // • auto-render specific keys, e.g.:
////          delimiters: [
////              {left: '$$', right: '$$', display: false},
////              {left: '$', right: '$', display: false},
////              {left: '\\(', right: '\\)', display: false},
////              {left: '\\[', right: '\\]', display: true}
////          ],
////          // • rendering keys, e.g.:
////          throwOnError : true
////        });
////    });
////</script>
////<style>
////    .katex { font-size: 1.10em; }
////</style>
////

import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/order
import gleam/set
import gleam/yielder.{type Yielder, Done, Next}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function calculates the greatest common divisor of two integers
/// \\(x, y \in \mathbb{Z}\\). The greatest common divisor is the largest positive
/// integer that is divisible by both \\(x\\) and \\(y\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.gcd(1, 1)
///       |> should.equal(1)
///
///       maths.gcd(100, 10)
///       |> should.equal(10)
///
///       maths.gcd(-36, -17)
///       |> should.equal(1)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn gcd(x: Int, y: Int) -> Int {
  let absx = int.absolute_value(x)
  let absy = int.absolute_value(y)

  do_gcd(absx, absy)
}

fn do_gcd(x: Int, y: Int) -> Int {
  case x == 0 {
    True -> y
    False -> do_gcd(y % x, x)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
///
/// Given two integers, \\(x\\) (dividend) and \\(y\\) (divisor), the Euclidean modulo
/// of \\(x\\) by \\(y\\), denoted as \\(x \mod y\\), is the remainder \\(r\\) of the
/// division of \\(x\\) by \\(y\\), such that:
///
/// \\[
/// x = q \cdot y + r \quad \text{and} \quad 0 \leq r < |y|,
/// \\]
///
/// where \\(q\\) is an integer that represents the quotient of the division.
///
/// Note that like the Gleam division operator `/` this will return `0` if one of
/// the arguments is `0`.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.euclidean_modulo(15, 4)
///       |> should.equal(3)
///
///       maths.euclidean_modulo(-3, -2)
///       |> should.equal(1)
///
///       maths.euclidean_modulo(5, 0)
///       |> should.equal(0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn euclidean_modulo(x: Int, y: Int) -> Int {
  case x % y, x, y {
    _, 0, _ -> 0
    _, _, 0 -> 0
    md, _, _ if md < 0 -> md + int.absolute_value(y)
    md, _, _ -> md
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function calculates the least common multiple of two integers
/// \\(x, y \in \mathbb{Z}\\). The least common multiple is the smallest positive
/// integer that has both \\(x\\) and \\(y\\) as factors.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.lcm(1, 1)
///       |> should.equal(1)
///
///       maths.lcm(100, 10)
///       |> should.equal(100)
///
///       maths.lcm(-36, -17)
///       |> should.equal(612)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn lcm(x: Int, y: Int) -> Int {
  let absx = int.absolute_value(x)
  let absy = int.absolute_value(y)

  absx * absy / do_gcd(absx, absy)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function returns all the positive divisors of an integer, including the
/// number itself.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.divisors(4)
///       |> should.equal([1, 2, 4])
///
///       maths.divisors(6)
///       |> should.equal([1, 2, 3, 6])
///
///       maths.divisors(13)
///       |> should.equal([1, 13])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn divisors(n: Int) -> List(Int) {
  find_divisors(n)
  |> set.to_list()
  |> list.sort(int.compare)
}

fn find_divisors(n: Int) -> set.Set(Int) {
  let nabs = float.absolute_value(int.to_float(n))
  // Usage of let assert: 'nabs' is non-negative so no error should occur. The
  // function `float.square_root` will only return an error in case a negative
  // value is given as input.
  let assert Ok(sqrt_result) = float.square_root(nabs)
  let max = float.round(sqrt_result) + 1

  do_find_divisors(n, max, set.new(), 1)
}

fn do_find_divisors(n: Int, max: Int, acc: set.Set(Int), i: Int) -> set.Set(Int) {
  case i <= max {
    False -> acc
    True -> {
      let updated_acc = case n % i == 0 {
        True -> set.insert(acc, i) |> set.insert(n / i)
        False -> acc
      }

      do_find_divisors(n, max, updated_acc, i + 1)
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function returns all the positive divisors of an integer, excluding the
/// number itself.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.proper_divisors(4)
///       |> should.equal([1, 2])
///
///       maths.proper_divisors(6)
///       |> should.equal([1, 2, 3])
///
///       maths.proper_divisors(13)
///       |> should.equal([1])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn proper_divisors(n: Int) -> List(Int) {
  find_divisors(n)
  |> set.delete(n)
  |> set.to_list()
  |> list.sort(int.compare)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the weighted sum of the elements in a list:
///
/// \\[
/// \sum_{i=1}^n w_i x_i
/// \\]
///
/// In the formula, \\(n\\) is the length of the list and \\(x_i \in \mathbb{R}\\)
/// is the value in the input list indexed by \\(i\\), while the \\(w_i \in \mathbb{R}\\)
/// are corresponding positive weights.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/float
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.weighted_sum()
///       |> should.equal(Ok(0.0))
///
///       [#(1.0, 1.0), #(2.0, 1.0), #(3.0, 1.0)]
///       |> maths.weighted_sum()
///       |> should.equal(Ok(6.0))
///
///       [#(9.0, 0.5), #(10.0, 0.5), #(10.0, 0.5)]
///       |> maths.weighted_sum()
///       |> should.equal(Ok(14.5))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn weighted_sum(arr: List(#(Float, Float))) -> Result(Float, Nil) {
  case arr {
    [] -> Ok(0.0)
    _ -> {
      use acc, tuple <- list.try_fold(arr, 0.0)

      case tuple.1 <. 0.0 {
        True -> Error(Nil)
        False -> Ok(tuple.0 *. tuple.1 +. acc)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the weighted product of the elements in a list:
///
/// \\[
/// \prod_{i=1}^n x_i^{w_i}
/// \\]
///
/// In the formula, \\(n\\) is the length of the list and \\(x_i \in \mathbb{R}\\) is
/// the value in the input list indexed by \\(i\\), while the \\(w_i \in \mathbb{R}\\)
/// are corresponding positive weights.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/float
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.weighted_product()
///       |> should.equal(Ok(1.0))
///
///       [#(1.0, 1.0), #(2.0, 1.0), #(3.0, 1.0)]
///       |> maths.weighted_product()
///       |> should.equal(Ok(6.0))
///
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///       let assert Ok(result) =
///         [#(9.0, 0.5), #(10.0, 0.5), #(10.0, 0.5)]
///         |> maths.weighted_product()
///       result
///       |> maths.is_close(30.0, 0.0, tolerance)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn weighted_product(arr: List(#(Float, Float))) -> Result(Float, Nil) {
  case arr {
    [] -> Ok(1.0)
    _ -> {
      use acc, tuple <- list.try_fold(arr, 1.0)

      case tuple.1 <. 0.0 {
        True -> Error(Nil)
        False ->
          case float.power(tuple.0, tuple.1) {
            Error(Nil) -> Error(Nil)
            Ok(value) -> Ok(value *. acc)
          }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the cumulative sum of the elements in a list:
///
/// \\[
/// v_j = \sum_{i=1}^j x_i \\;\\; \forall j = 1,\dots, n
/// \\]
///
/// In the formula, \\(v_j\\) is the \\(j\\)'th element in the cumulative sum of \\(n\\)
/// elements. That is, \\(n\\) is the length of the list and \\(x_i \in \mathbb{R}\\)
/// is the value in the input list indexed by \\(i\\). The value \\(v_j\\) is thus the
/// sum of the \\(1\\) to \\(j\\) first elements in the given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.cumulative_sum()
///       |> should.equal([])
///
///       [1.0, 2.0, 3.0]
///       |> maths.cumulative_sum()
///       |> should.equal([1.0, 3.0, 6.0])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn cumulative_sum(arr: List(Float)) -> List(Float) {
  case arr {
    [] -> []
    _ -> list.scan(arr, 0.0, fn(acc, element) { element +. acc })
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the cumulative sum of the elements in a list:
///
/// \\[
/// v_j = \sum_{i=1}^j x_i \\;\\; \forall j = 1,\dots, n
/// \\]
///
/// In the formula, \\(v_j\\) is the \\(j\\)'th element in the cumulative sum of \\(n\\)
/// elements. That is, \\(n\\) is the length of the list and \\(x_i \in \mathbb{Z}\\)
/// is the value in the input list indexed by \\(i\\). The value \\(v_j\\) is thus the
/// sum of the \\(1\\) to \\(j\\) first elements in the given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.int_cumulative_sum()
///       |> should.equal([])
///
///       [1, 2, 3]
///       |> maths.int_cumulative_sum()
///       |> should.equal([1, 3, 6])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn int_cumulative_sum(arr: List(Int)) -> List(Int) {
  case arr {
    [] -> []
    _ -> list.scan(arr, 0, fn(acc, element) { element + acc })
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the cumulative product of the elements in a list:
///
/// \\[
/// v_j = \prod_{i=1}^j x_i \\;\\; \forall j = 1,\dots, n
/// \\]
///
/// In the formula, \\(v_j\\) is the \\(j\\)'th element in the cumulative product
/// of \\(n\\) elements. That is, \\(n\\) is the length of the list and
/// \\(x_i \in \mathbb{R}\\) is the value in the input list indexed by \\(i\\).
/// The value \\(v_j\\) is thus the sum of the \\(1\\) to \\(j\\) first elements
/// in the given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.cumulative_product()
///       |> should.equal([])
///
///       [1.0, 2.0, 3.0]
///       |> maths.cumulative_product()
///       |> should.equal([1.0, 2.0, 6.0])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn cumulative_product(arr: List(Float)) -> List(Float) {
  case arr {
    [] -> []
    _ -> list.scan(arr, 1.0, fn(acc, element) { element *. acc })
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the cumulative product of the elements in a list:
///
/// \\[
/// v_j = \prod_{i=1}^j x_i \\;\\; \forall j = 1,\dots, n
/// \\]
///
/// In the formula, \\(v_j\\) is the \\(j\\)'th element in the cumulative product
/// of \\(n\\) elements. That is, \\(n\\) is the length of the list and
/// \\(x_i \in \mathbb{Z}\\) is the value in the input list indexed by \\(i\\).
/// The value \\(v_j\\) is thus the product of the \\(1\\) to \\(j\\) first elements
/// in the given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.int_cumulative_product()
///       |> should.equal([])
///
///       [1, 2, 3]
///       |> maths.int_cumulative_product()
///       |> should.equal([1, 2, 6])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn int_cumulative_product(arr: List(Int)) -> List(Int) {
  case arr {
    [] -> []
    _ -> list.scan(arr, 1, fn(acc, element) { element * acc })
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Convert a value in degrees to a value measured in radians.
/// That is, \\(1 \text{ degrees } = \frac{\pi}{180} \text{ radians }\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.degrees_to_radians(360.)
///       |> should.equal(2. *. maths.pi())
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn degrees_to_radians(x: Float) -> Float {
  x *. do_pi() /. 180.0
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Convert a value in degrees to a value measured in radians.
/// That is, \\(1 \text{ radians } = \frac{180}{\pi} \text{ degrees }\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.radians_to_degrees(0.0)
///       |> should.equal(0.0)
///
///       maths.radians_to_degrees(2. *. maths.pi())
///       |> should.equal(360.)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn radians_to_degrees(x: Float) -> Float {
  x *. 180.0 /. do_pi()
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Converts polar coordinates \\((r, \theta)\\) to Cartesian coordinates \\((x, y)\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.polar_to_cartesian(1.0, 0.0)
///       |> should.equal(#(1.0, 0.0))
///
///       maths.polar_to_cartesian(1.0, float.pi() /. 2.0)
///       |> should.equal(#(0.0, 1.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn polar_to_cartesian(r: Float, theta: Float) -> #(Float, Float) {
  // Calculate x and y
  let x = r *. cos(theta)
  let y = r *. sin(theta)

  #(x, y)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Converts Cartesian coordinates \\((x, y)\\) to polar coordinates \\((r, \theta)\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.cartesian_to_polar(1.0, 0.0)
///       |> should.equal((1.0, 0.0))
///
///       maths.cartesian_to_polar(0.0, 1.0)
///       |> should.equal((1.0, float.pi() /. 2.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn cartesian_to_polar(x: Float, y: Float) -> #(Float, Float) {
  // Calculate 'r' and 'theta'
  // Usage of let assert: a sum of squares is always non-negative so no error
  // should occur, i.e., the function `float.square_root` will only return an
  // error in case a negative value is given as input.
  let assert Ok(r) = float.square_root(x *. x +. y *. y)
  let theta = atan2(y, x)

  #(r, theta)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse cosine function:
///
/// \\[
/// \forall x \in \[-1, 1\],   \\; \cos^{-1}{(x)} = y \in \[0, \pi \]
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\[-1, 1\]\\) as input and
/// returns a numeric value \\(y\\) that lies in the range \\(\[0, \pi \]\\) (an
/// angle in radians). If the input value is outside the domain of the function
/// an error is returned.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.acos(1.0)
///       |> should.equal(Ok(0.0))
///
///       maths.acos(1.1)
///       |> should.be_error()
///
///       maths.acos(-1.1)
///       |> should.be_error()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn acos(x: Float) -> Result(Float, Nil) {
  case x >=. -1.0 && x <=. 1.0 {
    True -> Ok(do_acos(x))
    False -> Error(Nil)
  }
}

@external(erlang, "math", "acos")
@external(javascript, "../maths.mjs", "acos")
fn do_acos(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse hyperbolic cosine function:
///
/// \\[
/// \forall x \in [1, +\infty\),   \\; \cosh^{-1}{(x)} = y \in \[0, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\[1, +\infty\)\\) as input
/// and returns a numeric value \\(y\\) that lies in the range \\(\[0, +\infty\)\\)
/// (an angle in radians). If the input value is outside the domain of the function
/// an error is returned.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.acosh(1.0)
///       |> should.equal(Ok(0.0))
///
///       maths.acosh(0.0)
///       |> should.be_error()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn acosh(x: Float) -> Result(Float, Nil) {
  case x >=. 1.0 {
    True -> Ok(do_acosh(x))
    False -> Error(Nil)
  }
}

@external(erlang, "math", "acosh")
@external(javascript, "../maths.mjs", "acosh")
fn do_acosh(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse sine function:
///
/// \\[
/// \forall x \in \[-1, 1\],   \\; \sin^{-1}{(x)} = y \in \(-\infty, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\[-1, 1\]\\) as input and returns a numeric
/// value \\(y\\) that lies in the range \\(\[-\frac{\pi}{2}, \frac{\pi}{2}\]\\) (an angle in
/// radians). If the input value is outside the domain of the function an error is returned.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.asin(0.0)
///       |> should.equal(Ok(0.0))
///
///       maths.asin(1.1)
///       |> should.be_error()
///
///       maths.asin(-1.1)
///       |> should.be_error()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn asin(x: Float) -> Result(Float, Nil) {
  case x >=. -1.0 && x <=. 1.0 {
    True -> Ok(do_asin(x))
    False -> Error(Nil)
  }
}

@external(erlang, "math", "asin")
@external(javascript, "../maths.mjs", "asin")
fn do_asin(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse hyperbolic sine function:
///
/// \\[
/// \forall x \in \(-\infty, \infty\),   \\; \sinh^{-1}{(x)} = y \in \(-\infty, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(-\infty, +\infty\)\\)
/// as input and returns a numeric value \\(y\\) that lies in the range
/// \\(\(-\infty, +\infty\)\\) (an angle in radians).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.asinh(0.0)
///       |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn asinh(x: Float) -> Float {
  do_asinh(x)
}

@external(erlang, "math", "asinh")
@external(javascript, "../maths.mjs", "asinh")
fn do_asinh(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse tangent function:
///
/// \\[
/// \forall x \in \(-\infty, \infty\),  \\; \tan^{-1}{(x)} = y \in \[-\frac{\pi}{2}, \frac{\pi}{2}\]
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(-\infty, +\infty\)\\) as input and
/// returns a numeric value \\(y\\) that lies in the range \\(\[-\frac{\pi}{2}, \frac{\pi}{2}\]\\)
/// (an angle in radians).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.atan(0.0)
///       |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn atan(x: Float) -> Float {
  do_atan(x)
}

@external(erlang, "math", "atan")
@external(javascript, "../maths.mjs", "atan")
fn do_atan(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse 2-argument tangent function:
///
/// \\[
/// \text{atan2}(y, x) =
/// \begin{cases}
///  \tan^{-1}(\frac y x) &\text{if } x > 0, \\\\
///  \tan^{-1}(\frac y x) + \pi &\text{if } x < 0 \text{ and } y \ge 0, \\\\
///  \tan^{-1}(\frac y x) - \pi &\text{if } x < 0 \text{ and } y < 0, \\\\
///  +\frac{\pi}{2} &\text{if } x = 0 \text{ and } y > 0, \\\\
///  -\frac{\pi}{2} &\text{if } x = 0 \text{ and } y < 0, \\\\
///  \text{undefined} &\text{if } x = 0 \text{ and } y = 0.
/// \end{cases}
/// \\]
///
/// The function returns the angle in radians from the x-axis to the line containing
/// the origin \\(\(0, 0\)\\) and a point given as input with coordinates \\(\(x, y\)\\).
/// The numeric value returned by \\(\text{atan2}(y, x)\\) is in the range
/// \\(\[-\pi, \pi\]\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.atan2(0.0, 0.0)
///       |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn atan2(y: Float, x: Float) -> Float {
  do_atan2(y, x)
}

@external(erlang, "math", "atan2")
@external(javascript, "../maths.mjs", "atan2")
fn do_atan2(a: Float, b: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The inverse hyperbolic tangent function:
///
/// \\[
/// \forall x \in \(-1, 1\),   \\; \tanh^{-1}{(x)} = y \in \(-\infty, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(-1, 1\)\\) as input and returns
/// a numeric value \\(y\\) that lies in the range \\(\(-\infty, \infty\)\\) (an angle in radians).
/// If the input value is outside the domain of the function an error is returned.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.atanh(0.0)
///       |> should.equal(Ok(0.0))
///
///       maths.atanh(1.0)
///       |> should.be_error()
///
///       maths.atanh(-1.0)
///       |> should.be_error()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn atanh(x: Float) -> Result(Float, Nil) {
  case x >. -1.0 && x <. 1.0 {
    True -> Ok(do_atanh(x))
    False -> Error(Nil)
  }
}

@external(erlang, "math", "atanh")
@external(javascript, "../maths.mjs", "atanh")
fn do_atanh(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The cosine function:
///
/// \\[
/// \forall x \in \(-\infty, +\infty\),   \\; \cos{(x)} = y \in \[-1, 1\]
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(-\infty, \infty\)\\) (an angle in
/// radians) as input and returns a numeric value \\(y\\) that lies in the range \\(\[-1, 1\]\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.cos(0.0)
///       |> should.equal(1.0)
///
///       maths.cos(maths.pi())
///       |> should.equal(-1.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn cos(x: Float) -> Float {
  do_cos(x)
}

@external(erlang, "math", "cos")
@external(javascript, "../maths.mjs", "cos")
fn do_cos(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The hyperbolic cosine function:
///
/// \\[
/// \forall x \in \(-\infty, \infty\),   \\; \cosh{(x)} = y \in \(-\infty, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(-\infty, \infty\)\\) as input (an angle
/// in radians) and returns a numeric value \\(y\\) that lies in the range
/// \\(\(-\infty, \infty\)\\). If the input value is too large an overflow error might occur.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.cosh(0.0)
///       |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn cosh(x: Float) -> Float {
  do_cosh(x)
}

@external(erlang, "math", "cosh")
@external(javascript, "../maths.mjs", "cosh")
fn do_cosh(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The sine function:
///
/// \\[
/// \forall x \in \(-\infty, +\infty\),   \\; \sin{(x)} = y \in \[-1, 1\]
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(-\infty, \infty\)\\) (an angle in
/// radians) as input and returns a numeric value \\(y\\) that lies in the range \\(\[-1, 1\]\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.sin(0.0)
///       |> should.equal(0.0)
///
///       maths.sin(0.5 *. maths.pi())
///       |> should.equal(1.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn sin(x: Float) -> Float {
  do_sin(x)
}

@external(erlang, "math", "sin")
@external(javascript, "../maths.mjs", "sin")
fn do_sin(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The hyperbolic sine function:
///
/// \\[
/// \forall x \in \(-\infty, +\infty\),   \\; \sinh{(x)} = y \in \(-\infty, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(-\infty, +\infty\)\\) as input
/// (an angle in radians) and returns a numeric value \\(y\\) that lies in the range
/// \\(\(-\infty, +\infty\)\\). If the input value is too large an overflow error might occur.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.sinh(0.0)
///       |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn sinh(x: Float) -> Float {
  do_sinh(x)
}

@external(erlang, "math", "sinh")
@external(javascript, "../maths.mjs", "sinh")
fn do_sinh(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The tangent function:
///
/// \\[
/// \forall x \in \(-\infty, +\infty\),   \\; \tan{(x)} = y \in \(-\infty, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(-\infty, +\infty\)\\) as input
/// (an angle in radians) and returns a numeric value \\(y\\) that lies in the range
/// \\(\(-\infty, +\infty\)\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.tan(0.0)
///       |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn tan(x: Float) -> Float {
  do_tan(x)
}

@external(erlang, "math", "tan")
@external(javascript, "../maths.mjs", "tan")
fn do_tan(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The hyperbolic tangent function:
///
/// \\[
/// \forall x \in \(-\infty, \infty\),   \\; \tanh{(x)} = y \in \[-1, 1\]
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(-\infty, \infty\)\\) as input (an angle
/// in radians) and returns a numeric value \\(y\\) that lies in the range \\(\[-1, 1\]\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       maths.tanh(0.0)
///       |> should.equal(0.0)
///
///       maths.tanh(25.0)
///       |> should.equal(1.0)
///
///       maths.tanh(-25.0)
///       |> should.equal(-1.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn tanh(x: Float) -> Float {
  do_tanh(x)
}

@external(erlang, "math", "tanh")
@external(javascript, "../maths.mjs", "tanh")
fn do_tanh(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The exponential function:
///
/// \\[
/// \forall x \in \(-\infty, \infty\),   \\; e^{x} = y \in \(0, +\infty\)
/// \\]
///
/// where \\(e \approx 2.71828\dots\\) is Eulers' number.
///
/// Note: If the input value \\(x\\) is too large an overflow error might occur.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.exponential(0.0)
///       |> should.equal(1.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn exponential(x: Float) -> Float {
  do_exponential(x)
}

@external(erlang, "math", "exp")
@external(javascript, "../maths.mjs", "exponential")
fn do_exponential(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The natural logarithm function:
///
/// \\[
/// \forall x \in \(0, \infty\),   \\; \log_{e}{(x)} = y \in \(-\infty, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(0, \infty\)\\) as input and returns
/// a numeric value \\(y\\) that lies in the range \\(\(-\infty, \infty\)\\).
/// If the input value is outside the domain of the function an error is returned.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       maths.natural_logarithm(1.0)
///       |> should.equal(Ok(0.0))
///
///       maths.natural_logarithm(maths.e())
///       |> should.equal(Ok(1.0))
///
///       maths.natural_logarithm(-1.0)
///       |> should.be_error()
///     }
/// </details>
///
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn natural_logarithm(x: Float) -> Result(Float, Nil) {
  case x >. 0.0 {
    True -> Ok(do_natural_logarithm(x))
    False -> Error(Nil)
  }
}

@external(erlang, "math", "log")
@external(javascript, "../maths.mjs", "logarithm")
fn do_natural_logarithm(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The base \\(b\\) logarithm function (computed through the "change of base" formula):
///
/// \\[
/// \forall x \in \(0, \infty\) \textnormal{ and } b > 1,  \\; \log_{b}{(x)} = y \in \(-\infty, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(0, \infty\)\\) and a base \\(b > 1\\)
/// as input and returns a numeric value \\(y\\) that lies in the range \\(\(-\infty, \infty\)\\).
/// If the input value is outside the domain of the function an error is returned.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       maths.logarithm(1.0, 10.0)
///       |> should.equal(Ok(0.0))
///
///       maths.logarithm(maths.e(), maths.e())
///       |> should.equal(Ok(1.0))
///
///       maths.logarithm(-1.0, 2.0)
///       |> should.be_error()
///     }
/// </details>
///
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn logarithm(x: Float, base: Float) -> Result(Float, Nil) {
  case x >. 0.0 && base >. 0.0 && base != 1.0 {
    True -> {
      // Apply the "change of base formula".
      // Usage of let assert: No error will occur since 'x' and 'base' are within
      // the domain of the `logarithm_10` function.
      let assert Ok(numerator) = logarithm_10(x)
      let assert Ok(denominator) = logarithm_10(base)

      Ok(numerator /. denominator)
    }
    _ -> Error(Nil)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The base-2 logarithm function:
///
/// \\[
/// \forall x \in \(0, \infty),   \\; \log_{2}{(x)} = y \in \(-\infty, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(0, \infty\)\\) as input and returns a
/// numeric value \\(y\\) that lies in the range \\(\(-\infty, \infty\)\\).
/// If the input value is outside the domain of the function an error is returned.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       maths.logarithm_2(1.0)
///       |> should.equal(Ok(0.0))
///
///       maths.logarithm_2(2.0)
///       |> should.equal(Ok(1.0))
///
///       maths.logarithm_2(-1.0)
///       |> should.be_error()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn logarithm_2(x: Float) -> Result(Float, Nil) {
  case x >. 0.0 {
    True -> Ok(do_logarithm_2(x))
    False -> Error(Nil)
  }
}

@external(erlang, "math", "log2")
@external(javascript, "../maths.mjs", "logarithm_2")
fn do_logarithm_2(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The base-10 logarithm function:
///
/// \\[
/// \forall x \in \(0, \infty),   \\; \log_{10}{(x)} = y \in \(-\infty, +\infty\)
/// \\]
///
/// The function takes a number \\(x\\) in its domain \\(\(0, \infty\)\\) as input and returns a
/// numeric value \\(y\\) that lies in the range \\(\(-\infty, \infty\)\\).
/// If the input value is outside the domain of the function an error is returned.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       maths.logarithm_10(1.0)
///       |> should.equal(Ok(0.0))
///
///       maths.logarithm_10(10.0)
///       |> should.equal(Ok(1.0))
///
///       maths.logarithm_10(-1.0)
///       |> should.be_error()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn logarithm_10(x: Float) -> Result(Float, Nil) {
  case x >. 0.0 {
    True -> Ok(do_logarithm_10(x))
    False -> Error(Nil)
  }
}

@external(erlang, "math", "log10")
@external(javascript, "../maths.mjs", "logarithm_10")
fn do_logarithm_10(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The \\(n\\)'th root function: \\(y = \sqrt[n]{x} = x^{\frac{1}{n}}\\).
///
/// Note that the function is not defined if the input is negative (\\(x < 0\\)). An error will be
/// returned as an imaginary number will otherwise have to be returned.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.nth_root(-1.0, 2)
///       |> should.be_error()
///
///       maths.nth_root(1.0, 2)
///       |> should.equal(Ok(1.0))
///
///       maths.nth_root(27.0, 3)
///       |> should.equal(Ok(3.0))
///
///       maths.nth_root(256.0, 4)
///       |> should.equal(Ok(4.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn nth_root(x: Float, n: Int) -> Result(Float, Nil) {
  // In the following check:
  // 1. If x is negative then return an error as it will otherwise be an
  // imaginary number
  case x >=. 0.0 && n >= 1 {
    True -> float.power(x, 1.0 /. int.to_float(n))
    False -> Error(Nil)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The mathematical constant pi: \\(\pi \approx 3.1415\dots\\)
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn pi() -> Float {
  do_pi()
}

@external(erlang, "math", "pi")
@external(javascript, "../maths.mjs", "pi")
fn do_pi() -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The mathematical (circle) constant tau: \\(\tau = 2 \cdot \pi \approx 6.283\dots\\)
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn tau() -> Float {
  2.0 *. pi()
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The golden ratio: \\(\phi = \frac{1 + \sqrt{5}}{2}\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.golden_ratio()
///       |> should.equal(1.618033988749895)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn golden_ratio() -> Float {
  // Calculate the golden ratio: (1 + sqrt(5)) / 2
  // Usage of let assert: A positive number '5' is given a input so no error should occur, i.e.,
  // the function 'float.square_root' will only return an error in case a negative value is
  // given as input.
  let assert Ok(sqrt5) = float.square_root(5.0)
  { 1.0 +. sqrt5 } /. 2.0
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Euler's number \\(e \approx 2.71828\dots\\).
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///
///       // Test that the constant is approximately equal to 2.7128...
///       maths.e()
///       |> maths.is_close(2.7182818284590452353602, 0.0, tolerance)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn e() -> Float {
  exponential(1.0)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function rounds a float to a specific number of digits (after the decimal place or before
/// if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
/// specified digit) with ties (fractional values of 0.5) being rounded to the nearest even
/// integer.
///
/// <details>
/// <summary>Details</summary>
///
///   The rounding mode rounds \\(12.0654\\) to:
///   - \\(12.0\\) for 0 digits after the decimal point (`digits = 0`)
///   - \\(12.1\\) for 1 digit after the decimal point (`digits = 1`)
///   - \\(12.07\\) for 2 digits after the decimal point (`digits = 2`)
///   - \\(12.065\\) for 3 digits after the decimal point (`digits = 3`)
///
///   It is also possible to specify a negative number of digits. In that case, the negative
///   number refers to the digits before the decimal point.
///   - \\(10.0\\) for 1 digit before the decimal point (`digits = -1`)
///   - \\(0.0\\) for 2 digits before the decimal point (`digits = -2`)
///   - \\(0.0\\) for 3 digits before the decimal point (`digits = -3`)
///
/// </details>
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.round_to_nearest(12.0654, 2)
///       |> should.equal(12.07)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn round_to_nearest(x: Float, p: Int) -> Float {
  // Usage of let assert: The function 'float.power' will only return an error if:
  // 1. The base is negative and the exponent is fractional.
  // 2. If the base is 0 and the exponent is negative.
  // No error will occur since the base is a positive non-zero number.
  let assert Ok(p) = float.power(10.0, int.to_float(p))
  let xabs = float.absolute_value(x) *. p
  let xabs_truncated = truncate_float(xabs)
  let remainder = xabs -. xabs_truncated
  case remainder {
    _ if remainder >. 0.5 -> sign(x) *. truncate_float(xabs +. 1.0) /. p
    _ if remainder == 0.5 -> {
      let is_even = float.truncate(xabs) % 2
      case is_even == 0 {
        True -> sign(x) *. xabs_truncated /. p
        False -> sign(x) *. truncate_float(xabs +. 1.0) /. p
      }
    }
    _ -> sign(x) *. xabs_truncated /. p
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function rounds a float to a specific number of digits (after the decimal place or before
/// if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
/// specified digit) with ties (fractional values of 0.5) being rounded away from zero (C/C++
/// rounding behaviour).
///
/// <details>
/// <summary>Details</summary>
///
///   The rounding mode rounds \\(12.0654\\) to:
///   - \\(12.0\\) for 0 digits after the decimal point (`digits = 0`)
///   - \\(12.1\\) for 1 digit after the decimal point (`digits = 1`)
///   - \\(12.07\\) for 2 digits after the decimal point (`digits = 2`)
///   - \\(12.065\\) for 3 digits after the decimal point (`digits = 3`)
///
///   It is also possible to specify a negative number of digits. In that case, the negative
///   number refers to the digits before the decimal point.
///   - \\(10.0\\) for 1 digit before the decimal point (`digits = -1`)
///   - \\(0.0\\) for 2 digits before the decimal point (`digits = -2`)
///   - \\(0.0\\) for 3 digits before the decimal point (`digits = -3`)
///
/// </details>
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.round_ties_away(12.0654, 2)
///       |> should.equal(12.07)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn round_ties_away(x: Float, p: Int) -> Float {
  // Usage of let assert: The function 'float.power' will only return an error if:
  // 1. The base is negative and the exponent is fractional.
  // 2. If the base is 0 and the exponent is negative.
  // No error will occur since the base is a positive non-zero number.
  let assert Ok(p) = float.power(10.0, int.to_float(p))
  let xabs = float.absolute_value(x) *. p
  let remainder = xabs -. truncate_float(xabs)
  case remainder {
    _ if remainder >=. 0.5 -> sign(x) *. truncate_float(xabs +. 1.0) /. p
    _ -> sign(x) *. truncate_float(xabs) /. p
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function rounds a float to a specific number of digits (after the decimal place or before
/// if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
/// specified digit) with ties (fractional values of 0.5) being rounded towards \\(+\infty\\)
/// (Java/JavaScript rounding behaviour).
///
/// <details>
/// <summary>Details</summary>
///
///   The rounding mode rounds \\(12.0654\\) to:
///   - \\(12.0\\) for 0 digits after the decimal point (`digits = 0`)
///   - \\(12.1\\) for 1 digits after the decimal point (`digits = 1`)
///   - \\(12.07\\) for 2 digits after the decimal point (`digits = 2`)
///   - \\(12.065\\) for 3 digits after the decimal point (`digits = 3`)
///
///   It is also possible to specify a negative number of digits. In that case, the negative
///    number refers to the digits before the decimal point.
///   - \\(10.0\\) for 1 digit before the decimal point (`digits = -1`)
///   - \\(0.0\\) for 2 digits before the decimal point (`digits = -2`)
///   - \\(0.0\\) for 3 digits before the decimal point (`digits = -3`)
///
/// </details>
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.round_ties_up(12.0654, 2)
///       |> should.equal(12.07)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn round_ties_up(x: Float, p: Int) -> Float {
  // Usage of let assert: The function 'float.power' will only return an error if:
  // 1. The base is negative and the exponent is fractional.
  // 2. If the base is 0 and the exponent is negative.
  // No error will occur since the base is a positive non-zero number.
  let assert Ok(p) = float.power(10.0, int.to_float(p))
  let xabs = float.absolute_value(x) *. p
  let xabs_truncated = truncate_float(xabs)
  let remainder = xabs -. xabs_truncated
  case remainder {
    _ if remainder >=. 0.5 && x >=. 0.0 ->
      sign(x) *. truncate_float(xabs +. 1.0) /. p
    _ -> sign(x) *. xabs_truncated /. p
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function rounds a float to a specific number of digits (after the decimal place or before
/// if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
/// specified digit) that is less than or equal to the absolute value of the input \\(x\\). This
/// rounding behaviour is similar to behaviour of the Gleam stdlib `truncate` function.
///
/// <details>
/// <summary>Details</summary>
///
///   The rounding mode rounds \\(12.0654\\) to:
///   - \\(12.0\\) for 0 digits after the decimal point (`digits = 0`)
///   - \\(12.0\\) for 1 digit after the decimal point (`digits = 1`)
///   - \\(12.06\\) for 2 digits after the decimal point (`digits = 2`)
///   - \\(12.065\\) for 3 digits after the decimal point (`digits = 3`)
///
///   It is also possible to specify a negative number of digits. In that case, the negative
///   number refers to the digits before the decimal point.
///   - \\(10.0\\) for 1 digit before the decimal point (`digits = -1`)
///   - \\(0.0\\) for 2 digits before the decimal point (`digits = -2`)
///   - \\(0.0\\) for 3 digits before the decimal point (`digits = -3`)
///
/// </details>
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.round_to_zero(12.0654, 2)
///       |> should.equal(12.06)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn round_to_zero(x: Float, p: Int) -> Float {
  // Usage of let assert: The function 'float.power' will only return an error if:
  // 1. The base is negative and the exponent is fractional.
  // 2. If the base is 0 and the exponent is negative.
  // No error will occur since the base is a positive non-zero number.
  let assert Ok(p) = float.power(10.0, int.to_float(p))
  truncate_float(x *. p) /. p
}

fn truncate_float(x: Float) -> Float {
  do_truncate_float(x)
}

@external(erlang, "erlang", "trunc")
@external(javascript, "../maths.mjs", "truncate")
fn do_truncate_float(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function rounds a float to a specific number of digits (after the decimal place or before
/// if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
/// specified digit) that is less than or equal to the input \\(x\\). This rounding behaviour is
/// similar to behaviour of the Gleam stdlib `floor` function.
///
/// <details>
/// <summary>Details</summary>
///
///   The rounding mode rounds \\(12.0654\\) to:
///   - \\(12.0\\) for 0 digits after the decimal point (`digits = 0`)
///   - \\(12.0\\) for 1 digits after the decimal point (`digits = 1`)
///   - \\(12.06\\) for 2 digits after the decimal point (`digits = 2`)
///   - \\(12.065\\) for 3 digits after the decimal point (`digits = 3`)
///
///   It is also possible to specify a negative number of digits. In that case, the negative
///   number refers to the digits before the decimal point.
///   - \\(10.0\\) for 1 digit before the decimal point (`digits = -1`)
///   - \\(0.0\\) for 2 digits before the decimal point (`digits = -2`)
///   - \\(0.0\\) for 3 digits before the decimal point (`digits = -3`)
///
/// </details>
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.round_down(12.0654, 2)
///       |> should.equal(12.06)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn round_down(x: Float, p: Int) -> Float {
  // Usage of let assert: The function 'float.power' will only return an error if:
  // 1. The base is negative and the exponent is fractional.
  // 2. If the base is 0 and the exponent is negative.
  // No error will occur since the base is a positive non-zero number.
  let assert Ok(p) = float.power(10.0, int.to_float(p))
  do_floor(x *. p) /. p
}

@external(erlang, "math", "floor")
@external(javascript, "../maths.mjs", "floor")
fn do_floor(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function rounds a float to a specific number of digits (after the decimal place or before
/// if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
/// specified digit) that is larger than or equal to the input \\(x\\). This rounding behaviour is
/// similar to behaviour of the Gleam stdlib `ceiling` function.
///
/// <details>
/// <summary>Details</summary>
///
///   The rounding mode rounds \\(12.0654\\) to:
///   - \\(13.0\\) for 0 digits after the decimal point (`digits = 0`)
///   - \\(12.1\\) for 1 digit after the decimal point (`digits = 1`)
///   - \\(12.07\\) for 2 digits after the decimal point (`digits = 2`)
///   - \\(12.066\\) for 3 digits after the decimal point (`digits = 3`)
///
///   It is also possible to specify a negative number of digits. In that case, the negative
///   number refers to the digits before the decimal point.
///   - \\(20.0\\) for 1 digit places before the decimal point (`digit = -1`)
///   - \\(100.0\\) for 2 digits before the decimal point (`digits = -2`)
///   - \\(1000.0\\) for 3 digits before the decimal point (`digits = -3`)
///
/// </details>
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.round_up(12.0654, 2)
///       |> should.equal(12.07)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn round_up(x: Float, p: Int) -> Float {
  // Usage of let assert: The function 'float.power' will only return an error if:
  // 1. The base is negative and the exponent is fractional.
  // 2. If the base is 0 and the exponent is negative.
  // No error will occur since the base is a positive non-zero number.
  let assert Ok(p) = float.power(10.0, int.to_float(p))
  do_ceiling(x *. p) /. p
}

@external(erlang, "math", "ceil")
@external(javascript, "../maths.mjs", "ceiling")
fn do_ceiling(a: Float) -> Float

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The absolute difference:
///
/// \\[
///  \forall x, y \in \mathbb{R}, \\; |x - y|  \in \mathbb{R}_{+}.
/// \\]
///
/// The function takes two inputs \\(x\\) and \\(y\\) and returns a positive float
/// value which is the absolute difference of the inputs.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.absolute_difference(-10.0, 10.0)
///       |> should.equal(20.0)
///
///       maths.absolute_difference(0.0, -2.0)
///       |> should.equal(2.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn absolute_difference(a: Float, b: Float) -> Float {
  float.absolute_value(a -. b)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The absolute difference:
///
/// \\[
///  \forall x, y \in \mathbb{Z}, \\; |x - y|  \in \mathbb{Z}_{+}.
/// \\]
///
/// The function takes two inputs \\(x\\) and \\(y\\) and returns a positive integer
/// value which is the absolute difference of the inputs.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.absolute_difference(-10, 10)
///       |> should.equal(20)
///
///       maths.absolute_difference(0, -2)
///       |> should.equal(2)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn int_absolute_difference(a: Int, b: Int) -> Int {
  int.absolute_value(a - b)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function takes an input \\(x \in \mathbb{R}\\) and returns the sign of
/// the input, indicating whether it is positive (+1.0), negative (-1.0), or
/// zero (0.0).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn sign(x: Float) -> Float {
  do_sign(x)
}

@external(javascript, "../maths.mjs", "sign")
fn do_sign(x: Float) -> Float {
  case x {
    _ if x <. 0.0 -> -1.0
    _ if x >. 0.0 -> 1.0
    _ -> 0.0
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function takes an input \\(x \in \mathbb{Z}\\) and returns the sign of
/// the input, indicating whether it is positive (+1), negative (-1), or zero
/// (0).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn int_sign(x: Int) -> Int {
  do_int_sign(x)
}

@external(javascript, "../maths.mjs", "sign")
fn do_int_sign(x: Int) -> Int {
  case x {
    _ if x < 0 -> -1
    _ if x > 0 -> 1
    _ -> 0
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function takes two arguments \\(x, y \in \mathbb{R}\\) and returns \\(x\\)
/// such that it has the same sign as \\(y\\).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn copy_sign(x: Float, y: Float) -> Float {
  case sign(x) == sign(y) {
    // x and y have the same sign, just return x
    True -> x
    // x and y have different signs:
    // - x is positive and y is negative, then flip sign of x
    // - x is negative and y is positive, then flip sign of x
    False -> flip_sign(x)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function takes two arguments \\(x, y \in \mathbb{Z}\\) and returns \\(x\\)
/// such that it has the same sign as \\(y\\).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn int_copy_sign(x: Int, y: Int) -> Int {
  case int_sign(x) == int_sign(y) {
    // x and y have the same sign, just return x
    True -> x
    // x and y have different signs:
    // - x is positive and y is negative, then flip sign of x
    // - x is negative and y is positive, then flip sign of x
    False -> int_flip_sign(x)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function flips the sign of a given input value \\(x \in \mathbb{R}\\).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn flip_sign(x: Float) -> Float {
  -1.0 *. x
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function flips the sign of a given input value \\(x \in \mathbb{Z}\\).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn int_flip_sign(x: Int) -> Int {
  -1 * x
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The minmax function takes two arguments \\(x, y\\) along with a function
/// for comparing \\(x, y\\). The function returns a tuple with the smallest
/// value first and largest second.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleam/float
///     import gleam/int
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.minmax(2.0, 1.5, float.compare)
///       |> should.equal(#(1.5, 2.0))
///
///       maths.minmax(1, 2, int.compare)
///       |> should.equal(#(1, 2))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn minmax(x: a, y: a, compare: fn(a, a) -> order.Order) {
  case compare(x, y) {
    order.Lt -> #(x, y)
    order.Eq -> #(x, y)
    order.Gt -> #(y, x)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the minimum value of a given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/int
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.list_minimum(int.compare)
///       |> should.be_error()
///
///       [4, 4, 3, 2, 1]
///       |> maths.list_minimum(int.compare)
///       |> should.equal(Ok(1))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
pub fn list_minimum(
  arr: List(a),
  compare: fn(a, a) -> order.Order,
) -> Result(a, Nil) {
  case arr {
    [] -> Error(Nil)
    [x, ..rest] ->
      Ok(
        list.fold(rest, x, fn(acc, element) {
          case compare(element, acc) {
            order.Lt -> element
            _ -> acc
          }
        }),
      )
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the maximum value of a given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/float
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.list_maximum(float.compare)
///       |> should.be_error()
///
///       [4.0, 4.0, 3.0, 2.0, 1.0]
///       |> maths.list_maximum(float.compare)
///       |> should.equal(Ok(4.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn list_maximum(
  arr: List(a),
  compare: fn(a, a) -> order.Order,
) -> Result(a, Nil) {
  case arr {
    [] -> Error(Nil)
    [x, ..rest] ->
      Ok(
        list.fold(rest, x, fn(acc, element) {
          case compare(acc, element) {
            order.Lt -> element
            _ -> acc
          }
        }),
      )
  }
}

/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the indices of the minimum values in a given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/float
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.arg_minimum(float.compare)
///       |> should.be_error()
///
///       [4.0, 4.0, 3.0, 2.0, 1.0]
///       |> maths.arg_minimum(float.compare)
///       |> should.equal(Ok([4]))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn arg_minimum(
  arr: List(a),
  compare: fn(a, a) -> order.Order,
) -> Result(List(Int), Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      // Usage of let assert: No error will occur since we know input 'arr' is non-empty
      let assert Ok(min) = list_minimum(arr, compare)
      Ok(
        list.index_map(arr, fn(element, index) {
          case compare(element, min) {
            order.Eq -> index
            _ -> -1
          }
        })
        |> list.filter(fn(index) {
          case index {
            -1 -> False
            _ -> True
          }
        }),
      )
    }
  }
}

/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the indices of the maximum values in a given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/float
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.arg_maximum(float.compare)
///       |> should.be_error()
///
///       [4.0, 4.0, 3.0, 2.0, 1.0]
///       |> maths.arg_maximum(float.compare)
///       |> should.equal(Ok([0, 1]))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn arg_maximum(
  arr: List(a),
  compare: fn(a, a) -> order.Order,
) -> Result(List(Int), Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      // Usage of let assert: No error will occur since we know input 'arr' is non-empty
      let assert Ok(max) = list_maximum(arr, compare)
      Ok(
        list.index_map(arr, fn(element, index) {
          case compare(element, max) {
            order.Eq -> index
            _ -> -1
          }
        })
        |> list.filter(fn(index) {
          case index {
            -1 -> False
            _ -> True
          }
        }),
      )
    }
  }
}

/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns a tuple consisting of the minimum and maximum values of a given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/float
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.extrema(float.compare)
///       |> should.be_error()
///
///       [4.0, 4.0, 3.0, 2.0, 1.0]
///       |> maths.extrema(float.compare)
///       |> should.equal(Ok(#(1.0, 4.0)))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn extrema(
  arr: List(a),
  compare: fn(a, a) -> order.Order,
) -> Result(#(a, a), Nil) {
  case arr {
    [] -> Error(Nil)
    [x, ..rest] ->
      Ok(
        list.fold(rest, #(x, x), fn(acc, element) {
          let first = acc.0
          let second = acc.1
          case compare(element, first), compare(second, element) {
            order.Lt, order.Lt -> #(element, element)
            order.Lt, _ -> #(element, second)
            _, order.Lt -> #(first, element)
            _, _ -> #(first, second)
          }
        }),
      )
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A combinatorial function for computing the number of \\(k\\)-combinations of \\(n\\) elements
/// with repetitions:
///
/// \\[
/// C^*(n, k) = \binom{n + k - 1}{k} = \frac{(n + k - 1)!}{k! (n - 1)!}
/// \\]
///
/// Also known as the "stars and bars" problem in maths. Furthermore, the implementation uses an
/// efficient iterative multiplicative formula for computing the result.
///
/// <details>
/// <summary>Details</summary>
///
/// A \\(k\\)-combination with repetitions is a sequence of \\(k\\) elements selected from
/// \\(n\\) elements where the order of selection does not matter and elements are allowed to
/// repeat. For example, consider selecting 2 elements from a list of 3 elements: `["A", "B", "C"]`.
/// In this case, possible selections are:
///   - `["A", "A"], ["A", "B"], ["A", "C"]`
///   - `["B", "B"], ["B", "C"], ["C", "C"]`
///
/// </details>
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.combination_with_repetitions(-1, 1)
///       |> should.be_error()
///
///       maths.combination_with_repetitions(2, 3)
///       |> should.equal(Ok(4))
///
///       maths.combination_with_repetitions(13, 5)
///       |> should.equal(Ok(6188))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn combination_with_repetitions(n: Int, k: Int) -> Result(Int, Nil) {
  combination(n + k - 1, k)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A combinatorial function for computing the number of \\(k\\)-combinations of \\(n\\) elements
/// without repetitions:
///
/// \\[
/// C(n, k) = \binom{n}{k} = \frac{n!}{k! (n-k)!}
/// \\]
///
/// Also known as "\\(n\\) choose \\(k\\)" or the binomial coefficient.
///
///
/// <details>
/// <summary>Details</summary>
///
/// A \\(k\\)-combination without repetition is a sequence of \\(k\\) elements selected from
/// \\(n\\) elements where the order of selection does not matter and elements are not allowed to
/// repeat. For example, consider selecting  2 elements from a list of 3 elements:
/// `["A", "B", "C"]`. In this case, possible selections are:
///   - `["A", "B"]`
///   - `["A", "C"]`
///   - `["B", "C"]`
///
/// </details>
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.combination(-1, 1)
///       |> should.be_error()
///
///       maths.combination(4, 0)
///       |> should.equal(Ok(1))
///
///       maths.combination(4, 4)
///       |> should.equal(Ok(1))
///
///       maths.combination(13, 5)
///       |> should.equal(Ok(1287))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn combination(n: Int, k: Int) -> Result(Int, Nil) {
  case n, k {
    _, _ if n < 0 -> Error(Nil)
    _, _ if k < 0 -> Error(Nil)
    _, _ if k > n -> Ok(0)
    _, _ if k == 0 || k == n -> Ok(1)
    _, _ -> {
      let min = case k < n - k {
        True -> k
        False -> n - k
      }
      Ok(do_combination(n, min, 1, 1))
    }
  }
}

fn do_combination(n: Int, k: Int, acc: Int, element: Int) -> Int {
  case element > k {
    True -> acc
    False ->
      do_combination(n, k, acc * { n + 1 - element } / element, element + 1)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A combinatorial function for computing the total number of combinations of \\(n\\)
/// elements, that is \\(n!\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.factorial(-1)
///       |> should.be_error()
///
///       maths.factorial(0)
///       |> should.equal(Ok(1))
///
///       maths.factorial(3)
///       |> should.equal(Ok(6))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn factorial(n: Int) -> Result(Int, Nil) {
  case n {
    _ if n < 0 -> Error(Nil)
    _ -> Ok(do_factorial(n, 1))
  }
}

fn do_factorial(n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    1 -> acc
    _ -> do_factorial(n - 1, acc * n)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A combinatorial function for computing the number of \\(k\\)-permutations without
/// repetitions:
///
/// \\[
/// P(n, k) = \binom{n}{k} \cdot k! = \frac{n!}{(n - k)!}
/// \\]
///
/// The implementation uses an efficient iterative multiplicative formula for computing the result.
///
/// <details>
/// <summary>Details</summary>
///
/// A \\(k\\)-permutation without repetitions is a sequence of \\(k\\) elements selected from \
/// \\(n\\) elements where the order of selection matters and elements are not allowed to repeat.
/// For example, consider selecting 2 elements from a list of 3 elements: `["A", "B", "C"]`. In
/// this case, possible selections are:
///   - `["A", "B"], ["B", "A"]`
///   - `["A", "C"], ["C", "A"]`
///   - `["B", "C"], ["C", "B"]`
///
/// </details>
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.permutation(-1, 1)
///       |> should.be_error()
///
///       maths.permutation(4, 0)
///       |> should.equal(Ok(1))
///
///       maths.permutation(4, 2)
///       |> should.equal(Ok(12))
///
///       maths.permutation(13, 5)
///       |> should.equal(Ok(154_440))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn permutation(n: Int, k: Int) -> Result(Int, Nil) {
  case n, k {
    _, _ if n < 0 -> Error(Nil)
    _, _ if k < 0 -> Error(Nil)
    _, _ if k > n -> Ok(0)
    _, _ if k == 0 -> Ok(1)
    _, _ -> Ok(do_permutation(n, k, 1))
  }
}

fn do_permutation(n: Int, k: Int, acc: Int) -> Int {
  case k {
    0 -> acc
    _ -> do_permutation(n - 1, k - 1, acc * n)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A combinatorial function for computing the number of \\(k\\)-permutations with repetitions:
///
/// \\[
/// P^*(n, k) = n^k
/// \\]
///
/// <details>
/// <summary>Details</summary>
///
/// A \\(k\\)-permutation with repetitions is a sequence of \\(k\\) elements selected from \\(n\\)
/// elements where the order of selection matters and elements are allowed to repeat. For example,
/// consider selecting 2 elements from a list of 3 elements: `["A", "B", "C"]`. In this case,
/// possible selections are:
///   - `["A", "A"], ["A", "B"], ["A", "C"]`
///   - `["B", "A"], ["B", "B"], ["B", "C"]`
///   - `["C", "A"], ["C", "B"], ["C", "C"]`
///
/// </details>
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.permutation_with_repetitions(1, -1)
///       |> should.be_error()
///
///       maths.permutation_with_repetitions(2, 3)
///       |> should.equal(Ok(8))
///
///       maths.permutation_with_repetitions(4, 4)
///       |> should.equal(Ok(256))
///
///       maths.permutation_with_repetitions(6, 3)
///       |> should.equal(Ok(216))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn permutation_with_repetitions(n: Int, k: Int) -> Result(Int, Nil) {
  case n, k {
    _, _ if n < 0 -> Error(Nil)
    _, _ if k < 0 -> Error(Nil)
    _, _ -> {
      let n_float = int.to_float(n)
      let k_float = int.to_float(k)
      // Usage of let assert: The function 'float.power' will only return an error if:
      // 1. The base is negative and the exponent is fractional.
      // 2. If the base is 0 and the exponent is negative.
      // No error will occur since the base and exponent are positive numbers.
      let assert Ok(result) = float.power(n_float, k_float)
      Ok(float.round(result))
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generates all possible combinations of \\(k\\) elements selected from a given list of size
/// \\(n\\). The function handles the case without repetitions, that is, repeated elements
/// are not treated as distinct.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/yielder
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // All 2-combinations of [1, 2, 3] without repetition
///       let assert Ok(combinations) = maths.list_combination([1, 2, 3], 2)
///
///       combinations
///       |> yielder.to_list()
///       |> should.equal([[1, 2], [1, 3], [2, 3]])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn list_combination(arr: List(a), k: Int) -> Result(Yielder(List(a)), Nil) {
  case k, list.length(arr) {
    _, _ if k < 0 -> Error(Nil)
    _, arr_length if k > arr_length -> Error(Nil)
    // Special case: When k = n, then the entire list is the only valid combination
    _, arr_length if k == arr_length -> {
      Ok(yielder.single(arr))
    }
    _, _ -> {
      Ok(do_list_combination_without_repetitions(yielder.from_list(arr), k, []))
    }
  }
}

fn do_list_combination_without_repetitions(
  arr: Yielder(a),
  k: Int,
  prefix: List(a),
) -> Yielder(List(a)) {
  case k {
    0 -> yielder.single(list.reverse(prefix))
    _ ->
      case yielder.step(arr) {
        yielder.Done -> yielder.empty()
        yielder.Next(x, xs) -> {
          let with_x =
            do_list_combination_without_repetitions(xs, k - 1, [x, ..prefix])
          let without_x = do_list_combination_without_repetitions(xs, k, prefix)
          yielder.concat([with_x, without_x])
        }
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generates all possible combinations of \\(k\\) elements selected from a given list of size
/// \\(n\\). The function handles the case when the repetition of elements is allowed, that is,
/// repeated elements are treated as distinct.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/yielder
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // All 2-combinations of [1, 2, 3] with repetition
///       let assert Ok(combinations) =
///         maths.list_combination_with_repetitions([1, 2, 3], 2)
///
///       combinations
///       |> yielder.to_list()
///       |> should.equal([[1, 1], [1, 2], [1, 3], [2, 2], [2, 3], [3, 3]])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn list_combination_with_repetitions(
  arr: List(a),
  k: Int,
) -> Result(Yielder(List(a)), Nil) {
  case k {
    _ if k < 0 -> Error(Nil)
    _ -> {
      Ok(do_list_combination_with_repetitions(yielder.from_list(arr), k, []))
    }
  }
}

fn do_list_combination_with_repetitions(
  arr: Yielder(a),
  k: Int,
  prefix: List(a),
) -> Yielder(List(a)) {
  case k {
    0 -> yielder.single(list.reverse(prefix))
    _ ->
      case yielder.step(arr) {
        yielder.Done -> yielder.empty()
        yielder.Next(x, xs) -> {
          let with_x =
            do_list_combination_with_repetitions(arr, k - 1, [x, ..prefix])
          let without_x = do_list_combination_with_repetitions(xs, k, prefix)
          yielder.concat([with_x, without_x])
        }
      }
  }
}

fn remove_first_by_index(
  arr: Yielder(#(Int, a)),
  index_to_remove: Int,
) -> Yielder(#(Int, a)) {
  yielder.flat_map(arr, fn(tuple) {
    let #(index, element) = tuple
    case index == index_to_remove {
      True -> yielder.empty()
      False -> yielder.single(#(index, element))
    }
  })
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generates all possible permutations of \\(k\\) elements selected from a given list of size
/// \\(n\\). The function handles the case without repetitions, that is, repeated elements are
/// not treated as distinct.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/yielder
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // All 2-permutations of [1, 2] without repetition
///       let assert Ok(permutations) =
///         [1, 2]
///         |> maths.list_permutation(2)
///       permutations
///       |> yielder.to_list()
///       |> should.equal([[1, 2], [2, 1]])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn list_permutation(arr: List(a), k: Int) -> Result(Yielder(List(a)), Nil) {
  case k, list.length(arr) {
    _, _ if k < 0 -> Error(Nil)
    _, arr_length if k > arr_length -> Error(Nil)
    _, _ -> {
      let indexed_arr =
        list.index_map(arr, fn(element, index) { #(index, element) })
      Ok(do_list_permutation_without_repetitions(
        yielder.from_list(indexed_arr),
        k,
      ))
    }
  }
}

fn do_list_permutation_without_repetitions(
  arr: Yielder(#(Int, a)),
  k: Int,
) -> Yielder(List(a)) {
  case k {
    0 -> yielder.single([])
    _ ->
      yielder.flat_map(arr, fn(tuple) {
        let #(index, element) = tuple
        let remaining = remove_first_by_index(arr, index)
        let permutations =
          do_list_permutation_without_repetitions(remaining, k - 1)
        yielder.map(permutations, fn(permutation) { [element, ..permutation] })
      })
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generates all possible permutations of \\(k\\) elements selected from a given list of size
/// \\(n\\). The function handles the case when the repetition of elements is allowed, that is,
/// repeated elements are treated as distinct.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/yielder
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // All 2-permutations of [1, 2] with repetition
///       let assert Ok(permutations) =
///         [1, 2]
///         |> maths.list_permutation_with_repetitions(2)
///       permutations
///       |> yielder.to_list()
///       |> set.from_list()
///       |> should.equal(set.from_list([[1, 1], [1, 2], [2, 2], [2, 1]]))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn list_permutation_with_repetitions(
  arr: List(a),
  k: Int,
) -> Result(Yielder(List(a)), Nil) {
  case k {
    _ if k < 0 -> Error(Nil)
    _ -> {
      let indexed_arr =
        list.index_map(arr, fn(element, index) { #(index, element) })
      Ok(do_list_permutation_with_repetitions(yielder.from_list(indexed_arr), k))
    }
  }
}

fn do_list_permutation_with_repetitions(
  arr: Yielder(#(Int, a)),
  k: Int,
) -> Yielder(List(a)) {
  case k {
    0 -> yielder.single([])
    _ ->
      yielder.flat_map(arr, fn(tuple) {
        let #(_, element) = tuple
        // Allow the same element (by index) to be reused in future recursive calls
        let permutations = do_list_permutation_with_repetitions(arr, k - 1)
        // Prepend the current element to each generated permutation
        yielder.map(permutations, fn(permutation) { [element, ..permutation] })
      })
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate a set containing all combinations of pairs of elements coming from two given sets.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/set
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       set.from_list([])
///       |> maths.cartesian_product(set.from_list([]))
///       |> should.equal(set.from_list([]))
///
///       set.from_list([1.0, 10.0])
///       |> maths.cartesian_product(set.from_list([1.0, 2.0]))
///       |> should.equal(
///         set.from_list([#(1.0, 1.0), #(1.0, 2.0), #(10.0, 1.0), #(10.0, 2.0)]),
///       )
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn cartesian_product(xset: set.Set(a), yset: set.Set(b)) -> set.Set(#(a, b)) {
  set.fold(xset, set.new(), fn(acc0, element0) {
    set.fold(yset, acc0, fn(acc1, element1) {
      set.insert(acc1, #(element0, element1))
    })
  })
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the \\(p\\)-norm of a list (representing a vector):
///
/// \\[
/// \left( \sum_{i=1}^n \left|x_{i}\right|^{p} \right)^{\frac{1}{p}}
/// \\]
///
/// In the formula, \\(n\\) is the length of the list and \\(x_i\\) is the value in
/// the input list indexed by \\(i\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       [1.0, 1.0, 1.0]
///       |> maths.norm(1.0)
///       |> should.equal(Ok(3.0))
///
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///       let assert Ok(result) =
///         [1.0, 2.0, 3.0]
///         |> maths.norm(2.0)
///       result
///       |> maths.is_close(3.7416573867739413, 0.0, tolerance)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn norm(arr: List(Float), p: Float) -> Result(Float, Nil) {
  case arr {
    [] -> Ok(0.0)
    _ -> {
      case p {
        // Handle the special case when 'p' is equal to zero. In this case, we compute a
        // pseudo-norm, which is the number of non-zero elements in the given list 'arr'
        0.0 ->
          Ok(
            list.fold(arr, 0.0, fn(acc, element) {
              case element {
                0.0 -> acc
                _ -> acc +. 1.0
              }
            }),
          )
        // Handle the case when 'p' is negative
        _ if p <. 0.0 -> {
          let aggregate =
            list.try_fold(arr, 0.0, fn(acc, element) {
              case element {
                // Whenever 'p' is negative and an element in the list is zero, then we should
                // simply stop and return 0.0. Otherwise continue.
                0.0 -> Error(0.0)
                _ -> {
                  // Usage of let assert below: The function 'float.power' will only return an
                  // error if:
                  // 1. The base is negative and the exponent is fractional.
                  // 2. If the base is 0 and the exponent is negative.
                  // No error will occur below, since the base is positive and non-zero.
                  let assert Ok(result) =
                    float.power(float.absolute_value(element), p)
                  Ok(result +. acc)
                }
              }
            })
          case aggregate {
            Ok(result) -> float.power(result, 1.0 /. p)
            Error(_) -> Ok(0.0)
          }
        }
        // Handle the case when 'p' is positive
        _ -> {
          // Usage of let assert below: No error will occur, since both the exponent and base
          // are positive numbers.
          let aggregate =
            list.fold(arr, 0.0, fn(acc, element) {
              let assert Ok(result) =
                float.power(float.absolute_value(element), p)
              result +. acc
            })
          float.power(aggregate, 1.0 /. p)
        }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the weighted \\(p\\)-norm of a list (representing a vector):
///
/// \\[
/// \left( \sum_{i=1}^n w_{i} \left|x_{i}\right|^{p} \right)^{\frac{1}{p}}
/// \\]
///
/// In the formula, \\(n\\) is the length of the list and \\(x_i\\) is the value in
/// the input list indexed by \\(i\\), while \\(w_i \in \mathbb{R}_{+}\\) is
/// a corresponding positive weight.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       [#(1.0, 0.5), #(1.0, 0.5), #(1.0, 0.5)]
///       |> maths.norm_with_weights(1.0)
///       |> should.equal(Ok(1.5))
///
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///       let assert Ok(result) =
///         [#(1.0, 0.5), #(2.0, 0.5), #(3.0, 0.5)]
///         |> maths.norm_with_weights(2.0)
///       result
///       |> maths.is_close(2.6457513110645907, 0.0, tolerance)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn norm_with_weights(
  arr: List(#(Float, Float)),
  p: Float,
) -> Result(Float, Nil) {
  case arr {
    [] -> Ok(0.0)
    _ -> {
      let weight_is_invalid = list.any(arr, fn(tuple) { tuple.1 <. 0.0 })
      case weight_is_invalid {
        True -> Error(Nil)
        // Return an error if the weights are invalid (negative)
        False -> {
          case p {
            0.0 -> {
              // Handle the special case when 'p' is equal to zero. In this case, we compute
              // a pseudo-norm, which is the number of non-zero elements in the given list 'arr'
              Ok(
                list.fold(arr, 0.0, fn(acc, tuple) {
                  case tuple {
                    #(0.0, _) -> acc
                    _ -> acc +. 1.0
                  }
                }),
              )
            }
            _ if p <. 0.0 -> {
              // Handle the case when 'p' is negative
              let aggregate =
                list.try_fold(arr, 0.0, fn(acc, tuple) {
                  // Whenever 'p' is negative and an element or weight in the list is zero, then
                  // we should simply stop and return 0.0. Otherwise continue.
                  case tuple {
                    #(0.0, _) -> Error(0.0)
                    #(_, 0.0) -> Error(0.0)
                    _ -> {
                      // Usage of let assert below: The function 'float.power' will only return an
                      // error if:
                      // 1. The base is negative and the exponent is fractional.
                      // 2. If the base is 0 and the exponent is negative.
                      // No error will occur below, since the base is positive and non-zero.
                      let assert Ok(result) =
                        float.power(float.absolute_value(tuple.0), p)
                      Ok(tuple.1 *. result +. acc)
                    }
                  }
                })
              case aggregate {
                Ok(result) -> float.power(result, 1.0 /. p)
                Error(_) -> Ok(0.0)
              }
            }
            // Handle the case when 'p' is positive
            _ -> {
              // Usage of let assert below: No error will occur, since both the exponent and base
              // are positive numbers.
              let aggregate =
                list.fold(arr, 0.0, fn(acc, tuple) {
                  let assert Ok(result) =
                    float.power(float.absolute_value(tuple.0), p)
                  tuple.1 *. result +. acc
                })
              float.power(aggregate, 1.0 /. p)
            }
          }
        }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the Manhattan distance between two lists (representing
/// vectors):
///
/// \\[
/// \sum_{i=1}^n \left|x_i - y_i \right|
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
/// values in the respective input lists indexed by \\(i\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.manhattan_distance([#(1.0, 2.0), #(2.0, 3.0)])
///       |> should.equal(Ok(2.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn manhattan_distance(arr: List(#(Float, Float))) -> Result(Float, Nil) {
  minkowski_distance(arr, 1.0)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the weighted Manhattan distance between two lists (representing
/// vectors):
///
/// \\[
/// \sum_{i=1}^n w_{i} \left|x_i - y_i \right|
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
/// values in the respective input lists indexed by \\(i\\), while the
/// \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.manhattan_distance_with_weights([#(1.0, 2.0, 0.5), #(2.0, 3.0, 1.0)])
///       |> should.equal(Ok(1.5))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn manhattan_distance_with_weights(
  arr: List(#(Float, Float, Float)),
) -> Result(Float, Nil) {
  minkowski_distance_with_weights(arr, 1.0)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the Minkowski distance between two lists (representing
/// vectors):
///
/// \\[
/// \left( \sum_{i=1}^n w_{i} \left|x_i - y_i \right|^{p} \right)^{\frac{1}{p}}
/// \\]
///
/// In the formula, \\(p >= 1\\) is the order, \\(n\\) is the length of the two lists
/// and \\(x_i, y_i\\) are the values in the respective input lists indexed by \\(i\\).
///
/// The Minkowski distance is a generalization of the Euclidean distance
/// (\\(p=2\\)) and the Manhattan distance (\\(p = 1\\)).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///
///       let assert Ok(result) =
///         maths.minkowski_distance([#(1.0, 2.0), #(3.0, 4.0), #(5.0, 6.0)], 4.0)
///       result
///       |> maths.is_close(1.3160740129524924, 0.0, tolerance)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn minkowski_distance(
  arr: List(#(Float, Float)),
  p: Float,
) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      case p <. 1.0 {
        True -> Error(Nil)
        False -> {
          let differences = list.map(arr, fn(tuple) { tuple.0 -. tuple.1 })
          norm(differences, p)
        }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the weighted Minkowski distance between two lists (representing
/// vectors):
///
/// \\[
/// \left( \sum_{i=1}^n w_{i} \left|x_i - y_i \right|^{p} \right)^{\frac{1}{p}}
/// \\]
///
/// In the formula, \\(p >= 1\\) is the order, \\(n\\) is the length of the two lists
/// and \\(x_i, y_i\\) are the values in the respective input lists indexed by \\(i\\).
/// The \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
///
/// The Minkowski distance is a generalization of the Euclidean distance
/// (\\(p=2\\)) and the Manhattan distance (\\(p = 1\\)).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///
///       let assert Ok(result) =
///         maths.minkowski_distance_with_weights(
///           [#(1.0, 2.0, 0.5), #(3.0, 4.0, 1.0), #(5.0, 6.0, 1.0)],
///           4.0,
///         )
///       result
///       |> maths.is_close(1.2574334296829355, 0.0, tolerance)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn minkowski_distance_with_weights(
  arr: List(#(Float, Float, Float)),
  p: Float,
) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let weight_is_negative = list.any(arr, fn(tuple) { tuple.2 <. 0.0 })

      case p <. 1.0, weight_is_negative {
        False, False -> {
          let differences =
            list.map(arr, fn(tuple) { #(tuple.0 -. tuple.1, tuple.2) })
          norm_with_weights(differences, p)
        }
        _, _ -> Error(Nil)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the Euclidean distance between two lists (representing
/// vectors):
///
/// \\[
/// \left( \sum_{i=1}^n \left|x_i - y_i \right|^{2} \right)^{\frac{1}{2}}
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
/// values in the respective input lists indexed by \\(i\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///
///       let assert Ok(result) = maths.euclidean_distance([#(1.0, 2.0), #(3.0, 4.0)])
///       result
///       |> maths.is_close(1.4142135623730951, 0.0, tolerance)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn euclidean_distance(arr: List(#(Float, Float))) -> Result(Float, Nil) {
  minkowski_distance(arr, 2.0)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the weighted Euclidean distance between two lists (representing
/// vectors):
///
/// \\[
/// \left( \sum_{i=1}^n w_{i} \left|x_i - y_i \right|^{2} \right)^{\frac{1}{2}}
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
/// values in the respective input lists indexed by \\(i\\), while the
/// \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///
///       let assert Ok(result) =
///         maths.euclidean_distance_with_weights([#(1.0, 2.0, 0.5), #(3.0, 4.0, 1.0)])
///       result
///       |> maths.is_close(1.224744871391589, 0.0, tolerance)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn euclidean_distance_with_weights(
  arr: List(#(Float, Float, Float)),
) -> Result(Float, Nil) {
  minkowski_distance_with_weights(arr, 2.0)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the Chebyshev distance between two lists (representing vectors):
///
/// \\[
/// \text{max}_{i=1}^n \left|x_i - y_i \right|
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
/// values in the respective input lists indexed by \\(i\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.chebyshev_distance([#(-5.0, -1.0), #(-10.0, -12.0), #(-3.0, -3.0)])
///       |> should.equal(Ok(4.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn chebyshev_distance(arr: List(#(Float, Float))) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      list.map(arr, fn(tuple) { float.absolute_value({ tuple.0 -. tuple.1 }) })
      |> list_maximum(float.compare)
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the weighted Chebyshev distance between two lists (representing vectors):
///
/// \\[
/// \text{max}_{i=1}^n w_i \left|x_i - y_i \right|
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
/// values in the respective input lists indexed by \\(i\\), while the
/// \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.chebyshev_distance_with_weights([
///         #(-5.0, -1.0, 0.5),
///         #(-10.0, -12.0, 1.0),
///         #(-3.0, -3.0, 1.0),
///       ])
///       |> should.equal(Ok(2.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn chebyshev_distance_with_weights(
  arr: List(#(Float, Float, Float)),
) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let weight_is_negative = list.any(arr, fn(tuple) { tuple.2 <. 0.0 })

      case weight_is_negative {
        True -> Error(Nil)
        False -> {
          list.map(arr, fn(tuple) {
            float.absolute_value({ tuple.0 -. tuple.1 }) *. tuple.2
          })
          |> list_maximum(float.compare)
        }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the n'th moment about the mean of a list of elements.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> maths.moment(0)
///       |> should.be_error()
///
///       // 0th moment about the mean is 1. per definition
///       [0.0, 1.0, 2.0, 3.0, 4.0]
///       |> maths.moment(0)
///       |> should.equal(Ok(1.0))
///
///       // 1st moment about the mean is 0. per definition
///       [0.0, 1.0, 2.0, 3.0, 4.0]
///       |> maths.moment(1)
///       |> should.equal(Ok(0.0))
///
///       // 2nd moment about the mean
///       [0.0, 1.0, 2.0, 3.0, 4.0]
///       |> maths.moment(2)
///       |> should.equal(Ok(2.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn moment(arr: List(Float), n: Int) -> Result(Float, Nil) {
  case arr, n {
    // Handle empty list: no moments can be calculated
    [], _ -> Error(Nil)
    // 0th moment is always 1.0, regardless of the dataset
    _, 0 -> Ok(1.0)
    // 1st moment (about the mean) is always 0.0 by definition
    _, 1 -> Ok(0.0)
    // Higher moments for n >= 2
    _, n if n > 1 -> {
      // Calculate mean (safe because arr is non-empty)
      let assert Ok(m1) = mean(arr)

      // Compute nth moment
      let result =
        list.try_fold(arr, 0.0, fn(acc, a) {
          // Compute (a - mean)^n
          case float.power(a -. m1, int.to_float(n)) {
            Error(_) -> Error(Nil)
            // Error during power calculation
            Ok(value) -> Ok(acc +. value)
          }
        })

      // Finalize the result by dividing by the number of elements
      case result {
        Error(_) -> Error(Nil)
        // Error during accumulation
        Ok(value) -> Ok(value /. int.to_float(list.length(arr)))
      }
    }
    // Negative moments or invalid input
    _, _ -> Error(Nil)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the arithmetic mean of the elements in a list:
///
/// \\[
/// \bar{x} = \frac{1}{n}\sum_{i=1}^n x_i
/// \\]
///
/// In the formula, \\(n\\) is the sample size (the length of the list) and \\(x_i\\)
/// is the sample point in the input list indexed by \\(i\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.mean()
///       |> should.be_error()
///
///       [1.0, 2.0, 3.0]
///       |> maths.mean()
///       |> should.equal(Ok(2.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn mean(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> Ok(float.sum(arr) /. int.to_float(list.length(arr)))
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the harmonic mean \\(\bar{x}\\) of the elements in a list:
///
/// \\[
///   \bar{x} = \frac{n}{\sum_{i=1}^{n}\frac{1}{x_i}}
/// \\]
///
/// In the formula, \\(n\\) is the sample size (the length of the list) and
/// \\(x_i\\) is the sample point in the input list indexed by \\(i\\).
/// Note: The harmonic mean is only defined for positive numbers.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> maths.harmonic_mean()
///       |> should.be_error()
///
///       // List with negative numbers returns an error
///       [-1.0, -3.0, -6.0]
///       |> maths.harmonic_mean()
///       |> should.be_error()
///
///       // Valid input returns a result
///       [1.0, 3.0, 6.0]
///       |> maths.harmonic_mean()
///       |> should.equal(Ok(2.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn harmonic_mean(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let sum =
        list.try_fold(arr, 0.0, fn(acc, a) {
          case a {
            a if a >. 0.0 -> Ok(1.0 /. a +. acc)
            // If we encounter 0.0, then we can stop, as the geometric mean will be 0.0
            a if a == 0.0 -> Error(0.0)
            _ -> Error(-1.0)
          }
        })

      case sum {
        Ok(sum) -> Ok(int.to_float(list.length(arr)) /. sum)
        Error(0.0) -> Ok(0.0)
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the geometric mean \\(\bar{x}\\) of the elements in a list:
///
/// \\[
///   \bar{x} = \left(\prod^{n}_{i=1} x_i\right)^{\frac{1}{n}}
/// \\]
///
/// In the formula, \\(n\\) is the sample size (the length of the list) and
/// \\(x_i\\) is the sample point in the input list indexed by \\(i\\).
/// Note: The geometric mean is only defined for positive numbers.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> maths.geometric_mean()
///       |> should.be_error()
///
///       // List with negative numbers returns an error
///       [-1.0, -3.0, -6.0]
///       |> maths.geometric_mean()
///       |> should.be_error()
///
///       // Valid input returns a result
///       [1.0, 3.0, 9.0]
///       |> maths.geometric_mean()
///       |> should.equal(Ok(3.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn geometric_mean(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let product =
        list.try_fold(arr, 1.0, fn(acc, a) {
          case a {
            a if a >. 0.0 -> Ok(acc *. a)
            // If we encounter 0.0, then we can stop, as the geometric mean will be 0.0
            a if a == 0.0 -> Error(0.0)
            _ -> Error(-1.0)
          }
        })
      case product {
        Ok(product) ->
          float.power(product, 1.0 /. int.to_float(list.length(arr)))
        Error(0.0) -> Ok(0.0)
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the median of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       []
///       |> maths.median()
///       |> should.be_error()
///
///       [1.0, 2.0, 3.0]
///       |> maths.median()
///       |> should.equal(Ok(2.0))
///
///       [1.0, 2.0, 3.0, 4.0]
///       |> maths.median()
///       |> should.equal(Ok(2.5))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn median(arr: List(Float)) -> Result(Float, Nil) {
  use <- bool.guard(list.is_empty(arr), Error(Nil))
  let length = list.length(arr)
  let mid = length / 2
  let arr_sorted = list.sort(arr, float.compare)

  case length % 2 == 0 {
    True -> do_median(arr_sorted, mid, True, 0)
    False -> do_median(arr_sorted, mid, False, 0)
  }
}

fn do_median(
  xs: List(Float),
  mid: Int,
  mean: Bool,
  index: Int,
) -> Result(Float, Nil) {
  use <- bool.guard(index > mid, Error(Nil))
  let mid_less_one = mid - 1

  case xs {
    [x, ..] if !mean && index == mid -> Ok(x)
    [x, y, ..] if mean && index == mid_less_one -> Ok({ x +. y } /. 2.0)
    [_, ..rest] -> do_median(rest, mid, mean, index + 1)
    [] -> Error(Nil)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the sample variance of the elements in a list:
///
/// \\[
/// s^{2} = \frac{1}{n - d} \sum_{i=1}^{n}(x_i - \bar{x})
/// \\]
///
/// In the formula, \\(n\\) is the sample size (the length of the list) and \\(x_i\\)
/// is the sample point in the input list indexed by \\(i\\).
/// Furthermore, \\(\bar{x}\\) is the sample mean and \\(d\\) is the "Delta
/// Degrees of Freedom". It is typically set to \\(d = 1\\), which gives an unbiased estimate.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // Degrees of freedom
///       let ddof = 1
///
///       []
///       |> maths.variance(ddof)
///       |> should.be_error()
///
///       [1.0, 2.0, 3.0]
///       |> maths.variance(ddof)
///       |> should.equal(Ok(1.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn variance(arr: List(Float), ddof: Int) -> Result(Float, Nil) {
  case arr, ddof {
    [], _ -> Error(Nil)
    _, _ if ddof < 0 -> Error(Nil)
    _, _ -> {
      // Usage of let assert: No error will occur since we know input 'arr' is non-empty
      let assert Ok(mean) = mean(arr)
      Ok(
        list.map(arr, fn(element) {
          // Usage of let assert: The function 'float.power' will only return an error if:
          // 1. The base is negative and the exponent is fractional.
          // 2. If the base is 0 and the exponent is negative.
          // No error will occur since the exponent is positive and integer-valued.
          let assert Ok(result) = float.power(element -. mean, 2.0)
          result
        })
        |> float.sum()
        |> fn(element) {
          element /. { int.to_float(list.length(arr)) -. int.to_float(ddof) }
        },
      )
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the sample standard deviation of the elements in a list:
/// \\[
/// s = \left(\frac{1}{n - d} \sum_{i=1}^{n}(x_i - \bar{x})\right)^{\frac{1}{2}}
/// \\]
///
/// In the formula, \\(n\\) is the sample size (the length of the list) and \\(x_i\\)
/// is the sample point in the input list indexed by \\(i\\).
/// Furthermore, \\(\bar{x}\\) is the sample mean and \\(d\\) is the "Delta
/// Degrees of Freedom", and is typically set to \\(d = 1\\), which gives an unbiased estimate.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // Degrees of freedom
///       let ddof = 1
///
///       []
///       |> maths.standard_deviation(ddof)
///       |> should.be_error()
///
///       [1.0, 2.0, 3.0]
///       |> maths.standard_deviation(ddof)
///       |> should.equal(Ok(1.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn standard_deviation(arr: List(Float), ddof: Int) -> Result(Float, Nil) {
  case arr, ddof {
    [], _ -> Error(Nil)
    _, _ if ddof < 0 -> Error(Nil)
    _, _ -> {
      // Usage of let assert: No error will occur since we know input 'arr' is non-empty and
      // 'ddof' is larger than or equal to zero
      let assert Ok(variance) = variance(arr, ddof)
      float.square_root(variance)
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the sample kurtosis of a list of elements using the
/// definition of Fisher.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> maths.kurtosis()
///       |> should.be_error()
///
///       // To calculate kurtosis at least four values are needed
///       [1.0, 2.0, 3.0]
///       |> maths.kurtosis()
///       |> should.be_error()
///
///       [1.0, 2.0, 3.0, 4.0]
///       |> maths.kurtosis()
///       |> should.equal(Ok(-1.36))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn kurtosis(arr: List(Float)) -> Result(Float, Nil) {
  case list.length(arr) < 4 {
    True -> Error(Nil)
    False -> {
      case moment(arr, 2), moment(arr, 4) {
        Ok(m2), Ok(m4) if m2 != 0.0 -> {
          case float.power(m2, 2.0) {
            Ok(value) -> Ok(m4 /. value -. 3.0)
            Error(Nil) -> Error(Nil)
          }
        }
        _, _ -> Error(Nil)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the sample skewness of a list of elements using the
/// Fisher-Pearson coefficient of skewness.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> maths.skewness()
///       |> should.be_error()
///
///       // To calculate skewness at least three values are needed
///       [1.0, 2.0, 3.0]
///       |> maths.skewness()
///       |> should.equal(Ok(0.0))
///
///       [1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 3.0, 4.0]
///       |> maths.skewness()
///       |> should.equal(Ok(0.6))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn skewness(arr: List(Float)) -> Result(Float, Nil) {
  case list.length(arr) < 3 {
    True -> Error(Nil)
    False -> {
      case moment(arr, 2), moment(arr, 3) {
        Ok(m2), Ok(m3) if m2 != 0.0 -> {
          case float.power(m2, 1.5) {
            Ok(value) -> Ok(m3 /. value)
            Error(Nil) -> Error(Nil)
          }
        }
        _, _ -> Error(Nil)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the n'th percentile of the elements in a list using
/// linear interpolation between closest ranks.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> maths.percentile(40)
///       |> should.be_error()
///
///       // Calculate 40th percentile
///       [15.0, 20.0, 35.0, 40.0, 50.0]
///       |> maths.percentile(40)
///       |> should.equal(Ok(29.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn percentile(arr: List(Float), n: Int) -> Result(Float, Nil) {
  case arr, n {
    // Handle the special case when the given list is empty
    [], _ -> Error(Nil)
    // Handle the special case when the given list contains only a single element
    [element], _ -> Ok(element)
    _, n if n == 0 -> list.first(list.sort(arr, float.compare))
    _, n if n == 100 -> list.last(list.sort(arr, float.compare))
    _, n if n > 0 && n < 100 -> {
      // Calculate the rank of the n'th percentile
      let r: Float =
        int.to_float(n) /. 100.0 *. int.to_float(list.length(arr) - 1)
      let f: Int = float.truncate(r)
      let sorted_arr = list.drop(list.sort(arr, float.compare), f)
      // Directly extract the lower and upper values. Theoretically, an error value
      // will not be returned as the largest index in the array that is accessed will
      // be the length of the array - 1 (last element).
      case list.take(sorted_arr, 2) {
        [lower, upper] -> {
          Ok(lower +. { upper -. lower } *. { r -. int.to_float(f) })
        }
        _ -> Error(Nil)
      }
    }
    _, _ -> Error(Nil)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the z-score of each value in the list relative to the sample
/// mean and standard deviation.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       // Use degrees of freedom = 1
///       |> maths.zscore(1)
///       |> should.be_error()
///
///       [1.0, 2.0, 3.0]
///       // Use degrees of freedom = 1
///       |> maths.zscore(1)
///       |> should.equal(Ok([-1.0, 0.0, 1.0]))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn zscore(arr: List(Float), ddof: Int) -> Result(List(Float), Nil) {
  let length = list.length(arr)
  case arr, ddof {
    [], _ -> Error(Nil)
    // Invalid degrees of freedom
    _, ddof if ddof < 0 -> Error(Nil)
    // Insufficient data points for the given degrees of freedom
    _, ddof if length <= ddof -> Error(Nil)
    // Valid input
    _, _ -> {
      case mean(arr), standard_deviation(arr, ddof) {
        // The mean and standard deviation have been successfully computed
        Ok(mean), Ok(stdev) if stdev != 0.0 ->
          Ok(list.map(arr, fn(a) -> Float { { a -. mean } /. stdev }))
        // The standard deviation is zero (e.g., all elements are identical)
        _, _ -> Error(Nil)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the interquartile range (IQR) of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> maths.interquartile_range()
///       |> should.be_error()
///
///       // Valid input returns a result
///       [1.0, 2.0, 3.0, 4.0, 5.0]
///       |> maths.interquartile_range()
///       |> should.equal(Ok(3.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn interquartile_range(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let length = list.length(arr)
      let arr_sorted = list.sort(arr, float.compare)

      case int.is_even(length) {
        True -> {
          // 'lower_half' contains the smallest values
          // 'upper_half' contains the largest values
          let #(lower_half, upper_half) = list.split(arr_sorted, length / 2)

          case median(lower_half), median(upper_half) {
            // Compute IQR as Q3 - Q1
            Ok(q1), Ok(q3) -> Ok(q3 -. q1)
            // Handle potential errors from `median`
            _, _ -> Error(Nil)
          }
        }
        False -> {
          // 'lower_half' contains the smallest values
          let #(lower_half, _) = list.split(arr_sorted, { length - 1 } / 2)
          // 'upper_half' contains the largest values
          let #(_, upper_half) = list.split(arr_sorted, { length + 1 } / 2)

          case median(lower_half), median(upper_half) {
            // Compute IQR as Q3 - Q1
            Ok(q1), Ok(q3) -> Ok(q3 -. q1)
            // Handle potential errors from `median`
            _, _ -> Error(Nil)
          }
        }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate Pearson's sample correlation coefficient to determine the linear
/// relationship between the elements in two lists of equal
/// length. The correlation coefficient \\(r_{xy} \in \[-1, 1\]\\) is calculated
/// as:
///
/// \\[
/// r_{xy} =\frac{\sum ^n _{i=1}(x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum^n _{i=1}(x_i - \bar{x})^2} \sqrt{\sum^n _{i=1}(y_i - \bar{y})^2}}
/// \\]
///
/// In the formula, \\(n\\) is the sample size (the length of the input lists),
/// \\(x_i\\), \\(y_i\\) are the corresponding sample points indexed by \\(i\\) and
/// \\(\bar{x}\\), \\(\bar{y}\\) are the sample means.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       // An empty lists returns an error
///       maths.correlation([], [])
///       |> should.be_error()
///
///       // Perfect positive correlation
///       let xarr =
///         list.range(0, 100)
///         |> list.map(fn(x) { int.to_float(x) })
///       let yarr =
///         list.range(0, 100)
///         |> list.map(fn(y) { int.to_float(y) })
///       list.zip(xarr, yarr)
///       |> maths.correlation()
///       |> should.equal(Ok(1.0))
///
///       // Perfect negative correlation
///       let xarr =
///         list.range(0, 100)
///         |> list.map(fn(x) { -1.0 *. int.to_float(x) })
///       let yarr =
///         list.range(0, 100)
///         |> list.map(fn(y) { int.to_float(y) })
///       list.zip(xarr, yarr)
///       |> maths.correlation()
///       |> should.equal(Ok(-1.0))
///
///       // No correlation (independent variables)
///       let xarr = [1.0, 2.0, 3.0, 4.0]
///       let yarr = [5.0, 5.0, 5.0, 5.0]
///       list.zip(xarr, yarr)
///       |> maths.correlation()
///       |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn correlation(arr: List(#(Float, Float))) -> Result(Float, Nil) {
  let length = list.length(arr)
  case length >= 2 {
    False -> Error(Nil)
    True -> {
      let #(xarr, yarr) = list.unzip(arr)
      let assert Ok(xmean) = mean(xarr)
      let assert Ok(ymean) = mean(yarr)
      let a =
        list.map(arr, fn(tuple) -> Float {
          { tuple.0 -. xmean } *. { tuple.1 -. ymean }
        })
        |> float.sum()
      let b =
        list.map(xarr, fn(x) { { x -. xmean } *. { x -. xmean } })
        |> float.sum()
      let c =
        list.map(yarr, fn(y: Float) { { y -. ymean } *. { y -. ymean } })
        |> float.sum()
      // The argument is the product of two sums of squared differences
      // it will never be negative. So just extract it directly:
      let assert Ok(value) = float.square_root(b *. c)
      Ok(a /. value)
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The Jaccard index measures similarity between two sets of elements. Mathematically, the
/// Jaccard index is defined as:
///
/// \\[
/// \frac{|X \cap Y|}{|X \cup Y|} \\; \in \\; \left[0, 1\right]
/// \\]
///
/// where:
///
/// - \\(X\\) and \\(Y\\) are two sets being compared
/// - \\(|X \cap Y|\\) represents the size of the intersection of the two sets
/// - \\(|X \cup Y|\\) denotes the size of the union of the two sets
///
/// The value of the Jaccard index ranges from 0 to 1, where 0 indicates that the
/// two sets share no elements and 1 indicates that the sets are identical. The
/// Jaccard index is a special case of the  [Tversky index](#tversky_index) (with
/// \\(\alpha=\beta=1\\)).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/set
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let xset = set.from_list(["cat", "dog", "hippo", "monkey"])
///       let yset = set.from_list(["monkey", "rhino", "ostrich", "salmon"])
///       maths.jaccard_index(xset, yset)
///       |> should.equal(1.0 /. 7.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn jaccard_index(xset: set.Set(a), yset: set.Set(a)) -> Float {
  // Usage of let assert: No error will occur since the input parameters 'alpha' and 'beta'
  // are larger than or equal to zero
  let assert Ok(result) = tversky_index(xset, yset, 1.0, 1.0)
  result
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The Sørensen-Dice coefficient measures the similarity between two sets of elements.
/// Mathematically, the coefficient is defined as:
///
/// \\[
/// \frac{2 |X \cap Y|}{|X| + |Y|} \\; \in \\; \left[0, 1\right]
/// \\]
///
/// where:
///
/// - \\(X\\) and \\(Y\\) are two sets being compared
/// - \\(|X \cap Y|\\) is the size of the intersection of the two sets (i.e., the
/// number of elements common to both sets)
/// - \\(|X|\\) and \\(|Y|\\) are the sizes of the sets \\(X\\) and \\(Y\\), respectively
///
/// The coefficient ranges from 0 to 1, where 0 indicates no similarity (the sets
/// share no elements) and 1 indicates perfect similarity (the sets are identical).
/// The higher the coefficient, the greater the similarity between the two sets.
/// The Sørensen-Dice coefficient is a special case of the
/// [Tversky index](#tversky_index) (with \\(\alpha=\beta=0.5\\)).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/set
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let xset = set.from_list(["cat", "dog", "hippo", "monkey"])
///       let yset = set.from_list(["monkey", "rhino", "ostrich", "salmon", "spider"])
///       maths.sorensen_dice_coefficient(xset, yset)
///       |> should.equal(2.0 *. 1.0 /. { 4.0 +. 5.0 })
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn sorensen_dice_coefficient(xset: set.Set(a), yset: set.Set(a)) -> Float {
  // Usage of let assert: No error will occur since the input parameters 'alpha' and 'beta'
  // are larger than or equal to zero
  let assert Ok(result) = tversky_index(xset, yset, 0.5, 0.5)
  result
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The Tversky index is a generalization of the Jaccard index and Sørensen-Dice
/// coefficient, which adds flexibility in measuring similarity between two sets using two
/// parameters, \\(\alpha\\) and \\(\beta\\). These parameters allow for asymmetric
/// similarity measures between sets. The Tversky index is defined as:
///
/// \\[
/// \frac{|X \cap Y|}{|X \cap Y| + \alpha|X - Y| + \beta|Y - X|}
/// \\]
///
/// where:
///
/// - \\(X\\) and \\(Y\\) are the sets being compared
/// - \\(|X - Y|\\) and \\(|Y - X|\\) are the sizes of the relative complements of
/// \\(Y\\) in \\(X\\) and \\(X\\) in \\(Y\\), respectively,
/// - \\(\alpha\\) and \\(\beta\\) are parameters that weight the relative importance
/// of the elements unique to \\(X\\) and \\(Y\\)
///
/// The Tversky index reduces to the Jaccard index when \\(\alpha = \beta = 1\\) and
/// to the Sørensen-Dice coefficient when \\(\alpha = \beta = 0.5\\). In general, the
/// Tversky index can take on any non-negative value, including 0. The index equals
/// 0 when there is no intersection between the two sets, indicating no similarity.
/// The Tversky index does not have a strict upper limit of 1 when \\(\alpha \neq \beta\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/set
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let yset = set.from_list(["cat", "dog", "hippo", "monkey"])
///       let xset = set.from_list(["monkey", "rhino", "ostrich", "salmon"])
///       // Test Jaccard index (alpha = beta = 1)
///       maths.tversky_index(xset, yset, 1.0, 1.0)
///       |> should.equal(Ok(1.0 /. 7.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn tversky_index(
  xset: set.Set(a),
  yset: set.Set(a),
  alpha: Float,
  beta: Float,
) -> Result(Float, Nil) {
  case alpha >=. 0.0, beta >=. 0.0 {
    True, True -> {
      let intersection =
        set.intersection(xset, yset)
        |> set.size()
        |> int.to_float()
      let difference1 =
        set.difference(xset, yset)
        |> set.size()
        |> int.to_float()
      let difference2 =
        set.difference(yset, xset)
        |> set.size()
        |> int.to_float()
      Ok(
        intersection
        /. { intersection +. alpha *. difference1 +. beta *. difference2 },
      )
    }
    _, _ -> Error(Nil)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The Overlap coefficient, also known as the Szymkiewicz–Simpson coefficient, is
/// a measure of similarity between two sets that focuses on the size of the
/// intersection relative to the smaller of the two sets. It is defined
/// mathematically as:
///
/// \\[
/// \frac{|X \cap Y|}{\min(|X|, |Y|)} \\; \in \\; \left[0, 1\right]
/// \\]
///
/// where:
///
/// - \\(X\\) and \\(Y\\) are the sets being compared
/// - \\(|X \cap Y|\\) is the size of the intersection of the sets
/// - \\(\min(|X|, |Y|)\\) is the size of the smaller set among \\(X\\) and \\(Y\\)
///
/// The coefficient ranges from 0 to 1, where 0 indicates no overlap and 1
/// indicates that the smaller set is a suyset of the larger set. This
/// measure is especially useful in situations where the similarity in terms
/// of the proportion of overlap is more relevant than the difference in sizes
/// between the two sets.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/set
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let set_a = set.from_list(["horse", "dog", "hippo", "monkey", "bird"])
///       let set_b = set.from_list(["monkey", "bird", "ostrich", "salmon"])
///       maths.overlap_coefficient(set_a, set_b)
///       |> should.equal(2.0 /. 4.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn overlap_coefficient(xset: set.Set(a), yset: set.Set(a)) -> Float {
  let intersection =
    set.intersection(xset, yset)
    |> set.size()
    |> int.to_float()
  let minsize =
    int.min(set.size(xset), set.size(yset))
    |> int.to_float()
  intersection /. minsize
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the cosine similarity between two lists (representing
/// vectors):
///
/// \\[
/// \frac{\sum_{i=1}^n  x_i \cdot y_i}
/// {\left(\sum_{i=1}^n x_i^2\right)^{\frac{1}{2}}
/// \cdot
/// \left(\sum_{i=1}^n y_i^2\right)^{\frac{1}{2}}}
/// \\; \in \\; \left[-1, 1\right]
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists and \\(x_i\\), \\(y_i\\) are
/// the values in the respective input lists indexed by \\(i\\).
///
/// The cosine similarity provides a value between -1 and 1, where 1 means the
/// vectors are in the same direction, -1 means they are in exactly opposite
/// directions, and 0 indicates orthogonality.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/option
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       // Two orthogonal vectors
///       maths.cosine_similarity([#(-1.0, 1.0), #(1.0, 1.0), #(0.0, -1.0)])
///       |> should.equal(Ok(0.0))
///
///       // Two identical (parallel) vectors
///       maths.cosine_similarity([#(1.0, 1.0), #(2.0, 2.0), #(3.0, 3.0)])
///       |> should.equal(Ok(1.0))
///
///       // Two parallel, but oppositely oriented vectors
///       maths.cosine_similarity([#(-1.0, 1.0), #(-2.0, 2.0), #(-3.0, 3.0)])
///       |> should.equal(Ok(-1.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn cosine_similarity(arr: List(#(Float, Float))) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let numerator =
        list.fold(arr, 0.0, fn(acc, tuple) { acc +. tuple.0 *. tuple.1 })

      let xarr = list.map(arr, fn(tuple) { tuple.0 })
      let yarr = list.map(arr, fn(tuple) { tuple.1 })

      // Usage of let assert: No error will occur since the input lists 'xarr' and 'yarr'
      // are non-empty and p = 2 is positive and integer-valued.
      let assert Ok(xarr_norm) = norm(xarr, 2.0)
      let assert Ok(yarr_norm) = norm(yarr, 2.0)

      let denominator = {
        xarr_norm *. yarr_norm
      }
      Ok(numerator /. denominator)
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the weighted cosine similarity between two lists (representing
/// vectors):
///
/// \\[
/// \frac{\sum_{i=1}^n w_{i} \cdot x_i \cdot y_i}
/// {\left(\sum_{i=1}^n w_{i} \cdot x_i^2\right)^{\frac{1}{2}}
/// \cdot
/// \left(\sum_{i=1}^n w_{i} \cdot y_i^2\right)^{\frac{1}{2}}}
/// \\; \in \\; \left[-1, 1\right]
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists and \\(x_i\\), \\(y_i\\) are
/// the values in the respective input lists indexed by \\(i\\), while the
/// \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
///
/// The cosine similarity provides a value between -1 and 1, where 1 means the
/// vectors are in the same direction, -1 means they are in exactly opposite
/// directions, and 0 indicates orthogonality.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/option
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///
///       let assert Ok(result) =
///         maths.cosine_similarity_with_weights([
///           #(1.0, 1.0, 2.0),
///           #(2.0, 2.0, 3.0),
///           #(3.0, 3.0, 4.0),
///         ])
///       result
///       |> maths.is_close(1.0, 0.0, tolerance)
///       |> should.be_true()
///
///       let assert Ok(result) =
///         maths.cosine_similarity_with_weights([
///           #(-1.0, 1.0, 1.0),
///           #(-2.0, 2.0, 0.5),
///           #(-3.0, 3.0, 0.33),
///         ])
///       result
///       |> maths.is_close(-1.0, 0.0, tolerance)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn cosine_similarity_with_weights(
  arr: List(#(Float, Float, Float)),
) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let weight_is_negative = list.any(arr, fn(tuple) { tuple.2 <. 0.0 })

      case weight_is_negative {
        False -> {
          let numerator =
            list.fold(arr, 0.0, fn(acc, tuple) {
              acc +. tuple.0 *. tuple.1 *. tuple.2
            })

          let xarr = list.map(arr, fn(tuple) { #(tuple.0, tuple.2) })
          let yarr = list.map(arr, fn(tuple) { #(tuple.1, tuple.2) })

          // Usage of let assert: No error will occur since the input lists 'xarr' and 'yarr'
          // are non-empty and p = 2 is positive and integer valued.
          let assert Ok(xarr_norm) = norm_with_weights(xarr, 2.0)
          let assert Ok(yarr_norm) = norm_with_weights(yarr, 2.0)

          let denominator = {
            xarr_norm *. yarr_norm
          }
          Ok(numerator /. denominator)
        }
        True -> Error(Nil)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the Canberra distance between two lists:
///
/// \\[
/// \sum_{i=1}^n \frac{\left| x_i - y_i \right|}
/// {\left| x_i \right| + \left| y_i \right|}
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists, and \\(x_i, y_i\\) are the
/// values in the respective input lists indexed by \\(i\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.canberra_distance([])
///       |> should.be_error()
///
///       maths.canberra_distance([#(1.0, -2.0), #(2.0, -1.0)])
///       |> should.equal(Ok(2.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn canberra_distance(arr: List(#(Float, Float))) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      Ok(
        list.fold(arr, 0.0, fn(acc, tuple) {
          let numerator = float.absolute_value({ tuple.0 -. tuple.1 })
          let denominator = {
            float.absolute_value(tuple.0) +. float.absolute_value(tuple.1)
          }
          acc +. numerator /. denominator
        }),
      )
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the weighted Canberra distance between two lists:
///
/// \\[
/// \sum_{i=1}^n w_{i}\frac{\left| x_i - y_i \right|}
/// {\left| x_i \right| + \left| y_i \right|}
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists, and \\(x_i, y_i\\) are the
/// values in the respective input lists indexed by \\(i\\), while the
/// \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.canberra_distance_with_weights([])
///       |> should.be_error()
///
///       maths.canberra_distance_with_weights([#(1.0, -2.0, 0.5), #(2.0, -1.0, 1.0)])
///       |> should.equal(Ok(1.5))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn canberra_distance_with_weights(
  arr: List(#(Float, Float, Float)),
) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let weight_is_negative = list.any(arr, fn(tuple) { tuple.2 <. 0.0 })

      case weight_is_negative {
        True -> Error(Nil)
        False -> {
          Ok(
            list.fold(arr, 0.0, fn(acc, tuple) {
              let numerator = float.absolute_value({ tuple.0 -. tuple.1 })
              let denominator = {
                float.absolute_value(tuple.0) +. float.absolute_value(tuple.1)
              }
              acc +. tuple.2 *. numerator /. denominator
            }),
          )
        }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the Bray-Curtis distance between two lists:
///
/// \\[
/// \frac{\sum_{i=1}^n  \left| x_i - y_i \right|}
/// {\sum_{i=1}^n \left| x_i + y_i \right|}
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists, and \\(x_i, y_i\\) are the values
/// in the respective input lists indexed by \\(i\\).
///
/// The Bray-Curtis distance is in the range \\([0, 1]\\) if all entries \\(x_i, y_i\\) are
/// positive.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.braycurtis_distance([])
///       |> should.be_error()
///
///       maths.braycurtis_distance([#(1.0, 3.0), #(2.0, 4.0)])
///       |> should.equal(Ok(0.4))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
///
pub fn braycurtis_distance(arr: List(#(Float, Float))) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let numerator =
        list.fold(arr, 0.0, fn(acc, tuple) {
          acc +. float.absolute_value({ tuple.0 -. tuple.1 })
        })

      let denominator =
        list.fold(arr, 0.0, fn(acc, tuple) {
          acc +. float.absolute_value({ tuple.0 +. tuple.1 })
        })

      Ok({ numerator /. denominator })
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the weighted Bray-Curtis distance between two lists:
///
/// \\[
/// \frac{\sum_{i=1}^n w_{i} \left| x_i - y_i \right|}
/// {\sum_{i=1}^n w_{i}\left| x_i + y_i \right|}
/// \\]
///
/// In the formula, \\(n\\) is the length of the two lists, and \\(x_i, y_i\\) are the values
/// in the respective input lists indexed by \\(i\\), while the
/// \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
///
/// The Bray-Curtis distance is in the range \\([0, 1]\\) if all entries \\(x_i, y_i\\) are
/// positive and \\(w_i = 1.0\\;\forall i=1...n\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.braycurtis_distance_with_weights([])
///       |> should.be_error()
///
///       maths.braycurtis_distance_with_weights([#(1.0, 3.0, 0.5), #(2.0, 4.0, 1.0)])
///       |> should.equal(Ok(0.375))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn braycurtis_distance_with_weights(
  arr: List(#(Float, Float, Float)),
) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let weight_is_negative = list.any(arr, fn(tuple) { tuple.2 <. 0.0 })

      case weight_is_negative {
        True -> Error(Nil)
        False -> {
          let numerator =
            list.fold(arr, 0.0, fn(acc, tuple) {
              acc +. tuple.2 *. float.absolute_value({ tuple.0 -. tuple.1 })
            })

          let denominator =
            list.fold(arr, 0.0, fn(acc, tuple) {
              acc +. tuple.2 *. float.absolute_value({ tuple.0 +. tuple.1 })
            })

          Ok({ numerator /. denominator })
        }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Determine if a given value \\(x\\) is close to or equivalent to a reference value
/// \\(y\\) based on supplied relative \\(r_{tol}\\) and absolute \\(a_{tol}\\) tolerance
/// values. The equivalance of the two given values are then determined based on
/// the equation:
///
/// \\[
///     \|x - y\| \leq (a_{tol} + r_{tol} \cdot \|y\|)
/// \\]
///
/// `True` is returned if the statement holds, otherwise `False` is returned.
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let value = 99.
///       let reference_value = 100.
///       // We set 'absolute_tolerance' and 'relative_tolerance' such that the values are
///       // equivalent if 'value' is within 1 percent of 'reference_value' +/- 0.1
///       let relative_tolerance = 0.01
///       let absolute_tolerance = 0.10
///       maths.is_close(value, reference_value, relative_tolerance, absolute_tolerance)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn is_close(x: Float, y: Float, rtol: Float, atol: Float) -> Bool {
  let x = absolute_difference(x, y)
  let y = atol +. rtol *. float.absolute_value(y)
  x <=. y
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Determine if each value \\(x_i\\) is close to or equivalent to its corresponding reference value
/// \\(y_i\\), in a list of value pairs \\((x_i, y_i)\\), based on supplied relative \\(r_{tol}\\)
/// and absolute  \\(a_{tol}\\) tolerance values. The equivalence of each pair \\((x_i, y_i)\\) is
/// determined by the equation:
///
/// \\[
///     \|x_i - y_i\| \leq (a_{tol} + r_{tol} \cdot \|y_i\|), \\; \forall i=1,...,n.
/// \\]
///
/// A list of `Bool` values is returned, where each entry indicates if the corresponding pair
/// satisfies the condition. If all conditions are satisfied, the list will contain only `True`
/// values.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/list
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let value = 99.0
///       let reference_value = 100.0
///       let xarr = list.repeat(value, 42)
///       let yarr = list.repeat(reference_value, 42)
///       let arr = list.zip(xarr, yarr)
///       // We set 'absolute_tolerance' and 'relative_tolerance' such that
///       // the values are equivalent if 'value' is within 1 percent of
///       // 'reference_value' +/- 0.1
///       let relative_tolerance = 0.01
///       let absolute_tolerance = 0.1
///       let assert Ok(result) =
///         maths.all_close(arr, relative_tolerance, absolute_tolerance)
///       result
///       |> list.all(fn(x) { x == True })
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn all_close(
  arr: List(#(Float, Float)),
  rtol: Float,
  atol: Float,
) -> List(Bool) {
  use #(x, y) <- list.map(arr)

  is_close(x, y, rtol, atol)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Determine if a given value \\(x\\) is fractional, i.e., if it contains a fractional part:
///
/// \\[
///     x - \lfloor x \rfloor > 0
/// \\]
///
/// `True` is returned if the given value is fractional (i.e., it has a non-zero decimal part),
/// otherwise `False` is returned.
///
/// <details>
///     <summary>Example</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       maths.is_fractional(0.3333)
///       |> should.equal(True)
///
///       maths.is_fractional(1.0)
///       |> should.equal(False)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn is_fractional(x: Float) -> Bool {
  do_ceiling(x) -. x >. 0.0
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A function that determines if a given integer value \\(x \in \mathbb{Z}\\) is a power of
/// another integer value \\(y \in \mathbb{Z}\\), i.e., the function evaluates whether \\(x\\) can
/// be expressed as \\(y^n\\) for some integer \\(n \geq 0\\), by computing the base-\\(y\\)
/// logarithm of \\(x\\):
///
/// \\[
///     n = \log_y(x)
/// \\]
///
/// If \\(n\\) is an integer (i.e., it has no fractional part), then \\(x\\) is a power of \\(y\\)
/// and `True` is returned. Otherwise `False` is returned.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       // Check if 4 is a power of 2 (it is)
///       maths.is_power(4, 2)
///       |> should.equal(True)
///
///       // Check if 5 is a power of 2 (it is not)
///       maths.is_power(5, 2)
///       |> should.equal(False)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn is_power(x: Int, y: Int) -> Bool {
  case logarithm(int.to_float(x), int.to_float(y)) {
    Ok(value) -> {
      let truncated = round_to_zero(value, 0)
      let remainder = value -. truncated
      remainder == 0.0
    }
    Error(_) -> False
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A function that tests whether a given integer value \\(n \in \mathbb{Z}\\) is a
/// perfect number. A number is perfect if it is equal to the sum of its proper
/// positive divisors.
///
/// <details>
///     <summary>Details</summary>
///
///   For example:
///   - \\(6\\) is a perfect number since the divisors of 6 are \\(1 + 2 + 3 = 6\\).
///   - \\(28\\) is a perfect number since the divisors of 28 are \\(1 + 2 + 4 + 7 + 14 = 28\\).
///
/// </details>
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.is_perfect(6)
///       |> should.equal(True)
///
///       maths.is_perfect(28)
///       |> should.equal(True)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn is_perfect(n: Int) -> Bool {
  int.sum(proper_divisors(n)) == n
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A function that tests whether a given integer value \\(x \in \mathbb{Z}\\) is a
/// prime number. A prime number is a natural number greater than 1 that has no
/// positive divisors other than 1 and itself.
///
/// The function uses the Miller-Rabin primality test to assess if \\(x\\) is prime.
/// It is a probabilistic test, so it can mistakenly identify a composite number
/// as prime. However, the probability of such errors decreases with more testing
/// iterations (the function uses 64 iterations internally, which is typically
/// more than sufficient). The Miller-Rabin test is particularly useful for large
/// numbers.
///
/// <details>
///     <summary>Details</summary>
///
///   Examples of prime numbers:
///   - \\(2\\) is a prime number since it has only two divisors: \\(1\\) and \\(2\\).
///   - \\(7\\) is a prime number since it has only two divisors: \\(1\\) and \\(7\\).
///   - \\(4\\) is not a prime number since it has divisors other than \\(1\\) and itself, such
///     as \\(2\\).
///
/// </details>
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.is_prime(2)
///       |> should.equal(True)
///
///       maths.is_prime(4)
///       |> should.equal(False)
///
///       // Test the 2nd Carmichael number
///       maths.is_prime(1105)
///       |> should.equal(False)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn is_prime(x: Int) -> Bool {
  case x {
    x if x < 2 -> {
      False
    }
    x if x == 2 -> {
      True
    }
    _ -> {
      miller_rabin_test(x, 64)
    }
  }
}

fn miller_rabin_test(n: Int, k: Int) -> Bool {
  case n, k {
    _, 0 -> True
    _, _ -> {
      // Generate a random int in the range [2, n]
      let random_candidate = 2 + int.random(n - 2)
      case powmod_with_check(random_candidate, n - 1, n) == 1 {
        True -> miller_rabin_test(n, k - 1)
        False -> False
      }
    }
  }
}

fn powmod_with_check(base: Int, exponent: Int, modulus: Int) -> Int {
  case exponent, { exponent % 2 } == 0 {
    0, _ -> 1
    _, True -> {
      let x = powmod_with_check(base, exponent / 2, modulus)
      case { x * x } % modulus, x != 1 && x != { modulus - 1 } {
        1, True -> 0
        _, _ -> { x * x } % modulus
      }
    }
    _, _ -> { base * powmod_with_check(base, exponent - 1, modulus) } % modulus
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A function that tests whether a given real number \\(x \in \mathbb{R}\\) is strictly
/// between two other real numbers, \\(a,b \in \mathbb{R}\\), such that \\(a < x < b\\).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.is_between(5.5, 5.0, 6.0)
///       |> should.equal(True)
///
///       maths.is_between(5.0, 5.0, 6.0)
///       |> should.equal(False)
///
///       maths.is_between(6.0, 5.0, 6.0)
///       |> should.equal(False)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn is_between(x: Float, lower: Float, upper: Float) -> Bool {
  lower <. x && x <. upper
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A function that tests whether a given integer \\(n \in \mathbb{Z}\\) is divisible by another
/// integer \\(d \in \mathbb{Z}\\), such that \\(n \mod d = 0\\).
///
/// <details>
///     <summary>Details</summary>
///
///   For example:
///   - \\(n = 10\\) is divisible by \\(d = 2\\) because \\(10 \mod 2 = 0\\).
///   - \\(n = 7\\) is not divisible by \\(d = 3\\) because \\(7 \mod 3 \neq 0\\).
///
/// </details>
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.is_divisible(10, 2)
///       |> should.equal(True)
///
///       maths.is_divisible(7, 3)
///       |> should.equal(False)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn is_divisible(n: Int, d: Int) -> Bool {
  n % d == 0
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A function that tests whether a given integer \\(m \in \mathbb{Z}\\) is a multiple of another
/// integer \\(k \in \mathbb{Z}\\), such that \\(m = k \cdot q\\), with \\(q \in \mathbb{Z}\\).
///
/// <details>
///     <summary>Details</summary>
///
///   For example:
///   - \\(m = 15\\) is a multiple of \\(k = 5\\) because \\(15 = 5 \cdot 3\\).
///   - \\(m = 14\\) is not a multiple of \\(k = 5\\) because \\(\frac{14}{5}\\) does not yield an
///     integer quotient.
///
/// </details>
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       maths.is_multiple(15, 5)
///       |> should.equal(True)
///
///       maths.is_multiple(14, 5)
///       |> should.equal(False)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn is_multiple(m: Int, k: Int) -> Bool {
  m % k == 0
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The beta function over the real numbers:
///
/// \\[
/// \text{B}(x, y) = \frac{\Gamma(x) \cdot \Gamma(y)}{\Gamma(x + y)}
/// \\]
///
/// The beta function is evaluated through the use of the gamma function.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn beta(x: Float, y: Float) -> Float {
  gamma(x) *. gamma(y) /. gamma(x +. y)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The error function.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn erf(x: Float) -> Float {
  let a1 = 0.254829592
  let a2 = -0.284496736
  let a3 = 1.421413741
  let a4 = -1.453152027
  let a5 = 1.061405429

  let p = 0.3275911

  let sign = sign(x)
  let x = float.absolute_value(x)

  // Formula 7.1.26 given in Abramowitz and Stegun.
  let t = 1.0 /. { 1.0 +. p *. x }
  let y =
    1.0
    -. { { { { a5 *. t +. a4 } *. t +. a3 } *. t +. a2 } *. t +. a1 }
    *. t
    *. exponential(-1.0 *. x *. x)
  sign *. y
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The gamma function over the real numbers. The function is essentially equal to
/// the factorial for any positive integer argument: \\(\Gamma(n) = (n - 1)!\\)
///
/// The implemented gamma function is approximated through Lanczos approximation
/// using the same coefficients used by the GNU Scientific Library.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn gamma(x: Float) -> Float {
  gamma_lanczos(x)
}

/// A constant used in the Lanczos approximation formula.
const lanczos_g: Float = 7.0

/// Lanczos coefficients for the approximation formula. These coefficients are part of a
/// polynomial approximation to the Gamma function.
const lanczos_p: List(Float) = [
  0.99999999999980993, 676.5203681218851, -1259.1392167224028,
  771.32342877765313, -176.61502916214059, 12.507343278686905,
  -0.13857109526572012, 0.0000099843695780195716, 0.00000015056327351493116,
]

/// Compute the Gamma function using an approximation with the same coefficients used by the GNU
/// Scientific Library. The function handles both the reflection formula for `x < 0.5` and the
/// standard Lanczos computation for `x >= 0.5`.
fn gamma_lanczos(x: Float) -> Float {
  case x <. 0.5 {
    // Use the reflection formula to compute Gamma for small value of x
    True -> pi() /. { sin(pi() *. x) *. gamma_lanczos(1.0 -. x) }
    // Evaluate the polynomial approximation using the coefficients from the GNU Scientific Library
    False -> {
      let z = x -. 1.0
      let x =
        list.index_fold(lanczos_p, 0.0, fn(acc, v, index) {
          case index > 0 {
            True -> acc +. v /. { z +. int.to_float(index) }
            False -> v
          }
        })
      let t = z +. lanczos_g +. 0.5
      // Usage of let assert: The function 'float.power' will only return an error if:
      // 1. The base is negative and the exponent is fractional.
      // 2. If the base is 0 and the exponent is negative.
      // No error will occur below since the bases are/will always be non-zero and positive. The
      // exponents will also always be positive.
      let assert Ok(v1) = float.power(2.0 *. pi(), 0.5)
      let assert Ok(v2) = float.power(t, z +. 0.5)
      v1 *. v2 *. exponential(-1.0 *. t) *. x
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The lower incomplete gamma function over the real numbers.
///
/// The implemented incomplete gamma function is evaluated through a power series
/// expansion.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn incomplete_gamma(a: Float, x: Float) -> Result(Float, Nil) {
  case a >. 0.0 && x >=. 0.0 {
    True -> {
      // Usage of let assert: The function 'float.power' will only return an error if:
      // 1. The base is negative and the exponent is fractional.
      // 2. If the base is 0 and the exponent is negative.
      // No error will occur below since the base is non-zero and positive. The exponent will
      // always be positive.
      let assert Ok(v) = float.power(x, a)
      Ok(
        v
        *. exponential(-1.0 *. x)
        *. incomplete_gamma_sum(a, x, 1.0 /. a, 0.0, 1.0),
      )
    }

    False -> Error(Nil)
  }
}

fn incomplete_gamma_sum(
  a: Float,
  x: Float,
  t: Float,
  s: Float,
  n: Float,
) -> Float {
  case t {
    0.0 -> s
    _ -> {
      let ns = s +. t
      let nt = t *. { x /. { a +. n } }
      incomplete_gamma_sum(a, x, nt, ns, n +. 1.0)
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function returns a list of evenly spaced values within a specified interval
/// `[start, stop)` based on a given increment size.
///
/// Note that if `increment > 0`, the sequence progresses from `start`  towards `stop`, while if
/// `increment < 0`, the sequence progresses from `start` towards `stop` in reverse.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       maths.step_range(1.0, 5.0, 1.0)
///       |> should.equal([1.0, 2.0, 3.0, 4.0])
///
///       // No points returned since
///       // start is smaller than stop and the step is positive
///       maths.step_range(5.0, 1.0, 1.0)
///       |> should.equal([])
///
///       // Points returned since
///       // start smaller than stop but negative step
///       maths.step_range(5.0, 1.0, -1.0)
///       |> should.equal([5.0, 4.0, 3.0, 2.0])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn step_range(start: Float, stop: Float, increment: Float) -> List(Float) {
  case
    { start >=. stop && increment >. 0.0 }
    || { start <=. stop && increment <. 0.0 }
  {
    True -> []
    False -> {
      let direction = case start <=. stop {
        True -> 1.0
        False -> -1.0
      }

      let increment_abs = float.absolute_value(increment)
      let distance = float.absolute_value(start -. stop)
      let steps = float.round(distance /. increment_abs)
      let adjusted_stop = stop -. increment_abs *. direction

      // Generate the sequence from 'adjusted_stop' towards 'start'
      do_step_range(adjusted_stop, increment_abs *. direction, steps, [])
    }
  }
}

fn do_step_range(
  current: Float,
  increment: Float,
  remaining_steps: Int,
  acc: List(Float),
) -> List(Float) {
  case remaining_steps {
    0 -> acc
    _ ->
      do_step_range(current -. increment, increment, remaining_steps - 1, [
        current,
        ..acc
      ])
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function is similar to [`step_range`](#step_range) but instead returns a yielder
/// (lazily evaluated sequence of elements). This function can be used whenever there is a need
/// to generate a larger-than-usual sequence of elements.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/yielder.{Next, Done}
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let range = maths.yield_step_range(1.0, 2.5, 0.5)
///
///       let assert Next(element, rest) = yielder.step(range)
///       should.equal(element, 1.0)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 1.5)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 2.0)
///
///       // We have generated 3 values over the interval [1.0, 2.5)
///       // in increments of 0.5, so the 4th will be 'Done'
///       should.equal(yielder.step(rest), Done)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn yield_step_range(
  start: Float,
  stop: Float,
  increment: Float,
) -> Yielder(Float) {
  // Check if the range would be empty due to direction and increment
  case
    { start >=. stop && increment >. 0.0 }
    || { start <=. stop && increment <. 0.0 }
  {
    True -> yielder.empty()
    False -> {
      let direction = case start <=. stop {
        True -> 1.0
        False -> -1.0
      }
      let increment_abs = float.absolute_value(increment)
      let distance = float.absolute_value(start -. stop)
      let num = float.round(distance /. increment_abs)

      yielder.map(yielder.range(0, num - 1), fn(index) {
        start +. int.to_float(index) *. increment_abs *. direction
      })
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function returns a list of linearly spaced points over a specified
/// interval. The endpoint of the interval can optionally be included/excluded. The number of
/// points and whether the endpoint is included determine the spacing between values.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///       let assert Ok(linspace) = maths.linear_space(10.0, 20.0, 5, True)
///       let pairs = linspace |> list.zip([10.0, 12.5, 15.0, 17.5, 20.0])
///       let assert Ok(result) = maths.all_close(pairs, 0.0, tolerance)
///       result
///       |> list.all(fn(x) { x == True })
///       |> should.be_true()
///
///       // A negative number of points (-5) does not work
///       maths.linear_space(10.0, 50.0, -5, True)
///       |> should.be_error()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn linear_space(
  start: Float,
  stop: Float,
  steps: Int,
  endpoint: Bool,
) -> Result(List(Float), Nil) {
  let direction = case start <=. stop {
    True -> 1.0
    False -> -1.0
  }

  let increment_abs = case endpoint {
    True -> float.absolute_value(start -. stop) /. int.to_float(steps - 1)
    False -> float.absolute_value(start -. stop) /. int.to_float(steps)
  }

  let adjusted_stop = case endpoint {
    True -> stop
    False -> stop -. increment_abs *. direction
  }

  // Generate the sequence from 'adjusted_stop' towards 'start'
  case steps > 0 {
    True -> {
      Ok(do_linear_space(adjusted_stop, increment_abs *. direction, steps, []))
    }
    False -> Error(Nil)
  }
}

fn do_linear_space(
  current: Float,
  increment: Float,
  remaining_steps: Int,
  acc: List(Float),
) -> List(Float) {
  case remaining_steps {
    0 -> acc
    _ ->
      do_linear_space(current -. increment, increment, remaining_steps - 1, [
        current,
        ..acc
      ])
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function is similar to [`linear_space`](#linear_space) but instead returns a yielder
/// (lazily evaluated sequence of elements). This function can be used whenever there is a need
/// to generate a larger-than-usual sequence of elements.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/yielder.{Next, Done}
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let assert Ok(linspace) = maths.yield_linear_space(10.0, 20.0, 5, True)
///
///       let assert Next(element, rest) = yielder.step(linspace)
///       should.equal(element, 10.0)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 12.5)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 15.0)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 17.5)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 20.0)
///
///       // We have generated 5 values, so the 6th will be 'Done'
///       should.equal(yielder.step(rest), Done)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn yield_linear_space(
  start: Float,
  stop: Float,
  steps: Int,
  endpoint: Bool,
) -> Result(Yielder(Float), Nil) {
  let direction = case start <=. stop {
    True -> 1.0
    False -> -1.0
  }

  let increment = case endpoint {
    True -> float.absolute_value(start -. stop) /. int.to_float(steps - 1)
    False -> float.absolute_value(start -. stop) /. int.to_float(steps)
  }

  case steps > 0 {
    False -> Error(Nil)
    True ->
      Ok({
        use index <- yielder.map(yielder.range(0, steps - 1))

        start +. int.to_float(index) *. increment *. direction
      })
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function returns a list of logarithmically spaced points over a specified
/// interval. The endpoint of the interval can optionally be included/excluded.
/// The number of points, base, and whether the endpoint is included determine
/// the spacing between values.
///
/// The values in the sequence are computed as powers of the given base, where
/// the exponents are evenly spaced between `start` and `stop`. The `base`
/// parameter must be positive, as negative bases lead to undefined behavior when
/// computing fractional exponents. Similarly, the number of points (`steps`) must
/// be positive; specifying zero or a negative value will result in an error.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///       let assert Ok(logspace) = maths.logarithmic_space(1.0, 3.0, 3, True, 10.0)
///       let pairs = logspace |> list.zip([10.0, 100.0, 1000.0])
///       let assert Ok(result) = maths.all_close(pairs, 0.0, tolerance)
///       result
///       |> list.all(fn(x) { x == True })
///       |> should.be_true()
///
///       // A negative number of points (-3) does not work
///       maths.logarithmic_space(1.0, 3.0, -3, False, 10.0)
///       |> should.be_error()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn logarithmic_space(
  start: Float,
  stop: Float,
  steps: Int,
  endpoint: Bool,
  base: Float,
) -> Result(List(Float), Nil) {
  case steps > 0 && base >. 0.0 {
    True -> {
      // Usage of let assert: No error will occur since 'steps' > 0, i.e., a
      // non-empty list will actually be returned.
      let assert Ok(linspace) = linear_space(start, stop, steps, endpoint)

      Ok({
        use value <- list.map(linspace)

        // Usage of let assert: The function 'float.power' will only return an
        // error if:
        //
        //   1. The base is negative and the exponent is fractional.
        //   2. If the base is 0 and the exponent is negative.
        //
        // No error will occur below since the base is non-zero and positive.
        let assert Ok(result) = float.power(base, value)

        result
      })
    }
    False -> Error(Nil)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function is similar to [`logarithmic_space`](#logarithmic_space) but instead returns a yielder
/// (lazily evaluated sequence of elements). This function can be used whenever there is a need
/// to generate a larger-than-usual sequence of elements.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/yielder.{Next, Done}
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let assert Ok(logspace) =
///         maths.yield_logarithmic_space(1.0, 3.0, 3, True, 10.0)
///
///       let assert Next(element, rest) = yielder.step(logspace)
///       should.equal(element, 10.0)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 100.0)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 1000.0)
///
///       // We have generated 3 values, so the 4th will be 'Done'
///       should.equal(yielder.step(rest), Done)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn yield_logarithmic_space(
  start: Float,
  stop: Float,
  steps: Int,
  endpoint: Bool,
  base: Float,
) -> Result(Yielder(Float), Nil) {
  case steps > 0 && base >. 0.0 {
    False -> Error(Nil)
    True -> {
      // Usage of let assert: No error will occur since 'steps' > 0, i.e., a
      // non-empty list will actually be returned.
      let assert Ok(linspace) = yield_linear_space(start, stop, steps, endpoint)

      Ok({
        use value <- yielder.map(linspace)

        // Usage of let assert: The function 'float.power' will only return an
        // error if:
        //
        //   1. The base is negative and the exponent is fractional.
        //    2. If the base is 0 and the exponent is negative.
        //
        // No error will occur below since the base is non-zero and positive.
        let assert Ok(result) = float.power(base, value)

        result
      })
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function returns a list of a geometric progression between two specified
/// values, where each value is a constant multiple of the previous one. Unlike
/// [`logarithmic_space`](#logarithmic_space), this function allows specifying the starting
/// and ending values (`start` and `stop`) directly, without requiring them to be transformed
/// into exponents.
///
/// Internally, the function computes the logarithms of `start` and `stop` and generates evenly
/// spaced points in the logarithmic domain (using base 10). These points are then transformed back
/// into their original scale to create a sequence of values that grow multiplicatively.
///
/// The `start` and `stop` values must be positive, as logarithms are undefined for non-positive
/// values. The number of points (`steps`) must also be positive; specifying zero or a negative
/// value will result in an error.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/yielder
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let assert Ok(tolerance) = float.power(10.0, -6.0)
///       let assert Ok(logspace) = maths.geometric_space(10.0, 1000.0, 3, True)
///       let pairs = logspace |> list.zip([10.0, 100.0, 1000.0])
///       let assert Ok(result) = maths.all_close(pairs, 0.0, tolerance)
///       result
///       |> list.all(fn(x) { x == True })
///       |> should.be_true()
///
///       // Input (start and stop can't be less than or equal to 0.0)
///       maths.geometric_space(0.0, 1000.0, 3, False)
///       |> should.be_error()
///
///       maths.geometric_space(-1000.0, 0.0, 3, False)
///       |> should.be_error()
///
///       // A negative number of points (-3) does not work
///       maths.geometric_space(10.0, 1000.0, -3, False)
///       |> should.be_error()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn geometric_space(
  start: Float,
  stop: Float,
  steps: Int,
  endpoint: Bool,
) -> Result(List(Float), Nil) {
  case start <=. 0.0 || stop <=. 0.0 || steps < 0 {
    True -> Error(Nil)
    False -> {
      // Usage of let assert: No error will occur since 'start' and 'stop' have
      // been checked and we can ensure they will be within the domain of the
      // `logarithm_10` function.
      let assert Ok(log_start) = logarithm_10(start)
      let assert Ok(log_stop) = logarithm_10(stop)

      logarithmic_space(log_start, log_stop, steps, endpoint, 10.0)
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function is similar to [`geometric_space`](#geometric_space) but instead returns a yielder
/// (lazily evaluated sequence of elements). This function can be used whenever there is a need
/// to generate a larger-than-usual sequence of elements.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/yielder.{Next, Done}
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example () {
///       let assert Ok(logspace) = maths.yield_geometric_space(10.0, 1000.0, 3, True)
///
///       let assert Next(element, rest) = yielder.step(logspace)
///       should.equal(element, 10.0)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 100.0)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 1000.0)
///
///       // We have generated 3 values, so the 4th will be 'Done'
///       should.equal(yielder.step(rest), Done)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn yield_geometric_space(
  start: Float,
  stop: Float,
  steps: Int,
  endpoint: Bool,
) -> Result(Yielder(Float), Nil) {
  case start <=. 0.0 || stop <=. 0.0 || steps < 0 {
    True -> Error(Nil)
    False -> {
      // Usage of let assert: No error will occur since 'start' and 'stop' have
      // been checked and we can ensure they will be within the domain of the
      // `logarithm_10` function.
      let assert Ok(log_start) = logarithm_10(start)
      let assert Ok(log_stop) = logarithm_10(stop)

      yield_logarithmic_space(log_start, log_stop, steps, endpoint, 10.0)
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generates evenly spaced points around a center value. The total span (around the center value)
/// is determined by the `radius` argument of the function.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       let assert Ok(symspace) = maths.symmetric_space(0.0, 5.0, 5)
///       symspace
///       |> should.equal([-5.0, -2.5, 0.0, 2.5, 5.0])
///
///       // A negative radius reverses the order of the values
///       let assert Ok(symspace) = maths.symmetric_space(0.0, -5.0, 5)
///       symspace
///       |> should.equal([5.0, 2.5, 0.0, -2.5, -5.0])
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn symmetric_space(
  center: Float,
  radius: Float,
  steps: Int,
) -> Result(List(Float), Nil) {
  case steps > 0 {
    False -> Error(Nil)
    True -> {
      let start = center -. radius
      let stop = center +. radius

      linear_space(start, stop, steps, True)
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/gleam-community/maths/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function is similar to [`symmetric_space`](#symmetric_space) but instead returns a yielder
/// (lazily evaluated sequence of elements). This function can be used whenever there is a need
/// to generate a larger-than-usual sequence of elements.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/yielder.{Next, Done}
///     import gleeunit/should
///     import gleam_community/maths
///
///     pub fn example() {
///       let assert Ok(symspace) = maths.yield_symmetric_space(0.0, 5.0, 5)
///
///       let assert Next(element, rest) = yielder.step(symspace)
///       should.equal(element, -5.0)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, -2.5)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 0.0)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 2.5)
///
///       let assert Next(element, rest) = yielder.step(rest)
///       should.equal(element, 5.0)
///
///       // We have generated 5 values, so the 6th will be 'Done'
///       should.equal(yielder.step(rest), Done)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn yield_symmetric_space(
  center: Float,
  radius: Float,
  steps: Int,
) -> Result(Yielder(Float), Nil) {
  case steps > 0 {
    False -> Error(Nil)
    True -> {
      let start = center -. radius
      let stop = center +. radius

      yield_linear_space(start, stop, steps, True)
    }
  }
}
