import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import * as $yielder from "../../gleam_yielder/gleam/yielder.mjs";
import { Done, Next } from "../../gleam_yielder/gleam/yielder.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  makeError,
  remainderInt,
  divideFloat,
  divideInt,
} from "../gleam.mjs";
import {
  acos as do_acos,
  acosh as do_acosh,
  asin as do_asin,
  asinh as do_asinh,
  atan as do_atan,
  atan2 as do_atan2,
  atanh as do_atanh,
  cos as do_cos,
  cosh as do_cosh,
  sin as do_sin,
  sinh as do_sinh,
  tan as do_tan,
  tanh as do_tanh,
  exponential as do_exponential,
  logarithm as do_natural_logarithm,
  logarithm_2 as do_logarithm_2,
  logarithm_10 as do_logarithm_10,
  pi as do_pi,
  truncate as do_truncate_float,
  floor as do_floor,
  ceiling as do_ceiling,
  sign as do_sign,
  sign as do_int_sign,
} from "../maths.mjs";

const FILEPATH = "src/gleam_community/maths.gleam";

function do_gcd(loop$x, loop$y) {
  while (true) {
    let x = loop$x;
    let y = loop$y;
    let $ = x === 0;
    if ($) {
      return y;
    } else {
      loop$x = remainderInt(y, x);
      loop$y = x;
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function calculates the greatest common divisor of two integers
 * \\(x, y \in \mathbb{Z}\\). The greatest common divisor is the largest positive
 * integer that is divisible by both \\(x\\) and \\(y\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.gcd(1, 1)
 *       |> should.equal(1)
 *
 *       maths.gcd(100, 10)
 *       |> should.equal(10)
 *
 *       maths.gcd(-36, -17)
 *       |> should.equal(1)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function gcd(x, y) {
  let absx = $int.absolute_value(x);
  let absy = $int.absolute_value(y);
  return do_gcd(absx, absy);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 *
 * Given two integers, \\(x\\) (dividend) and \\(y\\) (divisor), the Euclidean modulo
 * of \\(x\\) by \\(y\\), denoted as \\(x \mod y\\), is the remainder \\(r\\) of the
 * division of \\(x\\) by \\(y\\), such that:
 *
 * \\[
 * x = q \cdot y + r \quad \text{and} \quad 0 \leq r < |y|,
 * \\]
 *
 * where \\(q\\) is an integer that represents the quotient of the division.
 *
 * Note that like the Gleam division operator `/` this will return `0` if one of
 * the arguments is `0`.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.euclidean_modulo(15, 4)
 *       |> should.equal(3)
 *
 *       maths.euclidean_modulo(-3, -2)
 *       |> should.equal(1)
 *
 *       maths.euclidean_modulo(5, 0)
 *       |> should.equal(0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function euclidean_modulo(x, y) {
  let $ = remainderInt(x, y);
  if (x === 0) {
    return x;
  } else if (y === 0) {
    return y;
  } else {
    let md = $;
    if (md < 0) {
      return md + $int.absolute_value(y);
    } else {
      return $;
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function calculates the least common multiple of two integers
 * \\(x, y \in \mathbb{Z}\\). The least common multiple is the smallest positive
 * integer that has both \\(x\\) and \\(y\\) as factors.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.lcm(1, 1)
 *       |> should.equal(1)
 *
 *       maths.lcm(100, 10)
 *       |> should.equal(100)
 *
 *       maths.lcm(-36, -17)
 *       |> should.equal(612)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function lcm(x, y) {
  let absx = $int.absolute_value(x);
  let absy = $int.absolute_value(y);
  return divideInt(absx * absy, do_gcd(absx, absy));
}

function do_find_divisors(loop$n, loop$max, loop$acc, loop$i) {
  while (true) {
    let n = loop$n;
    let max = loop$max;
    let acc = loop$acc;
    let i = loop$i;
    let $ = i <= max;
    if ($) {
      let _block;
      let $1 = (remainderInt(n, i)) === 0;
      if ($1) {
        let _pipe = $set.insert(acc, i);
        _block = $set.insert(_pipe, divideInt(n, i));
      } else {
        _block = acc;
      }
      let updated_acc = _block;
      loop$n = n;
      loop$max = max;
      loop$acc = updated_acc;
      loop$i = i + 1;
    } else {
      return acc;
    }
  }
}

function find_divisors(n) {
  let nabs = $float.absolute_value($int.to_float(n));
  let $ = $float.square_root(nabs);
  let sqrt_result;
  if ($ instanceof Ok) {
    sqrt_result = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      219,
      "find_divisors",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 6246,
        end: 6298,
        pattern_start: 6257,
        pattern_end: 6272
      }
    )
  }
  let max = $float.round(sqrt_result) + 1;
  return do_find_divisors(n, max, $set.new$(), 1);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function returns all the positive divisors of an integer, including the
 * number itself.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.divisors(4)
 *       |> should.equal([1, 2, 4])
 *
 *       maths.divisors(6)
 *       |> should.equal([1, 2, 3, 6])
 *
 *       maths.divisors(13)
 *       |> should.equal([1, 13])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function divisors(n) {
  let _pipe = find_divisors(n);
  let _pipe$1 = $set.to_list(_pipe);
  return $list.sort(_pipe$1, $int.compare);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function returns all the positive divisors of an integer, excluding the
 * number itself.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.proper_divisors(4)
 *       |> should.equal([1, 2])
 *
 *       maths.proper_divisors(6)
 *       |> should.equal([1, 2, 3])
 *
 *       maths.proper_divisors(13)
 *       |> should.equal([1])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function proper_divisors(n) {
  let _pipe = find_divisors(n);
  let _pipe$1 = $set.delete$(_pipe, n);
  let _pipe$2 = $set.to_list(_pipe$1);
  return $list.sort(_pipe$2, $int.compare);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the weighted sum of the elements in a list:
 *
 * \\[
 * \sum_{i=1}^n w_i x_i
 * \\]
 *
 * In the formula, \\(n\\) is the length of the list and \\(x_i \in \mathbb{R}\\)
 * is the value in the input list indexed by \\(i\\), while the \\(w_i \in \mathbb{R}\\)
 * are corresponding positive weights.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/float
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.weighted_sum()
 *       |> should.equal(Ok(0.0))
 *
 *       [#(1.0, 1.0), #(2.0, 1.0), #(3.0, 1.0)]
 *       |> maths.weighted_sum()
 *       |> should.equal(Ok(6.0))
 *
 *       [#(9.0, 0.5), #(10.0, 0.5), #(10.0, 0.5)]
 *       |> maths.weighted_sum()
 *       |> should.equal(Ok(14.5))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function weighted_sum(arr) {
  if (arr instanceof $Empty) {
    return new Ok(0.0);
  } else {
    return $list.try_fold(
      arr,
      0.0,
      (acc, tuple) => {
        let $ = tuple[1] < 0.0;
        if ($) {
          return new Error(undefined);
        } else {
          return new Ok((tuple[0] * tuple[1]) + acc);
        }
      },
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the weighted product of the elements in a list:
 *
 * \\[
 * \prod_{i=1}^n x_i^{w_i}
 * \\]
 *
 * In the formula, \\(n\\) is the length of the list and \\(x_i \in \mathbb{R}\\) is
 * the value in the input list indexed by \\(i\\), while the \\(w_i \in \mathbb{R}\\)
 * are corresponding positive weights.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/float
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.weighted_product()
 *       |> should.equal(Ok(1.0))
 *
 *       [#(1.0, 1.0), #(2.0, 1.0), #(3.0, 1.0)]
 *       |> maths.weighted_product()
 *       |> should.equal(Ok(6.0))
 *
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *       let assert Ok(result) =
 *         [#(9.0, 0.5), #(10.0, 0.5), #(10.0, 0.5)]
 *         |> maths.weighted_product()
 *       result
 *       |> maths.is_close(30.0, 0.0, tolerance)
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function weighted_product(arr) {
  if (arr instanceof $Empty) {
    return new Ok(1.0);
  } else {
    return $list.try_fold(
      arr,
      1.0,
      (acc, tuple) => {
        let $ = tuple[1] < 0.0;
        if ($) {
          return new Error(undefined);
        } else {
          let $1 = $float.power(tuple[0], tuple[1]);
          if ($1 instanceof Ok) {
            let value = $1[0];
            return new Ok(value * acc);
          } else {
            return $1;
          }
        }
      },
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the cumulative sum of the elements in a list:
 *
 * \\[
 * v_j = \sum_{i=1}^j x_i \\;\\; \forall j = 1,\dots, n
 * \\]
 *
 * In the formula, \\(v_j\\) is the \\(j\\)'th element in the cumulative sum of \\(n\\)
 * elements. That is, \\(n\\) is the length of the list and \\(x_i \in \mathbb{R}\\)
 * is the value in the input list indexed by \\(i\\). The value \\(v_j\\) is thus the
 * sum of the \\(1\\) to \\(j\\) first elements in the given list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.cumulative_sum()
 *       |> should.equal([])
 *
 *       [1.0, 2.0, 3.0]
 *       |> maths.cumulative_sum()
 *       |> should.equal([1.0, 3.0, 6.0])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function cumulative_sum(arr) {
  if (arr instanceof $Empty) {
    return arr;
  } else {
    return $list.scan(arr, 0.0, (acc, element) => { return element + acc; });
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the cumulative sum of the elements in a list:
 *
 * \\[
 * v_j = \sum_{i=1}^j x_i \\;\\; \forall j = 1,\dots, n
 * \\]
 *
 * In the formula, \\(v_j\\) is the \\(j\\)'th element in the cumulative sum of \\(n\\)
 * elements. That is, \\(n\\) is the length of the list and \\(x_i \in \mathbb{Z}\\)
 * is the value in the input list indexed by \\(i\\). The value \\(v_j\\) is thus the
 * sum of the \\(1\\) to \\(j\\) first elements in the given list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.int_cumulative_sum()
 *       |> should.equal([])
 *
 *       [1, 2, 3]
 *       |> maths.int_cumulative_sum()
 *       |> should.equal([1, 3, 6])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function int_cumulative_sum(arr) {
  if (arr instanceof $Empty) {
    return arr;
  } else {
    return $list.scan(arr, 0, (acc, element) => { return element + acc; });
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the cumulative product of the elements in a list:
 *
 * \\[
 * v_j = \prod_{i=1}^j x_i \\;\\; \forall j = 1,\dots, n
 * \\]
 *
 * In the formula, \\(v_j\\) is the \\(j\\)'th element in the cumulative product
 * of \\(n\\) elements. That is, \\(n\\) is the length of the list and
 * \\(x_i \in \mathbb{R}\\) is the value in the input list indexed by \\(i\\).
 * The value \\(v_j\\) is thus the sum of the \\(1\\) to \\(j\\) first elements
 * in the given list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.cumulative_product()
 *       |> should.equal([])
 *
 *       [1.0, 2.0, 3.0]
 *       |> maths.cumulative_product()
 *       |> should.equal([1.0, 2.0, 6.0])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function cumulative_product(arr) {
  if (arr instanceof $Empty) {
    return arr;
  } else {
    return $list.scan(arr, 1.0, (acc, element) => { return element * acc; });
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the cumulative product of the elements in a list:
 *
 * \\[
 * v_j = \prod_{i=1}^j x_i \\;\\; \forall j = 1,\dots, n
 * \\]
 *
 * In the formula, \\(v_j\\) is the \\(j\\)'th element in the cumulative product
 * of \\(n\\) elements. That is, \\(n\\) is the length of the list and
 * \\(x_i \in \mathbb{Z}\\) is the value in the input list indexed by \\(i\\).
 * The value \\(v_j\\) is thus the product of the \\(1\\) to \\(j\\) first elements
 * in the given list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.int_cumulative_product()
 *       |> should.equal([])
 *
 *       [1, 2, 3]
 *       |> maths.int_cumulative_product()
 *       |> should.equal([1, 2, 6])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function int_cumulative_product(arr) {
  if (arr instanceof $Empty) {
    return arr;
  } else {
    return $list.scan(arr, 1, (acc, element) => { return element * acc; });
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The inverse cosine function:
 *
 * \\[
 * \forall x \in \[-1, 1\],   \\; \cos^{-1}{(x)} = y \in \[0, \pi \]
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\[-1, 1\]\\) as input and
 * returns a numeric value \\(y\\) that lies in the range \\(\[0, \pi \]\\) (an
 * angle in radians). If the input value is outside the domain of the function
 * an error is returned.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.acos(1.0)
 *       |> should.equal(Ok(0.0))
 *
 *       maths.acos(1.1)
 *       |> should.be_error()
 *
 *       maths.acos(-1.1)
 *       |> should.be_error()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function acos(x) {
  let $ = (x >= -1.0) && (x <= 1.0);
  if ($) {
    return new Ok(do_acos(x));
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The inverse hyperbolic cosine function:
 *
 * \\[
 * \forall x \in [1, +\infty\),   \\; \cosh^{-1}{(x)} = y \in \[0, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\[1, +\infty\)\\) as input
 * and returns a numeric value \\(y\\) that lies in the range \\(\[0, +\infty\)\\)
 * (an angle in radians). If the input value is outside the domain of the function
 * an error is returned.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.acosh(1.0)
 *       |> should.equal(Ok(0.0))
 *
 *       maths.acosh(0.0)
 *       |> should.be_error()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function acosh(x) {
  let $ = x >= 1.0;
  if ($) {
    return new Ok(do_acosh(x));
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The inverse sine function:
 *
 * \\[
 * \forall x \in \[-1, 1\],   \\; \sin^{-1}{(x)} = y \in \(-\infty, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\[-1, 1\]\\) as input and returns a numeric
 * value \\(y\\) that lies in the range \\(\[-\frac{\pi}{2}, \frac{\pi}{2}\]\\) (an angle in
 * radians). If the input value is outside the domain of the function an error is returned.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.asin(0.0)
 *       |> should.equal(Ok(0.0))
 *
 *       maths.asin(1.1)
 *       |> should.be_error()
 *
 *       maths.asin(-1.1)
 *       |> should.be_error()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function asin(x) {
  let $ = (x >= -1.0) && (x <= 1.0);
  if ($) {
    return new Ok(do_asin(x));
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The inverse hyperbolic sine function:
 *
 * \\[
 * \forall x \in \(-\infty, \infty\),   \\; \sinh^{-1}{(x)} = y \in \(-\infty, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(-\infty, +\infty\)\\)
 * as input and returns a numeric value \\(y\\) that lies in the range
 * \\(\(-\infty, +\infty\)\\) (an angle in radians).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.asinh(0.0)
 *       |> should.equal(0.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function asinh(x) {
  return do_asinh(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The inverse tangent function:
 *
 * \\[
 * \forall x \in \(-\infty, \infty\),  \\; \tan^{-1}{(x)} = y \in \[-\frac{\pi}{2}, \frac{\pi}{2}\]
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(-\infty, +\infty\)\\) as input and
 * returns a numeric value \\(y\\) that lies in the range \\(\[-\frac{\pi}{2}, \frac{\pi}{2}\]\\)
 * (an angle in radians).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.atan(0.0)
 *       |> should.equal(0.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function atan(x) {
  return do_atan(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The inverse 2-argument tangent function:
 *
 * \\[
 * \text{atan2}(y, x) =
 * \begin{cases}
 *  \tan^{-1}(\frac y x) &\text{if } x > 0, \\\\
 *  \tan^{-1}(\frac y x) + \pi &\text{if } x < 0 \text{ and } y \ge 0, \\\\
 *  \tan^{-1}(\frac y x) - \pi &\text{if } x < 0 \text{ and } y < 0, \\\\
 *  +\frac{\pi}{2} &\text{if } x = 0 \text{ and } y > 0, \\\\
 *  -\frac{\pi}{2} &\text{if } x = 0 \text{ and } y < 0, \\\\
 *  \text{undefined} &\text{if } x = 0 \text{ and } y = 0.
 * \end{cases}
 * \\]
 *
 * The function returns the angle in radians from the x-axis to the line containing
 * the origin \\(\(0, 0\)\\) and a point given as input with coordinates \\(\(x, y\)\\).
 * The numeric value returned by \\(\text{atan2}(y, x)\\) is in the range
 * \\(\[-\pi, \pi\]\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.atan2(0.0, 0.0)
 *       |> should.equal(0.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function atan2(y, x) {
  return do_atan2(y, x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Converts Cartesian coordinates \\((x, y)\\) to polar coordinates \\((r, \theta)\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.cartesian_to_polar(1.0, 0.0)
 *       |> should.equal((1.0, 0.0))
 *
 *       maths.cartesian_to_polar(0.0, 1.0)
 *       |> should.equal((1.0, float.pi() /. 2.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function cartesian_to_polar(x, y) {
  let $ = $float.square_root((x * x) + (y * y));
  let r;
  if ($ instanceof Ok) {
    r = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      729,
      "cartesian_to_polar",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 19885,
        end: 19939,
        pattern_start: 19896,
        pattern_end: 19901
      }
    )
  }
  let theta = atan2(y, x);
  return [r, theta];
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The inverse hyperbolic tangent function:
 *
 * \\[
 * \forall x \in \(-1, 1\),   \\; \tanh^{-1}{(x)} = y \in \(-\infty, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(-1, 1\)\\) as input and returns
 * a numeric value \\(y\\) that lies in the range \\(\(-\infty, \infty\)\\) (an angle in radians).
 * If the input value is outside the domain of the function an error is returned.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.atanh(0.0)
 *       |> should.equal(Ok(0.0))
 *
 *       maths.atanh(1.0)
 *       |> should.be_error()
 *
 *       maths.atanh(-1.0)
 *       |> should.be_error()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function atanh(x) {
  let $ = (x > -1.0) && (x < 1.0);
  if ($) {
    return new Ok(do_atanh(x));
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The cosine function:
 *
 * \\[
 * \forall x \in \(-\infty, +\infty\),   \\; \cos{(x)} = y \in \[-1, 1\]
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(-\infty, \infty\)\\) (an angle in
 * radians) as input and returns a numeric value \\(y\\) that lies in the range \\(\[-1, 1\]\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.cos(0.0)
 *       |> should.equal(1.0)
 *
 *       maths.cos(maths.pi())
 *       |> should.equal(-1.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function cos(x) {
  return do_cos(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The hyperbolic cosine function:
 *
 * \\[
 * \forall x \in \(-\infty, \infty\),   \\; \cosh{(x)} = y \in \(-\infty, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(-\infty, \infty\)\\) as input (an angle
 * in radians) and returns a numeric value \\(y\\) that lies in the range
 * \\(\(-\infty, \infty\)\\). If the input value is too large an overflow error might occur.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.cosh(0.0)
 *       |> should.equal(0.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function cosh(x) {
  return do_cosh(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The sine function:
 *
 * \\[
 * \forall x \in \(-\infty, +\infty\),   \\; \sin{(x)} = y \in \[-1, 1\]
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(-\infty, \infty\)\\) (an angle in
 * radians) as input and returns a numeric value \\(y\\) that lies in the range \\(\[-1, 1\]\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.sin(0.0)
 *       |> should.equal(0.0)
 *
 *       maths.sin(0.5 *. maths.pi())
 *       |> should.equal(1.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function sin(x) {
  return do_sin(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Converts polar coordinates \\((r, \theta)\\) to Cartesian coordinates \\((x, y)\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.polar_to_cartesian(1.0, 0.0)
 *       |> should.equal(#(1.0, 0.0))
 *
 *       maths.polar_to_cartesian(1.0, float.pi() /. 2.0)
 *       |> should.equal(#(0.0, 1.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function polar_to_cartesian(r, theta) {
  let x = r * cos(theta);
  let y = r * sin(theta);
  return [x, y];
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The hyperbolic sine function:
 *
 * \\[
 * \forall x \in \(-\infty, +\infty\),   \\; \sinh{(x)} = y \in \(-\infty, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(-\infty, +\infty\)\\) as input
 * (an angle in radians) and returns a numeric value \\(y\\) that lies in the range
 * \\(\(-\infty, +\infty\)\\). If the input value is too large an overflow error might occur.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.sinh(0.0)
 *       |> should.equal(0.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function sinh(x) {
  return do_sinh(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The tangent function:
 *
 * \\[
 * \forall x \in \(-\infty, +\infty\),   \\; \tan{(x)} = y \in \(-\infty, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(-\infty, +\infty\)\\) as input
 * (an angle in radians) and returns a numeric value \\(y\\) that lies in the range
 * \\(\(-\infty, +\infty\)\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.tan(0.0)
 *       |> should.equal(0.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function tan(x) {
  return do_tan(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The hyperbolic tangent function:
 *
 * \\[
 * \forall x \in \(-\infty, \infty\),   \\; \tanh{(x)} = y \in \[-1, 1\]
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(-\infty, \infty\)\\) as input (an angle
 * in radians) and returns a numeric value \\(y\\) that lies in the range \\(\[-1, 1\]\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       maths.tanh(0.0)
 *       |> should.equal(0.0)
 *
 *       maths.tanh(25.0)
 *       |> should.equal(1.0)
 *
 *       maths.tanh(-25.0)
 *       |> should.equal(-1.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function tanh(x) {
  return do_tanh(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The exponential function:
 *
 * \\[
 * \forall x \in \(-\infty, \infty\),   \\; e^{x} = y \in \(0, +\infty\)
 * \\]
 *
 * where \\(e \approx 2.71828\dots\\) is Eulers' number.
 *
 * Note: If the input value \\(x\\) is too large an overflow error might occur.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.exponential(0.0)
 *       |> should.equal(1.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function exponential(x) {
  return do_exponential(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The natural logarithm function:
 *
 * \\[
 * \forall x \in \(0, \infty\),   \\; \log_{e}{(x)} = y \in \(-\infty, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(0, \infty\)\\) as input and returns
 * a numeric value \\(y\\) that lies in the range \\(\(-\infty, \infty\)\\).
 * If the input value is outside the domain of the function an error is returned.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       maths.natural_logarithm(1.0)
 *       |> should.equal(Ok(0.0))
 *
 *       maths.natural_logarithm(maths.e())
 *       |> should.equal(Ok(1.0))
 *
 *       maths.natural_logarithm(-1.0)
 *       |> should.be_error()
 *     }
 * </details>
 *
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function natural_logarithm(x) {
  let $ = x > 0.0;
  if ($) {
    return new Ok(do_natural_logarithm(x));
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The base-2 logarithm function:
 *
 * \\[
 * \forall x \in \(0, \infty),   \\; \log_{2}{(x)} = y \in \(-\infty, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(0, \infty\)\\) as input and returns a
 * numeric value \\(y\\) that lies in the range \\(\(-\infty, \infty\)\\).
 * If the input value is outside the domain of the function an error is returned.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       maths.logarithm_2(1.0)
 *       |> should.equal(Ok(0.0))
 *
 *       maths.logarithm_2(2.0)
 *       |> should.equal(Ok(1.0))
 *
 *       maths.logarithm_2(-1.0)
 *       |> should.be_error()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function logarithm_2(x) {
  let $ = x > 0.0;
  if ($) {
    return new Ok(do_logarithm_2(x));
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The base-10 logarithm function:
 *
 * \\[
 * \forall x \in \(0, \infty),   \\; \log_{10}{(x)} = y \in \(-\infty, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(0, \infty\)\\) as input and returns a
 * numeric value \\(y\\) that lies in the range \\(\(-\infty, \infty\)\\).
 * If the input value is outside the domain of the function an error is returned.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       maths.logarithm_10(1.0)
 *       |> should.equal(Ok(0.0))
 *
 *       maths.logarithm_10(10.0)
 *       |> should.equal(Ok(1.0))
 *
 *       maths.logarithm_10(-1.0)
 *       |> should.be_error()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function logarithm_10(x) {
  let $ = x > 0.0;
  if ($) {
    return new Ok(do_logarithm_10(x));
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The base \\(b\\) logarithm function (computed through the "change of base" formula):
 *
 * \\[
 * \forall x \in \(0, \infty\) \textnormal{ and } b > 1,  \\; \log_{b}{(x)} = y \in \(-\infty, +\infty\)
 * \\]
 *
 * The function takes a number \\(x\\) in its domain \\(\(0, \infty\)\\) and a base \\(b > 1\\)
 * as input and returns a numeric value \\(y\\) that lies in the range \\(\(-\infty, \infty\)\\).
 * If the input value is outside the domain of the function an error is returned.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       maths.logarithm(1.0, 10.0)
 *       |> should.equal(Ok(0.0))
 *
 *       maths.logarithm(maths.e(), maths.e())
 *       |> should.equal(Ok(1.0))
 *
 *       maths.logarithm(-1.0, 2.0)
 *       |> should.be_error()
 *     }
 * </details>
 *
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function logarithm(x, base) {
  let $ = ((x > 0.0) && (base > 0.0)) && (base !== 1.0);
  if ($) {
    let $1 = logarithm_10(x);
    let numerator;
    if ($1 instanceof Ok) {
      numerator = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        1475,
        "logarithm",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 39708,
          end: 39750,
          pattern_start: 39719,
          pattern_end: 39732
        }
      )
    }
    let $2 = logarithm_10(base);
    let denominator;
    if ($2 instanceof Ok) {
      denominator = $2[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        1476,
        "logarithm",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $2,
          start: 39757,
          end: 39804,
          pattern_start: 39768,
          pattern_end: 39783
        }
      )
    }
    return new Ok(divideFloat(numerator, denominator));
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The \\(n\\)'th root function: \\(y = \sqrt[n]{x} = x^{\frac{1}{n}}\\).
 *
 * Note that the function is not defined if the input is negative (\\(x < 0\\)). An error will be
 * returned as an imaginary number will otherwise have to be returned.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.nth_root(-1.0, 2)
 *       |> should.be_error()
 *
 *       maths.nth_root(1.0, 2)
 *       |> should.equal(Ok(1.0))
 *
 *       maths.nth_root(27.0, 3)
 *       |> should.equal(Ok(3.0))
 *
 *       maths.nth_root(256.0, 4)
 *       |> should.equal(Ok(4.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function nth_root(x, n) {
  let $ = (x >= 0.0) && (n >= 1);
  if ($) {
    return $float.power(x, divideFloat(1.0, $int.to_float(n)));
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Convert a value in degrees to a value measured in radians.
 * That is, \\(1 \text{ degrees } = \frac{\pi}{180} \text{ radians }\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.degrees_to_radians(360.)
 *       |> should.equal(2. *. maths.pi())
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function degrees_to_radians(x) {
  return ((x * do_pi())) / 180.0;
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Convert a value in degrees to a value measured in radians.
 * That is, \\(1 \text{ radians } = \frac{180}{\pi} \text{ degrees }\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.radians_to_degrees(0.0)
 *       |> should.equal(0.0)
 *
 *       maths.radians_to_degrees(2. *. maths.pi())
 *       |> should.equal(360.)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function radians_to_degrees(x) {
  return divideFloat((x * 180.0), do_pi());
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The mathematical constant pi: \\(\pi \approx 3.1415\dots\\)
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function pi() {
  return do_pi();
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The mathematical (circle) constant tau: \\(\tau = 2 \cdot \pi \approx 6.283\dots\\)
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function tau() {
  return 2.0 * pi();
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The golden ratio: \\(\phi = \frac{1 + \sqrt{5}}{2}\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.golden_ratio()
 *       |> should.equal(1.618033988749895)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function golden_ratio() {
  let $ = $float.square_root(5.0);
  let sqrt5;
  if ($ instanceof Ok) {
    sqrt5 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      1705,
      "golden_ratio",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 45847,
        end: 45892,
        pattern_start: 45858,
        pattern_end: 45867
      }
    )
  }
  return (1.0 + sqrt5) / 2.0;
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Euler's number \\(e \approx 2.71828\dots\\).
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *
 *       // Test that the constant is approximately equal to 2.7128...
 *       maths.e()
 *       |> maths.is_close(2.7182818284590452353602, 0.0, tolerance)
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function e() {
  return exponential(1.0);
}

function truncate_float(x) {
  return do_truncate_float(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function rounds a float to a specific number of digits (after the decimal place or before
 * if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
 * specified digit) that is less than or equal to the absolute value of the input \\(x\\). This
 * rounding behaviour is similar to behaviour of the Gleam stdlib `truncate` function.
 *
 * <details>
 * <summary>Details</summary>
 *
 *   The rounding mode rounds \\(12.0654\\) to:
 *   - \\(12.0\\) for 0 digits after the decimal point (`digits = 0`)
 *   - \\(12.0\\) for 1 digit after the decimal point (`digits = 1`)
 *   - \\(12.06\\) for 2 digits after the decimal point (`digits = 2`)
 *   - \\(12.065\\) for 3 digits after the decimal point (`digits = 3`)
 *
 *   It is also possible to specify a negative number of digits. In that case, the negative
 *   number refers to the digits before the decimal point.
 *   - \\(10.0\\) for 1 digit before the decimal point (`digits = -1`)
 *   - \\(0.0\\) for 2 digits before the decimal point (`digits = -2`)
 *   - \\(0.0\\) for 3 digits before the decimal point (`digits = -3`)
 *
 * </details>
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.round_to_zero(12.0654, 2)
 *       |> should.equal(12.06)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function round_to_zero(x, p) {
  let $ = $float.power(10.0, $int.to_float(p));
  let p$1;
  if ($ instanceof Ok) {
    p$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      1984,
      "round_to_zero",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 56003,
        end: 56056,
        pattern_start: 56014,
        pattern_end: 56019
      }
    )
  }
  return divideFloat(truncate_float(x * p$1), p$1);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function rounds a float to a specific number of digits (after the decimal place or before
 * if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
 * specified digit) that is less than or equal to the input \\(x\\). This rounding behaviour is
 * similar to behaviour of the Gleam stdlib `floor` function.
 *
 * <details>
 * <summary>Details</summary>
 *
 *   The rounding mode rounds \\(12.0654\\) to:
 *   - \\(12.0\\) for 0 digits after the decimal point (`digits = 0`)
 *   - \\(12.0\\) for 1 digits after the decimal point (`digits = 1`)
 *   - \\(12.06\\) for 2 digits after the decimal point (`digits = 2`)
 *   - \\(12.065\\) for 3 digits after the decimal point (`digits = 3`)
 *
 *   It is also possible to specify a negative number of digits. In that case, the negative
 *   number refers to the digits before the decimal point.
 *   - \\(10.0\\) for 1 digit before the decimal point (`digits = -1`)
 *   - \\(0.0\\) for 2 digits before the decimal point (`digits = -2`)
 *   - \\(0.0\\) for 3 digits before the decimal point (`digits = -3`)
 *
 * </details>
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.round_down(12.0654, 2)
 *       |> should.equal(12.06)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function round_down(x, p) {
  let $ = $float.power(10.0, $int.to_float(p));
  let p$1;
  if ($ instanceof Ok) {
    p$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      2047,
      "round_down",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 58313,
        end: 58366,
        pattern_start: 58324,
        pattern_end: 58329
      }
    )
  }
  return divideFloat(do_floor(x * p$1), p$1);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function rounds a float to a specific number of digits (after the decimal place or before
 * if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
 * specified digit) that is larger than or equal to the input \\(x\\). This rounding behaviour is
 * similar to behaviour of the Gleam stdlib `ceiling` function.
 *
 * <details>
 * <summary>Details</summary>
 *
 *   The rounding mode rounds \\(12.0654\\) to:
 *   - \\(13.0\\) for 0 digits after the decimal point (`digits = 0`)
 *   - \\(12.1\\) for 1 digit after the decimal point (`digits = 1`)
 *   - \\(12.07\\) for 2 digits after the decimal point (`digits = 2`)
 *   - \\(12.066\\) for 3 digits after the decimal point (`digits = 3`)
 *
 *   It is also possible to specify a negative number of digits. In that case, the negative
 *   number refers to the digits before the decimal point.
 *   - \\(20.0\\) for 1 digit places before the decimal point (`digit = -1`)
 *   - \\(100.0\\) for 2 digits before the decimal point (`digits = -2`)
 *   - \\(1000.0\\) for 3 digits before the decimal point (`digits = -3`)
 *
 * </details>
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.round_up(12.0654, 2)
 *       |> should.equal(12.07)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function round_up(x, p) {
  let $ = $float.power(10.0, $int.to_float(p));
  let p$1;
  if ($ instanceof Ok) {
    p$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      2106,
      "round_up",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 60548,
        end: 60601,
        pattern_start: 60559,
        pattern_end: 60564
      }
    )
  }
  return divideFloat(do_ceiling(x * p$1), p$1);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The absolute difference:
 *
 * \\[
 *  \forall x, y \in \mathbb{R}, \\; |x - y|  \in \mathbb{R}_{+}.
 * \\]
 *
 * The function takes two inputs \\(x\\) and \\(y\\) and returns a positive float
 * value which is the absolute difference of the inputs.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.absolute_difference(-10.0, 10.0)
 *       |> should.equal(20.0)
 *
 *       maths.absolute_difference(0.0, -2.0)
 *       |> should.equal(2.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function absolute_difference(a, b) {
  return $float.absolute_value(a - b);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The absolute difference:
 *
 * \\[
 *  \forall x, y \in \mathbb{Z}, \\; |x - y|  \in \mathbb{Z}_{+}.
 * \\]
 *
 * The function takes two inputs \\(x\\) and \\(y\\) and returns a positive integer
 * value which is the absolute difference of the inputs.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.absolute_difference(-10, 10)
 *       |> should.equal(20)
 *
 *       maths.absolute_difference(0, -2)
 *       |> should.equal(2)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function int_absolute_difference(a, b) {
  return $int.absolute_value(a - b);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function takes an input \\(x \in \mathbb{R}\\) and returns the sign of
 * the input, indicating whether it is positive (+1.0), negative (-1.0), or
 * zero (0.0).
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function sign(x) {
  return do_sign(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function rounds a float to a specific number of digits (after the decimal place or before
 * if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
 * specified digit) with ties (fractional values of 0.5) being rounded to the nearest even
 * integer.
 *
 * <details>
 * <summary>Details</summary>
 *
 *   The rounding mode rounds \\(12.0654\\) to:
 *   - \\(12.0\\) for 0 digits after the decimal point (`digits = 0`)
 *   - \\(12.1\\) for 1 digit after the decimal point (`digits = 1`)
 *   - \\(12.07\\) for 2 digits after the decimal point (`digits = 2`)
 *   - \\(12.065\\) for 3 digits after the decimal point (`digits = 3`)
 *
 *   It is also possible to specify a negative number of digits. In that case, the negative
 *   number refers to the digits before the decimal point.
 *   - \\(10.0\\) for 1 digit before the decimal point (`digits = -1`)
 *   - \\(0.0\\) for 2 digits before the decimal point (`digits = -2`)
 *   - \\(0.0\\) for 3 digits before the decimal point (`digits = -3`)
 *
 * </details>
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.round_to_nearest(12.0654, 2)
 *       |> should.equal(12.07)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function round_to_nearest(x, p) {
  let $ = $float.power(10.0, $int.to_float(p));
  let p$1;
  if ($ instanceof Ok) {
    p$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      1794,
      "round_to_nearest",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 48759,
        end: 48812,
        pattern_start: 48770,
        pattern_end: 48775
      }
    )
  }
  let xabs = $float.absolute_value(x) * p$1;
  let xabs_truncated = truncate_float(xabs);
  let remainder = xabs - xabs_truncated;
  if (remainder > 0.5) {
    return divideFloat((sign(x) * truncate_float(xabs + 1.0)), p$1);
  } else if (remainder === 0.5) {
    let is_even = $float.truncate(xabs) % 2;
    let $1 = is_even === 0;
    if ($1) {
      return divideFloat((sign(x) * xabs_truncated), p$1);
    } else {
      return divideFloat((sign(x) * truncate_float(xabs + 1.0)), p$1);
    }
  } else {
    return divideFloat((sign(x) * xabs_truncated), p$1);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function rounds a float to a specific number of digits (after the decimal place or before
 * if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
 * specified digit) with ties (fractional values of 0.5) being rounded away from zero (C/C++
 * rounding behaviour).
 *
 * <details>
 * <summary>Details</summary>
 *
 *   The rounding mode rounds \\(12.0654\\) to:
 *   - \\(12.0\\) for 0 digits after the decimal point (`digits = 0`)
 *   - \\(12.1\\) for 1 digit after the decimal point (`digits = 1`)
 *   - \\(12.07\\) for 2 digits after the decimal point (`digits = 2`)
 *   - \\(12.065\\) for 3 digits after the decimal point (`digits = 3`)
 *
 *   It is also possible to specify a negative number of digits. In that case, the negative
 *   number refers to the digits before the decimal point.
 *   - \\(10.0\\) for 1 digit before the decimal point (`digits = -1`)
 *   - \\(0.0\\) for 2 digits before the decimal point (`digits = -2`)
 *   - \\(0.0\\) for 3 digits before the decimal point (`digits = -3`)
 *
 * </details>
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.round_ties_away(12.0654, 2)
 *       |> should.equal(12.07)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function round_ties_away(x, p) {
  let $ = $float.power(10.0, $int.to_float(p));
  let p$1;
  if ($ instanceof Ok) {
    p$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      1862,
      "round_ties_away",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 51301,
        end: 51354,
        pattern_start: 51312,
        pattern_end: 51317
      }
    )
  }
  let xabs = $float.absolute_value(x) * p$1;
  let remainder = xabs - truncate_float(xabs);
  if (remainder >= 0.5) {
    return divideFloat((sign(x) * truncate_float(xabs + 1.0)), p$1);
  } else {
    return divideFloat((sign(x) * truncate_float(xabs)), p$1);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function rounds a float to a specific number of digits (after the decimal place or before
 * if negative). In particular, the input \\(x\\) is rounded to the nearest integer value (at the
 * specified digit) with ties (fractional values of 0.5) being rounded towards \\(+\infty\\)
 * (Java/JavaScript rounding behaviour).
 *
 * <details>
 * <summary>Details</summary>
 *
 *   The rounding mode rounds \\(12.0654\\) to:
 *   - \\(12.0\\) for 0 digits after the decimal point (`digits = 0`)
 *   - \\(12.1\\) for 1 digits after the decimal point (`digits = 1`)
 *   - \\(12.07\\) for 2 digits after the decimal point (`digits = 2`)
 *   - \\(12.065\\) for 3 digits after the decimal point (`digits = 3`)
 *
 *   It is also possible to specify a negative number of digits. In that case, the negative
 *    number refers to the digits before the decimal point.
 *   - \\(10.0\\) for 1 digit before the decimal point (`digits = -1`)
 *   - \\(0.0\\) for 2 digits before the decimal point (`digits = -2`)
 *   - \\(0.0\\) for 3 digits before the decimal point (`digits = -3`)
 *
 * </details>
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.round_ties_up(12.0654, 2)
 *       |> should.equal(12.07)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function round_ties_up(x, p) {
  let $ = $float.power(10.0, $int.to_float(p));
  let p$1;
  if ($ instanceof Ok) {
    p$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      1922,
      "round_ties_up",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 53603,
        end: 53656,
        pattern_start: 53614,
        pattern_end: 53619
      }
    )
  }
  let xabs = $float.absolute_value(x) * p$1;
  let xabs_truncated = truncate_float(xabs);
  let remainder = xabs - xabs_truncated;
  if ((remainder >= 0.5) && (x >= 0.0)) {
    return divideFloat((sign(x) * truncate_float(xabs + 1.0)), p$1);
  } else {
    return divideFloat((sign(x) * xabs_truncated), p$1);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function takes an input \\(x \in \mathbb{Z}\\) and returns the sign of
 * the input, indicating whether it is positive (+1), negative (-1), or zero
 * (0).
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function int_sign(x) {
  return do_int_sign(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function flips the sign of a given input value \\(x \in \mathbb{R}\\).
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function flip_sign(x) {
  return -1.0 * x;
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function takes two arguments \\(x, y \in \mathbb{R}\\) and returns \\(x\\)
 * such that it has the same sign as \\(y\\).
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function copy_sign(x, y) {
  let $ = sign(x) === sign(y);
  if ($) {
    return x;
  } else {
    return flip_sign(x);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function flips the sign of a given input value \\(x \in \mathbb{Z}\\).
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function int_flip_sign(x) {
  return -1 * x;
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function takes two arguments \\(x, y \in \mathbb{Z}\\) and returns \\(x\\)
 * such that it has the same sign as \\(y\\).
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function int_copy_sign(x, y) {
  let $ = int_sign(x) === int_sign(y);
  if ($) {
    return x;
  } else {
    return int_flip_sign(x);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The minmax function takes two arguments \\(x, y\\) along with a function
 * for comparing \\(x, y\\). The function returns a tuple with the smallest
 * value first and largest second.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleam/float
 *     import gleam/int
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.minmax(2.0, 1.5, float.compare)
 *       |> should.equal(#(1.5, 2.0))
 *
 *       maths.minmax(1, 2, int.compare)
 *       |> should.equal(#(1, 2))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function minmax(x, y, compare) {
  let $ = compare(x, y);
  if ($ instanceof $order.Lt) {
    return [x, y];
  } else if ($ instanceof $order.Eq) {
    return [x, y];
  } else {
    return [y, x];
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Returns the minimum value of a given list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/int
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.list_minimum(int.compare)
 *       |> should.be_error()
 *
 *       [4, 4, 3, 2, 1]
 *       |> maths.list_minimum(int.compare)
 *       |> should.equal(Ok(1))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function list_minimum(arr, compare) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let x = arr.head;
    let rest = arr.tail;
    return new Ok(
      $list.fold(
        rest,
        x,
        (acc, element) => {
          let $ = compare(element, acc);
          if ($ instanceof $order.Lt) {
            return element;
          } else {
            return acc;
          }
        },
      ),
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Returns the maximum value of a given list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/float
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.list_maximum(float.compare)
 *       |> should.be_error()
 *
 *       [4.0, 4.0, 3.0, 2.0, 1.0]
 *       |> maths.list_maximum(float.compare)
 *       |> should.equal(Ok(4.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function list_maximum(arr, compare) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let x = arr.head;
    let rest = arr.tail;
    return new Ok(
      $list.fold(
        rest,
        x,
        (acc, element) => {
          let $ = compare(acc, element);
          if ($ instanceof $order.Lt) {
            return element;
          } else {
            return acc;
          }
        },
      ),
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 *
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Returns the indices of the minimum values in a given list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/float
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.arg_minimum(float.compare)
 *       |> should.be_error()
 *
 *       [4.0, 4.0, 3.0, 2.0, 1.0]
 *       |> maths.arg_minimum(float.compare)
 *       |> should.equal(Ok([4]))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function arg_minimum(arr, compare) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let $ = list_minimum(arr, compare);
    let min;
    if ($ instanceof Ok) {
      min = $[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        2526,
        "arg_minimum",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $,
          start: 71111,
          end: 71158,
          pattern_start: 71122,
          pattern_end: 71129
        }
      )
    }
    return new Ok(
      (() => {
        let _pipe = $list.index_map(
          arr,
          (element, index) => {
            let $1 = compare(element, min);
            if ($1 instanceof $order.Eq) {
              return index;
            } else {
              return -1;
            }
          },
        );
        return $list.filter(
          _pipe,
          (index) => {
            if (index === -1) {
              return false;
            } else {
              return true;
            }
          },
        );
      })(),
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 *
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Returns the indices of the maximum values in a given list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/float
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.arg_maximum(float.compare)
 *       |> should.be_error()
 *
 *       [4.0, 4.0, 3.0, 2.0, 1.0]
 *       |> maths.arg_maximum(float.compare)
 *       |> should.equal(Ok([0, 1]))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function arg_maximum(arr, compare) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let $ = list_maximum(arr, compare);
    let max;
    if ($ instanceof Ok) {
      max = $[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        2591,
        "arg_maximum",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $,
          start: 72657,
          end: 72704,
          pattern_start: 72668,
          pattern_end: 72675
        }
      )
    }
    return new Ok(
      (() => {
        let _pipe = $list.index_map(
          arr,
          (element, index) => {
            let $1 = compare(element, max);
            if ($1 instanceof $order.Eq) {
              return index;
            } else {
              return -1;
            }
          },
        );
        return $list.filter(
          _pipe,
          (index) => {
            if (index === -1) {
              return false;
            } else {
              return true;
            }
          },
        );
      })(),
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 *
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Returns a tuple consisting of the minimum and maximum values of a given list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/float
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.extrema(float.compare)
 *       |> should.be_error()
 *
 *       [4.0, 4.0, 3.0, 2.0, 1.0]
 *       |> maths.extrema(float.compare)
 *       |> should.equal(Ok(#(1.0, 4.0)))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function extrema(arr, compare) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let x = arr.head;
    let rest = arr.tail;
    return new Ok(
      $list.fold(
        rest,
        [x, x],
        (acc, element) => {
          let first = acc[0];
          let second = acc[1];
          let $ = compare(element, first);
          let $1 = compare(second, element);
          if ($1 instanceof $order.Lt) {
            if ($ instanceof $order.Lt) {
              return [element, element];
            } else {
              return [first, element];
            }
          } else if ($ instanceof $order.Lt) {
            return [element, second];
          } else {
            return [first, second];
          }
        },
      ),
    );
  }
}

function do_combination(loop$n, loop$k, loop$acc, loop$element) {
  while (true) {
    let n = loop$n;
    let k = loop$k;
    let acc = loop$acc;
    let element = loop$element;
    let $ = element > k;
    if ($) {
      return acc;
    } else {
      loop$n = n;
      loop$k = k;
      loop$acc = divideInt(acc * ((n + 1) - element), element);
      loop$element = element + 1;
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A combinatorial function for computing the number of \\(k\\)-combinations of \\(n\\) elements
 * without repetitions:
 *
 * \\[
 * C(n, k) = \binom{n}{k} = \frac{n!}{k! (n-k)!}
 * \\]
 *
 * Also known as "\\(n\\) choose \\(k\\)" or the binomial coefficient.
 *
 *
 * <details>
 * <summary>Details</summary>
 *
 * A \\(k\\)-combination without repetition is a sequence of \\(k\\) elements selected from
 * \\(n\\) elements where the order of selection does not matter and elements are not allowed to
 * repeat. For example, consider selecting  2 elements from a list of 3 elements:
 * `["A", "B", "C"]`. In this case, possible selections are:
 *   - `["A", "B"]`
 *   - `["A", "C"]`
 *   - `["B", "C"]`
 *
 * </details>
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.combination(-1, 1)
 *       |> should.be_error()
 *
 *       maths.combination(4, 0)
 *       |> should.equal(Ok(1))
 *
 *       maths.combination(4, 4)
 *       |> should.equal(Ok(1))
 *
 *       maths.combination(13, 5)
 *       |> should.equal(Ok(1287))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function combination(n, k) {
  if (n < 0) {
    return new Error(undefined);
  } else if (k < 0) {
    return new Error(undefined);
  } else if (k > n) {
    return new Ok(0);
  } else if ((k === 0) || (k === n)) {
    return new Ok(1);
  } else {
    let _block;
    let $ = k < (n - k);
    if ($) {
      _block = k;
    } else {
      _block = n - k;
    }
    let min = _block;
    return new Ok(do_combination(n, min, 1, 1));
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A combinatorial function for computing the number of \\(k\\)-combinations of \\(n\\) elements
 * with repetitions:
 *
 * \\[
 * C^*(n, k) = \binom{n + k - 1}{k} = \frac{(n + k - 1)!}{k! (n - 1)!}
 * \\]
 *
 * Also known as the "stars and bars" problem in maths. Furthermore, the implementation uses an
 * efficient iterative multiplicative formula for computing the result.
 *
 * <details>
 * <summary>Details</summary>
 *
 * A \\(k\\)-combination with repetitions is a sequence of \\(k\\) elements selected from
 * \\(n\\) elements where the order of selection does not matter and elements are allowed to
 * repeat. For example, consider selecting 2 elements from a list of 3 elements: `["A", "B", "C"]`.
 * In this case, possible selections are:
 *   - `["A", "A"], ["A", "B"], ["A", "C"]`
 *   - `["B", "B"], ["B", "C"], ["C", "C"]`
 *
 * </details>
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.combination_with_repetitions(-1, 1)
 *       |> should.be_error()
 *
 *       maths.combination_with_repetitions(2, 3)
 *       |> should.equal(Ok(4))
 *
 *       maths.combination_with_repetitions(13, 5)
 *       |> should.equal(Ok(6188))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function combination_with_repetitions(n, k) {
  return combination((n + k) - 1, k);
}

function do_factorial(loop$n, loop$acc) {
  while (true) {
    let n = loop$n;
    let acc = loop$acc;
    if (n === 0) {
      return acc;
    } else if (n === 1) {
      return acc;
    } else {
      loop$n = n - 1;
      loop$acc = acc * n;
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A combinatorial function for computing the total number of combinations of \\(n\\)
 * elements, that is \\(n!\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.factorial(-1)
 *       |> should.be_error()
 *
 *       maths.factorial(0)
 *       |> should.equal(Ok(1))
 *
 *       maths.factorial(3)
 *       |> should.equal(Ok(6))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function factorial(n) {
  if (n < 0) {
    return new Error(undefined);
  } else {
    return new Ok(do_factorial(n, 1));
  }
}

function do_permutation(loop$n, loop$k, loop$acc) {
  while (true) {
    let n = loop$n;
    let k = loop$k;
    let acc = loop$acc;
    if (k === 0) {
      return acc;
    } else {
      loop$n = n - 1;
      loop$k = k - 1;
      loop$acc = acc * n;
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A combinatorial function for computing the number of \\(k\\)-permutations without
 * repetitions:
 *
 * \\[
 * P(n, k) = \binom{n}{k} \cdot k! = \frac{n!}{(n - k)!}
 * \\]
 *
 * The implementation uses an efficient iterative multiplicative formula for computing the result.
 *
 * <details>
 * <summary>Details</summary>
 *
 * A \\(k\\)-permutation without repetitions is a sequence of \\(k\\) elements selected from \
 * \\(n\\) elements where the order of selection matters and elements are not allowed to repeat.
 * For example, consider selecting 2 elements from a list of 3 elements: `["A", "B", "C"]`. In
 * this case, possible selections are:
 *   - `["A", "B"], ["B", "A"]`
 *   - `["A", "C"], ["C", "A"]`
 *   - `["B", "C"], ["C", "B"]`
 *
 * </details>
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.permutation(-1, 1)
 *       |> should.be_error()
 *
 *       maths.permutation(4, 0)
 *       |> should.equal(Ok(1))
 *
 *       maths.permutation(4, 2)
 *       |> should.equal(Ok(12))
 *
 *       maths.permutation(13, 5)
 *       |> should.equal(Ok(154_440))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function permutation(n, k) {
  if (n < 0) {
    return new Error(undefined);
  } else if (k < 0) {
    return new Error(undefined);
  } else if (k > n) {
    return new Ok(0);
  } else if (k === 0) {
    return new Ok(1);
  } else {
    return new Ok(do_permutation(n, k, 1));
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A combinatorial function for computing the number of \\(k\\)-permutations with repetitions:
 *
 * \\[
 * P^*(n, k) = n^k
 * \\]
 *
 * <details>
 * <summary>Details</summary>
 *
 * A \\(k\\)-permutation with repetitions is a sequence of \\(k\\) elements selected from \\(n\\)
 * elements where the order of selection matters and elements are allowed to repeat. For example,
 * consider selecting 2 elements from a list of 3 elements: `["A", "B", "C"]`. In this case,
 * possible selections are:
 *   - `["A", "A"], ["A", "B"], ["A", "C"]`
 *   - `["B", "A"], ["B", "B"], ["B", "C"]`
 *   - `["C", "A"], ["C", "B"], ["C", "C"]`
 *
 * </details>
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.permutation_with_repetitions(1, -1)
 *       |> should.be_error()
 *
 *       maths.permutation_with_repetitions(2, 3)
 *       |> should.equal(Ok(8))
 *
 *       maths.permutation_with_repetitions(4, 4)
 *       |> should.equal(Ok(256))
 *
 *       maths.permutation_with_repetitions(6, 3)
 *       |> should.equal(Ok(216))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function permutation_with_repetitions(n, k) {
  if (n < 0) {
    return new Error(undefined);
  } else if (k < 0) {
    return new Error(undefined);
  } else {
    let n_float = $int.to_float(n);
    let k_float = $int.to_float(k);
    let $ = $float.power(n_float, k_float);
    let result;
    if ($ instanceof Ok) {
      result = $[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        2987,
        "permutation_with_repetitions",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $,
          start: 83409,
          end: 83462,
          pattern_start: 83420,
          pattern_end: 83430
        }
      )
    }
    return new Ok($float.round(result));
  }
}

function do_list_combination_without_repetitions(arr, k, prefix) {
  if (k === 0) {
    return $yielder.single($list.reverse(prefix));
  } else {
    let $ = $yielder.step(arr);
    if ($ instanceof $yielder.Next) {
      let x = $.element;
      let xs = $.accumulator;
      let with_x = do_list_combination_without_repetitions(
        xs,
        k - 1,
        listPrepend(x, prefix),
      );
      let without_x = do_list_combination_without_repetitions(xs, k, prefix);
      return $yielder.concat(toList([with_x, without_x]));
    } else {
      return $yielder.empty();
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Generates all possible combinations of \\(k\\) elements selected from a given list of size
 * \\(n\\). The function handles the case without repetitions, that is, repeated elements
 * are not treated as distinct.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/yielder
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // All 2-combinations of [1, 2, 3] without repetition
 *       let assert Ok(combinations) = maths.list_combination([1, 2, 3], 2)
 *
 *       combinations
 *       |> yielder.to_list()
 *       |> should.equal([[1, 2], [1, 3], [2, 3]])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function list_combination(arr, k) {
  let $ = $list.length(arr);
  if (k < 0) {
    return new Error(undefined);
  } else {
    let arr_length = $;
    if (k > arr_length) {
      return new Error(undefined);
    } else {
      let arr_length$1 = $;
      if (k === arr_length$1) {
        return new Ok($yielder.single(arr));
      } else {
        return new Ok(
          do_list_combination_without_repetitions(
            $yielder.from_list(arr),
            k,
            toList([]),
          ),
        );
      }
    }
  }
}

function do_list_combination_with_repetitions(arr, k, prefix) {
  if (k === 0) {
    return $yielder.single($list.reverse(prefix));
  } else {
    let $ = $yielder.step(arr);
    if ($ instanceof $yielder.Next) {
      let x = $.element;
      let xs = $.accumulator;
      let with_x = do_list_combination_with_repetitions(
        arr,
        k - 1,
        listPrepend(x, prefix),
      );
      let without_x = do_list_combination_with_repetitions(xs, k, prefix);
      return $yielder.concat(toList([with_x, without_x]));
    } else {
      return $yielder.empty();
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Generates all possible combinations of \\(k\\) elements selected from a given list of size
 * \\(n\\). The function handles the case when the repetition of elements is allowed, that is,
 * repeated elements are treated as distinct.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/yielder
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // All 2-combinations of [1, 2, 3] with repetition
 *       let assert Ok(combinations) =
 *         maths.list_combination_with_repetitions([1, 2, 3], 2)
 *
 *       combinations
 *       |> yielder.to_list()
 *       |> should.equal([[1, 1], [1, 2], [1, 3], [2, 2], [2, 3], [3, 3]])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function list_combination_with_repetitions(arr, k) {
  if (k < 0) {
    return new Error(undefined);
  } else {
    return new Ok(
      do_list_combination_with_repetitions(
        $yielder.from_list(arr),
        k,
        toList([]),
      ),
    );
  }
}

function remove_first_by_index(arr, index_to_remove) {
  return $yielder.flat_map(
    arr,
    (tuple) => {
      let index;
      let element;
      index = tuple[0];
      element = tuple[1];
      let $ = index === index_to_remove;
      if ($) {
        return $yielder.empty();
      } else {
        return $yielder.single([index, element]);
      }
    },
  );
}

function do_list_permutation_without_repetitions(arr, k) {
  if (k === 0) {
    return $yielder.single(toList([]));
  } else {
    return $yielder.flat_map(
      arr,
      (tuple) => {
        let index;
        let element;
        index = tuple[0];
        element = tuple[1];
        let remaining = remove_first_by_index(arr, index);
        let permutations = do_list_permutation_without_repetitions(
          remaining,
          k - 1,
        );
        return $yielder.map(
          permutations,
          (permutation) => { return listPrepend(element, permutation); },
        );
      },
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Generates all possible permutations of \\(k\\) elements selected from a given list of size
 * \\(n\\). The function handles the case without repetitions, that is, repeated elements are
 * not treated as distinct.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/yielder
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // All 2-permutations of [1, 2] without repetition
 *       let assert Ok(permutations) =
 *         [1, 2]
 *         |> maths.list_permutation(2)
 *       permutations
 *       |> yielder.to_list()
 *       |> should.equal([[1, 2], [2, 1]])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function list_permutation(arr, k) {
  let $ = $list.length(arr);
  if (k < 0) {
    return new Error(undefined);
  } else {
    let arr_length = $;
    if (k > arr_length) {
      return new Error(undefined);
    } else {
      let indexed_arr = $list.index_map(
        arr,
        (element, index) => { return [index, element]; },
      );
      return new Ok(
        do_list_permutation_without_repetitions(
          $yielder.from_list(indexed_arr),
          k,
        ),
      );
    }
  }
}

function do_list_permutation_with_repetitions(arr, k) {
  if (k === 0) {
    return $yielder.single(toList([]));
  } else {
    return $yielder.flat_map(
      arr,
      (tuple) => {
        let element;
        element = tuple[1];
        let permutations = do_list_permutation_with_repetitions(arr, k - 1);
        return $yielder.map(
          permutations,
          (permutation) => { return listPrepend(element, permutation); },
        );
      },
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Generates all possible permutations of \\(k\\) elements selected from a given list of size
 * \\(n\\). The function handles the case when the repetition of elements is allowed, that is,
 * repeated elements are treated as distinct.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/yielder
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // All 2-permutations of [1, 2] with repetition
 *       let assert Ok(permutations) =
 *         [1, 2]
 *         |> maths.list_permutation_with_repetitions(2)
 *       permutations
 *       |> yielder.to_list()
 *       |> set.from_list()
 *       |> should.equal(set.from_list([[1, 1], [1, 2], [2, 2], [2, 1]]))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function list_permutation_with_repetitions(arr, k) {
  if (k < 0) {
    return new Error(undefined);
  } else {
    let indexed_arr = $list.index_map(
      arr,
      (element, index) => { return [index, element]; },
    );
    return new Ok(
      do_list_permutation_with_repetitions($yielder.from_list(indexed_arr), k),
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Generate a set containing all combinations of pairs of elements coming from two given sets.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/set
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       set.from_list([])
 *       |> maths.cartesian_product(set.from_list([]))
 *       |> should.equal(set.from_list([]))
 *
 *       set.from_list([1.0, 10.0])
 *       |> maths.cartesian_product(set.from_list([1.0, 2.0]))
 *       |> should.equal(
 *         set.from_list([#(1.0, 1.0), #(1.0, 2.0), #(10.0, 1.0), #(10.0, 2.0)]),
 *       )
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function cartesian_product(xset, yset) {
  return $set.fold(
    xset,
    $set.new$(),
    (acc0, element0) => {
      return $set.fold(
        yset,
        acc0,
        (acc1, element1) => { return $set.insert(acc1, [element0, element1]); },
      );
    },
  );
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the \\(p\\)-norm of a list (representing a vector):
 *
 * \\[
 * \left( \sum_{i=1}^n \left|x_{i}\right|^{p} \right)^{\frac{1}{p}}
 * \\]
 *
 * In the formula, \\(n\\) is the length of the list and \\(x_i\\) is the value in
 * the input list indexed by \\(i\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       [1.0, 1.0, 1.0]
 *       |> maths.norm(1.0)
 *       |> should.equal(Ok(3.0))
 *
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *       let assert Ok(result) =
 *         [1.0, 2.0, 3.0]
 *         |> maths.norm(2.0)
 *       result
 *       |> maths.is_close(3.7416573867739413, 0.0, tolerance)
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function norm(arr, p) {
  if (arr instanceof $Empty) {
    return new Ok(0.0);
  } else {
    if (p === 0.0) {
      return new Ok(
        $list.fold(
          arr,
          0.0,
          (acc, element) => {
            if (element === 0.0) {
              return acc;
            } else {
              return acc + 1.0;
            }
          },
        ),
      );
    } else if (p < 0.0) {
      let aggregate = $list.try_fold(
        arr,
        0.0,
        (acc, element) => {
          if (element === 0.0) {
            return new Error(0.0);
          } else {
            let $ = $float.power($float.absolute_value(element), p);
            let result;
            if ($ instanceof Ok) {
              result = $[0];
            } else {
              throw makeError(
                "let_assert",
                FILEPATH,
                "gleam_community/maths",
                3385,
                "norm",
                "Pattern match failed, no pattern matched the value.",
                {
                  value: $,
                  start: 95228,
                  end: 95317,
                  pattern_start: 95239,
                  pattern_end: 95249
                }
              )
            }
            return new Ok(result + acc);
          }
        },
      );
      if (aggregate instanceof Ok) {
        let result = aggregate[0];
        return $float.power(result, divideFloat(1.0, p));
      } else {
        return new Ok(0.0);
      }
    } else {
      let aggregate = $list.fold(
        arr,
        0.0,
        (acc, element) => {
          let $ = $float.power($float.absolute_value(element), p);
          let result;
          if ($ instanceof Ok) {
            result = $[0];
          } else {
            throw makeError(
              "let_assert",
              FILEPATH,
              "gleam_community/maths",
              3402,
              "norm",
              "Pattern match failed, no pattern matched the value.",
              {
                value: $,
                start: 95823,
                end: 95908,
                pattern_start: 95834,
                pattern_end: 95844
              }
            )
          }
          return result + acc;
        },
      );
      return $float.power(aggregate, divideFloat(1.0, p));
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the weighted \\(p\\)-norm of a list (representing a vector):
 *
 * \\[
 * \left( \sum_{i=1}^n w_{i} \left|x_{i}\right|^{p} \right)^{\frac{1}{p}}
 * \\]
 *
 * In the formula, \\(n\\) is the length of the list and \\(x_i\\) is the value in
 * the input list indexed by \\(i\\), while \\(w_i \in \mathbb{R}_{+}\\) is
 * a corresponding positive weight.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       [#(1.0, 0.5), #(1.0, 0.5), #(1.0, 0.5)]
 *       |> maths.norm_with_weights(1.0)
 *       |> should.equal(Ok(1.5))
 *
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *       let assert Ok(result) =
 *         [#(1.0, 0.5), #(2.0, 0.5), #(3.0, 0.5)]
 *         |> maths.norm_with_weights(2.0)
 *       result
 *       |> maths.is_close(2.6457513110645907, 0.0, tolerance)
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function norm_with_weights(arr, p) {
  if (arr instanceof $Empty) {
    return new Ok(0.0);
  } else {
    let weight_is_invalid = $list.any(
      arr,
      (tuple) => { return tuple[1] < 0.0; },
    );
    if (weight_is_invalid) {
      return new Error(undefined);
    } else {
      if (p === 0.0) {
        return new Ok(
          $list.fold(
            arr,
            0.0,
            (acc, tuple) => {
              let $ = tuple[0];
              if ($ === 0.0) {
                return acc;
              } else {
                return acc + 1.0;
              }
            },
          ),
        );
      } else if (p < 0.0) {
        let aggregate = $list.try_fold(
          arr,
          0.0,
          (acc, tuple) => {
            let $ = tuple[0];
            if ($ === 0.0) {
              return new Error(0.0);
            } else {
              let $1 = tuple[1];
              if ($1 === 0.0) {
                return new Error(0.0);
              } else {
                let $2 = $float.power($float.absolute_value(tuple[0]), p);
                let result;
                if ($2 instanceof Ok) {
                  result = $2[0];
                } else {
                  throw makeError(
                    "let_assert",
                    FILEPATH,
                    "gleam_community/maths",
                    3496,
                    "norm_with_weights",
                    "Pattern match failed, no pattern matched the value.",
                    {
                      value: $2,
                      start: 99059,
                      end: 99152,
                      pattern_start: 99070,
                      pattern_end: 99080
                    }
                  )
                }
                return new Ok((tuple[1] * result) + acc);
              }
            }
          },
        );
        if (aggregate instanceof Ok) {
          let result = aggregate[0];
          return $float.power(result, divideFloat(1.0, p));
        } else {
          return new Ok(0.0);
        }
      } else {
        let aggregate = $list.fold(
          arr,
          0.0,
          (acc, tuple) => {
            let $ = $float.power($float.absolute_value(tuple[0]), p);
            let result;
            if ($ instanceof Ok) {
              result = $[0];
            } else {
              throw makeError(
                "let_assert",
                FILEPATH,
                "gleam_community/maths",
                3513,
                "norm_with_weights",
                "Pattern match failed, no pattern matched the value.",
                {
                  value: $,
                  start: 99731,
                  end: 99820,
                  pattern_start: 99742,
                  pattern_end: 99752
                }
              )
            }
            return (tuple[1] * result) + acc;
          },
        );
        return $float.power(aggregate, divideFloat(1.0, p));
      }
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the Minkowski distance between two lists (representing
 * vectors):
 *
 * \\[
 * \left( \sum_{i=1}^n w_{i} \left|x_i - y_i \right|^{p} \right)^{\frac{1}{p}}
 * \\]
 *
 * In the formula, \\(p >= 1\\) is the order, \\(n\\) is the length of the two lists
 * and \\(x_i, y_i\\) are the values in the respective input lists indexed by \\(i\\).
 *
 * The Minkowski distance is a generalization of the Euclidean distance
 * (\\(p=2\\)) and the Manhattan distance (\\(p = 1\\)).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *
 *       let assert Ok(result) =
 *         maths.minkowski_distance([#(1.0, 2.0), #(3.0, 4.0), #(5.0, 6.0)], 4.0)
 *       result
 *       |> maths.is_close(1.3160740129524924, 0.0, tolerance)
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function minkowski_distance(arr, p) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let $ = p < 1.0;
    if ($) {
      return new Error(undefined);
    } else {
      let differences = $list.map(
        arr,
        (tuple) => { return tuple[0] - tuple[1]; },
      );
      return norm(differences, p);
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the Manhattan distance between two lists (representing
 * vectors):
 *
 * \\[
 * \sum_{i=1}^n \left|x_i - y_i \right|
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
 * values in the respective input lists indexed by \\(i\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.manhattan_distance([#(1.0, 2.0), #(2.0, 3.0)])
 *       |> should.equal(Ok(2.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function manhattan_distance(arr) {
  return minkowski_distance(arr, 1.0);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the weighted Minkowski distance between two lists (representing
 * vectors):
 *
 * \\[
 * \left( \sum_{i=1}^n w_{i} \left|x_i - y_i \right|^{p} \right)^{\frac{1}{p}}
 * \\]
 *
 * In the formula, \\(p >= 1\\) is the order, \\(n\\) is the length of the two lists
 * and \\(x_i, y_i\\) are the values in the respective input lists indexed by \\(i\\).
 * The \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
 *
 * The Minkowski distance is a generalization of the Euclidean distance
 * (\\(p=2\\)) and the Manhattan distance (\\(p = 1\\)).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *
 *       let assert Ok(result) =
 *         maths.minkowski_distance_with_weights(
 *           [#(1.0, 2.0, 0.5), #(3.0, 4.0, 1.0), #(5.0, 6.0, 1.0)],
 *           4.0,
 *         )
 *       result
 *       |> maths.is_close(1.2574334296829355, 0.0, tolerance)
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function minkowski_distance_with_weights(arr, p) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let weight_is_negative = $list.any(
      arr,
      (tuple) => { return tuple[2] < 0.0; },
    );
    let $ = p < 1.0;
    if (!weight_is_negative && !$) {
      let differences = $list.map(
        arr,
        (tuple) => { return [tuple[0] - tuple[1], tuple[2]]; },
      );
      return norm_with_weights(differences, p);
    } else {
      return new Error(undefined);
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the weighted Manhattan distance between two lists (representing
 * vectors):
 *
 * \\[
 * \sum_{i=1}^n w_{i} \left|x_i - y_i \right|
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
 * values in the respective input lists indexed by \\(i\\), while the
 * \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.manhattan_distance_with_weights([#(1.0, 2.0, 0.5), #(2.0, 3.0, 1.0)])
 *       |> should.equal(Ok(1.5))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function manhattan_distance_with_weights(arr) {
  return minkowski_distance_with_weights(arr, 1.0);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the Euclidean distance between two lists (representing
 * vectors):
 *
 * \\[
 * \left( \sum_{i=1}^n \left|x_i - y_i \right|^{2} \right)^{\frac{1}{2}}
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
 * values in the respective input lists indexed by \\(i\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *
 *       let assert Ok(result) = maths.euclidean_distance([#(1.0, 2.0), #(3.0, 4.0)])
 *       result
 *       |> maths.is_close(1.4142135623730951, 0.0, tolerance)
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function euclidean_distance(arr) {
  return minkowski_distance(arr, 2.0);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the weighted Euclidean distance between two lists (representing
 * vectors):
 *
 * \\[
 * \left( \sum_{i=1}^n w_{i} \left|x_i - y_i \right|^{2} \right)^{\frac{1}{2}}
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
 * values in the respective input lists indexed by \\(i\\), while the
 * \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *
 *       let assert Ok(result) =
 *         maths.euclidean_distance_with_weights([#(1.0, 2.0, 0.5), #(3.0, 4.0, 1.0)])
 *       result
 *       |> maths.is_close(1.224744871391589, 0.0, tolerance)
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function euclidean_distance_with_weights(arr) {
  return minkowski_distance_with_weights(arr, 2.0);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the Chebyshev distance between two lists (representing vectors):
 *
 * \\[
 * \text{max}_{i=1}^n \left|x_i - y_i \right|
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
 * values in the respective input lists indexed by \\(i\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.chebyshev_distance([#(-5.0, -1.0), #(-10.0, -12.0), #(-3.0, -3.0)])
 *       |> should.equal(Ok(4.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function chebyshev_distance(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let _pipe = $list.map(
      arr,
      (tuple) => { return $float.absolute_value((tuple[0] - tuple[1])); },
    );
    return list_maximum(_pipe, $float.compare);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the weighted Chebyshev distance between two lists (representing vectors):
 *
 * \\[
 * \text{max}_{i=1}^n w_i \left|x_i - y_i \right|
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists and \\(x_i, y_i\\) are the
 * values in the respective input lists indexed by \\(i\\), while the
 * \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.chebyshev_distance_with_weights([
 *         #(-5.0, -1.0, 0.5),
 *         #(-10.0, -12.0, 1.0),
 *         #(-3.0, -3.0, 1.0),
 *       ])
 *       |> should.equal(Ok(2.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function chebyshev_distance_with_weights(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let weight_is_negative = $list.any(
      arr,
      (tuple) => { return tuple[2] < 0.0; },
    );
    if (weight_is_negative) {
      return new Error(undefined);
    } else {
      let _pipe = $list.map(
        arr,
        (tuple) => {
          return $float.absolute_value((tuple[0] - tuple[1])) * tuple[2];
        },
      );
      return list_maximum(_pipe, $float.compare);
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the arithmetic mean of the elements in a list:
 *
 * \\[
 * \bar{x} = \frac{1}{n}\sum_{i=1}^n x_i
 * \\]
 *
 * In the formula, \\(n\\) is the sample size (the length of the list) and \\(x_i\\)
 * is the sample point in the input list indexed by \\(i\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.mean()
 *       |> should.be_error()
 *
 *       [1.0, 2.0, 3.0]
 *       |> maths.mean()
 *       |> should.equal(Ok(2.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function mean(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    return new Ok(
      divideFloat($float.sum(arr), $int.to_float($list.length(arr))),
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the n'th moment about the mean of a list of elements.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // An empty list returns an error
 *       []
 *       |> maths.moment(0)
 *       |> should.be_error()
 *
 *       // 0th moment about the mean is 1. per definition
 *       [0.0, 1.0, 2.0, 3.0, 4.0]
 *       |> maths.moment(0)
 *       |> should.equal(Ok(1.0))
 *
 *       // 1st moment about the mean is 0. per definition
 *       [0.0, 1.0, 2.0, 3.0, 4.0]
 *       |> maths.moment(1)
 *       |> should.equal(Ok(0.0))
 *
 *       // 2nd moment about the mean
 *       [0.0, 1.0, 2.0, 3.0, 4.0]
 *       |> maths.moment(2)
 *       |> should.equal(Ok(2.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function moment(arr, n) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else if (n === 0) {
    return new Ok(1.0);
  } else if (n === 1) {
    return new Ok(0.0);
  } else {
    let n$1 = n;
    if (n$1 > 1) {
      let $ = mean(arr);
      let m1;
      if ($ instanceof Ok) {
        m1 = $[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "gleam_community/maths",
          3976,
          "moment",
          "Pattern match failed, no pattern matched the value.",
          {
            value: $,
            start: 112666,
            end: 112695,
            pattern_start: 112677,
            pattern_end: 112683
          }
        )
      }
      let result = $list.try_fold(
        arr,
        0.0,
        (acc, a) => {
          let $1 = $float.power(a - m1, $int.to_float(n$1));
          if ($1 instanceof Ok) {
            let value = $1[0];
            return new Ok(acc + value);
          } else {
            return new Error(undefined);
          }
        },
      );
      if (result instanceof Ok) {
        let value = result[0];
        return new Ok(divideFloat(value, $int.to_float($list.length(arr))));
      } else {
        return new Error(undefined);
      }
    } else {
      return new Error(undefined);
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the harmonic mean \\(\bar{x}\\) of the elements in a list:
 *
 * \\[
 *   \bar{x} = \frac{n}{\sum_{i=1}^{n}\frac{1}{x_i}}
 * \\]
 *
 * In the formula, \\(n\\) is the sample size (the length of the list) and
 * \\(x_i\\) is the sample point in the input list indexed by \\(i\\).
 * Note: The harmonic mean is only defined for positive numbers.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // An empty list returns an error
 *       []
 *       |> maths.harmonic_mean()
 *       |> should.be_error()
 *
 *       // List with negative numbers returns an error
 *       [-1.0, -3.0, -6.0]
 *       |> maths.harmonic_mean()
 *       |> should.be_error()
 *
 *       // Valid input returns a result
 *       [1.0, 3.0, 6.0]
 *       |> maths.harmonic_mean()
 *       |> should.equal(Ok(2.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function harmonic_mean(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let sum = $list.try_fold(
      arr,
      0.0,
      (acc, a) => {
        let a$1 = a;
        if (a$1 > 0.0) {
          return new Ok((divideFloat(1.0, a$1)) + acc);
        } else {
          let a$2 = a;
          if (a$2 === 0.0) {
            return new Error(0.0);
          } else {
            return new Error(-1.0);
          }
        }
      },
    );
    if (sum instanceof Ok) {
      let sum$1 = sum[0];
      return new Ok(divideFloat($int.to_float($list.length(arr)), sum$1));
    } else {
      let $ = sum[0];
      if ($ === 0.0) {
        return new Ok(0.0);
      } else {
        return new Error(undefined);
      }
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the geometric mean \\(\bar{x}\\) of the elements in a list:
 *
 * \\[
 *   \bar{x} = \left(\prod^{n}_{i=1} x_i\right)^{\frac{1}{n}}
 * \\]
 *
 * In the formula, \\(n\\) is the sample size (the length of the list) and
 * \\(x_i\\) is the sample point in the input list indexed by \\(i\\).
 * Note: The geometric mean is only defined for positive numbers.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // An empty list returns an error
 *       []
 *       |> maths.geometric_mean()
 *       |> should.be_error()
 *
 *       // List with negative numbers returns an error
 *       [-1.0, -3.0, -6.0]
 *       |> maths.geometric_mean()
 *       |> should.be_error()
 *
 *       // Valid input returns a result
 *       [1.0, 3.0, 9.0]
 *       |> maths.geometric_mean()
 *       |> should.equal(Ok(3.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function geometric_mean(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let product = $list.try_fold(
      arr,
      1.0,
      (acc, a) => {
        let a$1 = a;
        if (a$1 > 0.0) {
          return new Ok(acc * a$1);
        } else {
          let a$2 = a;
          if (a$2 === 0.0) {
            return new Error(0.0);
          } else {
            return new Error(-1.0);
          }
        }
      },
    );
    if (product instanceof Ok) {
      let product$1 = product[0];
      return $float.power(
        product$1,
        divideFloat(1.0, $int.to_float($list.length(arr))),
      );
    } else {
      let $ = product[0];
      if ($ === 0.0) {
        return new Ok(0.0);
      } else {
        return new Error(undefined);
      }
    }
  }
}

function do_median(xs, mid, mean, index) {
  return $bool.guard(
    index > mid,
    new Error(undefined),
    () => {
      let mid_less_one = mid - 1;
      if (xs instanceof $Empty) {
        return new Error(undefined);
      } else if (!mean && (index === mid)) {
        let x = xs.head;
        return new Ok(x);
      } else {
        let $ = xs.tail;
        if ($ instanceof $Empty) {
          let rest = $;
          return do_median(rest, mid, mean, index + 1);
        } else if (mean && (index === mid_less_one)) {
          let x = xs.head;
          let y = $.head;
          return new Ok((x + y) / 2.0);
        } else {
          let rest = $;
          return do_median(rest, mid, mean, index + 1);
        }
      }
    },
  );
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the median of the elements in a list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       []
 *       |> maths.median()
 *       |> should.be_error()
 *
 *       [1.0, 2.0, 3.0]
 *       |> maths.median()
 *       |> should.equal(Ok(2.0))
 *
 *       [1.0, 2.0, 3.0, 4.0]
 *       |> maths.median()
 *       |> should.equal(Ok(2.5))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function median(arr) {
  return $bool.guard(
    $list.is_empty(arr),
    new Error(undefined),
    () => {
      let length = $list.length(arr);
      let mid = globalThis.Math.trunc(length / 2);
      let arr_sorted = $list.sort(arr, $float.compare);
      let $ = (length % 2) === 0;
      if ($) {
        return do_median(arr_sorted, mid, true, 0);
      } else {
        return do_median(arr_sorted, mid, false, 0);
      }
    },
  );
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the sample variance of the elements in a list:
 *
 * \\[
 * s^{2} = \frac{1}{n - d} \sum_{i=1}^{n}(x_i - \bar{x})
 * \\]
 *
 * In the formula, \\(n\\) is the sample size (the length of the list) and \\(x_i\\)
 * is the sample point in the input list indexed by \\(i\\).
 * Furthermore, \\(\bar{x}\\) is the sample mean and \\(d\\) is the "Delta
 * Degrees of Freedom". It is typically set to \\(d = 1\\), which gives an unbiased estimate.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // Degrees of freedom
 *       let ddof = 1
 *
 *       []
 *       |> maths.variance(ddof)
 *       |> should.be_error()
 *
 *       [1.0, 2.0, 3.0]
 *       |> maths.variance(ddof)
 *       |> should.equal(Ok(1.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function variance(arr, ddof) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else if (ddof < 0) {
    return new Error(undefined);
  } else {
    let $ = mean(arr);
    let mean$1;
    if ($ instanceof Ok) {
      mean$1 = $[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        4297,
        "variance",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $,
          start: 121215,
          end: 121246,
          pattern_start: 121226,
          pattern_end: 121234
        }
      )
    }
    return new Ok(
      (() => {
        let _pipe = $list.map(
          arr,
          (element) => {
            let $1 = $float.power(element - mean$1, 2.0);
            let result;
            if ($1 instanceof Ok) {
              result = $1[0];
            } else {
              throw makeError(
                "let_assert",
                FILEPATH,
                "gleam_community/maths",
                4304,
                "variance",
                "Pattern match failed, no pattern matched the value.",
                {
                  value: $1,
                  start: 121610,
                  end: 121667,
                  pattern_start: 121621,
                  pattern_end: 121631
                }
              )
            }
            return result;
          },
        );
        let _pipe$1 = $float.sum(_pipe);
        return ((element) => {
          return divideFloat(
            element,
            ($int.to_float($list.length(arr)) - $int.to_float(ddof))
          );
        })(_pipe$1);
      })(),
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the sample standard deviation of the elements in a list:
 * \\[
 * s = \left(\frac{1}{n - d} \sum_{i=1}^{n}(x_i - \bar{x})\right)^{\frac{1}{2}}
 * \\]
 *
 * In the formula, \\(n\\) is the sample size (the length of the list) and \\(x_i\\)
 * is the sample point in the input list indexed by \\(i\\).
 * Furthermore, \\(\bar{x}\\) is the sample mean and \\(d\\) is the "Delta
 * Degrees of Freedom", and is typically set to \\(d = 1\\), which gives an unbiased estimate.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // Degrees of freedom
 *       let ddof = 1
 *
 *       []
 *       |> maths.standard_deviation(ddof)
 *       |> should.be_error()
 *
 *       [1.0, 2.0, 3.0]
 *       |> maths.standard_deviation(ddof)
 *       |> should.equal(Ok(1.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function standard_deviation(arr, ddof) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else if (ddof < 0) {
    return new Error(undefined);
  } else {
    let $ = variance(arr, ddof);
    let variance$1;
    if ($ instanceof Ok) {
      variance$1 = $[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        4365,
        "standard_deviation",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $,
          start: 123423,
          end: 123468,
          pattern_start: 123434,
          pattern_end: 123446
        }
      )
    }
    return $float.square_root(variance$1);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the sample kurtosis of a list of elements using the
 * definition of Fisher.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // An empty list returns an error
 *       []
 *       |> maths.kurtosis()
 *       |> should.be_error()
 *
 *       // To calculate kurtosis at least four values are needed
 *       [1.0, 2.0, 3.0]
 *       |> maths.kurtosis()
 *       |> should.be_error()
 *
 *       [1.0, 2.0, 3.0, 4.0]
 *       |> maths.kurtosis()
 *       |> should.equal(Ok(-1.36))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function kurtosis(arr) {
  let $ = $list.length(arr) < 4;
  if ($) {
    return new Error(undefined);
  } else {
    let $1 = moment(arr, 2);
    let $2 = moment(arr, 4);
    if ($2 instanceof Ok && $1 instanceof Ok) {
      let m2 = $1[0];
      if (m2 !== 0.0) {
        let m4 = $2[0];
        let $3 = $float.power(m2, 2.0);
        if ($3 instanceof Ok) {
          let value = $3[0];
          return new Ok((divideFloat(m4, value)) - 3.0);
        } else {
          return $3;
        }
      } else {
        return new Error(undefined);
      }
    } else {
      return new Error(undefined);
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the sample skewness of a list of elements using the
 * Fisher-Pearson coefficient of skewness.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // An empty list returns an error
 *       []
 *       |> maths.skewness()
 *       |> should.be_error()
 *
 *       // To calculate skewness at least three values are needed
 *       [1.0, 2.0, 3.0]
 *       |> maths.skewness()
 *       |> should.equal(Ok(0.0))
 *
 *       [1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 3.0, 4.0]
 *       |> maths.skewness()
 *       |> should.equal(Ok(0.6))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function skewness(arr) {
  let $ = $list.length(arr) < 3;
  if ($) {
    return new Error(undefined);
  } else {
    let $1 = moment(arr, 2);
    let $2 = moment(arr, 3);
    if ($2 instanceof Ok && $1 instanceof Ok) {
      let m2 = $1[0];
      if (m2 !== 0.0) {
        let m3 = $2[0];
        let $3 = $float.power(m2, 1.5);
        if ($3 instanceof Ok) {
          let value = $3[0];
          return new Ok(divideFloat(m3, value));
        } else {
          return $3;
        }
      } else {
        return new Error(undefined);
      }
    } else {
      return new Error(undefined);
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the n'th percentile of the elements in a list using
 * linear interpolation between closest ranks.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // An empty list returns an error
 *       []
 *       |> maths.percentile(40)
 *       |> should.be_error()
 *
 *       // Calculate 40th percentile
 *       [15.0, 20.0, 35.0, 40.0, 50.0]
 *       |> maths.percentile(40)
 *       |> should.equal(Ok(29.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function percentile(arr, n) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let $ = arr.tail;
    if ($ instanceof $Empty) {
      let element = arr.head;
      return new Ok(element);
    } else {
      let n$1 = n;
      if (n$1 === 0) {
        return $list.first($list.sort(arr, $float.compare));
      } else {
        let n$2 = n;
        if (n$2 === 100) {
          return $list.last($list.sort(arr, $float.compare));
        } else {
          let n$3 = n;
          if ((n$3 > 0) && (n$3 < 100)) {
            let r = ($int.to_float(n$3) / 100.0) * $int.to_float(
              $list.length(arr) - 1,
            );
            let f = $float.truncate(r);
            let sorted_arr = $list.drop($list.sort(arr, $float.compare), f);
            let $1 = $list.take(sorted_arr, 2);
            if ($1 instanceof $Empty) {
              return new Error(undefined);
            } else {
              let $2 = $1.tail;
              if ($2 instanceof $Empty) {
                return new Error(undefined);
              } else {
                let $3 = $2.tail;
                if ($3 instanceof $Empty) {
                  let lower = $1.head;
                  let upper = $2.head;
                  return new Ok(
                    lower + ((upper - lower) * (r - $int.to_float(f))),
                  );
                } else {
                  return new Error(undefined);
                }
              }
            }
          } else {
            return new Error(undefined);
          }
        }
      }
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the z-score of each value in the list relative to the sample
 * mean and standard deviation.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // An empty list returns an error
 *       []
 *       // Use degrees of freedom = 1
 *       |> maths.zscore(1)
 *       |> should.be_error()
 *
 *       [1.0, 2.0, 3.0]
 *       // Use degrees of freedom = 1
 *       |> maths.zscore(1)
 *       |> should.equal(Ok([-1.0, 0.0, 1.0]))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function zscore(arr, ddof) {
  let length = $list.length(arr);
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let ddof$1 = ddof;
    if (ddof$1 < 0) {
      return new Error(undefined);
    } else {
      let ddof$2 = ddof;
      if (length <= ddof$2) {
        return new Error(undefined);
      } else {
        let $ = mean(arr);
        let $1 = standard_deviation(arr, ddof$2);
        if ($1 instanceof Ok && $ instanceof Ok) {
          let stdev = $1[0];
          if (stdev !== 0.0) {
            let mean$1 = $[0];
            return new Ok(
              $list.map(
                arr,
                (a) => { return divideFloat((a - mean$1), stdev); },
              ),
            );
          } else {
            return new Error(undefined);
          }
        } else {
          return new Error(undefined);
        }
      }
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the interquartile range (IQR) of the elements in a list.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // An empty list returns an error
 *       []
 *       |> maths.interquartile_range()
 *       |> should.be_error()
 *
 *       // Valid input returns a result
 *       [1.0, 2.0, 3.0, 4.0, 5.0]
 *       |> maths.interquartile_range()
 *       |> should.equal(Ok(3.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function interquartile_range(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let length = $list.length(arr);
    let arr_sorted = $list.sort(arr, $float.compare);
    let $ = $int.is_even(length);
    if ($) {
      let $1 = $list.split(arr_sorted, globalThis.Math.trunc(length / 2));
      let lower_half;
      let upper_half;
      lower_half = $1[0];
      upper_half = $1[1];
      let $2 = median(lower_half);
      let $3 = median(upper_half);
      if ($3 instanceof Ok && $2 instanceof Ok) {
        let q3 = $3[0];
        let q1 = $2[0];
        return new Ok(q3 - q1);
      } else {
        return new Error(undefined);
      }
    } else {
      let $1 = $list.split(arr_sorted, globalThis.Math.trunc((length - 1) / 2));
      let lower_half;
      lower_half = $1[0];
      let $2 = $list.split(arr_sorted, globalThis.Math.trunc((length + 1) / 2));
      let upper_half;
      upper_half = $2[1];
      let $3 = median(lower_half);
      let $4 = median(upper_half);
      if ($4 instanceof Ok && $3 instanceof Ok) {
        let q3 = $4[0];
        let q1 = $3[0];
        return new Ok(q3 - q1);
      } else {
        return new Error(undefined);
      }
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate Pearson's sample correlation coefficient to determine the linear
 * relationship between the elements in two lists of equal
 * length. The correlation coefficient \\(r_{xy} \in \[-1, 1\]\\) is calculated
 * as:
 *
 * \\[
 * r_{xy} =\frac{\sum ^n _{i=1}(x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum^n _{i=1}(x_i - \bar{x})^2} \sqrt{\sum^n _{i=1}(y_i - \bar{y})^2}}
 * \\]
 *
 * In the formula, \\(n\\) is the sample size (the length of the input lists),
 * \\(x_i\\), \\(y_i\\) are the corresponding sample points indexed by \\(i\\) and
 * \\(\bar{x}\\), \\(\bar{y}\\) are the sample means.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       // An empty lists returns an error
 *       maths.correlation([], [])
 *       |> should.be_error()
 *
 *       // Perfect positive correlation
 *       let xarr =
 *         list.range(0, 100)
 *         |> list.map(fn(x) { int.to_float(x) })
 *       let yarr =
 *         list.range(0, 100)
 *         |> list.map(fn(y) { int.to_float(y) })
 *       list.zip(xarr, yarr)
 *       |> maths.correlation()
 *       |> should.equal(Ok(1.0))
 *
 *       // Perfect negative correlation
 *       let xarr =
 *         list.range(0, 100)
 *         |> list.map(fn(x) { -1.0 *. int.to_float(x) })
 *       let yarr =
 *         list.range(0, 100)
 *         |> list.map(fn(y) { int.to_float(y) })
 *       list.zip(xarr, yarr)
 *       |> maths.correlation()
 *       |> should.equal(Ok(-1.0))
 *
 *       // No correlation (independent variables)
 *       let xarr = [1.0, 2.0, 3.0, 4.0]
 *       let yarr = [5.0, 5.0, 5.0, 5.0]
 *       list.zip(xarr, yarr)
 *       |> maths.correlation()
 *       |> should.equal(Ok(0.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function correlation(arr) {
  let length = $list.length(arr);
  let $ = length >= 2;
  if ($) {
    let $1 = $list.unzip(arr);
    let xarr;
    let yarr;
    xarr = $1[0];
    yarr = $1[1];
    let $2 = mean(xarr);
    let xmean;
    if ($2 instanceof Ok) {
      xmean = $2[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        4743,
        "correlation",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $2,
          start: 134466,
          end: 134499,
          pattern_start: 134477,
          pattern_end: 134486
        }
      )
    }
    let $3 = mean(yarr);
    let ymean;
    if ($3 instanceof Ok) {
      ymean = $3[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        4744,
        "correlation",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $3,
          start: 134506,
          end: 134539,
          pattern_start: 134517,
          pattern_end: 134526
        }
      )
    }
    let _block;
    let _pipe = $list.map(
      arr,
      (tuple) => { return (tuple[0] - xmean) * (tuple[1] - ymean); },
    );
    _block = $float.sum(_pipe);
    let a = _block;
    let _block$1;
    let _pipe$1 = $list.map(xarr, (x) => { return (x - xmean) * (x - xmean); });
    _block$1 = $float.sum(_pipe$1);
    let b = _block$1;
    let _block$2;
    let _pipe$2 = $list.map(yarr, (y) => { return (y - ymean) * (y - ymean); });
    _block$2 = $float.sum(_pipe$2);
    let c = _block$2;
    let $4 = $float.square_root(b * c);
    let value;
    if ($4 instanceof Ok) {
      value = $4[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        4758,
        "correlation",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $4,
          start: 135044,
          end: 135092,
          pattern_start: 135055,
          pattern_end: 135064
        }
      )
    }
    return new Ok(divideFloat(a, value));
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The Tversky index is a generalization of the Jaccard index and Sørensen-Dice
 * coefficient, which adds flexibility in measuring similarity between two sets using two
 * parameters, \\(\alpha\\) and \\(\beta\\). These parameters allow for asymmetric
 * similarity measures between sets. The Tversky index is defined as:
 *
 * \\[
 * \frac{|X \cap Y|}{|X \cap Y| + \alpha|X - Y| + \beta|Y - X|}
 * \\]
 *
 * where:
 *
 * - \\(X\\) and \\(Y\\) are the sets being compared
 * - \\(|X - Y|\\) and \\(|Y - X|\\) are the sizes of the relative complements of
 * \\(Y\\) in \\(X\\) and \\(X\\) in \\(Y\\), respectively,
 * - \\(\alpha\\) and \\(\beta\\) are parameters that weight the relative importance
 * of the elements unique to \\(X\\) and \\(Y\\)
 *
 * The Tversky index reduces to the Jaccard index when \\(\alpha = \beta = 1\\) and
 * to the Sørensen-Dice coefficient when \\(\alpha = \beta = 0.5\\). In general, the
 * Tversky index can take on any non-negative value, including 0. The index equals
 * 0 when there is no intersection between the two sets, indicating no similarity.
 * The Tversky index does not have a strict upper limit of 1 when \\(\alpha \neq \beta\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/set
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let yset = set.from_list(["cat", "dog", "hippo", "monkey"])
 *       let xset = set.from_list(["monkey", "rhino", "ostrich", "salmon"])
 *       // Test Jaccard index (alpha = beta = 1)
 *       maths.tversky_index(xset, yset, 1.0, 1.0)
 *       |> should.equal(Ok(1.0 /. 7.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function tversky_index(xset, yset, alpha, beta) {
  let $ = alpha >= 0.0;
  let $1 = beta >= 0.0;
  if ($1 && $) {
    let _block;
    let _pipe = $set.intersection(xset, yset);
    let _pipe$1 = $set.size(_pipe);
    _block = $int.to_float(_pipe$1);
    let intersection = _block;
    let _block$1;
    let _pipe$2 = $set.difference(xset, yset);
    let _pipe$3 = $set.size(_pipe$2);
    _block$1 = $int.to_float(_pipe$3);
    let difference1 = _block$1;
    let _block$2;
    let _pipe$4 = $set.difference(yset, xset);
    let _pipe$5 = $set.size(_pipe$4);
    _block$2 = $int.to_float(_pipe$5);
    let difference2 = _block$2;
    return new Ok(
      divideFloat(
        intersection,
        ((intersection + (alpha * difference1)) + (beta * difference2))
      ),
    );
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The Jaccard index measures similarity between two sets of elements. Mathematically, the
 * Jaccard index is defined as:
 *
 * \\[
 * \frac{|X \cap Y|}{|X \cup Y|} \\; \in \\; \left[0, 1\right]
 * \\]
 *
 * where:
 *
 * - \\(X\\) and \\(Y\\) are two sets being compared
 * - \\(|X \cap Y|\\) represents the size of the intersection of the two sets
 * - \\(|X \cup Y|\\) denotes the size of the union of the two sets
 *
 * The value of the Jaccard index ranges from 0 to 1, where 0 indicates that the
 * two sets share no elements and 1 indicates that the sets are identical. The
 * Jaccard index is a special case of the  [Tversky index](#tversky_index) (with
 * \\(\alpha=\beta=1\\)).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/set
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let xset = set.from_list(["cat", "dog", "hippo", "monkey"])
 *       let yset = set.from_list(["monkey", "rhino", "ostrich", "salmon"])
 *       maths.jaccard_index(xset, yset)
 *       |> should.equal(1.0 /. 7.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function jaccard_index(xset, yset) {
  let $ = tversky_index(xset, yset, 1.0, 1.0);
  let result;
  if ($ instanceof Ok) {
    result = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      4812,
      "jaccard_index",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 136787,
        end: 136846,
        pattern_start: 136798,
        pattern_end: 136808
      }
    )
  }
  return result;
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The Sørensen-Dice coefficient measures the similarity between two sets of elements.
 * Mathematically, the coefficient is defined as:
 *
 * \\[
 * \frac{2 |X \cap Y|}{|X| + |Y|} \\; \in \\; \left[0, 1\right]
 * \\]
 *
 * where:
 *
 * - \\(X\\) and \\(Y\\) are two sets being compared
 * - \\(|X \cap Y|\\) is the size of the intersection of the two sets (i.e., the
 * number of elements common to both sets)
 * - \\(|X|\\) and \\(|Y|\\) are the sizes of the sets \\(X\\) and \\(Y\\), respectively
 *
 * The coefficient ranges from 0 to 1, where 0 indicates no similarity (the sets
 * share no elements) and 1 indicates perfect similarity (the sets are identical).
 * The higher the coefficient, the greater the similarity between the two sets.
 * The Sørensen-Dice coefficient is a special case of the
 * [Tversky index](#tversky_index) (with \\(\alpha=\beta=0.5\\)).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/set
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let xset = set.from_list(["cat", "dog", "hippo", "monkey"])
 *       let yset = set.from_list(["monkey", "rhino", "ostrich", "salmon", "spider"])
 *       maths.sorensen_dice_coefficient(xset, yset)
 *       |> should.equal(2.0 *. 1.0 /. { 4.0 +. 5.0 })
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function sorensen_dice_coefficient(xset, yset) {
  let $ = tversky_index(xset, yset, 0.5, 0.5);
  let result;
  if ($ instanceof Ok) {
    result = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/maths",
      4866,
      "sorensen_dice_coefficient",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 138758,
        end: 138817,
        pattern_start: 138769,
        pattern_end: 138779
      }
    )
  }
  return result;
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The Overlap coefficient, also known as the Szymkiewicz–Simpson coefficient, is
 * a measure of similarity between two sets that focuses on the size of the
 * intersection relative to the smaller of the two sets. It is defined
 * mathematically as:
 *
 * \\[
 * \frac{|X \cap Y|}{\min(|X|, |Y|)} \\; \in \\; \left[0, 1\right]
 * \\]
 *
 * where:
 *
 * - \\(X\\) and \\(Y\\) are the sets being compared
 * - \\(|X \cap Y|\\) is the size of the intersection of the sets
 * - \\(\min(|X|, |Y|)\\) is the size of the smaller set among \\(X\\) and \\(Y\\)
 *
 * The coefficient ranges from 0 to 1, where 0 indicates no overlap and 1
 * indicates that the smaller set is a suyset of the larger set. This
 * measure is especially useful in situations where the similarity in terms
 * of the proportion of overlap is more relevant than the difference in sizes
 * between the two sets.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/set
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let set_a = set.from_list(["horse", "dog", "hippo", "monkey", "bird"])
 *       let set_b = set.from_list(["monkey", "bird", "ostrich", "salmon"])
 *       maths.overlap_coefficient(set_a, set_b)
 *       |> should.equal(2.0 /. 4.0)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function overlap_coefficient(xset, yset) {
  let _block;
  let _pipe = $set.intersection(xset, yset);
  let _pipe$1 = $set.size(_pipe);
  _block = $int.to_float(_pipe$1);
  let intersection = _block;
  let _block$1;
  let _pipe$2 = $int.min($set.size(xset), $set.size(yset));
  _block$1 = $int.to_float(_pipe$2);
  let minsize = _block$1;
  return divideFloat(intersection, minsize);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the cosine similarity between two lists (representing
 * vectors):
 *
 * \\[
 * \frac{\sum_{i=1}^n  x_i \cdot y_i}
 * {\left(\sum_{i=1}^n x_i^2\right)^{\frac{1}{2}}
 * \cdot
 * \left(\sum_{i=1}^n y_i^2\right)^{\frac{1}{2}}}
 * \\; \in \\; \left[-1, 1\right]
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists and \\(x_i\\), \\(y_i\\) are
 * the values in the respective input lists indexed by \\(i\\).
 *
 * The cosine similarity provides a value between -1 and 1, where 1 means the
 * vectors are in the same direction, -1 means they are in exactly opposite
 * directions, and 0 indicates orthogonality.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/option
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       // Two orthogonal vectors
 *       maths.cosine_similarity([#(-1.0, 1.0), #(1.0, 1.0), #(0.0, -1.0)])
 *       |> should.equal(Ok(0.0))
 *
 *       // Two identical (parallel) vectors
 *       maths.cosine_similarity([#(1.0, 1.0), #(2.0, 2.0), #(3.0, 3.0)])
 *       |> should.equal(Ok(1.0))
 *
 *       // Two parallel, but oppositely oriented vectors
 *       maths.cosine_similarity([#(-1.0, 1.0), #(-2.0, 2.0), #(-3.0, 3.0)])
 *       |> should.equal(Ok(-1.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function cosine_similarity(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let numerator = $list.fold(
      arr,
      0.0,
      (acc, tuple) => { return acc + (tuple[0] * tuple[1]); },
    );
    let xarr = $list.map(arr, (tuple) => { return tuple[0]; });
    let yarr = $list.map(arr, (tuple) => { return tuple[1]; });
    let $ = norm(xarr, 2.0);
    let xarr_norm;
    if ($ instanceof Ok) {
      xarr_norm = $[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        5073,
        "cosine_similarity",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $,
          start: 145613,
          end: 145655,
          pattern_start: 145624,
          pattern_end: 145637
        }
      )
    }
    let $1 = norm(yarr, 2.0);
    let yarr_norm;
    if ($1 instanceof Ok) {
      yarr_norm = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        5074,
        "cosine_similarity",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 145662,
          end: 145704,
          pattern_start: 145673,
          pattern_end: 145686
        }
      )
    }
    let denominator = (xarr_norm * yarr_norm);
    return new Ok(divideFloat(numerator, denominator));
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the weighted cosine similarity between two lists (representing
 * vectors):
 *
 * \\[
 * \frac{\sum_{i=1}^n w_{i} \cdot x_i \cdot y_i}
 * {\left(\sum_{i=1}^n w_{i} \cdot x_i^2\right)^{\frac{1}{2}}
 * \cdot
 * \left(\sum_{i=1}^n w_{i} \cdot y_i^2\right)^{\frac{1}{2}}}
 * \\; \in \\; \left[-1, 1\right]
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists and \\(x_i\\), \\(y_i\\) are
 * the values in the respective input lists indexed by \\(i\\), while the
 * \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
 *
 * The cosine similarity provides a value between -1 and 1, where 1 means the
 * vectors are in the same direction, -1 means they are in exactly opposite
 * directions, and 0 indicates orthogonality.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/option
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *
 *       let assert Ok(result) =
 *         maths.cosine_similarity_with_weights([
 *           #(1.0, 1.0, 2.0),
 *           #(2.0, 2.0, 3.0),
 *           #(3.0, 3.0, 4.0),
 *         ])
 *       result
 *       |> maths.is_close(1.0, 0.0, tolerance)
 *       |> should.be_true()
 *
 *       let assert Ok(result) =
 *         maths.cosine_similarity_with_weights([
 *           #(-1.0, 1.0, 1.0),
 *           #(-2.0, 2.0, 0.5),
 *           #(-3.0, 3.0, 0.33),
 *         ])
 *       result
 *       |> maths.is_close(-1.0, 0.0, tolerance)
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function cosine_similarity_with_weights(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let weight_is_negative = $list.any(
      arr,
      (tuple) => { return tuple[2] < 0.0; },
    );
    if (weight_is_negative) {
      return new Error(undefined);
    } else {
      let numerator = $list.fold(
        arr,
        0.0,
        (acc, tuple) => { return acc + ((tuple[0] * tuple[1]) * tuple[2]); },
      );
      let xarr = $list.map(arr, (tuple) => { return [tuple[0], tuple[2]]; });
      let yarr = $list.map(arr, (tuple) => { return [tuple[1], tuple[2]]; });
      let $ = norm_with_weights(xarr, 2.0);
      let xarr_norm;
      if ($ instanceof Ok) {
        xarr_norm = $[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "gleam_community/maths",
          5167,
          "cosine_similarity_with_weights",
          "Pattern match failed, no pattern matched the value.",
          {
            value: $,
            start: 148512,
            end: 148567,
            pattern_start: 148523,
            pattern_end: 148536
          }
        )
      }
      let $1 = norm_with_weights(yarr, 2.0);
      let yarr_norm;
      if ($1 instanceof Ok) {
        yarr_norm = $1[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "gleam_community/maths",
          5168,
          "cosine_similarity_with_weights",
          "Pattern match failed, no pattern matched the value.",
          {
            value: $1,
            start: 148578,
            end: 148633,
            pattern_start: 148589,
            pattern_end: 148602
          }
        )
      }
      let denominator = (xarr_norm * yarr_norm);
      return new Ok(divideFloat(numerator, denominator));
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the Canberra distance between two lists:
 *
 * \\[
 * \sum_{i=1}^n \frac{\left| x_i - y_i \right|}
 * {\left| x_i \right| + \left| y_i \right|}
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists, and \\(x_i, y_i\\) are the
 * values in the respective input lists indexed by \\(i\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.canberra_distance([])
 *       |> should.be_error()
 *
 *       maths.canberra_distance([#(1.0, -2.0), #(2.0, -1.0)])
 *       |> should.equal(Ok(2.0))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function canberra_distance(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    return new Ok(
      $list.fold(
        arr,
        0.0,
        (acc, tuple) => {
          let numerator = $float.absolute_value((tuple[0] - tuple[1]));
          let denominator = ($float.absolute_value(tuple[0]) + $float.absolute_value(
            tuple[1],
          ));
          return acc + (divideFloat(numerator, denominator));
        },
      ),
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the weighted Canberra distance between two lists:
 *
 * \\[
 * \sum_{i=1}^n w_{i}\frac{\left| x_i - y_i \right|}
 * {\left| x_i \right| + \left| y_i \right|}
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists, and \\(x_i, y_i\\) are the
 * values in the respective input lists indexed by \\(i\\), while the
 * \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.canberra_distance_with_weights([])
 *       |> should.be_error()
 *
 *       maths.canberra_distance_with_weights([#(1.0, -2.0, 0.5), #(2.0, -1.0, 1.0)])
 *       |> should.equal(Ok(1.5))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function canberra_distance_with_weights(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let weight_is_negative = $list.any(
      arr,
      (tuple) => { return tuple[2] < 0.0; },
    );
    if (weight_is_negative) {
      return new Error(undefined);
    } else {
      return new Ok(
        $list.fold(
          arr,
          0.0,
          (acc, tuple) => {
            let numerator = $float.absolute_value((tuple[0] - tuple[1]));
            let denominator = ($float.absolute_value(tuple[0]) + $float.absolute_value(
              tuple[1],
            ));
            return acc + (divideFloat((tuple[2] * numerator), denominator));
          },
        ),
      );
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the Bray-Curtis distance between two lists:
 *
 * \\[
 * \frac{\sum_{i=1}^n  \left| x_i - y_i \right|}
 * {\sum_{i=1}^n \left| x_i + y_i \right|}
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists, and \\(x_i, y_i\\) are the values
 * in the respective input lists indexed by \\(i\\).
 *
 * The Bray-Curtis distance is in the range \\([0, 1]\\) if all entries \\(x_i, y_i\\) are
 * positive.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.braycurtis_distance([])
 *       |> should.be_error()
 *
 *       maths.braycurtis_distance([#(1.0, 3.0), #(2.0, 4.0)])
 *       |> should.equal(Ok(0.4))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function braycurtis_distance(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let numerator = $list.fold(
      arr,
      0.0,
      (acc, tuple) => {
        return acc + $float.absolute_value((tuple[0] - tuple[1]));
      },
    );
    let denominator = $list.fold(
      arr,
      0.0,
      (acc, tuple) => {
        return acc + $float.absolute_value((tuple[0] + tuple[1]));
      },
    );
    return new Ok((divideFloat(numerator, denominator)));
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Calculate the weighted Bray-Curtis distance between two lists:
 *
 * \\[
 * \frac{\sum_{i=1}^n w_{i} \left| x_i - y_i \right|}
 * {\sum_{i=1}^n w_{i}\left| x_i + y_i \right|}
 * \\]
 *
 * In the formula, \\(n\\) is the length of the two lists, and \\(x_i, y_i\\) are the values
 * in the respective input lists indexed by \\(i\\), while the
 * \\(w_i \in \mathbb{R}_{+}\\) are corresponding positive weights.
 *
 * The Bray-Curtis distance is in the range \\([0, 1]\\) if all entries \\(x_i, y_i\\) are
 * positive and \\(w_i = 1.0\\;\forall i=1...n\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.braycurtis_distance_with_weights([])
 *       |> should.be_error()
 *
 *       maths.braycurtis_distance_with_weights([#(1.0, 3.0, 0.5), #(2.0, 4.0, 1.0)])
 *       |> should.equal(Ok(0.375))
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function braycurtis_distance_with_weights(arr) {
  if (arr instanceof $Empty) {
    return new Error(undefined);
  } else {
    let weight_is_negative = $list.any(
      arr,
      (tuple) => { return tuple[2] < 0.0; },
    );
    if (weight_is_negative) {
      return new Error(undefined);
    } else {
      let numerator = $list.fold(
        arr,
        0.0,
        (acc, tuple) => {
          return acc + (tuple[2] * $float.absolute_value((tuple[0] - tuple[1])));
        },
      );
      let denominator = $list.fold(
        arr,
        0.0,
        (acc, tuple) => {
          return acc + (tuple[2] * $float.absolute_value((tuple[0] + tuple[1])));
        },
      );
      return new Ok((divideFloat(numerator, denominator)));
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Determine if a given value \\(x\\) is close to or equivalent to a reference value
 * \\(y\\) based on supplied relative \\(r_{tol}\\) and absolute \\(a_{tol}\\) tolerance
 * values. The equivalance of the two given values are then determined based on
 * the equation:
 *
 * \\[
 *     \|x - y\| \leq (a_{tol} + r_{tol} \cdot \|y\|)
 * \\]
 *
 * `True` is returned if the statement holds, otherwise `False` is returned.
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let value = 99.
 *       let reference_value = 100.
 *       // We set 'absolute_tolerance' and 'relative_tolerance' such that the values are
 *       // equivalent if 'value' is within 1 percent of 'reference_value' +/- 0.1
 *       let relative_tolerance = 0.01
 *       let absolute_tolerance = 0.10
 *       maths.is_close(value, reference_value, relative_tolerance, absolute_tolerance)
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function is_close(x, y, rtol, atol) {
  let x$1 = absolute_difference(x, y);
  let y$1 = atol + (rtol * $float.absolute_value(y));
  return x$1 <= y$1;
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Determine if each value \\(x_i\\) is close to or equivalent to its corresponding reference value
 * \\(y_i\\), in a list of value pairs \\((x_i, y_i)\\), based on supplied relative \\(r_{tol}\\)
 * and absolute  \\(a_{tol}\\) tolerance values. The equivalence of each pair \\((x_i, y_i)\\) is
 * determined by the equation:
 *
 * \\[
 *     \|x_i - y_i\| \leq (a_{tol} + r_{tol} \cdot \|y_i\|), \\; \forall i=1,...,n.
 * \\]
 *
 * A list of `Bool` values is returned, where each entry indicates if the corresponding pair
 * satisfies the condition. If all conditions are satisfied, the list will contain only `True`
 * values.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/list
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let value = 99.0
 *       let reference_value = 100.0
 *       let xarr = list.repeat(value, 42)
 *       let yarr = list.repeat(reference_value, 42)
 *       let arr = list.zip(xarr, yarr)
 *       // We set 'absolute_tolerance' and 'relative_tolerance' such that
 *       // the values are equivalent if 'value' is within 1 percent of
 *       // 'reference_value' +/- 0.1
 *       let relative_tolerance = 0.01
 *       let absolute_tolerance = 0.1
 *       let assert Ok(result) =
 *         maths.all_close(arr, relative_tolerance, absolute_tolerance)
 *       result
 *       |> list.all(fn(x) { x == True })
 *       |> should.be_true()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function all_close(arr, rtol, atol) {
  return $list.map(
    arr,
    (_use0) => {
      let x;
      let y;
      x = _use0[0];
      y = _use0[1];
      return is_close(x, y, rtol, atol);
    },
  );
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Determine if a given value \\(x\\) is fractional, i.e., if it contains a fractional part:
 *
 * \\[
 *     x - \lfloor x \rfloor > 0
 * \\]
 *
 * `True` is returned if the given value is fractional (i.e., it has a non-zero decimal part),
 * otherwise `False` is returned.
 *
 * <details>
 *     <summary>Example</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       maths.is_fractional(0.3333)
 *       |> should.equal(True)
 *
 *       maths.is_fractional(1.0)
 *       |> should.equal(False)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function is_fractional(x) {
  return (do_ceiling(x) - x) > 0.0;
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A function that determines if a given integer value \\(x \in \mathbb{Z}\\) is a power of
 * another integer value \\(y \in \mathbb{Z}\\), i.e., the function evaluates whether \\(x\\) can
 * be expressed as \\(y^n\\) for some integer \\(n \geq 0\\), by computing the base-\\(y\\)
 * logarithm of \\(x\\):
 *
 * \\[
 *     n = \log_y(x)
 * \\]
 *
 * If \\(n\\) is an integer (i.e., it has no fractional part), then \\(x\\) is a power of \\(y\\)
 * and `True` is returned. Otherwise `False` is returned.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       // Check if 4 is a power of 2 (it is)
 *       maths.is_power(4, 2)
 *       |> should.equal(True)
 *
 *       // Check if 5 is a power of 2 (it is not)
 *       maths.is_power(5, 2)
 *       |> should.equal(False)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function is_power(x, y) {
  let $ = logarithm($int.to_float(x), $int.to_float(y));
  if ($ instanceof Ok) {
    let value = $[0];
    let truncated = round_to_zero(value, 0);
    let remainder = value - truncated;
    return remainder === 0.0;
  } else {
    return false;
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A function that tests whether a given integer value \\(n \in \mathbb{Z}\\) is a
 * perfect number. A number is perfect if it is equal to the sum of its proper
 * positive divisors.
 *
 * <details>
 *     <summary>Details</summary>
 *
 *   For example:
 *   - \\(6\\) is a perfect number since the divisors of 6 are \\(1 + 2 + 3 = 6\\).
 *   - \\(28\\) is a perfect number since the divisors of 28 are \\(1 + 2 + 4 + 7 + 14 = 28\\).
 *
 * </details>
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.is_perfect(6)
 *       |> should.equal(True)
 *
 *       maths.is_perfect(28)
 *       |> should.equal(True)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function is_perfect(n) {
  return $int.sum(proper_divisors(n)) === n;
}

function powmod_with_check(base, exponent, modulus) {
  let $ = (exponent % 2) === 0;
  if (exponent === 0) {
    return 1;
  } else if ($) {
    let x = powmod_with_check(
      base,
      globalThis.Math.trunc(exponent / 2),
      modulus,
    );
    let $1 = remainderInt(x * x, modulus);
    let $2 = (x !== 1) && (x !== (modulus - 1));
    if ($2 && $1 === 1) {
      return 0;
    } else {
      return remainderInt(x * x, modulus);
    }
  } else {
    return remainderInt(
      base * powmod_with_check(base, exponent - 1, modulus),
      modulus
    );
  }
}

function miller_rabin_test(loop$n, loop$k) {
  while (true) {
    let n = loop$n;
    let k = loop$k;
    if (k === 0) {
      return true;
    } else {
      let random_candidate = 2 + $int.random(n - 2);
      let $ = powmod_with_check(random_candidate, n - 1, n) === 1;
      if ($) {
        loop$n = n;
        loop$k = k - 1;
      } else {
        return $;
      }
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A function that tests whether a given integer value \\(x \in \mathbb{Z}\\) is a
 * prime number. A prime number is a natural number greater than 1 that has no
 * positive divisors other than 1 and itself.
 *
 * The function uses the Miller-Rabin primality test to assess if \\(x\\) is prime.
 * It is a probabilistic test, so it can mistakenly identify a composite number
 * as prime. However, the probability of such errors decreases with more testing
 * iterations (the function uses 64 iterations internally, which is typically
 * more than sufficient). The Miller-Rabin test is particularly useful for large
 * numbers.
 *
 * <details>
 *     <summary>Details</summary>
 *
 *   Examples of prime numbers:
 *   - \\(2\\) is a prime number since it has only two divisors: \\(1\\) and \\(2\\).
 *   - \\(7\\) is a prime number since it has only two divisors: \\(1\\) and \\(7\\).
 *   - \\(4\\) is not a prime number since it has divisors other than \\(1\\) and itself, such
 *     as \\(2\\).
 *
 * </details>
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.is_prime(2)
 *       |> should.equal(True)
 *
 *       maths.is_prime(4)
 *       |> should.equal(False)
 *
 *       // Test the 2nd Carmichael number
 *       maths.is_prime(1105)
 *       |> should.equal(False)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function is_prime(x) {
  let x$1 = x;
  if (x$1 < 2) {
    return false;
  } else {
    let x$2 = x;
    if (x$2 === 2) {
      return true;
    } else {
      return miller_rabin_test(x$2, 64);
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A function that tests whether a given real number \\(x \in \mathbb{R}\\) is strictly
 * between two other real numbers, \\(a,b \in \mathbb{R}\\), such that \\(a < x < b\\).
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.is_between(5.5, 5.0, 6.0)
 *       |> should.equal(True)
 *
 *       maths.is_between(5.0, 5.0, 6.0)
 *       |> should.equal(False)
 *
 *       maths.is_between(6.0, 5.0, 6.0)
 *       |> should.equal(False)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function is_between(x, lower, upper) {
  return (lower < x) && (x < upper);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A function that tests whether a given integer \\(n \in \mathbb{Z}\\) is divisible by another
 * integer \\(d \in \mathbb{Z}\\), such that \\(n \mod d = 0\\).
 *
 * <details>
 *     <summary>Details</summary>
 *
 *   For example:
 *   - \\(n = 10\\) is divisible by \\(d = 2\\) because \\(10 \mod 2 = 0\\).
 *   - \\(n = 7\\) is not divisible by \\(d = 3\\) because \\(7 \mod 3 \neq 0\\).
 *
 * </details>
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.is_divisible(10, 2)
 *       |> should.equal(True)
 *
 *       maths.is_divisible(7, 3)
 *       |> should.equal(False)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function is_divisible(n, d) {
  return (remainderInt(n, d)) === 0;
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * A function that tests whether a given integer \\(m \in \mathbb{Z}\\) is a multiple of another
 * integer \\(k \in \mathbb{Z}\\), such that \\(m = k \cdot q\\), with \\(q \in \mathbb{Z}\\).
 *
 * <details>
 *     <summary>Details</summary>
 *
 *   For example:
 *   - \\(m = 15\\) is a multiple of \\(k = 5\\) because \\(15 = 5 \cdot 3\\).
 *   - \\(m = 14\\) is not a multiple of \\(k = 5\\) because \\(\frac{14}{5}\\) does not yield an
 *     integer quotient.
 *
 * </details>
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       maths.is_multiple(15, 5)
 *       |> should.equal(True)
 *
 *       maths.is_multiple(14, 5)
 *       |> should.equal(False)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function is_multiple(m, k) {
  return (remainderInt(m, k)) === 0;
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The error function.
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function erf(x) {
  let a1 = 0.254829592;
  let a2 = -0.284496736;
  let a3 = 1.421413741;
  let a4 = -1.453152027;
  let a5 = 1.061405429;
  let p = 0.3275911;
  let sign$1 = sign(x);
  let x$1 = $float.absolute_value(x);
  let t = divideFloat(1.0, (1.0 + (p * x$1)));
  let y = 1.0 - ((((((((((a5 * t) + a4) * t) + a3) * t) + a2) * t) + a1) * t) * exponential(
    (-1.0 * x$1) * x$1,
  ));
  return sign$1 * y;
}

function incomplete_gamma_sum(loop$a, loop$x, loop$t, loop$s, loop$n) {
  while (true) {
    let a = loop$a;
    let x = loop$x;
    let t = loop$t;
    let s = loop$s;
    let n = loop$n;
    if (t === 0.0) {
      return s;
    } else {
      let ns = s + t;
      let nt = t * (divideFloat(x, (a + n)));
      loop$a = a;
      loop$x = x;
      loop$t = nt;
      loop$s = ns;
      loop$n = n + 1.0;
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The lower incomplete gamma function over the real numbers.
 *
 * The implemented incomplete gamma function is evaluated through a power series
 * expansion.
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function incomplete_gamma(a, x) {
  let $ = (a > 0.0) && (x >= 0.0);
  if ($) {
    let $1 = $float.power(x, a);
    let v;
    if ($1 instanceof Ok) {
      v = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        6038,
        "incomplete_gamma",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 173749,
          end: 173785,
          pattern_start: 173760,
          pattern_end: 173765
        }
      )
    }
    return new Ok(
      (v * exponential(-1.0 * x)) * incomplete_gamma_sum(
        a,
        x,
        divideFloat(1.0, a),
        0.0,
        1.0,
      ),
    );
  } else {
    return new Error(undefined);
  }
}

function do_step_range(
  loop$current,
  loop$increment,
  loop$remaining_steps,
  loop$acc
) {
  while (true) {
    let current = loop$current;
    let increment = loop$increment;
    let remaining_steps = loop$remaining_steps;
    let acc = loop$acc;
    if (remaining_steps === 0) {
      return acc;
    } else {
      loop$current = current - increment;
      loop$increment = increment;
      loop$remaining_steps = remaining_steps - 1;
      loop$acc = listPrepend(current, acc);
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function returns a list of evenly spaced values within a specified interval
 * `[start, stop)` based on a given increment size.
 *
 * Note that if `increment > 0`, the sequence progresses from `start`  towards `stop`, while if
 * `increment < 0`, the sequence progresses from `start` towards `stop` in reverse.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       maths.step_range(1.0, 5.0, 1.0)
 *       |> should.equal([1.0, 2.0, 3.0, 4.0])
 *
 *       // No points returned since
 *       // start is smaller than stop and the step is positive
 *       maths.step_range(5.0, 1.0, 1.0)
 *       |> should.equal([])
 *
 *       // Points returned since
 *       // start smaller than stop but negative step
 *       maths.step_range(5.0, 1.0, -1.0)
 *       |> should.equal([5.0, 4.0, 3.0, 2.0])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function step_range(start, stop, increment) {
  let $ = ((start >= stop) && (increment > 0.0)) || ((start <= stop) && (increment < 0.0));
  if ($) {
    return toList([]);
  } else {
    let _block;
    let $1 = start <= stop;
    if ($1) {
      _block = 1.0;
    } else {
      _block = -1.0;
    }
    let direction = _block;
    let increment_abs = $float.absolute_value(increment);
    let distance = $float.absolute_value(start - stop);
    let steps = $float.round(divideFloat(distance, increment_abs));
    let adjusted_stop = stop - (increment_abs * direction);
    return do_step_range(
      adjusted_stop,
      increment_abs * direction,
      steps,
      toList([]),
    );
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function is similar to [`step_range`](#step_range) but instead returns a yielder
 * (lazily evaluated sequence of elements). This function can be used whenever there is a need
 * to generate a larger-than-usual sequence of elements.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/yielder.{Next, Done}
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let range = maths.yield_step_range(1.0, 2.5, 0.5)
 *
 *       let assert Next(element, rest) = yielder.step(range)
 *       should.equal(element, 1.0)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 1.5)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 2.0)
 *
 *       // We have generated 3 values over the interval [1.0, 2.5)
 *       // in increments of 0.5, so the 4th will be 'Done'
 *       should.equal(yielder.step(rest), Done)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function yield_step_range(start, stop, increment) {
  let $ = ((start >= stop) && (increment > 0.0)) || ((start <= stop) && (increment < 0.0));
  if ($) {
    return $yielder.empty();
  } else {
    let _block;
    let $1 = start <= stop;
    if ($1) {
      _block = 1.0;
    } else {
      _block = -1.0;
    }
    let direction = _block;
    let increment_abs = $float.absolute_value(increment);
    let distance = $float.absolute_value(start - stop);
    let num = $float.round(divideFloat(distance, increment_abs));
    return $yielder.map(
      $yielder.range(0, num - 1),
      (index) => {
        return start + (($int.to_float(index) * increment_abs) * direction);
      },
    );
  }
}

function do_linear_space(
  loop$current,
  loop$increment,
  loop$remaining_steps,
  loop$acc
) {
  while (true) {
    let current = loop$current;
    let increment = loop$increment;
    let remaining_steps = loop$remaining_steps;
    let acc = loop$acc;
    if (remaining_steps === 0) {
      return acc;
    } else {
      loop$current = current - increment;
      loop$increment = increment;
      loop$remaining_steps = remaining_steps - 1;
      loop$acc = listPrepend(current, acc);
    }
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function returns a list of linearly spaced points over a specified
 * interval. The endpoint of the interval can optionally be included/excluded. The number of
 * points and whether the endpoint is included determine the spacing between values.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *       let assert Ok(linspace) = maths.linear_space(10.0, 20.0, 5, True)
 *       let pairs = linspace |> list.zip([10.0, 12.5, 15.0, 17.5, 20.0])
 *       let assert Ok(result) = maths.all_close(pairs, 0.0, tolerance)
 *       result
 *       |> list.all(fn(x) { x == True })
 *       |> should.be_true()
 *
 *       // A negative number of points (-5) does not work
 *       maths.linear_space(10.0, 50.0, -5, True)
 *       |> should.be_error()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function linear_space(start, stop, steps, endpoint) {
  let _block;
  let $ = start <= stop;
  if ($) {
    _block = 1.0;
  } else {
    _block = -1.0;
  }
  let direction = _block;
  let _block$1;
  if (endpoint) {
    _block$1 = divideFloat(
      $float.absolute_value(start - stop),
      $int.to_float(steps - 1)
    );
  } else {
    _block$1 = divideFloat(
      $float.absolute_value(start - stop),
      $int.to_float(steps)
    );
  }
  let increment_abs = _block$1;
  let _block$2;
  if (endpoint) {
    _block$2 = stop;
  } else {
    _block$2 = stop - (increment_abs * direction);
  }
  let adjusted_stop = _block$2;
  let $1 = steps > 0;
  if ($1) {
    return new Ok(
      do_linear_space(
        adjusted_stop,
        increment_abs * direction,
        steps,
        toList([]),
      ),
    );
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function is similar to [`linear_space`](#linear_space) but instead returns a yielder
 * (lazily evaluated sequence of elements). This function can be used whenever there is a need
 * to generate a larger-than-usual sequence of elements.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/yielder.{Next, Done}
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let assert Ok(linspace) = maths.yield_linear_space(10.0, 20.0, 5, True)
 *
 *       let assert Next(element, rest) = yielder.step(linspace)
 *       should.equal(element, 10.0)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 12.5)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 15.0)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 17.5)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 20.0)
 *
 *       // We have generated 5 values, so the 6th will be 'Done'
 *       should.equal(yielder.step(rest), Done)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function yield_linear_space(start, stop, steps, endpoint) {
  let _block;
  let $ = start <= stop;
  if ($) {
    _block = 1.0;
  } else {
    _block = -1.0;
  }
  let direction = _block;
  let _block$1;
  if (endpoint) {
    _block$1 = divideFloat(
      $float.absolute_value(start - stop),
      $int.to_float(steps - 1)
    );
  } else {
    _block$1 = divideFloat(
      $float.absolute_value(start - stop),
      $int.to_float(steps)
    );
  }
  let increment = _block$1;
  let $1 = steps > 0;
  if ($1) {
    return new Ok(
      $yielder.map(
        $yielder.range(0, steps - 1),
        (index) => {
          return start + (($int.to_float(index) * increment) * direction);
        },
      ),
    );
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function returns a list of logarithmically spaced points over a specified
 * interval. The endpoint of the interval can optionally be included/excluded.
 * The number of points, base, and whether the endpoint is included determine
 * the spacing between values.
 *
 * The values in the sequence are computed as powers of the given base, where
 * the exponents are evenly spaced between `start` and `stop`. The `base`
 * parameter must be positive, as negative bases lead to undefined behavior when
 * computing fractional exponents. Similarly, the number of points (`steps`) must
 * be positive; specifying zero or a negative value will result in an error.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *       let assert Ok(logspace) = maths.logarithmic_space(1.0, 3.0, 3, True, 10.0)
 *       let pairs = logspace |> list.zip([10.0, 100.0, 1000.0])
 *       let assert Ok(result) = maths.all_close(pairs, 0.0, tolerance)
 *       result
 *       |> list.all(fn(x) { x == True })
 *       |> should.be_true()
 *
 *       // A negative number of points (-3) does not work
 *       maths.logarithmic_space(1.0, 3.0, -3, False, 10.0)
 *       |> should.be_error()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function logarithmic_space(start, stop, steps, endpoint, base) {
  let $ = (steps > 0) && (base > 0.0);
  if ($) {
    let $1 = linear_space(start, stop, steps, endpoint);
    let linspace;
    if ($1 instanceof Ok) {
      linspace = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        6425,
        "logarithmic_space",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 184951,
          end: 185019,
          pattern_start: 184962,
          pattern_end: 184974
        }
      )
    }
    return new Ok(
      $list.map(
        linspace,
        (value) => {
          let $2 = $float.power(base, value);
          let result;
          if ($2 instanceof Ok) {
            result = $2[0];
          } else {
            throw makeError(
              "let_assert",
              FILEPATH,
              "gleam_community/maths",
              6437,
              "logarithmic_space",
              "Pattern match failed, no pattern matched the value.",
              {
                value: $2,
                start: 185413,
                end: 185461,
                pattern_start: 185424,
                pattern_end: 185434
              }
            )
          }
          return result;
        },
      ),
    );
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function is similar to [`logarithmic_space`](#logarithmic_space) but instead returns a yielder
 * (lazily evaluated sequence of elements). This function can be used whenever there is a need
 * to generate a larger-than-usual sequence of elements.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/yielder.{Next, Done}
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let assert Ok(logspace) =
 *         maths.yield_logarithmic_space(1.0, 3.0, 3, True, 10.0)
 *
 *       let assert Next(element, rest) = yielder.step(logspace)
 *       should.equal(element, 10.0)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 100.0)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 1000.0)
 *
 *       // We have generated 3 values, so the 4th will be 'Done'
 *       should.equal(yielder.step(rest), Done)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function yield_logarithmic_space(start, stop, steps, endpoint, base) {
  let $ = (steps > 0) && (base > 0.0);
  if ($) {
    let $1 = yield_linear_space(start, stop, steps, endpoint);
    let linspace;
    if ($1 instanceof Ok) {
      linspace = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        6499,
        "yield_logarithmic_space",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 187219,
          end: 187293,
          pattern_start: 187230,
          pattern_end: 187242
        }
      )
    }
    return new Ok(
      $yielder.map(
        linspace,
        (value) => {
          let $2 = $float.power(base, value);
          let result;
          if ($2 instanceof Ok) {
            result = $2[0];
          } else {
            throw makeError(
              "let_assert",
              FILEPATH,
              "gleam_community/maths",
              6511,
              "yield_logarithmic_space",
              "Pattern match failed, no pattern matched the value.",
              {
                value: $2,
                start: 187691,
                end: 187739,
                pattern_start: 187702,
                pattern_end: 187712
              }
            )
          }
          return result;
        },
      ),
    );
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function returns a list of a geometric progression between two specified
 * values, where each value is a constant multiple of the previous one. Unlike
 * [`logarithmic_space`](#logarithmic_space), this function allows specifying the starting
 * and ending values (`start` and `stop`) directly, without requiring them to be transformed
 * into exponents.
 *
 * Internally, the function computes the logarithms of `start` and `stop` and generates evenly
 * spaced points in the logarithmic domain (using base 10). These points are then transformed back
 * into their original scale to create a sequence of values that grow multiplicatively.
 *
 * The `start` and `stop` values must be positive, as logarithms are undefined for non-positive
 * values. The number of points (`steps`) must also be positive; specifying zero or a negative
 * value will result in an error.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/yielder
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let assert Ok(tolerance) = float.power(10.0, -6.0)
 *       let assert Ok(logspace) = maths.geometric_space(10.0, 1000.0, 3, True)
 *       let pairs = logspace |> list.zip([10.0, 100.0, 1000.0])
 *       let assert Ok(result) = maths.all_close(pairs, 0.0, tolerance)
 *       result
 *       |> list.all(fn(x) { x == True })
 *       |> should.be_true()
 *
 *       // Input (start and stop can't be less than or equal to 0.0)
 *       maths.geometric_space(0.0, 1000.0, 3, False)
 *       |> should.be_error()
 *
 *       maths.geometric_space(-1000.0, 0.0, 3, False)
 *       |> should.be_error()
 *
 *       // A negative number of points (-3) does not work
 *       maths.geometric_space(10.0, 1000.0, -3, False)
 *       |> should.be_error()
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function geometric_space(start, stop, steps, endpoint) {
  let $ = ((start <= 0.0) || (stop <= 0.0)) || (steps < 0);
  if ($) {
    return new Error(undefined);
  } else {
    let $1 = logarithm_10(start);
    let log_start;
    if ($1 instanceof Ok) {
      log_start = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        6586,
        "geometric_space",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 190380,
          end: 190426,
          pattern_start: 190391,
          pattern_end: 190404
        }
      )
    }
    let $2 = logarithm_10(stop);
    let log_stop;
    if ($2 instanceof Ok) {
      log_stop = $2[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        6587,
        "geometric_space",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $2,
          start: 190433,
          end: 190477,
          pattern_start: 190444,
          pattern_end: 190456
        }
      )
    }
    return logarithmic_space(log_start, log_stop, steps, endpoint, 10.0);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function is similar to [`geometric_space`](#geometric_space) but instead returns a yielder
 * (lazily evaluated sequence of elements). This function can be used whenever there is a need
 * to generate a larger-than-usual sequence of elements.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/yielder.{Next, Done}
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example () {
 *       let assert Ok(logspace) = maths.yield_geometric_space(10.0, 1000.0, 3, True)
 *
 *       let assert Next(element, rest) = yielder.step(logspace)
 *       should.equal(element, 10.0)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 100.0)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 1000.0)
 *
 *       // We have generated 3 values, so the 4th will be 'Done'
 *       should.equal(yielder.step(rest), Done)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function yield_geometric_space(start, stop, steps, endpoint) {
  let $ = ((start <= 0.0) || (stop <= 0.0)) || (steps < 0);
  if ($) {
    return new Error(undefined);
  } else {
    let $1 = logarithm_10(start);
    let log_start;
    if ($1 instanceof Ok) {
      log_start = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        6646,
        "yield_geometric_space",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 192300,
          end: 192346,
          pattern_start: 192311,
          pattern_end: 192324
        }
      )
    }
    let $2 = logarithm_10(stop);
    let log_stop;
    if ($2 instanceof Ok) {
      log_stop = $2[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        6647,
        "yield_geometric_space",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $2,
          start: 192353,
          end: 192397,
          pattern_start: 192364,
          pattern_end: 192376
        }
      )
    }
    return yield_logarithmic_space(log_start, log_stop, steps, endpoint, 10.0);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * Generates evenly spaced points around a center value. The total span (around the center value)
 * is determined by the `radius` argument of the function.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       let assert Ok(symspace) = maths.symmetric_space(0.0, 5.0, 5)
 *       symspace
 *       |> should.equal([-5.0, -2.5, 0.0, 2.5, 5.0])
 *
 *       // A negative radius reverses the order of the values
 *       let assert Ok(symspace) = maths.symmetric_space(0.0, -5.0, 5)
 *       symspace
 *       |> should.equal([5.0, 2.5, 0.0, -2.5, -5.0])
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function symmetric_space(center, radius, steps) {
  let $ = steps > 0;
  if ($) {
    let start = center - radius;
    let stop = center + radius;
    return linear_space(start, stop, steps, true);
  } else {
    return new Error(undefined);
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The function is similar to [`symmetric_space`](#symmetric_space) but instead returns a yielder
 * (lazily evaluated sequence of elements). This function can be used whenever there is a need
 * to generate a larger-than-usual sequence of elements.
 *
 * <details>
 *     <summary>Example:</summary>
 *
 *     import gleam/yielder.{Next, Done}
 *     import gleeunit/should
 *     import gleam_community/maths
 *
 *     pub fn example() {
 *       let assert Ok(symspace) = maths.yield_symmetric_space(0.0, 5.0, 5)
 *
 *       let assert Next(element, rest) = yielder.step(symspace)
 *       should.equal(element, -5.0)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, -2.5)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 0.0)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 2.5)
 *
 *       let assert Next(element, rest) = yielder.step(rest)
 *       should.equal(element, 5.0)
 *
 *       // We have generated 5 values, so the 6th will be 'Done'
 *       should.equal(yielder.step(rest), Done)
 *     }
 * </details>
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function yield_symmetric_space(center, radius, steps) {
  let $ = steps > 0;
  if ($) {
    let start = center - radius;
    let stop = center + radius;
    return yield_linear_space(start, stop, steps, true);
  } else {
    return new Error(undefined);
  }
}

/**
 * A constant used in the Lanczos approximation formula.
 * 
 * @ignore
 */
const lanczos_g = 7.0;

/**
 * Lanczos coefficients for the approximation formula. These coefficients are part of a
 * polynomial approximation to the Gamma function.
 * 
 * @ignore
 */
const lanczos_p = /* @__PURE__ */ toList([
  0.99999999999980993,
  676.5203681218851,
  -1259.1392167224028,
  771.32342877765313,
  -176.61502916214059,
  12.507343278686905,
  -0.13857109526572012,
  0.0000099843695780195716,
  0.00000015056327351493116,
]);

/**
 * Compute the Gamma function using an approximation with the same coefficients used by the GNU
 * Scientific Library. The function handles both the reflection formula for `x < 0.5` and the
 * standard Lanczos computation for `x >= 0.5`.
 * 
 * @ignore
 */
function gamma_lanczos(x) {
  let $ = x < 0.5;
  if ($) {
    return divideFloat(pi(), (sin(pi() * x) * gamma_lanczos(1.0 - x)));
  } else {
    let z = x - 1.0;
    let x$1 = $list.index_fold(
      lanczos_p,
      0.0,
      (acc, v, index) => {
        let $1 = index > 0;
        if ($1) {
          return acc + (divideFloat(v, (z + $int.to_float(index))));
        } else {
          return v;
        }
      },
    );
    let t = (z + lanczos_g) + 0.5;
    let $1 = $float.power(2.0 * pi(), 0.5);
    let v1;
    if ($1 instanceof Ok) {
      v1 = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        6006,
        "gamma_lanczos",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 172652,
          end: 172701,
          pattern_start: 172663,
          pattern_end: 172669
        }
      )
    }
    let $2 = $float.power(t, z + 0.5);
    let v2;
    if ($2 instanceof Ok) {
      v2 = $2[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/maths",
        6007,
        "gamma_lanczos",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $2,
          start: 172708,
          end: 172752,
          pattern_start: 172719,
          pattern_end: 172725
        }
      )
    }
    return ((v1 * v2) * exponential(-1.0 * t)) * x$1;
  }
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The gamma function over the real numbers. The function is essentially equal to
 * the factorial for any positive integer argument: \\(\Gamma(n) = (n - 1)!\\)
 *
 * The implemented gamma function is approximated through Lanczos approximation
 * using the same coefficients used by the GNU Scientific Library.
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function gamma(x) {
  return gamma_lanczos(x);
}

/**
 * <div style="text-align: right;">
 *     <a href="https://github.com/gleam-community/maths/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 * </div>
 *
 * The beta function over the real numbers:
 *
 * \\[
 * \text{B}(x, y) = \frac{\Gamma(x) \cdot \Gamma(y)}{\Gamma(x + y)}
 * \\]
 *
 * The beta function is evaluated through the use of the gamma function.
 *
 * <div style="text-align: right;">
 *     <a href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function beta(x, y) {
  return divideFloat((gamma(x) * gamma(y)), gamma(x + y));
}
