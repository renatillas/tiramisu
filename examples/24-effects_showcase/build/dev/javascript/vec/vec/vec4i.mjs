import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Ok, makeError, divideInt } from "../gleam.mjs";
import * as $vec4 from "../vec/vec4.mjs";
import { Vec4 } from "../vec/vec4.mjs";

const FILEPATH = "src/vec/vec4i.gleam";

/**
 * Returns a new vector with all components clamped between a lower and upper
 * bound.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> clamp(
 *   Vec4(10, 21, -54, 75),
 *   Vec4(14, 18, 323, 91),
 * )
 * // -> Vec4(12, 21, 323, 75)
 * ```
 */
export function clamp(vector, start_bound, stop_bound) {
  return new Vec4(
    $int.clamp(vector.x, start_bound.x, stop_bound.x),
    $int.clamp(vector.y, start_bound.y, stop_bound.y),
    $int.clamp(vector.z, start_bound.z, stop_bound.z),
    $int.clamp(vector.w, start_bound.w, stop_bound.w),
  );
}

/**
 * Compares two vectors, returning the smaller of the two.
 *
 * ## Examples
 *
 * ```gleam
 * min(Vec4(12, -34, 420, 69), Vec4(10, 21, -54, 75))
 * // -> Vec4(10, -34, -54, 69)
 * ```
 */
export function min(a, b) {
  let _pipe = a;
  return $vec4.map2(_pipe, b, $int.min);
}

/**
 * Compares two vectors, returning the larger of the two.
 *
 * ## Examples
 *
 * ```gleam
 * max(Vec4(12, -34, 420, 69), Vec4(14, -93, 323, 91))
 * // -> Vec4(14, -34, 420, 91)
 * ```
 */
export function max(a, b) {
  let _pipe = a;
  return $vec4.map2(_pipe, b, $int.max);
}

/**
 * Returns a new vector with all elements in absolute values.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> absolute_value()
 * // -> Vec4(12, 34, 420, 69)
 * ```
 */
export function absolute_value(vector) {
  let _pipe = vector;
  return $vec4.map(_pipe, $int.absolute_value);
}

/**
 * Takes an int vector and returns its value as a int vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> to_vec4f()
 * // -> Vec4(12.0, -34.0, 420.0, 69.0)
 * ```
 */
export function to_vec4f(vector) {
  let _pipe = vector;
  return $vec4.map(_pipe, $int.to_float);
}

/**
 * Returns a new vector with all elements negated.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> negate()
 * // -> Vec4(-12, 34, -420, -69)
 * ```
 */
export function negate(vector) {
  let _pipe = vector;
  return $vec4.map(_pipe, $int.negate);
}

/**
 * Computes the remainder of an integer vector division of inputs as a
 * `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(13, -13, 13, -13) |> remainder(Vec4(3, 3, -3, -3))
 * // -> Ok(Vec4(1, -1, 1, -1))
 * ```
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> remainder(Vec4(0, 1, 2, 3))
 * // -> Error(Nil)
 * ```
 */
export function remainder(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec4.map2(_pipe, divisor, $int.remainder);
  return $vec4.result(_pipe$1);
}

/**
 * Returns the modulo of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(13, -13, 13, -13) |> modulo(Vec4(3, 3, -3, -3))
 * // -> Ok(Vec4(1, 2, -2, -1))
 * ```
 */
export function modulo(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec4.map2(_pipe, divisor, $int.modulo);
  return $vec4.result(_pipe$1);
}

/**
 * Returns division of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> divide(Vec4(2, 5, 4, 1))
 * // -> Ok(Vec4(6, -6, 105, 69))
 * ```
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> divide(Vec4(0, 5, 4, 1))
 * // -> Error(Nil)
 * ```
 */
export function divide(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec4.map2(_pipe, divisor, $int.divide);
  return $vec4.result(_pipe$1);
}

/**
 * Performs a *floored* integer vector division, which means that the result
 * will always be rounded towards negative infinity.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> floor_divide(Vec4(2, 5, 4, 1))
 * // -> Ok(Vec4(6, -7, 105, 69))
 * ```
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> floor_divide(Vec4(0, 5, 4, 1))
 * // -> Error(Nil)
 * ```
 */
export function floor_divide(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec4.map2(_pipe, divisor, $int.floor_divide);
  return $vec4.result(_pipe$1);
}

/**
 * Adds two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> add(Vec4(21, 45, -20, -9))
 * // -> Vec4(33, 11, 400, 60)
 * ```
 */
export function add(a, b) {
  let _pipe = a;
  return $vec4.map2(_pipe, b, $int.add);
}

/**
 * Sums a list of vectors.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec4(12, -34, 420, 69),
 *   Vec4(21, 45, -20, 9),
 *   Vec4(33, 0, -200, 3),
 * ]
 * |> sum()
 * // -> Vec4(66, 11, 200, 81)
 * ```
 */
export function sum(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec4.splat(0), add);
}

/**
 * Multiplies two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> multiply(Vec4(2, -3, 0, 1))
 * // -> Vec4(24, 102, 0, 69)
 * ```
 */
export function multiply(a, b) {
  let _pipe = a;
  return $vec4.map2(_pipe, b, $int.multiply);
}

/**
 * Multiplies a list of vectors and returns the product.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec4(12, -34, 420, 69),
 *   Vec4(21, -10, 999, 20),
 *   Vec4(32, 20, 0, 5),
 * ]
 * |> product()
 * // -> Vec4(8064, 6800, 0, 6900)
 * ```
 */
export function product(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec4.splat(1), multiply);
}

/**
 * Subtracts one vector from another.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> subtract(Vec4(7, -45, 20, 32))
 * // -> Vec4(5, 11, 400, 37)
 * ```
 */
export function subtract(a, b) {
  let _pipe = a;
  return $vec4.map2(_pipe, b, $int.subtract);
}

/**
 * Returns the squared length (squared magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> length_squared()
 * // -> 182_461
 * ```
 */
export function length_squared(vector) {
  let _pipe = vector;
  let _pipe$1 = $vec4.to_list(_pipe);
  let _pipe$2 = $list.map(_pipe$1, (element) => { return element * element; });
  return $int.sum(_pipe$2);
}

/**
 * Returns the length (magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> length()
 * // -> 427.15
 * ```
 */
export function length(vector) {
  let _block;
  let _pipe = vector;
  let _pipe$1 = length_squared(_pipe);
  _block = $int.square_root(_pipe$1);
  let $ = _block;
  let length$1;
  if ($ instanceof Ok) {
    length$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "vec/vec4i",
      287,
      "length",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 6159,
        end: 6230,
        pattern_start: 6170,
        pattern_end: 6180
      }
    )
  }
  return length$1;
}

/**
 * Compares two vector's lengths, returning an `Order`:
 * `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
 *
 * ## Examples
 *
 * ```gleam
 * compare_length(Vec4(12, -34, 420, 69), Vec4(2, 3, 4, 5))
 * // -> Gt
 * ```
 */
export function compare_length(a, b) {
  return $int.compare(
    (() => {
      let _pipe = a;
      return length_squared(_pipe);
    })(),
    (() => {
      let _pipe = b;
      return length_squared(_pipe);
    })(),
  );
}

/**
 * Returns the squared distance between two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> distance_squared(Vec4(2, 3, 4, 5))
 * // -> 178_621
 * ```
 */
export function distance_squared(a, b) {
  let _pipe = a;
  let _pipe$1 = $vec4.map2(_pipe, b, $int.subtract);
  return length_squared(_pipe$1);
}

/**
 * Returns the distance between two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> distance(Vec4(2, 3, 4, 5))
 * // -> 422.64
 * ```
 */
export function distance(a, b) {
  let _block;
  let _pipe = distance_squared(a, b);
  _block = $int.square_root(_pipe);
  let $ = _block;
  let distance$1;
  if ($ instanceof Ok) {
    distance$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "vec/vec4i",
      329,
      "distance",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 7161,
        end: 7230,
        pattern_start: 7172,
        pattern_end: 7184
      }
    )
  }
  return distance$1;
}

/**
 * Compares two vector's distances to a vector, returning an `Order`:
 * `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
 *
 * ## Examples
 *
 * ```gleam
 * compare_distance(
 *   Vec4(12, -34, 420, 69),
 *   Vec4(2, 3, 4, 5),
 *   Vec4(-25, 67, 194, 0),
 * )
 * // -> Gt
 * ```
 */
export function compare_distance(a, b, vector) {
  return $int.compare(
    (() => {
      let _pipe = a;
      return distance_squared(_pipe, vector);
    })(),
    (() => {
      let _pipe = b;
      return distance_squared(_pipe, vector);
    })(),
  );
}

/**
 * Returns a new vector containing the elements multiplies by `scalar`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> scale(2)
 * // -> Vec4(24, -68, 840, 138)
 * ```
 */
export function scale(vector, scalar) {
  let _pipe = vector;
  return $vec4.map(
    _pipe,
    (_capture) => { return $int.multiply(_capture, scalar); },
  );
}

/**
 * Returns the dot product of two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> dot(Vec4(2, 3, 4, 5))
 * // -> 1947
 * ```
 */
export function dot(a, b) {
  let _pipe = a;
  let _pipe$1 = multiply(_pipe, b);
  let _pipe$2 = $vec4.to_list(_pipe$1);
  return $int.sum(_pipe$2);
}

/**
 * Returns the projection of a vector on another vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> project(Vec4(2, 3, 4, 5))
 * // -> Vec4(72, 108, 144, 180)
 * ```
 */
export function project(a, b) {
  let _pipe = b;
  return scale(_pipe, divideInt(dot(a, b), dot(b, b)));
}

/**
 * Returns a new vector resulting from sliding this vector along a plane
 * defined by the given normal vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> slide(Vec4(2, 3, 4, 5))
 * // -> Vec4(-60, -142, 276, -111)
 * ```
 */
export function slide(a, b) {
  let _pipe = a;
  return subtract(
    _pipe,
    (() => {
      let _pipe$1 = a;
      return project(_pipe$1, b);
    })(),
  );
}

/**
 * Returns the reflection of a vector through a plane defined by the given
 * normal vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> reflect(Vec4(2, 3, 4, 5))
 * // -> Vec4(132, 250, -132, 291)
 * ```
 */
export function reflect(vector, normal) {
  let _pipe = vector;
  let _pipe$1 = project(_pipe, normal);
  let _pipe$2 = scale(_pipe$1, 2);
  return subtract(_pipe$2, vector);
}

/**
 * Returns the mirror of a vector through a plane defined by the given normal
 * vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> mirror(Vec4(2, 3, 4, 5))
 * // -> Vec4(-132, -250, 132, -291)
 * ```
 */
export function mirror(vector, normal) {
  let _pipe = vector;
  let _pipe$1 = reflect(_pipe, normal);
  return negate(_pipe$1);
}

/**
 * Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69)
 * |> anchor_position(Vec4(2, 3, 4, 5), scale(_, 2))
 * // -> Vec4(22, -71, 836, 133)
 * ```
 */
export function anchor_position(vector, position, fun) {
  let _pipe = vector;
  let _pipe$1 = subtract(_pipe, position);
  let _pipe$2 = fun(_pipe$1);
  return add(_pipe$2, position);
}

/**
 * Zero vector, a vector with all components set to `0`.
 */
export const zero = /* @__PURE__ */ new Vec4(0, 0, 0, 0);

/**
 * One vector, a vector with all components set to `1`.
 */
export const one = /* @__PURE__ */ new Vec4(1, 1, 1, 1);
