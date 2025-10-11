import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Ok, makeError, divideInt } from "../gleam.mjs";
import * as $vec3 from "../vec/vec3.mjs";
import { Vec3 } from "../vec/vec3.mjs";

const FILEPATH = "src/vec/vec3i.gleam";

/**
 * Returns a new vector with all components clamped between a lower and upper
 * bound.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> clamp(
 *   Vec3(10, 21, -54),
 *   Vec3(14, 18, 323),
 * )
 * // -> Vec3(12, 21, 323)
 * ```
 */
export function clamp(vector, start_bound, stop_bound) {
  return new Vec3(
    $int.clamp(vector.x, start_bound.x, stop_bound.x),
    $int.clamp(vector.y, start_bound.y, stop_bound.y),
    $int.clamp(vector.z, start_bound.z, stop_bound.z),
  );
}

/**
 * Compares two vectors, returning the smaller of the two.
 *
 * ## Examples
 *
 * ```gleam
 * min(Vec3(12, -34, 420), Vec3(10, 21, -54))
 * // -> Vec3(10, -34, -54)
 * ```
 */
export function min(a, b) {
  let _pipe = a;
  return $vec3.map2(_pipe, b, $int.min);
}

/**
 * Compares two vectors, returning the larger of the two.
 *
 * ## Examples
 *
 * ```gleam
 * max(Vec3(12, -34, 420), Vec3(14, -93, 323))
 * // -> Vec3(14, -34, 420)
 * ```
 */
export function max(a, b) {
  let _pipe = a;
  return $vec3.map2(_pipe, b, $int.max);
}

/**
 * Returns a new vector with all elements in absolute values.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> absolute_value()
 * // -> Vec3(12, 34, 420)
 * ```
 */
export function absolute_value(vector) {
  let _pipe = vector;
  return $vec3.map(_pipe, $int.absolute_value);
}

/**
 * Takes an int vector and returns its value as a float vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> to_vec3f()
 * // -> Vec3(12.0, -34.0, 420.0)
 * ```
 */
export function to_vec3f(vector) {
  let _pipe = vector;
  return $vec3.map(_pipe, $int.to_float);
}

/**
 * Returns a new vector with all elements negated.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> negate()
 * // -> Vec3(-12, 34, -420)
 * ```
 */
export function negate(vector) {
  let _pipe = vector;
  return $vec3.map(_pipe, $int.negate);
}

/**
 * Computes the remainder of an integer vector division of inputs as a
 * `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(13, -13, 13) |> remainder(Vec3(3, 3, -3))
 * // -> Ok(Vec3(1, -1, 1))
 * ```
 *
 * ```gleam
 * Vec3(12, -34, 420) |> remainder(Vec3(0, 1, 2))
 * // -> Error(Nil)
 * ```
 */
export function remainder(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec3.map2(_pipe, divisor, $int.remainder);
  return $vec3.result(_pipe$1);
}

/**
 * Returns the modulo of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(13, -13, 13) |> modulo(Vec3(3, 3, -3))
 * // -> Ok(Vec3(1, 2, -2))
 * ```
 */
export function modulo(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec3.map2(_pipe, divisor, $int.modulo);
  return $vec3.result(_pipe$1);
}

/**
 * Returns division of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> divide(Vec3(2, 5, 4))
 * // -> Ok(Vec3(6, -6, 105))
 * ```
 *
 * ```gleam
 * Vec3(12, -34, 420) |> divide(Vec3(0, 5, 4))
 * // -> Error(Nil)
 * ```
 */
export function divide(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec3.map2(_pipe, divisor, $int.divide);
  return $vec3.result(_pipe$1);
}

/**
 * Performs a *floored* integer vector division, which means that the result
 * will always be rounded towards negative infinity.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> floor_divide(Vec3(2, 5, 4))
 * // -> Ok(Vec3(6, -7, 105))
 * ```
 *
 * ```gleam
 * Vec3(12, -34, 420) |> floor_divide(Vec3(0, 5, 4))
 * // -> Error(Nil)
 * ```
 */
export function floor_divide(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec3.map2(_pipe, divisor, $int.floor_divide);
  return $vec3.result(_pipe$1);
}

/**
 * Adds two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> add(Vec3(21, 45, -20))
 * // -> Vec3(33, 11, 400)
 * ```
 */
export function add(a, b) {
  let _pipe = a;
  return $vec3.map2(_pipe, b, $int.add);
}

/**
 * Sums a list of vectors.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec3(12, -34, 420),
 *   Vec3(21, 45, -20),
 *   Vec3(33, 0, -200),
 * ]
 * |> sum()
 * // -> Vec3(66, 11, 200)
 * ```
 */
export function sum(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec3.splat(0), add);
}

/**
 * Multiplies two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> multiply(Vec3(2, -3, 0))
 * // -> Vec3(24, 102, 0)
 * ```
 */
export function multiply(a, b) {
  let _pipe = a;
  return $vec3.map2(_pipe, b, $int.multiply);
}

/**
 * Multiplies a list of vectors and returns the product.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec3(12, -34, 420),
 *   Vec3(21, -10, 999),
 *   Vec3(32, 20, 0),
 * ]
 * |> product()
 * // -> Vec3(8064, 6800, 0)
 * ```
 */
export function product(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec3.splat(1), multiply);
}

/**
 * Subtracts one vector from another.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> subtract(Vec3(7, -45, 20))
 * // -> Vec3(5, 11, 400)
 * ```
 */
export function subtract(a, b) {
  let _pipe = a;
  return $vec3.map2(_pipe, b, $int.subtract);
}

/**
 * Returns the squared length (squared magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> length_squared()
 * // -> 177_700
 * ```
 */
export function length_squared(vector) {
  let _pipe = vector;
  let _pipe$1 = $vec3.to_list(_pipe);
  let _pipe$2 = $list.map(_pipe$1, (element) => { return element * element; });
  return $int.sum(_pipe$2);
}

/**
 * Returns the length (magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> length()
 * // -> 421.54
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
      "vec/vec3i",
      286,
      "length",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 5891,
        end: 5962,
        pattern_start: 5902,
        pattern_end: 5912
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
 * compare_length(Vec3(12, -34, 420), Vec3(2, 3, 4))
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
 * Vec3(12, -34, 420) |> distance_squared(Vec3(2, 3, 4))
 * // -> 174_525
 * ```
 */
export function distance_squared(a, b) {
  let _pipe = a;
  let _pipe$1 = $vec3.map2(_pipe, b, $int.subtract);
  return length_squared(_pipe$1);
}

/**
 * Returns the distance between two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> distance(Vec3(2, 3, 4))
 * // -> 417.76
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
      "vec/vec3i",
      328,
      "distance",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 6872,
        end: 6941,
        pattern_start: 6883,
        pattern_end: 6895
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
 * compare_distance(Vec3(12, -34, 420), Vec3(2, 3, 4), Vec3(-25, 67, 194))
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
 * Vec3(12, -34, 420) |> scale(2)
 * // -> Vec3(24, -68, 840)
 * ```
 */
export function scale(vector, scalar) {
  let _pipe = vector;
  return $vec3.map(
    _pipe,
    (_capture) => { return $int.multiply(_capture, scalar); },
  );
}

/**
 * Returns the cross product of two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> cross(Vec3(2, 3, 4))
 * // -> Vec3(-1396, 792, 104)
 * ```
 */
export function cross(a, b) {
  return new Vec3(
    a.y * b.z - a.z * b.y,
    a.z * b.x - a.x * b.z,
    a.x * b.y - a.y * b.x,
  );
}

/**
 * Returns the dot product of two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> dot(Vec3(2, 3, 4))
 * // -> 1602
 * ```
 */
export function dot(a, b) {
  let _pipe = a;
  let _pipe$1 = multiply(_pipe, b);
  let _pipe$2 = $vec3.to_list(_pipe$1);
  return $int.sum(_pipe$2);
}

/**
 * Returns the projection of a vector on another vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> project(Vec3(2, 3, 4))
 * // -> Vec3(110, 165, 220)
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
 * Vec3(12, -34, 420) |> slide(Vec3(2, 3, 4))
 * // -> Vec3(-98, -199, 200)
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
 * Vec3(12, -34, 420) |> reflect(Vec3(2, 3, 4))
 * // -> Vec3(208, 364, 20)
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
 * Vec3(12, -34, 420) |> mirror(Vec3(2, 3, 4))
 * // -> Vec3(-208, -364, -20)
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
 * Vec3(12, -34, 420)
 * |> anchor_position(Vec3(20, 40, 0), scale(_, 2))
 * // -> Vec3(4, -108, 840)
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
export const zero = /* @__PURE__ */ new Vec3(0, 0, 0);

/**
 * One vector, a vector with all components set to `1`.
 */
export const one = /* @__PURE__ */ new Vec3(1, 1, 1);
