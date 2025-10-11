import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Ok, makeError, divideFloat } from "../gleam.mjs";
import * as $internal from "../vec/internal.mjs";
import * as $vec4 from "../vec/vec4.mjs";
import { Vec4 } from "../vec/vec4.mjs";

const FILEPATH = "src/vec/vec4f.gleam";

/**
 * Returns a new vector with all components clamped between a lower and upper
 * bound.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> clamp(
 *   Vec4(1.0, 2.1, -5.4, 7.5),
 *   Vec4(1.4, 18.2, 32.3, 9.1),
 * )
 * // -> Vec4(1.2, 2.1, 32.3, 7.5)
 * ```
 */
export function clamp(vector, start_bound, stop_bound) {
  return new Vec4(
    $float.clamp(vector.x, start_bound.x, stop_bound.x),
    $float.clamp(vector.y, start_bound.y, stop_bound.y),
    $float.clamp(vector.z, start_bound.z, stop_bound.z),
    $float.clamp(vector.w, start_bound.w, stop_bound.w),
  );
}

/**
 * Checks for equality of two vectors within a tolerance, returning an `Bool`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69)
 * |> loosely_equals(Vec4(1.25, -3.43, 42.0001, 0.6999), tolerating: 0.1)
 * // -> True
 * ```
 */
export function loosely_equals(a, b, tolerance) {
  let $ = (() => {
    let _pipe = a;
    return $vec4.map2(
      _pipe,
      b,
      (a, b) => { return $float.loosely_equals(a, b, tolerance); },
    );
  })();
  let $1 = $.w;
  if ($1) {
    let $2 = $.z;
    if ($2) {
      let $3 = $.y;
      if ($3) {
        let $4 = $.x;
        if ($4) {
          return true;
        } else {
          return false;
        }
      } else {
        return false;
      }
    } else {
      return false;
    }
  } else {
    return false;
  }
}

/**
 * Compares two vectors, returning the smaller of the two.
 *
 * ## Examples
 *
 * ```gleam
 * min(Vec4(1.2, -3.4, 42.0, 0.69), Vec4(1.0, 2.1, -5.4, 7.5))
 * // -> Vec4(1.0, -3.4, -5.4, 0.69)
 * ```
 */
export function min(a, b) {
  let _pipe = a;
  return $vec4.map2(_pipe, b, $float.min);
}

/**
 * Compares two vectors, returning the larger of the two.
 *
 * ## Examples
 *
 * ```gleam
 * max(Vec4(1.2, -3.4, 42.0, 0.69), Vec4(1.4, -9.3, 32.3, 9.1))
 * // -> Vec4(1.4, -3.4, 42.0, 9.1)
 * ```
 */
export function max(a, b) {
  let _pipe = a;
  return $vec4.map2(_pipe, b, $float.max);
}

/**
 * Returns a new vector with all elements rounded to the next highest whole
 * number as a `Float`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.6, 42.0, 0.5) |> ceiling()
 * // -> Vec4(2.0, -3.0, 42.0, 1.0)
 * ```
 */
export function ceiling(vector) {
  let _pipe = vector;
  return $vec4.map(_pipe, $float.ceiling);
}

/**
 * Returns a new vector with all elements rounded to the next lowest whole
 * number as an `Float`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.6, 42.0, 0.5) |> floor()
 * // -> Vec4(1.0, -4.0, 42.0, 0.0)
 * ```
 */
export function floor(vector) {
  let _pipe = vector;
  return $vec4.map(_pipe, $float.floor);
}

/**
 * Returns a new vector with all elements rounded to the nearest whole number
 * as an `Int`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.6, 42.0, 0.5) |> round()
 * // -> Vec4(1, -4, 42, 1)
 * ```
 */
export function round(vector) {
  let _pipe = vector;
  return $vec4.map(_pipe, $float.round);
}

/**
 * Returns a new vector with all elements truncated as an `Int`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2323232827383238, -3.656565, 42.0, 0.5) |> truncate()
 * // -> Vec4(1, -3, 42, 0)
 * ```
 */
export function truncate(vector) {
  let _pipe = vector;
  return $vec4.map(_pipe, $float.truncate);
}

/**
 * Returns a new vector with all elements converted to a given precision.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(2.43434348473, -3.656565, 42.0, 0.5) |> to_precision(2)
 * // -> Vec4(2.43, -3.66, 42.0, 0.5)
 * ```
 *
 * ```gleam
 * Vec4(547_890.453444, -3.656565, 42.0, 0.5) |> to_precision(-3)
 * // -> Vec4(548_000.0, 0.0, 0.0, 0.0)
 * ```
 */
export function to_precision(vector, precision) {
  let _pipe = vector;
  return $vec4.map(
    _pipe,
    (_capture) => { return $float.to_precision(_capture, precision); },
  );
}

/**
 * Returns a new vector with all elements in absolute values.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> absolute_value()
 * // -> Vec4(1.2, 3.4, 42.0, 0.69)
 * ```
 */
export function absolute_value(vector) {
  let _pipe = vector;
  return $vec4.map(_pipe, $float.absolute_value);
}

/**
 * Returns a new vector with all elements negated.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> negate()
 * // -> Vec4(-1.2, 3.4, -42.0, -0.69)
 * ```
 */
export function negate(vector) {
  let _pipe = vector;
  return $vec4.map(_pipe, $float.negate);
}

/**
 * Returns the modulo of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(13.3, -13.3, 13.3, -13.3) |> modulo(Vec4(3.3, 3.3, -3.3, -3.3))
 * // -> Ok(Vec4(0.1, 3.2, -3.2, -0.1))
 * ```
 */
export function modulo(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec4.map2(_pipe, divisor, $float.modulo);
  return $vec4.result(_pipe$1);
}

/**
 * Returns division of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> divide(Vec4(2.0, 0.5, 4.0, 1.0))
 * // -> Ok(Vec4(0.6, -6.8, 10.5, 0.69))
 * ```
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> divide(Vec4(0.0, 0.5, 4.0, 1.0))
 * // -> Error(Nil)
 * ```
 */
export function divide(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec4.map2(_pipe, divisor, $float.divide);
  return $vec4.result(_pipe$1);
}

/**
 * Adds two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> add(Vec4(2.1, 4.5, -2.0, 9.01))
 * // -> Vec4(3.3, 1.1, 40.0, 9.7)
 * ```
 */
export function add(a, b) {
  let _pipe = a;
  return $vec4.map2(_pipe, b, $float.add);
}

/**
 * Sums a list of vectors.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec4(1.2, -3.4, 42.0, 0.69),
 *   Vec4(2.1, 4.5, -2.0, 9.01),
 *   Vec4(3.3, 0.0, -20.0, 0.3),
 * ]
 * |> sum()
 * // -> Vec4(6.6, 1.1, 20.0, 10.0)
 * ```
 */
export function sum(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec4.splat(0.0), add);
}

/**
 * Multiplies two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> multiply(Vec4(2.1, -1.0, 0.0, 1.0))
 * // -> Vec4(2.52, 3.4, 0.0, 0.69)
 * ```
 */
export function multiply(a, b) {
  let _pipe = a;
  return $vec4.map2(_pipe, b, $float.multiply);
}

/**
 * Multiplies a list of vectors and returns the product.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec4(1.2, -3.4, 42.0, 0.69),
 *   Vec4(2.1, -1.0, 999.9, 2.0),
 *   Vec4(3.2, 2.0, 0.0, 0.5),
 * ]
 * |> product()
 * // -> Vec4(8.064, 6.8, 0.0, 0.69)
 * ```
 */
export function product(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec4.splat(1.0), multiply);
}

/**
 * Subtracts one vector from another.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> subtract(Vec4(0.7, -4.5, 2.0, 1.39))
 * // -> Vec4(0.5, 1.1, 40.0, -0.7)
 * ```
 */
export function subtract(a, b) {
  let _pipe = a;
  return $vec4.map2(_pipe, b, $float.subtract);
}

/**
 * Returns the squared length (squared magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> length_squared()
 * // -> 1777.47
 * ```
 */
export function length_squared(vector) {
  let _pipe = vector;
  let _pipe$1 = $vec4.to_list(_pipe);
  let _pipe$2 = $list.map(_pipe$1, (element) => { return element * element; });
  return $float.sum(_pipe$2);
}

/**
 * Returns the length (magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> length()
 * // -> 42.16
 * ```
 */
export function length(vector) {
  let _block;
  let _pipe = vector;
  let _pipe$1 = length_squared(_pipe);
  _block = $float.square_root(_pipe$1);
  let $ = _block;
  let length$1;
  if ($ instanceof Ok) {
    length$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "vec/vec4f",
      325,
      "length",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 7431,
        end: 7504,
        pattern_start: 7442,
        pattern_end: 7452
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
 * compare_length(
 *   Vec4(1.2, -3.4, 42.0, 0.69),
 *   Vec4(1.0, 2.1, 3.2, 4.3),
 * )
 * // -> Gt
 * ```
 */
export function compare_length(a, b) {
  return $float.compare(
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
 * Compares two vector's lengths within a tolerance, returning an `Order`:
 * `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
 *
 * ## Examples
 *
 * ```gleam
 * loosely_compare_length(
 *   Vec4(1.2, -3.4, 42.0, 0.69),
 *   Vec4(-1.25, 3.43, -42.0001, -0.6999),
 *   tolerating: 0.5,
 * )
 * // -> Eq
 * ```
 */
export function loosely_compare_length(a, b, tolerance) {
  return $float.loosely_compare(
    (() => {
      let _pipe = a;
      return length_squared(_pipe);
    })(),
    (() => {
      let _pipe = b;
      return length_squared(_pipe);
    })(),
    tolerance,
  );
}

/**
 * Returns the squared distance between two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> distance_squared(Vec4(1.0, 2.1, 3.2, 4.3))
 * // -> 1548.76
 * ```
 */
export function distance_squared(a, b) {
  let _pipe = a;
  let _pipe$1 = $vec4.map2(_pipe, b, $float.subtract);
  return length_squared(_pipe$1);
}

/**
 * Returns the distance between two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> distance(Vec4(1.0, 2.1, 3.2, 4.3))
 * // -> 39.35
 * ```
 */
export function distance(a, b) {
  let _block;
  let _pipe = distance_squared(a, b);
  _block = $float.square_root(_pipe);
  let $ = _block;
  let distance$1;
  if ($ instanceof Ok) {
    distance$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "vec/vec4f",
      392,
      "distance",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 9047,
        end: 9118,
        pattern_start: 9058,
        pattern_end: 9070
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
 *   Vec4(1.2, -3.4, 42.0, 0.69),
 *   Vec4(1.0, 2.1, 3.2, 4.3),
 *   Vec4(-2.5, 6.7, 19.4, 0.0),
 * )
 * // -> Gt
 * ```
 */
export function compare_distance(a, b, vector) {
  return $float.compare(
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
 * Compares two vector's distances to a vector within a tolerance, returning
 * an `Order`:
 * `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
 *
 * ## Examples
 *
 * ```gleam
 * loosely_compare_distance(
 *   Vec4(1.2, -3.4, 42.0, 0.69),
 *   Vec4(1.25, -3.43, 42.0001, 0.6999),
 *   Vec4(-2.5, 6.7, 19.4, 0.0),
 *   tolerating: 1.0,
 * )
 * // -> Eq
 * ```
 */
export function loosely_compare_distance(a, b, vector, tolerance) {
  return $float.loosely_compare(
    (() => {
      let _pipe = a;
      return distance_squared(_pipe, vector);
    })(),
    (() => {
      let _pipe = b;
      return distance_squared(_pipe, vector);
    })(),
    tolerance,
  );
}

/**
 * Returns a new vector containing the elements multiplies by `scalar`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> scale(2.5)
 * // -> Vec4(3.0, -8.5, 105.0, 1.72)
 * ```
 */
export function scale(vector, scalar) {
  let _pipe = vector;
  return $vec4.map(
    _pipe,
    (_capture) => { return $float.multiply(_capture, scalar); },
  );
}

/**
 * Normalize the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> normalize()
 * // -> Vec4(0.03, -0.08, 1.0, 0.02)
 * ```
 */
export function normalize(vector) {
  let _pipe = vector;
  return scale(
    _pipe,
    divideFloat(
      1.0,
      (() => {
        let _pipe$1 = vector;
        return length(_pipe$1);
      })()
    ),
  );
}

/**
 * Returns a normalized vector pointing from `a` to `b`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> direction(Vec4(1.0, 2.1, 3.2, 4.3))
 * // -> Vec4(-0.0, 0.14, -0.99, 0.092)
 * ```
 */
export function direction(a, b) {
  let _pipe = b;
  let _pipe$1 = subtract(_pipe, a);
  return normalize(_pipe$1);
}

/**
 * Returns the dot product of two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> dot(Vec4(1.0, 2.1, 3.2, 4.3))
 * // -> 131.43
 * ```
 */
export function dot(a, b) {
  let _pipe = a;
  let _pipe$1 = multiply(_pipe, b);
  let _pipe$2 = $vec4.to_list(_pipe$1);
  return $float.sum(_pipe$2);
}

/**
 * Returns the projection of a vector on another vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> project(Vec4(1.0, 2.1, 3.2, 4.3))
 * // -> Vec4(3.85, 8.08, 12.32, 16.55)
 * ```
 */
export function project(a, b) {
  let _pipe = b;
  return scale(_pipe, divideFloat(dot(a, b), dot(b, b)));
}

/**
 * Returns a new vector resulting from sliding this vector along a plane
 * defined by the given normal vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> slide(Vec4(1.0, 2.1, 3.2, 4.3))
 * // -> Vec4(-2.65, -11.48, 29.68, -15.86)
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
 * Vec4(1.2, -3.4, 42.0, 0.69) |> reflect(Vec4(1.0, 2.1, 3.2, 4.3))
 * // -> Vec4(6.5, 19.57, -17.36, 32.42)
 * ```
 */
export function reflect(vector, normal) {
  let _pipe = vector;
  let _pipe$1 = project(_pipe, normal);
  let _pipe$2 = scale(_pipe$1, 2.0);
  return subtract(_pipe$2, vector);
}

/**
 * Returns the mirror of a vector through a plane defined by the given normal
 * vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> mirror(Vec4(1.0, 2.1, 3.2, 4.3))
 * // -> Vec4(-6.5, -19.57, 17.36, -32.42)
 * ```
 */
export function mirror(vector, normal) {
  let _pipe = vector;
  let _pipe$1 = reflect(_pipe, normal);
  return negate(_pipe$1);
}

/**
 * Returns the angle (in radians) between two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69) |> angle(Vec4(1.0, 2.1, 3.2, 4.3))
 * // -> 1.0
 * ```
 */
export function angle(a, b) {
  let _block;
  let _pipe = dot(normalize(a), normalize(b));
  let _pipe$1 = $float.clamp(_pipe, -1.0, 1.0);
  _block = $internal.acos(_pipe$1);
  let $ = _block;
  let angle$1;
  if ($ instanceof Ok) {
    angle$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "vec/vec4f",
      565,
      "angle",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 13231,
        end: 13334,
        pattern_start: 13242,
        pattern_end: 13251
      }
    )
  }
  return angle$1;
}

/**
 * Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(1.2, -3.4, 42.0, 0.69)
 * |> anchor_position(Vec4(1.0, 2.1, 3.2, 4.3), scale(_, 2.0))
 * // -> Vec4(1.4, -8.9, 80.8, -2.92)
 * ```
 */
export function anchor_position(vector, position, fun) {
  let _pipe = vector;
  let _pipe$1 = subtract(_pipe, position);
  let _pipe$2 = fun(_pipe$1);
  return add(_pipe$2, position);
}

/**
 * Zero vector, a vector with all components set to `0.0`.
 */
export const zero = /* @__PURE__ */ new Vec4(0.0, 0.0, 0.0, 0.0);

/**
 * One vector, a vector with all components set to `1.0`.
 */
export const one = /* @__PURE__ */ new Vec4(1.0, 1.0, 1.0, 1.0);
