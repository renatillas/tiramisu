import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Ok, makeError, divideFloat } from "../gleam.mjs";
import * as $internal from "../vec/internal.mjs";
import * as $vec2 from "../vec/vec2.mjs";
import { Vec2 } from "../vec/vec2.mjs";

const FILEPATH = "src/vec/vec2f.gleam";

/**
 * Returns a new vector with all components clamped between a lower and upper
 * bound.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> clamp(Vec2(1.0, 2.1), Vec2(1.4, 18.2))
 * // -> Vec2(1.2, 2.1)
 * ```
 */
export function clamp(vector, start_bound, stop_bound) {
  return new Vec2(
    $float.clamp(vector.x, start_bound.x, stop_bound.x),
    $float.clamp(vector.y, start_bound.y, stop_bound.y),
  );
}

/**
 * Checks for equality of two vectors within a tolerance, returning an `Bool`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4)
 * |> loosely_equals(Vec2(1.25, -3.43), tolerating: 0.1)
 * // -> True
 * ```
 */
export function loosely_equals(a, b, tolerance) {
  let $ = (() => {
    let _pipe = a;
    return $vec2.map2(
      _pipe,
      b,
      (a, b) => { return $float.loosely_equals(a, b, tolerance); },
    );
  })();
  let $1 = $.y;
  if ($1) {
    let $2 = $.x;
    if ($2) {
      return true;
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
 * min(Vec2(1.2, -3.4), Vec2(1.0, 2.1))
 * // -> Vec2(1.0, -3.4)
 * ```
 */
export function min(a, b) {
  let _pipe = a;
  return $vec2.map2(_pipe, b, $float.min);
}

/**
 * Compares two vectors, returning the larger of the two.
 *
 * ## Examples
 *
 * ```gleam
 * max(Vec2(1.2, -3.4), Vec2(1.4, -9.3))
 * // -> Vec2(1.4, -3.4)
 * ```
 */
export function max(a, b) {
  let _pipe = a;
  return $vec2.map2(_pipe, b, $float.max);
}

/**
 * Returns a new vector with all elements rounded to the next highest whole
 * number as a `Float`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.6) |> ceiling()
 * // -> Vec2(2.0, -3.0)
 * ```
 */
export function ceiling(vector) {
  let _pipe = vector;
  return $vec2.map(_pipe, $float.ceiling);
}

/**
 * Returns a new vector with all elements rounded to the next lowest whole
 * number as an `Float`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.6) |> floor()
 * // -> Vec2(1.0, -4.0)
 * ```
 */
export function floor(vector) {
  let _pipe = vector;
  return $vec2.map(_pipe, $float.floor);
}

/**
 * Returns a new vector with all elements rounded to the nearest whole number
 * as an `Int`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.6) |> round()
 * // -> Vec2(1, -4)
 * ```
 */
export function round(vector) {
  let _pipe = vector;
  return $vec2.map(_pipe, $float.round);
}

/**
 * Returns a new vector with all elements truncated as an `Int`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2323232827383238, -3.656565) |> truncate()
 * // -> Vec2(1, -3)
 * ```
 */
export function truncate(vector) {
  let _pipe = vector;
  return $vec2.map(_pipe, $float.truncate);
}

/**
 * Returns a new vector with all elements converted to a given precision.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(2.43434348473, -3.656565) |> to_precision(2)
 * // -> Vec2(2.43, -3.66)
 * ```
 *
 * ```gleam
 * Vec2(547_890.453444, -3.656565) |> to_precision(-3)
 * // -> Vec2(548_000.0, 0.0)
 * ```
 */
export function to_precision(vector, precision) {
  let _pipe = vector;
  return $vec2.map(
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
 * Vec2(1.2, -3.4) |> absolute_value()
 * // -> Vec2(1.2, 3.4)
 * ```
 */
export function absolute_value(vector) {
  let _pipe = vector;
  return $vec2.map(_pipe, $float.absolute_value);
}

/**
 * Returns a new vector with all elements negated.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> negate()
 * // -> Vec2(-1.2, 3.4)
 * ```
 */
export function negate(vector) {
  let _pipe = vector;
  return $vec2.map(_pipe, $float.negate);
}

/**
 * Returns the modulo of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(13.3, 13.3) |> modulo(Vec2(3.3, -3.3))
 * // -> Ok(Vec2(0.1, -3.2))
 * ```
 *
 * ```gleam
 * Vec2(-13.3, -13.3) |> modulo(Vec2(3.3, -3.3))
 * // -> Ok(Vec2(3.2, -0.1))
 * ```
 */
export function modulo(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec2.map2(_pipe, divisor, $float.modulo);
  return $vec2.result(_pipe$1);
}

/**
 * Returns division of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> divide(Vec2(2.0, 0.5))
 * // -> Ok(Vec2(0.6, -6.8))
 * ```
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> divide(Vec2(0.0, 0.5))
 * // -> Error(Nil)
 * ```
 */
export function divide(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec2.map2(_pipe, divisor, $float.divide);
  return $vec2.result(_pipe$1);
}

/**
 * Adds two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> add(Vec2(2.1, 4.5))
 * // -> Vec2(3.3, 1.1)
 * ```
 */
export function add(a, b) {
  let _pipe = a;
  return $vec2.map2(_pipe, b, $float.add);
}

/**
 * Sums a list of vectors.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec2(1.2, -3.4),
 *   Vec2(2.1, 4.5),
 *   Vec2(3.3, 0.0),
 * ]
 * |> sum()
 * // -> Vec2(6.6, 1.1)
 * ```
 */
export function sum(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec2.splat(0.0), add);
}

/**
 * Multiplies two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> multiply(Vec2(2.1, -1.0))
 * // -> Vec2(2.52, 3.4)
 * ```
 */
export function multiply(a, b) {
  let _pipe = a;
  return $vec2.map2(_pipe, b, $float.multiply);
}

/**
 * Multiplies a list of vectors and returns the product.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec2(1.2, -3.4),
 *   Vec2(2.1, -1.0),
 *   Vec2(3.2, 2.0),
 * ]
 * |> product()
 * // -> Vec2(8.064, 6.8)
 * ```
 */
export function product(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec2.splat(1.0), multiply);
}

/**
 * Subtracts one vector from another.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> subtract(Vec2(0.7, -4.5))
 * // -> Vec2(0.5, 1.1)
 * ```
 */
export function subtract(a, b) {
  let _pipe = a;
  return $vec2.map2(_pipe, b, $float.subtract);
}

/**
 * Returns the squared length (squared magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> length_squared()
 * // -> 13.0
 * ```
 */
export function length_squared(vector) {
  let _pipe = vector;
  let _pipe$1 = $vec2.to_list(_pipe);
  let _pipe$2 = $list.map(_pipe$1, (element) => { return element * element; });
  return $float.sum(_pipe$2);
}

/**
 * Returns the length (magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> length()
 * // -> 3.61
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
      "vec/vec2f",
      325,
      "length",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 6741,
        end: 6814,
        pattern_start: 6752,
        pattern_end: 6762
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
 * compare_length(Vec2(1.2, -3.4), Vec2(1.0, 2.1))
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
 * loosely_compare_length(Vec2(1.2, -3.4), Vec2(-1.25, 3.43), tolerating: 0.5)
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
 * Vec2(1.2, -3.4) |> distance_squared(Vec2(1.0, 2.1))
 * // -> 30.29
 * ```
 */
export function distance_squared(a, b) {
  let _pipe = a;
  let _pipe$1 = $vec2.map2(_pipe, b, $float.subtract);
  return length_squared(_pipe$1);
}

/**
 * Returns the distance between two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> distance(Vec2(1.0, 2.1))
 * // -> 5.5
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
      "vec/vec2f",
      385,
      "distance",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 8217,
        end: 8288,
        pattern_start: 8228,
        pattern_end: 8240
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
 * compare_distance(Vec2(1.2, -3.4), Vec2(1.0, 2.1), Vec2(-2.5, 6.7))
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
 *   Vec2(1.2, -3.4),
 *   Vec2(1.25, -3.43),
 *   Vec2(-2.5, 6.7),
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
 * Vec2(1.2, -3.4) |> scale(2.5)
 * // -> Vec2(3.0, -8.5)
 * ```
 */
export function scale(vector, scalar) {
  let _pipe = vector;
  return $vec2.map(
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
 * Vec2(1.2, -3.4) |> normalize()
 * // -> Vec2(0.33, -0.94)
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
 * Vec2(1.2, -3.4) |> direction(Vec2(1.0, 2.1))
 * // -> Vec2(-0.04, 1.0)
 * ```
 */
export function direction(a, b) {
  let _pipe = b;
  let _pipe$1 = subtract(_pipe, a);
  return normalize(_pipe$1);
}

/**
 * Returns the cross product of two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> cross(Vec2(1.0, 2.1))
 * // -> 5.92
 * ```
 */
export function cross(a, b) {
  return (a.x * b.y) - (b.x * a.y);
}

/**
 * Returns the dot product of two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> dot(Vec2(1.0, 2.1))
 * // -> -5.94
 * ```
 */
export function dot(a, b) {
  let _pipe = a;
  let _pipe$1 = multiply(_pipe, b);
  let _pipe$2 = $vec2.to_list(_pipe$1);
  return $float.sum(_pipe$2);
}

/**
 * Returns the projection of a vector on another vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> project(Vec2(1.0, 2.1))
 * // -> Vec2(-1.1, -2.31)
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
 * Vec2(1.2, -3.4) |> slide(Vec2(1.0, 2.1))
 * // -> Vec2(2.3, -1.09)
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
 * Vec2(1.2, -3.4) |> reflect(Vec2(1.0, 2.1))
 * // -> Vec2(-3.4, -1.21)
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
 * Vec2(1.2, -3.4) |> mirror(Vec2(1.0, 2.1))
 * // -> Vec2(3.34, 1.21)
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
 * Vec2(1.2, -3.4) |> angle(Vec2(1.0, 2.1))
 * // -> 2.36
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
      "vec/vec2f",
      567,
      "angle",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 12266,
        end: 12369,
        pattern_start: 12277,
        pattern_end: 12286
      }
    )
  }
  return angle$1;
}

/**
 * Rotate a vector by an angle (in radians).
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4) |> rotate(maths.pi() *. 0.25)
 * // -> Vec2(3.25, -1.56)
 * ```
 */
export function rotate(vector, angle) {
  let cos_angle = $internal.cos(angle);
  let sin_angle = $internal.sin(angle);
  return new Vec2(
    (vector.x * cos_angle) - (vector.y * sin_angle),
    (vector.x * sin_angle) + (vector.y * cos_angle),
  );
}

/**
 * Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4)
 * |> anchor_position(
 *   Vec2(1.0, 2.1),
 *   rotate(_, maths.pi() *. 0.25),
 * )
 * // -> Vec2(5.03, -1.65)
 * ```
 */
export function anchor_position(vector, position, fun) {
  let _pipe = vector;
  let _pipe$1 = subtract(_pipe, position);
  let _pipe$2 = fun(_pipe$1);
  return add(_pipe$2, position);
}

/**
 * Return the equivalent of `vector |> rotate(float.negate(angle)) |> fun() |> rotate(angle)`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(1.2, -3.4)
 * |> anchor_rotation(
 *   maths.pi() *. 0.25,
 *   add(_, Vec2(6.0, 9.0)),
 * )
 * // -> Vec2(-0.92, 7.21)
 * ```
 */
export function anchor_rotation(vector, angle, fun) {
  let _pipe = vector;
  let _pipe$1 = rotate(_pipe, $float.negate(angle));
  let _pipe$2 = fun(_pipe$1);
  return rotate(_pipe$2, angle);
}

/**
 * Zero vector, a vector with all components set to `0.0`.
 */
export const zero = /* @__PURE__ */ new Vec2(0.0, 0.0);

/**
 * One vector, a vector with all components set to `1.0`.
 */
export const one = /* @__PURE__ */ new Vec2(1.0, 1.0);
