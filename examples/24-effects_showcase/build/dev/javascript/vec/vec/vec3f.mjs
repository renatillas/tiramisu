import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Ok, makeError, divideFloat } from "../gleam.mjs";
import * as $internal from "../vec/internal.mjs";
import * as $vec3 from "../vec/vec3.mjs";
import { Vec3 } from "../vec/vec3.mjs";

const FILEPATH = "src/vec/vec3f.gleam";

/**
 * Returns a new vector with all components clamped between a lower and upper
 * bound.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> clamp(
 *   Vec3(1.0, 2.1, -5.4),
 *   Vec3(1.4, 18.2, 32.3),
 * )
 * // -> Vec3(1.2, 2.1, 32.3)
 * ```
 */
export function clamp(vector, start_bound, stop_bound) {
  return new Vec3(
    $float.clamp(vector.x, start_bound.x, stop_bound.x),
    $float.clamp(vector.y, start_bound.y, stop_bound.y),
    $float.clamp(vector.z, start_bound.z, stop_bound.z),
  );
}

/**
 * Checks for equality of two vectors within a tolerance, returning an `Bool`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0)
 * |> loosely_equals(Vec3(1.25, -3.43, 42.0001), tolerating: 0.1)
 * // -> True
 * ```
 */
export function loosely_equals(a, b, tolerance) {
  let $ = (() => {
    let _pipe = a;
    return $vec3.map2(
      _pipe,
      b,
      (a, b) => { return $float.loosely_equals(a, b, tolerance); },
    );
  })();
  let $1 = $.z;
  if ($1) {
    let $2 = $.y;
    if ($2) {
      let $3 = $.x;
      if ($3) {
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
}

/**
 * Compares two vectors, returning the smaller of the two.
 *
 * ## Examples
 *
 * ```gleam
 * min(Vec3(1.2, -3.4, 42.0), Vec3(1.0, 2.1, -5.4))
 * // -> Vec3(1.0, -3.4, -5.4)
 * ```
 */
export function min(a, b) {
  let _pipe = a;
  return $vec3.map2(_pipe, b, $float.min);
}

/**
 * Compares two vectors, returning the larger of the two.
 *
 * ## Examples
 *
 * ```gleam
 * max(Vec3(1.2, -3.4, 42.0), Vec3(1.4, -9.3, 32.3))
 * // -> Vec3(1.4, -3.4, 42.0)
 * ```
 */
export function max(a, b) {
  let _pipe = a;
  return $vec3.map2(_pipe, b, $float.max);
}

/**
 * Returns a new vector with all elements rounded to the next highest whole
 * number as a `Float`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.6, 42.0) |> ceiling()
 * // -> Vec3(2.0, -3.0, 42.0)
 * ```
 */
export function ceiling(vector) {
  let _pipe = vector;
  return $vec3.map(_pipe, $float.ceiling);
}

/**
 * Returns a new vector with all elements rounded to the next lowest whole
 * number as an `Float`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.6, 42.0) |> floor()
 * // -> Vec3(1.0, -4.0, 42.0)
 * ```
 */
export function floor(vector) {
  let _pipe = vector;
  return $vec3.map(_pipe, $float.floor);
}

/**
 * Returns a new vector with all elements rounded to the nearest whole number
 * as an `Int`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.6, 42.0) |> round()
 * // -> Vec3(1, -4, 42)
 * ```
 */
export function round(vector) {
  let _pipe = vector;
  return $vec3.map(_pipe, $float.round);
}

/**
 * Returns a new vector with all elements truncated as an `Int`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2323232827383238, -3.656565, 42.0) |> truncate()
 * // -> Vec3(1, -3, 42)
 * ```
 */
export function truncate(vector) {
  let _pipe = vector;
  return $vec3.map(_pipe, $float.truncate);
}

/**
 * Returns a new vector with all elements converted to a given precision.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(2.43434348473, -3.656565, 42.0) |> to_precision(2)
 * // -> Vec3(2.43, -3.66, 42.0)
 * ```
 *
 * ```gleam
 * Vec3(547_890.453444, -3.656565, 42.0) |> to_precision(-3)
 * // -> Vec3(548_000.0, 0.0, 0.0)
 * ```
 */
export function to_precision(vector, precision) {
  let _pipe = vector;
  return $vec3.map(
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
 * Vec3(1.2, -3.4, 42.0) |> absolute_value()
 * // -> Vec3(1.2, 3.4, 42.0)
 * ```
 */
export function absolute_value(vector) {
  let _pipe = vector;
  return $vec3.map(_pipe, $float.absolute_value);
}

/**
 * Returns a new vector with all elements negated.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> negate()
 * // -> Vec3(-1.2, 3.4, -42.0)
 * ```
 */
export function negate(vector) {
  let _pipe = vector;
  return $vec3.map(_pipe, $float.negate);
}

/**
 * Returns the modulo of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(13.3, -13.3, 13.3) |> modulo(Vec3(3.3, 3.3, -3.3))
 * // -> Ok(Vec3(0.1, 3.2, -3.2))
 * ```
 */
export function modulo(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec3.map2(_pipe, divisor, $float.modulo);
  return $vec3.result(_pipe$1);
}

/**
 * Returns division of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> divide(Vec3(2.0, 0.5, 4.0))
 * // -> Ok(Vec3(0.6, -6.8, 10.5))
 * ```
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> divide(Vec3(0.0, 0.5, 4.0))
 * // -> Error(Nil)
 * ```
 */
export function divide(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec3.map2(_pipe, divisor, $float.divide);
  return $vec3.result(_pipe$1);
}

/**
 * Adds two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> add(Vec3(2.1, 4.5, -2.0))
 * // -> Vec3(3.3, 1.1, 40.0)
 * ```
 */
export function add(a, b) {
  let _pipe = a;
  return $vec3.map2(_pipe, b, $float.add);
}

/**
 * Sums a list of vectors.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec3(1.2, -3.4, 42.0),
 *   Vec3(2.1, 4.5, -2.0),
 *   Vec3(3.3, 0.0, -20.0),
 * ]
 * |> sum()
 * // -> Vec3(6.6, 1.1, 20.0)
 * ```
 */
export function sum(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec3.splat(0.0), add);
}

/**
 * Multiplies two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> multiply(Vec3(2.1, -1.0, 0.0))
 * // -> Vec3(2.52, 3.4, 0.0)
 * ```
 */
export function multiply(a, b) {
  let _pipe = a;
  return $vec3.map2(_pipe, b, $float.multiply);
}

/**
 * Multiplies a list of vectors and returns the product.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec3(1.2, -3.4, 42.0),
 *   Vec3(2.1, -1.0, 999.9),
 *   Vec3(3.2, 2.0, 0.0),
 * ]
 * |> product()
 * // -> Vec3(8.064, 6.8, 0.0)
 * ```
 */
export function product(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec3.splat(1.0), multiply);
}

/**
 * Subtracts one vector from another.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> subtract(Vec3(0.7, -4.5, 2.0))
 * // -> Vec3(0.5, 1.1, 40.0)
 * ```
 */
export function subtract(a, b) {
  let _pipe = a;
  return $vec3.map2(_pipe, b, $float.subtract);
}

/**
 * Returns the squared length (squared magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> length_squared()
 * // -> 1777.0
 * ```
 */
export function length_squared(vector) {
  let _pipe = vector;
  let _pipe$1 = $vec3.to_list(_pipe);
  let _pipe$2 = $list.map(_pipe$1, (element) => { return element * element; });
  return $float.sum(_pipe$2);
}

/**
 * Returns the length (magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> length()
 * // -> 42.15
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
      "vec/vec3f",
      324,
      "length",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 7053,
        end: 7126,
        pattern_start: 7064,
        pattern_end: 7074
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
 *   Vec3(1.2, -3.4, 42.0),
 *   Vec3(1.0, 2.1, 3.2),
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
 *   Vec3(1.2, -3.4, 42.0),
 *   Vec3(-1.25, 3.43, -42.0001),
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
 * Vec3(1.2, -3.4, 42.0) |> distance_squared(Vec3(1.0, 2.1, 3.2))
 * // -> 1535.73
 * ```
 */
export function distance_squared(a, b) {
  let _pipe = a;
  let _pipe$1 = $vec3.map2(_pipe, b, $float.subtract);
  return length_squared(_pipe$1);
}

/**
 * Returns the distance between two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> distance(Vec3(1.0, 2.1, 3.2))
 * // -> 39.19
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
      "vec/vec3f",
      391,
      "distance",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 8626,
        end: 8697,
        pattern_start: 8637,
        pattern_end: 8649
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
 *   Vec3(1.2, -3.4, 42.0),
 *   Vec3(1.0, 2.1, 3.2),
 *   Vec3(-2.5, 6.7, 19.4),
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
 *   Vec3(1.2, -3.4, 42.0),
 *   Vec3(1.25, -3.43, 42.0001),
 *   Vec3(-2.5, 6.7, 19.4),
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
 * Vec3(1.2, -3.4, 42.0) |> scale(2.5)
 * // -> Vec3(3.0, -8.5, 105.0)
 * ```
 */
export function scale(vector, scalar) {
  let _pipe = vector;
  return $vec3.map(
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
 * Vec3(1.2, -3.4, 42.0) |> normalize()
 * // -> Vec3(0.03, -0.08, 1.0)
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
 * Vec3(1.2, -3.4, 42.0) |> direction(Vec3(1.0, 2.1, 3.2))
 * // -> Vec3(-0.01, 0.14, -1.0)
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
 * Vec3(1.2, -3.4, 42.0) |> dot(Vec3(1.0, 2.1, 3.2))
 * // -> Vec3(-99.08, 38.16, 5.92)
 * ```
 */
export function cross(a, b) {
  return new Vec3(
    (a.y * b.z) - (a.z * b.y),
    (a.z * b.x) - (a.x * b.z),
    (a.x * b.y) - (a.y * b.x),
  );
}

/**
 * Returns the dot product of two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> dot(Vec3(1.0, 2.1, 3.2))
 * // -> 128.46
 * ```
 */
export function dot(a, b) {
  let _pipe = a;
  let _pipe$1 = multiply(_pipe, b);
  let _pipe$2 = $vec3.to_list(_pipe$1);
  return $float.sum(_pipe$2);
}

/**
 * Returns the projection of a vector on another vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0) |> project(Vec3(1.0, 2.1, 3.2))
 * // -> Vec3(8.21, 17.24, 26.27)
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
 * Vec3(1.2, -3.4, 42.0) |> slide(Vec3(1.0, 2.1, 3.2))
 * // -> Vec3(-7.01, -20.64, 15.73)
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
 * Vec3(1.2, -3.4, 42.0) |> reflect(Vec3(1.0, 2.1, 3.2))
 * // -> Vec3(15.22, 37.87, 10.53)
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
 * Vec3(1.2, -3.4, 42.0) |> mirror(Vec3(1.0, 2.1, 3.2))
 * // -> Vec3(-15.22, -37.87, -10.53)
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
 * Vec3(1.2, -3.4, 42.0) |> angle(Vec3(1.0, 2.1, 3.2))
 * // -> 0.69
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
      "vec/vec3f",
      581,
      "angle",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 12995,
        end: 13098,
        pattern_start: 13006,
        pattern_end: 13015
      }
    )
  }
  return angle$1;
}

/**
 * Rotate a vector around a given axis by an angle (in radians).
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0)
 * |> rotate(around: Vec3(1.0, 2.1, 3.2), by: maths.pi() *. 0.25)
 * // -> Vec3(20.96, -4.18, 36.33)
 * ```
 */
export function rotate(vector, axis, angle) {
  let _block;
  let _pipe = axis;
  _block = normalize(_pipe);
  let axis$1 = _block;
  let cos_angle = $internal.cos(angle);
  let sin_angle = $internal.sin(angle);
  return new Vec3(
    ((vector.x * (cos_angle + ((axis$1.x * axis$1.x) * (1.0 - cos_angle)))) + (vector.y * (((axis$1.x * axis$1.y) * (1.0 - cos_angle)) - (axis$1.z * sin_angle)))) + (vector.z * (((axis$1.x * axis$1.z) * (1.0 - cos_angle)) + (axis$1.y * sin_angle))),
    ((vector.x * (((axis$1.y * axis$1.x) * (1.0 - cos_angle)) + (axis$1.z * sin_angle))) + (vector.y * (cos_angle + ((axis$1.y * axis$1.y) * (1.0 - cos_angle))))) + (vector.z * (((axis$1.y * axis$1.z) * (1.0 - cos_angle)) - (axis$1.x * sin_angle))),
    ((vector.x * (((axis$1.z * axis$1.x) * (1.0 - cos_angle)) - (axis$1.y * sin_angle))) + (vector.y * (((axis$1.z * axis$1.y) * (1.0 - cos_angle)) + (axis$1.x * sin_angle)))) + (vector.z * (cos_angle + ((axis$1.z * axis$1.z) * (1.0 - cos_angle)))),
  );
}

/**
 * Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0)
 * |> anchor_position(
 *   Vec3(2.0, 4.0, 0.0),
 *   rotate(_, Vec3(6.0, 9.0, 1.0), maths.pi() *. 0.25),
 * )
 * // -> Vec3(26.08, -18.35, 27.2)
 * ```
 */
export function anchor_position(vector, position, fun) {
  let _pipe = vector;
  let _pipe$1 = subtract(_pipe, position);
  let _pipe$2 = fun(_pipe$1);
  return add(_pipe$2, position);
}

/**
 * Return the equivalent of `vector |> rotate(axis, float.negate(angle)) |> fun() |> rotate(axis, angle)`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(1.2, -3.4, 42.0)
 * |> anchor_rotation(
 *   Vec3(6.0, 9.0, 1.0),
 *   maths.pi() *. 0.25,
 *   add(_, Vec3(2.0, 4.0, 0.0)),
 * )
 * // -> Vec3(3.07, 0.63, 42.51)
 * ```
 */
export function anchor_rotation(vector, axis, angle, fun) {
  let _pipe = vector;
  let _pipe$1 = rotate(_pipe, axis, $float.negate(angle));
  let _pipe$2 = fun(_pipe$1);
  return rotate(_pipe$2, axis, angle);
}

/**
 * Zero vector, a vector with all components set to `0.0`.
 */
export const zero = /* @__PURE__ */ new Vec3(0.0, 0.0, 0.0);

/**
 * One vector, a vector with all components set to `1.0`.
 */
export const one = /* @__PURE__ */ new Vec3(1.0, 1.0, 1.0);
