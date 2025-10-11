import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Ok, makeError, divideInt } from "../gleam.mjs";
import * as $vec2 from "../vec/vec2.mjs";
import { Vec2 } from "../vec/vec2.mjs";

const FILEPATH = "src/vec/vec2i.gleam";

/**
 * Returns a new vector with all components clamped between a lower and upper
 * bound.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> clamp(Vec2(10, 21), Vec2(14, 18))
 * // -> Vec2(12, 21)
 * ```
 */
export function clamp(vector, start_bound, stop_bound) {
  return new Vec2(
    $int.clamp(vector.x, start_bound.x, stop_bound.x),
    $int.clamp(vector.y, start_bound.y, stop_bound.y),
  );
}

/**
 * Compares two vectors, returning the smaller of the two.
 *
 * ## Examples
 *
 * ```gleam
 * min(Vec2(12, -34), Vec2(10, 21))
 * // -> Vec2(10, -34)
 * ```
 */
export function min(a, b) {
  let _pipe = a;
  return $vec2.map2(_pipe, b, $int.min);
}

/**
 * Compares two vectors, returning the larger of the two.
 *
 * ## Examples
 *
 * ```gleam
 * max(Vec2(12, -34), Vec2(14, -93))
 * // -> Vec2(14, -34)
 * ```
 */
export function max(a, b) {
  let _pipe = a;
  return $vec2.map2(_pipe, b, $int.max);
}

/**
 * Returns a new vector with all elements in absolute values.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> absolute_value()
 * // -> Vec2(12, 34)
 * ```
 */
export function absolute_value(vector) {
  let _pipe = vector;
  return $vec2.map(_pipe, $int.absolute_value);
}

/**
 * Takes an int vector and returns its value as a float vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> to_vec2f()
 * // -> Vec2(12.0, -34.0)
 * ```
 */
export function to_vec2f(vector) {
  let _pipe = vector;
  return $vec2.map(_pipe, $int.to_float);
}

/**
 * Returns a new vector with all elements negated.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> negate()
 * // -> Vec2(-12, 34)
 * ```
 */
export function negate(vector) {
  let _pipe = vector;
  return $vec2.map(_pipe, $int.negate);
}

/**
 * Computes the remainder of an integer vector division of inputs as a
 * `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(13, -13) |> remainder(Vec2(3, 3))
 * // -> Ok(Vec2(1, -1))
 * ```
 *
 * ```gleam
 * Vec2(12, -34) |> remainder(Vec2(0, 1))
 * // -> Error(Nil)
 * ```
 */
export function remainder(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec2.map2(_pipe, divisor, $int.remainder);
  return $vec2.result(_pipe$1);
}

/**
 * Returns the modulo of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(13, 13) |> modulo(Vec2(3, -3))
 * // -> Ok(Vec2(1, -2))
 * ```
 *
 * ```gleam
 * Vec2(-13, -13) |> modulo(Vec2(3, -3))
 * // -> Ok(Vec2(2, -1))
 * ```
 */
export function modulo(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec2.map2(_pipe, divisor, $int.modulo);
  return $vec2.result(_pipe$1);
}

/**
 * Returns division of the inputs as a `Result`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> divide(Vec2(2, 5))
 * // -> Ok(Vec2(6, -6))
 * ```
 *
 * ```gleam
 * Vec2(12, -34) |> divide(Vec2(0, 5))
 * // -> Error(Nil)
 * ```
 */
export function divide(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec2.map2(_pipe, divisor, $int.divide);
  return $vec2.result(_pipe$1);
}

/**
 * Performs a *floored* integer vector division, which means that the result
 * will always be rounded towards negative infinity.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> floor_divide(Vec2(2, 5))
 * // -> Ok(Vec2(6, -7))
 * ```
 *
 * ```gleam
 * Vec2(12, -34) |> floor_divide(Vec2(0, 5))
 * // -> Error(Nil)
 * ```
 */
export function floor_divide(dividend, divisor) {
  let _pipe = dividend;
  let _pipe$1 = $vec2.map2(_pipe, divisor, $int.floor_divide);
  return $vec2.result(_pipe$1);
}

/**
 * Adds two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> add(Vec2(21, 45))
 * // -> Vec2(33, 11)
 * ```
 */
export function add(a, b) {
  let _pipe = a;
  return $vec2.map2(_pipe, b, $int.add);
}

/**
 * Sums a list of vectors.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec2(12, -34),
 *   Vec2(21, 45),
 *   Vec2(33, 0),
 * ]
 * |> sum()
 * // -> Vec2(66, 11)
 * ```
 */
export function sum(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec2.splat(0), add);
}

/**
 * Multiplies two vectors together.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> multiply(Vec2(2, -3))
 * // -> Vec2(24, 102)
 * ```
 */
export function multiply(a, b) {
  let _pipe = a;
  return $vec2.map2(_pipe, b, $int.multiply);
}

/**
 * Multiplies a list of vectors and returns the product.
 *
 * ## Examples
 *
 * ```gleam
 * [
 *   Vec2(12, -34),
 *   Vec2(21, -10),
 *   Vec2(32, 20),
 * ]
 * |> product()
 * // -> Vec2(8064, 6800)
 * ```
 */
export function product(vectors) {
  let _pipe = vectors;
  return $list.fold(_pipe, $vec2.splat(1), multiply);
}

/**
 * Subtracts one vector from another.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> subtract(Vec2(7, -45))
 * // -> Vec2(5, 11)
 * ```
 */
export function subtract(a, b) {
  let _pipe = a;
  return $vec2.map2(_pipe, b, $int.subtract);
}

/**
 * Returns the squared length (squared magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> length_squared()
 * // -> 1300
 * ```
 */
export function length_squared(vector) {
  let _pipe = vector;
  let _pipe$1 = $vec2.to_list(_pipe);
  let _pipe$2 = $list.map(_pipe$1, (element) => { return element * element; });
  return $int.sum(_pipe$2);
}

/**
 * Returns the length (magnitude) of the vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> length()
 * // -> 36.06
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
      "vec/vec2i",
      287,
      "length",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 5659,
        end: 5730,
        pattern_start: 5670,
        pattern_end: 5680
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
 * compare_length(Vec2(12, -34), Vec2(2, 3))
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
 * Vec2(12, -34) |> distance_squared(Vec2(2, 3))
 * // -> 1469
 * ```
 */
export function distance_squared(a, b) {
  let _pipe = a;
  let _pipe$1 = $vec2.map2(_pipe, b, $int.subtract);
  return length_squared(_pipe$1);
}

/**
 * Returns the distance between two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> distance(Vec2(2, 3))
 * // -> 38.33
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
      "vec/vec2i",
      329,
      "distance",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 6612,
        end: 6681,
        pattern_start: 6623,
        pattern_end: 6635
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
 * compare_distance(Vec2(12, -34), Vec2(2, 3), Vec2(-25, 67))
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
 * Vec2(12, -34) |> scale(2)
 * // -> Vec2(24, -68)
 * ```
 */
export function scale(vector, scalar) {
  let _pipe = vector;
  return $vec2.map(
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
 * Vec2(12, -34) |> cross(Vec2(2, 3))
 * // -> 104
 * ```
 */
export function cross(a, b) {
  return a.x * b.y - b.x * a.y;
}

/**
 * Returns the dot product of two vectors.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> dot(Vec2(2, 3))
 * // -> -78
 * ```
 */
export function dot(a, b) {
  let _pipe = a;
  let _pipe$1 = multiply(_pipe, b);
  let _pipe$2 = $vec2.to_list(_pipe$1);
  return $int.sum(_pipe$2);
}

/**
 * Returns the projection of a vector on another vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> project(Vec2(2, 3))
 * // -> Vec2(-12, -18)
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
 * Vec2(12, -34) |> slide(Vec2(2, 3))
 * // -> -16
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
 * Vec2(12, -34) |> reflect(Vec2(2, 3))
 * // -> Vec2(-36, -2)
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
 * Vec2(12, -34) |> mirror(Vec2(2, 3))
 * // -> Vec2(36, 2)
 * ```
 */
export function mirror(vector, normal) {
  let _pipe = vector;
  let _pipe$1 = reflect(_pipe, normal);
  return negate(_pipe$1);
}

/**
 * Rotate a vector by an angle (in 90 degree steps).
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> rotate(1)
 * // -> Vec2(34, 12)
 * ```
 */
export function rotate(vector, angle) {
  let $ = (() => {
    let _pipe = angle;
    return $int.modulo(_pipe, 4);
  })();
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 === 0) {
      return vector;
    } else if ($1 === 1) {
      return new Vec2(- vector.y, vector.x);
    } else if ($1 === 2) {
      return new Vec2(- vector.x, - vector.y);
    } else if ($1 === 3) {
      return new Vec2(vector.y, - vector.x);
    } else {
      throw makeError(
        "panic",
        FILEPATH,
        "vec/vec2i",
        461,
        "rotate",
        "`panic` expression evaluated.",
        {}
      )
    }
  } else {
    throw makeError(
      "panic",
      FILEPATH,
      "vec/vec2i",
      461,
      "rotate",
      "`panic` expression evaluated.",
      {}
    )
  }
}

/**
 * Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34)
 * |> anchor_position(Vec2(10, 21), rotate(_, 1))
 * // -> Vec2(65, 23)
 * ```
 */
export function anchor_position(vector, position, fun) {
  let _pipe = vector;
  let _pipe$1 = subtract(_pipe, position);
  let _pipe$2 = fun(_pipe$1);
  return add(_pipe$2, position);
}

/**
 * Return the equivalent of `vector |> rotate(-angle) |> fun() |> rotate(angle)`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34)
 * |> anchor_rotation(1, add(_, Vec2(6, 9)))
 * // -> Vec2(3, -28)
 * ```
 */
export function anchor_rotation(vector, angle, fun) {
  let _pipe = vector;
  let _pipe$1 = rotate(_pipe, - angle);
  let _pipe$2 = fun(_pipe$1);
  return rotate(_pipe$2, angle);
}

/**
 * Zero vector, a vector with all components set to `0`.
 */
export const zero = /* @__PURE__ */ new Vec2(0, 0);

/**
 * One vector, a vector with all components set to `1`.
 */
export const one = /* @__PURE__ */ new Vec2(1, 1);
