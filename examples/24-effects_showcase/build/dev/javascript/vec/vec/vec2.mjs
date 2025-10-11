import { Ok, Error, toList, CustomType as $CustomType } from "../gleam.mjs";

export class Vec2 extends $CustomType {
  constructor(x, y) {
    super();
    this.x = x;
    this.y = y;
  }
}

/**
 * Creates a vector with all elements set to a value.
 *
 * ## Examples
 *
 * ```gleam
 * splat(12)
 * // -> Vec2(12, 12)
 * ```
 */
export function splat(value) {
  return new Vec2(value, value);
}

/**
 * Converts a tuple of the contained elements into a vector.
 *
 * ## Examples
 *
 * ```gleam
 * #(12, -34) |> from_tuple()
 * // -> Vec2(12, -34)
 * ```
 */
export function from_tuple(tuple) {
  return new Vec2(tuple[0], tuple[1]);
}

/**
 * Converts the vector into a tuple of the contained elements.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> to_tuple()
 * // -> #(12, -34)
 * ```
 */
export function to_tuple(vector) {
  return [vector.x, vector.y];
}

/**
 * Converts the vector into a list of the contained elements.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> to_list()
 * // -> [12, -34]
 * ```
 */
export function to_list(vector) {
  return toList([vector.x, vector.y]);
}

/**
 * Returns the x element in a vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> x()
 * // -> 12
 * ```
 */
export function x(vector) {
  return vector.x;
}

/**
 * Returns the y element in a vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> y()
 * // -> -34
 * ```
 */
export function y(vector) {
  return vector.y;
}

/**
 * Returns a new vector with the x element replace with `value`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> replace_x(777)
 * // -> Vec2(777, -34)
 * ```
 */
export function replace_x(vector, value) {
  return new Vec2(value, vector.y);
}

/**
 * Returns a new vector with the y element replace with `value`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> replace_y(777)
 * // -> Vec2(12, 777)
 * ```
 */
export function replace_y(vector, value) {
  return new Vec2(vector.x, value);
}

/**
 * Returns a new vector with the x element having had `with` applied to it.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> map_x(fn(n) { n * 2 })
 * // -> Vec2(24, -34)
 * ```
 */
export function map_x(vector, fun) {
  return new Vec2(fun(vector.x), vector.y);
}

/**
 * Returns a new vector with the y element having had `with` applied to it.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> map_y(fn(n) { n * 2 })
 * // -> Vec2(12, -68)
 * ```
 */
export function map_y(vector, fun) {
  return new Vec2(vector.x, fun(vector.y));
}

/**
 * Returns a new vector with the x and y elements swaped.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> swap()
 * // -> Vec2(-34, 12)
 * ```
 */
export function swap(vector) {
  return new Vec2(vector.y, vector.x);
}

/**
 * Returns a new vector containing only the elements of the first vector after
 * the function has been applied to each one.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> map(fn(x) { x * 2 })
 * // -> Vec2(24, -68)
 * ```
 */
export function map(vector, fun) {
  return new Vec2(fun(vector.x), fun(vector.y));
}

/**
 * Combines two vectors into a single vector using the given function.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(12, -34) |> map2(Vec2(420, 69), fn(x, y) { x + y })
 * // -> Vec2(432, 35)
 * ```
 */
export function map2(a, b, fun) {
  return new Vec2(fun(a.x, b.x), fun(a.y, b.y));
}

/**
 * Combines a vector of results into a single result. If all elements in the
 * vector are `Ok` then returns an `Ok` holding the vector of values. If any
 * element is `Error` then returns the first error.
 *
 * ## Examples
 *
 * ```gleam
 * Vec2(Ok(12), Ok(-34)) |> result()
 * // -> Ok(Vec2(12, -34))
 * ```
 *
 * ```gleam
 * Vec2(Ok(12), Error("foo")) |> result()
 * // -> Error("foo")
 * ```
 */
export function result(vector) {
  let $ = vector.y;
  if ($ instanceof Ok) {
    let $1 = vector.x;
    if ($1 instanceof Ok) {
      let y$1 = $[0];
      let x$1 = $1[0];
      return new Ok(new Vec2(x$1, y$1));
    } else {
      let error = $1[0];
      return new Error(error);
    }
  } else {
    let $1 = vector.x;
    if ($1 instanceof Error) {
      let error = $1[0];
      return new Error(error);
    } else {
      let error = $[0];
      return new Error(error);
    }
  }
}
