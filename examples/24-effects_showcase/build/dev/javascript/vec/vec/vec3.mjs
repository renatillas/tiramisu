import { Ok, Error, toList, CustomType as $CustomType } from "../gleam.mjs";

export class Vec3 extends $CustomType {
  constructor(x, y, z) {
    super();
    this.x = x;
    this.y = y;
    this.z = z;
  }
}

/**
 * Creates a vector with all elements set to a value.
 *
 * ## Examples
 *
 * ```gleam
 * splat(12)
 * // -> Vec3(12, 12, 12)
 * ```
 */
export function splat(value) {
  return new Vec3(value, value, value);
}

/**
 * Converts a tuple of the contained elements into a vector.
 *
 * ## Examples
 *
 * ```gleam
 * #(12, -34, 420) |> from_tuple()
 * // -> Vec3(12, -34, 420)
 * ```
 */
export function from_tuple(tuple) {
  return new Vec3(tuple[0], tuple[1], tuple[2]);
}

/**
 * Converts the vector into a tuple of the contained elements.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> to_tuple()
 * // -> #(12, -34, 420)
 * ```
 */
export function to_tuple(vector) {
  return [vector.x, vector.y, vector.z];
}

/**
 * Converts the vector into a list of the contained elements.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> to_list()
 * // -> [12, -34, 420]
 * ```
 */
export function to_list(vector) {
  return toList([vector.x, vector.y, vector.z]);
}

/**
 * Returns the x element in a vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> x()
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
 * Vec3(12, -34, 420) |> y()
 * // -> -34
 * ```
 */
export function y(vector) {
  return vector.y;
}

/**
 * Returns the z element in a vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> z()
 * // -> 420
 * ```
 */
export function z(vector) {
  return vector.z;
}

/**
 * Returns a new vector with the x element replace with `value`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> replace_x(777)
 * // -> Vec3(777, -34, 420)
 * ```
 */
export function replace_x(vector, value) {
  return new Vec3(value, vector.y, vector.z);
}

/**
 * Returns a new vector with the y element replace with `value`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> replace_y(777)
 * // -> Vec3(12, 777, 420)
 * ```
 */
export function replace_y(vector, value) {
  return new Vec3(vector.x, value, vector.z);
}

/**
 * Returns a new vector with the z element replace with `value`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> replace_z(777)
 * // -> Vec3(12, -34, 777)
 * ```
 */
export function replace_z(vector, value) {
  return new Vec3(vector.x, vector.y, value);
}

/**
 * Returns a new vector with the x element having had `with` applied to it.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> map_x(fn(n) { n * 2 })
 * // -> Vec3(24, -34, 420)
 * ```
 */
export function map_x(vector, fun) {
  return new Vec3(fun(vector.x), vector.y, vector.z);
}

/**
 * Returns a new vector with the y element having had `with` applied to it.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> map_y(fn(n) { n * 2 })
 * // -> Vec3(12, -68, 420)
 * ```
 */
export function map_y(vector, fun) {
  return new Vec3(vector.x, fun(vector.y), vector.z);
}

/**
 * Returns a new vector with the z element having had `with` applied to it.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> map_z(fn(n) { n * 2 })
 * // -> Vec3(12, -34, 840)
 * ```
 */
export function map_z(vector, fun) {
  return new Vec3(vector.x, vector.y, fun(vector.z));
}

/**
 * Returns a new vector with the x and y elements swaped.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> swap_xy()
 * // -> Vec3(-34, 12, 420)
 * ```
 */
export function swap_xy(vector) {
  return new Vec3(vector.y, vector.x, vector.z);
}

/**
 * Returns a new vector with the x and z elements swaped.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> swap_xz()
 * // -> Vec3(420, -34, 12)
 * ```
 */
export function swap_xz(vector) {
  return new Vec3(vector.z, vector.y, vector.x);
}

/**
 * Returns a new vector with the y and z elements swaped.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> swap_yz()
 * // -> Vec3(12, 420, -34)
 * ```
 */
export function swap_yz(vector) {
  return new Vec3(vector.x, vector.z, vector.y);
}

/**
 * Returns a new vector containing only the elements of the first vector after
 * the function has been applied to each one.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> map(fn(x) { x * 2 })
 * // -> Vec3(24, -68, 840)
 * ```
 */
export function map(vector, fun) {
  return new Vec3(fun(vector.x), fun(vector.y), fun(vector.z));
}

/**
 * Combines two vectors into a single vector using the given function.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(12, -34, 420) |> map2(Vec3(1, 2, 3), fn(x, y) { x * y })
 * // -> Vec3(12, -68, 1260)
 * ```
 */
export function map2(a, b, fun) {
  return new Vec3(fun(a.x, b.x), fun(a.y, b.y), fun(a.z, b.z));
}

/**
 * Combines a vector of results into a single result. If all elements in the
 * vector are `Ok` then returns an `Ok` holding the vector of values. If any
 * element is `Error` then returns the first error.
 *
 * ## Examples
 *
 * ```gleam
 * Vec3(Ok(12), Ok(-34), Ok(420)) |> result()
 * // -> Ok(Vec3(12, -34, 420))
 * ```
 *
 * ```gleam
 * Vec3(Ok(12), Error("foo"), Error("bar")) |> result()
 * // -> Error("foo")
 * ```
 */
export function result(vector) {
  let $ = vector.z;
  if ($ instanceof Ok) {
    let $1 = vector.y;
    if ($1 instanceof Ok) {
      let $2 = vector.x;
      if ($2 instanceof Ok) {
        let z$1 = $[0];
        let y$1 = $1[0];
        let x$1 = $2[0];
        return new Ok(new Vec3(x$1, y$1, z$1));
      } else {
        let error = $2[0];
        return new Error(error);
      }
    } else {
      let $2 = vector.x;
      if ($2 instanceof Error) {
        let error = $2[0];
        return new Error(error);
      } else {
        let error = $1[0];
        return new Error(error);
      }
    }
  } else {
    let $1 = vector.x;
    if ($1 instanceof Error) {
      let error = $1[0];
      return new Error(error);
    } else {
      let $2 = vector.y;
      if ($2 instanceof Error) {
        let error = $2[0];
        return new Error(error);
      } else {
        let error = $[0];
        return new Error(error);
      }
    }
  }
}
