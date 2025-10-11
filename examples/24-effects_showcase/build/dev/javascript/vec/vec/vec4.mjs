import { Ok, Error, toList, CustomType as $CustomType } from "../gleam.mjs";

export class Vec4 extends $CustomType {
  constructor(x, y, z, w) {
    super();
    this.x = x;
    this.y = y;
    this.z = z;
    this.w = w;
  }
}

/**
 * Creates a vector with all elements set to a value.
 *
 * ## Examples
 *
 * ```gleam
 * splat(12)
 * // -> Vec4(12, 12, 12, 12)
 * ```
 */
export function splat(value) {
  return new Vec4(value, value, value, value);
}

/**
 * Converts a tuple of the contained elements into a vector.
 *
 * ## Examples
 *
 * ```gleam
 * #(12, -34, 420, 69) |> from_tuple()
 * // -> Vec4(12, -34, 420, 69)
 * ```
 */
export function from_tuple(tuple) {
  return new Vec4(tuple[0], tuple[1], tuple[2], tuple[3]);
}

/**
 * Converts the vector into a tuple of the contained elements.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> to_tuple()
 * // -> #(12, -34, 420, 69)
 * ```
 */
export function to_tuple(vector) {
  return [vector.x, vector.y, vector.z, vector.w];
}

/**
 * Converts the vector into a list of the contained elements.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> to_list()
 * // -> [12, -34, 420, 69]
 * ```
 */
export function to_list(vector) {
  return toList([vector.x, vector.y, vector.z, vector.w]);
}

/**
 * Returns the x element in a vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> x()
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
 * Vec4(12, -34, 420, 69) |> y()
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
 * Vec4(12, -34, 420, 69) |> z()
 * // -> 420
 * ```
 */
export function z(vector) {
  return vector.z;
}

/**
 * Returns the w element in a vector.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> w()
 * // -> 69
 * ```
 */
export function w(vector) {
  return vector.w;
}

/**
 * Returns a new vector with the x element replace with `value`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> replace_x(777)
 * // -> Vec4(777, -34, 420, 69)
 * ```
 */
export function replace_x(vector, value) {
  return new Vec4(value, vector.y, vector.z, vector.w);
}

/**
 * Returns a new vector with the y element replace with `value`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> replace_y(777)
 * // -> Vec4(12, 777, 420, 69)
 * ```
 */
export function replace_y(vector, value) {
  return new Vec4(vector.x, value, vector.z, vector.w);
}

/**
 * Returns a new vector with the z element replace with `value`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> replace_z(777)
 * // -> Vec4(12, -34, 777, 69)
 * ```
 */
export function replace_z(vector, value) {
  return new Vec4(vector.x, vector.y, value, vector.w);
}

/**
 * Returns a new vector with the w element replace with `value`.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> replace_w(777)
 * // -> Vec4(12, -34, 420, 777)
 * ```
 */
export function replace_w(vector, value) {
  return new Vec4(vector.x, vector.y, vector.z, value);
}

/**
 * Returns a new vector with the x element having had `with` applied to it.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> map_x(fn(n) { n * 2 })
 * // -> Vec4(24, -34, 420, 69)
 * ```
 */
export function map_x(vector, fun) {
  return new Vec4(fun(vector.x), vector.y, vector.z, vector.w);
}

/**
 * Returns a new vector with the y element having had `with` applied to it.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> map_y(fn(n) { n * 2 })
 * // -> Vec4(12, -68, 420, 69)
 * ```
 */
export function map_y(vector, fun) {
  return new Vec4(vector.x, fun(vector.y), vector.z, vector.w);
}

/**
 * Returns a new vector with the z element having had `with` applied to it.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> map_z(fn(n) { n * 2 })
 * // -> Vec4(12, -34, 840, 69)
 * ```
 */
export function map_z(vector, fun) {
  return new Vec4(vector.x, vector.y, fun(vector.z), vector.w);
}

/**
 * Returns a new vector with the w element having had `with` applied to it.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> map_w(fn(n) { n * 2 })
 * // -> Vec4(12, -34, 420, 138)
 * ```
 */
export function map_w(vector, fun) {
  return new Vec4(vector.x, vector.y, vector.z, fun(vector.w));
}

/**
 * Returns a new vector with the x and y elements swaped.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> swap_xy()
 * // -> Vec4(-34, 12, 420, 69)
 * ```
 */
export function swap_xy(vector) {
  return new Vec4(vector.y, vector.x, vector.z, vector.w);
}

/**
 * Returns a new vector with the x and z elements swaped.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> swap_xz()
 * // -> Vec4(420, -34, 12, 69)
 * ```
 */
export function swap_xz(vector) {
  return new Vec4(vector.z, vector.y, vector.x, vector.w);
}

/**
 * Returns a new vector with the x and w elements swaped.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> swap_xw()
 * // -> Vec4(69, -34, 420, 12)
 * ```
 */
export function swap_xw(vector) {
  return new Vec4(vector.w, vector.y, vector.z, vector.x);
}

/**
 * Returns a new vector with the y and z elements swaped.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> swap_yz()
 * // -> Vec4(12, 420, -34, 69)
 * ```
 */
export function swap_yz(vector) {
  return new Vec4(vector.x, vector.z, vector.y, vector.w);
}

/**
 * Returns a new vector with the y and w elements swaped.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> swap_yw()
 * // -> Vec4(12, 69, 420, -34)
 * ```
 */
export function swap_yw(vector) {
  return new Vec4(vector.x, vector.w, vector.z, vector.y);
}

/**
 * Returns a new vector with the z and w elements swaped.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> swap_zw()
 * // -> Vec4(12, -34, 69, 420)
 * ```
 */
export function swap_zw(vector) {
  return new Vec4(vector.x, vector.y, vector.w, vector.z);
}

/**
 * Returns a new vector containing only the elements of the first vector after
 * the function has been applied to each one.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> map(fn(x) { x * 2 })
 * // -> Vec4(24, -68, 840, 138)
 * ```
 */
export function map(vector, fun) {
  return new Vec4(fun(vector.x), fun(vector.y), fun(vector.z), fun(vector.w));
}

/**
 * Combines two vectors into a single vector using the given function.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(12, -34, 420, 69) |> map2(Vec4(1, 2, 3, 4), fn(x, y) { x * y })
 * // -> Vec4(12, -68, 1260, 276)
 * ```
 */
export function map2(a, b, fun) {
  return new Vec4(fun(a.x, b.x), fun(a.y, b.y), fun(a.z, b.z), fun(a.w, b.w));
}

/**
 * Combines a vector of results into a single result. If all elements in the
 * vector are `Ok` then returns an `Ok` holding the vector of values. If any
 * element is `Error` then returns the first error.
 *
 * ## Examples
 *
 * ```gleam
 * Vec4(Ok(12), Ok(-34), Ok(420), Ok(69)) |> result()
 * // -> Ok(Vec4(12, -34, 420, 69))
 * ```
 *
 * ```gleam
 * Vec4(Ok(12), Error("foo"), Ok(420), Error("bar")) |> result()
 * // -> Error("foo")
 * ```
 */
export function result(vector) {
  let $ = vector.w;
  if ($ instanceof Ok) {
    let $1 = vector.z;
    if ($1 instanceof Ok) {
      let $2 = vector.y;
      if ($2 instanceof Ok) {
        let $3 = vector.x;
        if ($3 instanceof Ok) {
          let w$1 = $[0];
          let z$1 = $1[0];
          let y$1 = $2[0];
          let x$1 = $3[0];
          return new Ok(new Vec4(x$1, y$1, z$1, w$1));
        } else {
          let error = $3[0];
          return new Error(error);
        }
      } else {
        let $3 = vector.x;
        if ($3 instanceof Error) {
          let error = $3[0];
          return new Error(error);
        } else {
          let error = $2[0];
          return new Error(error);
        }
      }
    } else {
      let $2 = vector.x;
      if ($2 instanceof Error) {
        let error = $2[0];
        return new Error(error);
      } else {
        let $3 = vector.y;
        if ($3 instanceof Error) {
          let error = $3[0];
          return new Error(error);
        } else {
          let error = $1[0];
          return new Error(error);
        }
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
        let $3 = vector.z;
        if ($3 instanceof Error) {
          let error = $3[0];
          return new Error(error);
        } else {
          let error = $[0];
          return new Error(error);
        }
      }
    }
  }
}
