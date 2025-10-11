/// A 4-element structure that can be used to represent 4D coordinates or any
/// other quadruplet of any values.
///
pub type Vec4(value) {
  Vec4(x: value, y: value, z: value, w: value)
}

/// Creates a vector with all elements set to a value.
///
/// ## Examples
///
/// ```gleam
/// splat(12)
/// // -> Vec4(12, 12, 12, 12)
/// ```
///
pub fn splat(value) -> Vec4(value) {
  Vec4(value, value, value, value)
}

/// Converts a tuple of the contained elements into a vector.
///
/// ## Examples
///
/// ```gleam
/// #(12, -34, 420, 69) |> from_tuple()
/// // -> Vec4(12, -34, 420, 69)
/// ```
///
pub fn from_tuple(tuple: #(value, value, value, value)) -> Vec4(value) {
  Vec4(tuple.0, tuple.1, tuple.2, tuple.3)
}

/// Converts the vector into a tuple of the contained elements.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> to_tuple()
/// // -> #(12, -34, 420, 69)
/// ```
///
pub fn to_tuple(vector: Vec4(value)) -> #(value, value, value, value) {
  #(vector.x, vector.y, vector.z, vector.w)
}

/// Converts the vector into a list of the contained elements.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> to_list()
/// // -> [12, -34, 420, 69]
/// ```
///
pub fn to_list(vector: Vec4(value)) -> List(value) {
  [vector.x, vector.y, vector.z, vector.w]
}

/// Returns the x element in a vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> x()
/// // -> 12
/// ```
///
pub fn x(vector: Vec4(value)) -> value {
  vector.x
}

/// Returns the y element in a vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> y()
/// // -> -34
/// ```
///
pub fn y(vector: Vec4(value)) -> value {
  vector.y
}

/// Returns the z element in a vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> z()
/// // -> 420
/// ```
///
pub fn z(vector: Vec4(value)) -> value {
  vector.z
}

/// Returns the w element in a vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> w()
/// // -> 69
/// ```
///
pub fn w(vector: Vec4(value)) -> value {
  vector.w
}

/// Returns a new vector with the x element replace with `value`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> replace_x(777)
/// // -> Vec4(777, -34, 420, 69)
/// ```
///
pub fn replace_x(vector: Vec4(value), to value: value) -> Vec4(value) {
  Vec4(..vector, x: value)
}

/// Returns a new vector with the y element replace with `value`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> replace_y(777)
/// // -> Vec4(12, 777, 420, 69)
/// ```
///
pub fn replace_y(vector: Vec4(value), to value: value) -> Vec4(value) {
  Vec4(..vector, y: value)
}

/// Returns a new vector with the z element replace with `value`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> replace_z(777)
/// // -> Vec4(12, -34, 777, 69)
/// ```
///
pub fn replace_z(vector: Vec4(value), to value: value) -> Vec4(value) {
  Vec4(..vector, z: value)
}

/// Returns a new vector with the w element replace with `value`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> replace_w(777)
/// // -> Vec4(12, -34, 420, 777)
/// ```
///
pub fn replace_w(vector: Vec4(value), to value: value) -> Vec4(value) {
  Vec4(..vector, w: value)
}

/// Returns a new vector with the x element having had `with` applied to it.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> map_x(fn(n) { n * 2 })
/// // -> Vec4(24, -34, 420, 69)
/// ```
///
pub fn map_x(vector: Vec4(value), with fun: fn(value) -> value) -> Vec4(value) {
  Vec4(..vector, x: fun(vector.x))
}

/// Returns a new vector with the y element having had `with` applied to it.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> map_y(fn(n) { n * 2 })
/// // -> Vec4(12, -68, 420, 69)
/// ```
///
pub fn map_y(vector: Vec4(value), with fun: fn(value) -> value) -> Vec4(value) {
  Vec4(..vector, y: fun(vector.y))
}

/// Returns a new vector with the z element having had `with` applied to it.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> map_z(fn(n) { n * 2 })
/// // -> Vec4(12, -34, 840, 69)
/// ```
///
pub fn map_z(vector: Vec4(value), with fun: fn(value) -> value) -> Vec4(value) {
  Vec4(..vector, z: fun(vector.z))
}

/// Returns a new vector with the w element having had `with` applied to it.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> map_w(fn(n) { n * 2 })
/// // -> Vec4(12, -34, 420, 138)
/// ```
///
pub fn map_w(vector: Vec4(value), with fun: fn(value) -> value) -> Vec4(value) {
  Vec4(..vector, w: fun(vector.w))
}

/// Returns a new vector with the x and y elements swaped.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> swap_xy()
/// // -> Vec4(-34, 12, 420, 69)
/// ```
///
pub fn swap_xy(vector: Vec4(value)) -> Vec4(value) {
  Vec4(..vector, x: vector.y, y: vector.x)
}

/// Returns a new vector with the x and z elements swaped.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> swap_xz()
/// // -> Vec4(420, -34, 12, 69)
/// ```
///
pub fn swap_xz(vector: Vec4(value)) -> Vec4(value) {
  Vec4(..vector, x: vector.z, z: vector.x)
}

/// Returns a new vector with the x and w elements swaped.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> swap_xw()
/// // -> Vec4(69, -34, 420, 12)
/// ```
///
pub fn swap_xw(vector: Vec4(value)) -> Vec4(value) {
  Vec4(..vector, x: vector.w, w: vector.x)
}

/// Returns a new vector with the y and z elements swaped.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> swap_yz()
/// // -> Vec4(12, 420, -34, 69)
/// ```
///
pub fn swap_yz(vector: Vec4(value)) -> Vec4(value) {
  Vec4(..vector, y: vector.z, z: vector.y)
}

/// Returns a new vector with the y and w elements swaped.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> swap_yw()
/// // -> Vec4(12, 69, 420, -34)
/// ```
///
pub fn swap_yw(vector: Vec4(value)) -> Vec4(value) {
  Vec4(..vector, y: vector.w, w: vector.y)
}

/// Returns a new vector with the z and w elements swaped.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> swap_zw()
/// // -> Vec4(12, -34, 69, 420)
/// ```
///
pub fn swap_zw(vector: Vec4(value)) -> Vec4(value) {
  Vec4(..vector, z: vector.w, w: vector.z)
}

/// Returns a new vector containing only the elements of the first vector after
/// the function has been applied to each one.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> map(fn(x) { x * 2 })
/// // -> Vec4(24, -68, 840, 138)
/// ```
///
pub fn map(vector: Vec4(a), with fun: fn(a) -> b) -> Vec4(b) {
  Vec4(fun(vector.x), fun(vector.y), fun(vector.z), fun(vector.w))
}

/// Combines two vectors into a single vector using the given function.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> map2(Vec4(1, 2, 3, 4), fn(x, y) { x * y })
/// // -> Vec4(12, -68, 1260, 276)
/// ```
///
pub fn map2(a: Vec4(a), b: Vec4(b), with fun: fn(a, b) -> c) -> Vec4(c) {
  Vec4(fun(a.x, b.x), fun(a.y, b.y), fun(a.z, b.z), fun(a.w, b.w))
}

/// Combines a vector of results into a single result. If all elements in the
/// vector are `Ok` then returns an `Ok` holding the vector of values. If any
/// element is `Error` then returns the first error.
///
/// ## Examples
///
/// ```gleam
/// Vec4(Ok(12), Ok(-34), Ok(420), Ok(69)) |> result()
/// // -> Ok(Vec4(12, -34, 420, 69))
/// ```
///
/// ```gleam
/// Vec4(Ok(12), Error("foo"), Ok(420), Error("bar")) |> result()
/// // -> Error("foo")
/// ```
///
pub fn result(vector: Vec4(Result(a, e))) -> Result(Vec4(a), e) {
  case vector {
    Vec4(Ok(x), Ok(y), Ok(z), Ok(w)) -> Ok(Vec4(x, y, z, w))
    Vec4(Error(error), _, _, _) -> Error(error)
    Vec4(_, Error(error), _, _) -> Error(error)
    Vec4(_, _, Error(error), _) -> Error(error)
    Vec4(_, _, _, Error(error)) -> Error(error)
  }
}
