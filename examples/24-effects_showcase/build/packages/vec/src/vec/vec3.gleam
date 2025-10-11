/// A 3-element structure that can be used to represent 3D coordinates or any
/// other triplet of any values.
///
pub type Vec3(value) {
  Vec3(x: value, y: value, z: value)
}

/// Creates a vector with all elements set to a value.
///
/// ## Examples
///
/// ```gleam
/// splat(12)
/// // -> Vec3(12, 12, 12)
/// ```
///
pub fn splat(value) -> Vec3(value) {
  Vec3(value, value, value)
}

/// Converts a tuple of the contained elements into a vector.
///
/// ## Examples
///
/// ```gleam
/// #(12, -34, 420) |> from_tuple()
/// // -> Vec3(12, -34, 420)
/// ```
///
pub fn from_tuple(tuple: #(value, value, value)) -> Vec3(value) {
  Vec3(tuple.0, tuple.1, tuple.2)
}

/// Converts the vector into a tuple of the contained elements.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> to_tuple()
/// // -> #(12, -34, 420)
/// ```
///
pub fn to_tuple(vector: Vec3(value)) -> #(value, value, value) {
  #(vector.x, vector.y, vector.z)
}

/// Converts the vector into a list of the contained elements.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> to_list()
/// // -> [12, -34, 420]
/// ```
///
pub fn to_list(vector: Vec3(value)) -> List(value) {
  [vector.x, vector.y, vector.z]
}

/// Returns the x element in a vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> x()
/// // -> 12
/// ```
///
pub fn x(vector: Vec3(value)) -> value {
  vector.x
}

/// Returns the y element in a vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> y()
/// // -> -34
/// ```
///
pub fn y(vector: Vec3(value)) -> value {
  vector.y
}

/// Returns the z element in a vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> z()
/// // -> 420
/// ```
///
pub fn z(vector: Vec3(value)) -> value {
  vector.z
}

/// Returns a new vector with the x element replace with `value`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> replace_x(777)
/// // -> Vec3(777, -34, 420)
/// ```
///
pub fn replace_x(vector: Vec3(value), to value: value) -> Vec3(value) {
  Vec3(..vector, x: value)
}

/// Returns a new vector with the y element replace with `value`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> replace_y(777)
/// // -> Vec3(12, 777, 420)
/// ```
///
pub fn replace_y(vector: Vec3(value), to value: value) -> Vec3(value) {
  Vec3(..vector, y: value)
}

/// Returns a new vector with the z element replace with `value`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> replace_z(777)
/// // -> Vec3(12, -34, 777)
/// ```
///
pub fn replace_z(vector: Vec3(value), to value: value) -> Vec3(value) {
  Vec3(..vector, z: value)
}

/// Returns a new vector with the x element having had `with` applied to it.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> map_x(fn(n) { n * 2 })
/// // -> Vec3(24, -34, 420)
/// ```
///
pub fn map_x(vector: Vec3(value), with fun: fn(value) -> value) -> Vec3(value) {
  Vec3(..vector, x: fun(vector.x))
}

/// Returns a new vector with the y element having had `with` applied to it.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> map_y(fn(n) { n * 2 })
/// // -> Vec3(12, -68, 420)
/// ```
///
pub fn map_y(vector: Vec3(value), with fun: fn(value) -> value) -> Vec3(value) {
  Vec3(..vector, y: fun(vector.y))
}

/// Returns a new vector with the z element having had `with` applied to it.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> map_z(fn(n) { n * 2 })
/// // -> Vec3(12, -34, 840)
/// ```
///
pub fn map_z(vector: Vec3(value), with fun: fn(value) -> value) -> Vec3(value) {
  Vec3(..vector, z: fun(vector.z))
}

/// Returns a new vector with the x and y elements swaped.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> swap_xy()
/// // -> Vec3(-34, 12, 420)
/// ```
///
pub fn swap_xy(vector: Vec3(value)) -> Vec3(value) {
  Vec3(..vector, x: vector.y, y: vector.x)
}

/// Returns a new vector with the x and z elements swaped.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> swap_xz()
/// // -> Vec3(420, -34, 12)
/// ```
///
pub fn swap_xz(vector: Vec3(value)) -> Vec3(value) {
  Vec3(..vector, x: vector.z, z: vector.x)
}

/// Returns a new vector with the y and z elements swaped.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> swap_yz()
/// // -> Vec3(12, 420, -34)
/// ```
///
pub fn swap_yz(vector: Vec3(value)) -> Vec3(value) {
  Vec3(..vector, y: vector.z, z: vector.y)
}

/// Returns a new vector containing only the elements of the first vector after
/// the function has been applied to each one.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> map(fn(x) { x * 2 })
/// // -> Vec3(24, -68, 840)
/// ```
///
pub fn map(vector: Vec3(a), with fun: fn(a) -> b) -> Vec3(b) {
  Vec3(fun(vector.x), fun(vector.y), fun(vector.z))
}

/// Combines two vectors into a single vector using the given function.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> map2(Vec3(1, 2, 3), fn(x, y) { x * y })
/// // -> Vec3(12, -68, 1260)
/// ```
///
pub fn map2(a: Vec3(a), b: Vec3(b), with fun: fn(a, b) -> c) -> Vec3(c) {
  Vec3(fun(a.x, b.x), fun(a.y, b.y), fun(a.z, b.z))
}

/// Combines a vector of results into a single result. If all elements in the
/// vector are `Ok` then returns an `Ok` holding the vector of values. If any
/// element is `Error` then returns the first error.
///
/// ## Examples
///
/// ```gleam
/// Vec3(Ok(12), Ok(-34), Ok(420)) |> result()
/// // -> Ok(Vec3(12, -34, 420))
/// ```
///
/// ```gleam
/// Vec3(Ok(12), Error("foo"), Error("bar")) |> result()
/// // -> Error("foo")
/// ```
///
pub fn result(vector: Vec3(Result(a, e))) -> Result(Vec3(a), e) {
  case vector {
    Vec3(Ok(x), Ok(y), Ok(z)) -> Ok(Vec3(x, y, z))
    Vec3(Error(error), _, _) -> Error(error)
    Vec3(_, Error(error), _) -> Error(error)
    Vec3(_, _, Error(error)) -> Error(error)
  }
}
