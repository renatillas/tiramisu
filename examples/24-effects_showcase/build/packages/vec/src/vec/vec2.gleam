/// A 2-element structure that can be used to represent 2D coordinates or any
/// other pair of any values.
///
pub type Vec2(value) {
  Vec2(x: value, y: value)
}

/// Creates a vector with all elements set to a value.
///
/// ## Examples
///
/// ```gleam
/// splat(12)
/// // -> Vec2(12, 12)
/// ```
///
pub fn splat(value) -> Vec2(value) {
  Vec2(value, value)
}

/// Converts a tuple of the contained elements into a vector.
///
/// ## Examples
///
/// ```gleam
/// #(12, -34) |> from_tuple()
/// // -> Vec2(12, -34)
/// ```
///
pub fn from_tuple(tuple: #(value, value)) -> Vec2(value) {
  Vec2(tuple.0, tuple.1)
}

/// Converts the vector into a tuple of the contained elements.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> to_tuple()
/// // -> #(12, -34)
/// ```
///
pub fn to_tuple(vector: Vec2(value)) -> #(value, value) {
  #(vector.x, vector.y)
}

/// Converts the vector into a list of the contained elements.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> to_list()
/// // -> [12, -34]
/// ```
///
pub fn to_list(vector: Vec2(value)) -> List(value) {
  [vector.x, vector.y]
}

/// Returns the x element in a vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> x()
/// // -> 12
/// ```
///
pub fn x(vector: Vec2(value)) -> value {
  vector.x
}

/// Returns the y element in a vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> y()
/// // -> -34
/// ```
///
pub fn y(vector: Vec2(value)) -> value {
  vector.y
}

/// Returns a new vector with the x element replace with `value`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> replace_x(777)
/// // -> Vec2(777, -34)
/// ```
///
pub fn replace_x(vector: Vec2(value), to value: value) -> Vec2(value) {
  Vec2(..vector, x: value)
}

/// Returns a new vector with the y element replace with `value`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> replace_y(777)
/// // -> Vec2(12, 777)
/// ```
///
pub fn replace_y(vector: Vec2(value), to value: value) -> Vec2(value) {
  Vec2(..vector, y: value)
}

/// Returns a new vector with the x element having had `with` applied to it.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> map_x(fn(n) { n * 2 })
/// // -> Vec2(24, -34)
/// ```
///
pub fn map_x(vector: Vec2(value), with fun: fn(value) -> value) -> Vec2(value) {
  Vec2(..vector, x: fun(vector.x))
}

/// Returns a new vector with the y element having had `with` applied to it.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> map_y(fn(n) { n * 2 })
/// // -> Vec2(12, -68)
/// ```
///
pub fn map_y(vector: Vec2(value), with fun: fn(value) -> value) -> Vec2(value) {
  Vec2(..vector, y: fun(vector.y))
}

/// Returns a new vector with the x and y elements swaped.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> swap()
/// // -> Vec2(-34, 12)
/// ```
///
pub fn swap(vector: Vec2(value)) -> Vec2(value) {
  Vec2(vector.y, vector.x)
}

/// Returns a new vector containing only the elements of the first vector after
/// the function has been applied to each one.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> map(fn(x) { x * 2 })
/// // -> Vec2(24, -68)
/// ```
///
pub fn map(vector: Vec2(a), with fun: fn(a) -> b) -> Vec2(b) {
  Vec2(fun(vector.x), fun(vector.y))
}

/// Combines two vectors into a single vector using the given function.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> map2(Vec2(420, 69), fn(x, y) { x + y })
/// // -> Vec2(432, 35)
/// ```
///
pub fn map2(a: Vec2(a), b: Vec2(b), with fun: fn(a, b) -> c) -> Vec2(c) {
  Vec2(fun(a.x, b.x), fun(a.y, b.y))
}

/// Combines a vector of results into a single result. If all elements in the
/// vector are `Ok` then returns an `Ok` holding the vector of values. If any
/// element is `Error` then returns the first error.
///
/// ## Examples
///
/// ```gleam
/// Vec2(Ok(12), Ok(-34)) |> result()
/// // -> Ok(Vec2(12, -34))
/// ```
///
/// ```gleam
/// Vec2(Ok(12), Error("foo")) |> result()
/// // -> Error("foo")
/// ```
///
pub fn result(vector: Vec2(Result(a, e))) -> Result(Vec2(a), e) {
  case vector {
    Vec2(Ok(x), Ok(y)) -> Ok(Vec2(x, y))
    Vec2(Error(error), _) -> Error(error)
    Vec2(_, Error(error)) -> Error(error)
  }
}
