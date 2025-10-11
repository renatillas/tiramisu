import gleam/int
import gleam/list
import gleam/order.{type Order}
import vec/vec4.{type Vec4, Vec4}

/// Zero vector, a vector with all components set to `0`.
///
pub const zero = Vec4(0, 0, 0, 0)

/// One vector, a vector with all components set to `1`.
///
pub const one = Vec4(1, 1, 1, 1)

/// Returns a new vector with all components clamped between a lower and upper
/// bound.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> clamp(
///   Vec4(10, 21, -54, 75),
///   Vec4(14, 18, 323, 91),
/// )
/// // -> Vec4(12, 21, 323, 75)
/// ```
///
pub fn clamp(
  vector: Vec4(Int),
  start_bound: Vec4(Int),
  stop_bound: Vec4(Int),
) -> Vec4(Int) {
  Vec4(
    int.clamp(vector.x, start_bound.x, stop_bound.x),
    int.clamp(vector.y, start_bound.y, stop_bound.y),
    int.clamp(vector.z, start_bound.z, stop_bound.z),
    int.clamp(vector.w, start_bound.w, stop_bound.w),
  )
}

/// Compares two vectors, returning the smaller of the two.
///
/// ## Examples
///
/// ```gleam
/// min(Vec4(12, -34, 420, 69), Vec4(10, 21, -54, 75))
/// // -> Vec4(10, -34, -54, 69)
/// ```
///
pub fn min(a: Vec4(Int), b: Vec4(Int)) -> Vec4(Int) {
  a |> vec4.map2(b, int.min)
}

/// Compares two vectors, returning the larger of the two.
///
/// ## Examples
///
/// ```gleam
/// max(Vec4(12, -34, 420, 69), Vec4(14, -93, 323, 91))
/// // -> Vec4(14, -34, 420, 91)
/// ```
///
pub fn max(a: Vec4(Int), b: Vec4(Int)) -> Vec4(Int) {
  a |> vec4.map2(b, int.max)
}

/// Returns a new vector with all elements in absolute values.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> absolute_value()
/// // -> Vec4(12, 34, 420, 69)
/// ```
///
pub fn absolute_value(vector: Vec4(Int)) -> Vec4(Int) {
  vector |> vec4.map(int.absolute_value)
}

/// Takes an int vector and returns its value as a int vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> to_vec4f()
/// // -> Vec4(12.0, -34.0, 420.0, 69.0)
/// ```
///
pub fn to_vec4f(vector: Vec4(Int)) -> Vec4(Float) {
  vector |> vec4.map(int.to_float)
}

/// Returns a new vector with all elements negated.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> negate()
/// // -> Vec4(-12, 34, -420, -69)
/// ```
///
pub fn negate(vector: Vec4(Int)) -> Vec4(Int) {
  vector |> vec4.map(int.negate)
}

/// Sums a list of vectors.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec4(12, -34, 420, 69),
///   Vec4(21, 45, -20, 9),
///   Vec4(33, 0, -200, 3),
/// ]
/// |> sum()
/// // -> Vec4(66, 11, 200, 81)
/// ```
///
pub fn sum(vectors: List(Vec4(Int))) -> Vec4(Int) {
  vectors |> list.fold(vec4.splat(0), add)
}

/// Multiplies a list of vectors and returns the product.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec4(12, -34, 420, 69),
///   Vec4(21, -10, 999, 20),
///   Vec4(32, 20, 0, 5),
/// ]
/// |> product()
/// // -> Vec4(8064, 6800, 0, 6900)
/// ```
///
pub fn product(vectors: List(Vec4(Int))) -> Vec4(Int) {
  vectors |> list.fold(vec4.splat(1), multiply)
}

/// Computes the remainder of an integer vector division of inputs as a
/// `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(13, -13, 13, -13) |> remainder(Vec4(3, 3, -3, -3))
/// // -> Ok(Vec4(1, -1, 1, -1))
/// ```
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> remainder(Vec4(0, 1, 2, 3))
/// // -> Error(Nil)
/// ```
///
pub fn remainder(
  dividend: Vec4(Int),
  by divisor: Vec4(Int),
) -> Result(Vec4(Int), Nil) {
  dividend |> vec4.map2(divisor, int.remainder) |> vec4.result()
}

/// Returns the modulo of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(13, -13, 13, -13) |> modulo(Vec4(3, 3, -3, -3))
/// // -> Ok(Vec4(1, 2, -2, -1))
/// ```
///
pub fn modulo(
  dividend: Vec4(Int),
  by divisor: Vec4(Int),
) -> Result(Vec4(Int), Nil) {
  dividend |> vec4.map2(divisor, int.modulo) |> vec4.result()
}

/// Returns division of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> divide(Vec4(2, 5, 4, 1))
/// // -> Ok(Vec4(6, -6, 105, 69))
/// ```
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> divide(Vec4(0, 5, 4, 1))
/// // -> Error(Nil)
/// ```
///
pub fn divide(
  dividend: Vec4(Int),
  by divisor: Vec4(Int),
) -> Result(Vec4(Int), Nil) {
  dividend |> vec4.map2(divisor, int.divide) |> vec4.result()
}

/// Performs a *floored* integer vector division, which means that the result
/// will always be rounded towards negative infinity.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> floor_divide(Vec4(2, 5, 4, 1))
/// // -> Ok(Vec4(6, -7, 105, 69))
/// ```
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> floor_divide(Vec4(0, 5, 4, 1))
/// // -> Error(Nil)
/// ```
///
pub fn floor_divide(
  dividend: Vec4(Int),
  by divisor: Vec4(Int),
) -> Result(Vec4(Int), Nil) {
  dividend |> vec4.map2(divisor, int.floor_divide) |> vec4.result()
}

/// Adds two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> add(Vec4(21, 45, -20, -9))
/// // -> Vec4(33, 11, 400, 60)
/// ```
///
pub fn add(a: Vec4(Int), b: Vec4(Int)) -> Vec4(Int) {
  a |> vec4.map2(b, int.add)
}

/// Multiplies two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> multiply(Vec4(2, -3, 0, 1))
/// // -> Vec4(24, 102, 0, 69)
/// ```
///
pub fn multiply(a: Vec4(Int), b: Vec4(Int)) -> Vec4(Int) {
  a |> vec4.map2(b, int.multiply)
}

/// Subtracts one vector from another.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> subtract(Vec4(7, -45, 20, 32))
/// // -> Vec4(5, 11, 400, 37)
/// ```
///
pub fn subtract(a: Vec4(Int), b: Vec4(Int)) -> Vec4(Int) {
  a |> vec4.map2(b, int.subtract)
}

/// Returns the squared length (squared magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> length_squared()
/// // -> 182_461
/// ```
///
pub fn length_squared(vector: Vec4(Int)) -> Int {
  vector
  |> vec4.to_list()
  |> list.map(fn(element) { element * element })
  |> int.sum()
}

/// Returns the length (magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> length()
/// // -> 427.15
/// ```
///
pub fn length(vector: Vec4(Int)) -> Float {
  let assert Ok(length) = vector |> length_squared() |> int.square_root()

  length
}

/// Compares two vector's lengths, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare_length(Vec4(12, -34, 420, 69), Vec4(2, 3, 4, 5))
/// // -> Gt
/// ```
///
pub fn compare_length(a: Vec4(Int), with b: Vec4(Int)) -> Order {
  int.compare(a |> length_squared(), b |> length_squared())
}

/// Returns the squared distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> distance_squared(Vec4(2, 3, 4, 5))
/// // -> 178_621
/// ```
///
pub fn distance_squared(a: Vec4(Int), with b: Vec4(Int)) -> Int {
  a |> vec4.map2(b, int.subtract) |> length_squared()
}

/// Returns the distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> distance(Vec4(2, 3, 4, 5))
/// // -> 422.64
/// ```
///
pub fn distance(a: Vec4(Int), with b: Vec4(Int)) -> Float {
  let assert Ok(distance) = distance_squared(a, b) |> int.square_root()

  distance
}

/// Compares two vector's distances to a vector, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare_distance(
///   Vec4(12, -34, 420, 69),
///   Vec4(2, 3, 4, 5),
///   Vec4(-25, 67, 194, 0),
/// )
/// // -> Gt
/// ```
///
pub fn compare_distance(
  a: Vec4(Int),
  with b: Vec4(Int),
  to vector: Vec4(Int),
) -> Order {
  int.compare(a |> distance_squared(vector), b |> distance_squared(vector))
}

/// Returns a new vector containing the elements multiplies by `scalar`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> scale(2)
/// // -> Vec4(24, -68, 840, 138)
/// ```
///
pub fn scale(vector: Vec4(Int), by scalar: Int) -> Vec4(Int) {
  vector |> vec4.map(int.multiply(_, scalar))
}

/// Returns the dot product of two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> dot(Vec4(2, 3, 4, 5))
/// // -> 1947
/// ```
///
pub fn dot(a: Vec4(Int), b: Vec4(Int)) -> Int {
  a |> multiply(b) |> vec4.to_list() |> int.sum()
}

/// Returns the projection of a vector on another vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> project(Vec4(2, 3, 4, 5))
/// // -> Vec4(72, 108, 144, 180)
/// ```
///
pub fn project(a: Vec4(Int), on b: Vec4(Int)) -> Vec4(Int) {
  b |> scale(dot(a, b) / dot(b, b))
}

/// Returns a new vector resulting from sliding this vector along a plane
/// defined by the given normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> slide(Vec4(2, 3, 4, 5))
/// // -> Vec4(-60, -142, 276, -111)
/// ```
///
pub fn slide(a: Vec4(Int), on b: Vec4(Int)) -> Vec4(Int) {
  a |> subtract(a |> project(b))
}

/// Returns the reflection of a vector through a plane defined by the given
/// normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> reflect(Vec4(2, 3, 4, 5))
/// // -> Vec4(132, 250, -132, 291)
/// ```
///
pub fn reflect(vector: Vec4(Int), through normal: Vec4(Int)) -> Vec4(Int) {
  vector |> project(normal) |> scale(2) |> subtract(vector)
}

/// Returns the mirror of a vector through a plane defined by the given normal
/// vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69) |> mirror(Vec4(2, 3, 4, 5))
/// // -> Vec4(-132, -250, 132, -291)
/// ```
///
pub fn mirror(vector: Vec4(Int), through normal: Vec4(Int)) -> Vec4(Int) {
  vector |> reflect(normal) |> negate()
}

/// Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(12, -34, 420, 69)
/// |> anchor_position(Vec4(2, 3, 4, 5), scale(_, 2))
/// // -> Vec4(22, -71, 836, 133)
/// ```
///
pub fn anchor_position(
  vector: Vec4(Int),
  at position: Vec4(Int),
  then fun: fn(Vec4(Int)) -> Vec4(Int),
) -> Vec4(Int) {
  vector |> subtract(position) |> fun() |> add(position)
}
