import gleam/int
import gleam/list
import gleam/order.{type Order}
import vec/vec3.{type Vec3, Vec3}

/// Zero vector, a vector with all components set to `0`.
///
pub const zero = Vec3(0, 0, 0)

/// One vector, a vector with all components set to `1`.
///
pub const one = Vec3(1, 1, 1)

/// Returns a new vector with all components clamped between a lower and upper
/// bound.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> clamp(
///   Vec3(10, 21, -54),
///   Vec3(14, 18, 323),
/// )
/// // -> Vec3(12, 21, 323)
/// ```
///
pub fn clamp(
  vector: Vec3(Int),
  start_bound: Vec3(Int),
  stop_bound: Vec3(Int),
) -> Vec3(Int) {
  Vec3(
    int.clamp(vector.x, start_bound.x, stop_bound.x),
    int.clamp(vector.y, start_bound.y, stop_bound.y),
    int.clamp(vector.z, start_bound.z, stop_bound.z),
  )
}

/// Compares two vectors, returning the smaller of the two.
///
/// ## Examples
///
/// ```gleam
/// min(Vec3(12, -34, 420), Vec3(10, 21, -54))
/// // -> Vec3(10, -34, -54)
/// ```
///
pub fn min(a: Vec3(Int), b: Vec3(Int)) -> Vec3(Int) {
  a |> vec3.map2(b, int.min)
}

/// Compares two vectors, returning the larger of the two.
///
/// ## Examples
///
/// ```gleam
/// max(Vec3(12, -34, 420), Vec3(14, -93, 323))
/// // -> Vec3(14, -34, 420)
/// ```
///
pub fn max(a: Vec3(Int), b: Vec3(Int)) -> Vec3(Int) {
  a |> vec3.map2(b, int.max)
}

/// Returns a new vector with all elements in absolute values.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> absolute_value()
/// // -> Vec3(12, 34, 420)
/// ```
///
pub fn absolute_value(vector: Vec3(Int)) -> Vec3(Int) {
  vector |> vec3.map(int.absolute_value)
}

/// Takes an int vector and returns its value as a float vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> to_vec3f()
/// // -> Vec3(12.0, -34.0, 420.0)
/// ```
///
pub fn to_vec3f(vector: Vec3(Int)) -> Vec3(Float) {
  vector |> vec3.map(int.to_float)
}

/// Returns a new vector with all elements negated.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> negate()
/// // -> Vec3(-12, 34, -420)
/// ```
///
pub fn negate(vector: Vec3(Int)) -> Vec3(Int) {
  vector |> vec3.map(int.negate)
}

/// Sums a list of vectors.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec3(12, -34, 420),
///   Vec3(21, 45, -20),
///   Vec3(33, 0, -200),
/// ]
/// |> sum()
/// // -> Vec3(66, 11, 200)
/// ```
///
pub fn sum(vectors: List(Vec3(Int))) -> Vec3(Int) {
  vectors |> list.fold(vec3.splat(0), add)
}

/// Multiplies a list of vectors and returns the product.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec3(12, -34, 420),
///   Vec3(21, -10, 999),
///   Vec3(32, 20, 0),
/// ]
/// |> product()
/// // -> Vec3(8064, 6800, 0)
/// ```
///
pub fn product(vectors: List(Vec3(Int))) -> Vec3(Int) {
  vectors |> list.fold(vec3.splat(1), multiply)
}

/// Computes the remainder of an integer vector division of inputs as a
/// `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(13, -13, 13) |> remainder(Vec3(3, 3, -3))
/// // -> Ok(Vec3(1, -1, 1))
/// ```
///
/// ```gleam
/// Vec3(12, -34, 420) |> remainder(Vec3(0, 1, 2))
/// // -> Error(Nil)
/// ```
///
pub fn remainder(
  dividend: Vec3(Int),
  by divisor: Vec3(Int),
) -> Result(Vec3(Int), Nil) {
  dividend |> vec3.map2(divisor, int.remainder) |> vec3.result()
}

/// Returns the modulo of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(13, -13, 13) |> modulo(Vec3(3, 3, -3))
/// // -> Ok(Vec3(1, 2, -2))
/// ```
///
pub fn modulo(
  dividend: Vec3(Int),
  by divisor: Vec3(Int),
) -> Result(Vec3(Int), Nil) {
  dividend |> vec3.map2(divisor, int.modulo) |> vec3.result()
}

/// Returns division of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> divide(Vec3(2, 5, 4))
/// // -> Ok(Vec3(6, -6, 105))
/// ```
///
/// ```gleam
/// Vec3(12, -34, 420) |> divide(Vec3(0, 5, 4))
/// // -> Error(Nil)
/// ```
///
pub fn divide(
  dividend: Vec3(Int),
  by divisor: Vec3(Int),
) -> Result(Vec3(Int), Nil) {
  dividend |> vec3.map2(divisor, int.divide) |> vec3.result()
}

/// Performs a *floored* integer vector division, which means that the result
/// will always be rounded towards negative infinity.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> floor_divide(Vec3(2, 5, 4))
/// // -> Ok(Vec3(6, -7, 105))
/// ```
///
/// ```gleam
/// Vec3(12, -34, 420) |> floor_divide(Vec3(0, 5, 4))
/// // -> Error(Nil)
/// ```
///
pub fn floor_divide(
  dividend: Vec3(Int),
  by divisor: Vec3(Int),
) -> Result(Vec3(Int), Nil) {
  dividend |> vec3.map2(divisor, int.floor_divide) |> vec3.result()
}

/// Adds two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> add(Vec3(21, 45, -20))
/// // -> Vec3(33, 11, 400)
/// ```
///
pub fn add(a: Vec3(Int), b: Vec3(Int)) -> Vec3(Int) {
  a |> vec3.map2(b, int.add)
}

/// Multiplies two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> multiply(Vec3(2, -3, 0))
/// // -> Vec3(24, 102, 0)
/// ```
///
pub fn multiply(a: Vec3(Int), b: Vec3(Int)) -> Vec3(Int) {
  a |> vec3.map2(b, int.multiply)
}

/// Subtracts one vector from another.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> subtract(Vec3(7, -45, 20))
/// // -> Vec3(5, 11, 400)
/// ```
///
pub fn subtract(a: Vec3(Int), b: Vec3(Int)) -> Vec3(Int) {
  a |> vec3.map2(b, int.subtract)
}

/// Returns the squared length (squared magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> length_squared()
/// // -> 177_700
/// ```
///
pub fn length_squared(vector: Vec3(Int)) -> Int {
  vector
  |> vec3.to_list()
  |> list.map(fn(element) { element * element })
  |> int.sum()
}

/// Returns the length (magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> length()
/// // -> 421.54
/// ```
///
pub fn length(vector: Vec3(Int)) -> Float {
  let assert Ok(length) = vector |> length_squared() |> int.square_root()

  length
}

/// Compares two vector's lengths, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare_length(Vec3(12, -34, 420), Vec3(2, 3, 4))
/// // -> Gt
/// ```
///
pub fn compare_length(a: Vec3(Int), with b: Vec3(Int)) -> Order {
  int.compare(a |> length_squared(), b |> length_squared())
}

/// Returns the squared distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> distance_squared(Vec3(2, 3, 4))
/// // -> 174_525
/// ```
///
pub fn distance_squared(a: Vec3(Int), with b: Vec3(Int)) -> Int {
  a |> vec3.map2(b, int.subtract) |> length_squared()
}

/// Returns the distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> distance(Vec3(2, 3, 4))
/// // -> 417.76
/// ```
///
pub fn distance(a: Vec3(Int), with b: Vec3(Int)) -> Float {
  let assert Ok(distance) = distance_squared(a, b) |> int.square_root()

  distance
}

/// Compares two vector's distances to a vector, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare_distance(Vec3(12, -34, 420), Vec3(2, 3, 4), Vec3(-25, 67, 194))
/// // -> Gt
/// ```
///
pub fn compare_distance(
  a: Vec3(Int),
  with b: Vec3(Int),
  to vector: Vec3(Int),
) -> Order {
  int.compare(a |> distance_squared(vector), b |> distance_squared(vector))
}

/// Returns a new vector containing the elements multiplies by `scalar`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> scale(2)
/// // -> Vec3(24, -68, 840)
/// ```
///
pub fn scale(vector: Vec3(Int), by scalar: Int) -> Vec3(Int) {
  vector |> vec3.map(int.multiply(_, scalar))
}

/// Returns the cross product of two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> cross(Vec3(2, 3, 4))
/// // -> Vec3(-1396, 792, 104)
/// ```
///
pub fn cross(a: Vec3(Int), b: Vec3(Int)) -> Vec3(Int) {
  Vec3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)
}

/// Returns the dot product of two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> dot(Vec3(2, 3, 4))
/// // -> 1602
/// ```
///
pub fn dot(a: Vec3(Int), b: Vec3(Int)) -> Int {
  a |> multiply(b) |> vec3.to_list() |> int.sum()
}

/// Returns the projection of a vector on another vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> project(Vec3(2, 3, 4))
/// // -> Vec3(110, 165, 220)
/// ```
///
pub fn project(a: Vec3(Int), on b: Vec3(Int)) -> Vec3(Int) {
  b |> scale(dot(a, b) / dot(b, b))
}

/// Returns a new vector resulting from sliding this vector along a plane
/// defined by the given normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> slide(Vec3(2, 3, 4))
/// // -> Vec3(-98, -199, 200)
/// ```
///
pub fn slide(a: Vec3(Int), on b: Vec3(Int)) -> Vec3(Int) {
  a |> subtract(a |> project(b))
}

/// Returns the reflection of a vector through a plane defined by the given
/// normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> reflect(Vec3(2, 3, 4))
/// // -> Vec3(208, 364, 20)
/// ```
///
pub fn reflect(vector: Vec3(Int), through normal: Vec3(Int)) -> Vec3(Int) {
  vector |> project(normal) |> scale(2) |> subtract(vector)
}

/// Returns the mirror of a vector through a plane defined by the given normal
/// vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420) |> mirror(Vec3(2, 3, 4))
/// // -> Vec3(-208, -364, -20)
/// ```
///
pub fn mirror(vector: Vec3(Int), through normal: Vec3(Int)) -> Vec3(Int) {
  vector |> reflect(normal) |> negate()
}

/// Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(12, -34, 420)
/// |> anchor_position(Vec3(20, 40, 0), scale(_, 2))
/// // -> Vec3(4, -108, 840)
/// ```
///
pub fn anchor_position(
  vector: Vec3(Int),
  at position: Vec3(Int),
  then fun: fn(Vec3(Int)) -> Vec3(Int),
) -> Vec3(Int) {
  vector |> subtract(position) |> fun() |> add(position)
}
