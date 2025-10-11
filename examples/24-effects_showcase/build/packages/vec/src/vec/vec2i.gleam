import gleam/int
import gleam/list
import gleam/order.{type Order}
import vec/vec2.{type Vec2, Vec2}

/// Zero vector, a vector with all components set to `0`.
///
pub const zero = Vec2(0, 0)

/// One vector, a vector with all components set to `1`.
///
pub const one = Vec2(1, 1)

/// Returns a new vector with all components clamped between a lower and upper
/// bound.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> clamp(Vec2(10, 21), Vec2(14, 18))
/// // -> Vec2(12, 21)
/// ```
///
pub fn clamp(
  vector: Vec2(Int),
  start_bound: Vec2(Int),
  stop_bound: Vec2(Int),
) -> Vec2(Int) {
  Vec2(
    int.clamp(vector.x, start_bound.x, stop_bound.x),
    int.clamp(vector.y, start_bound.y, stop_bound.y),
  )
}

/// Compares two vectors, returning the smaller of the two.
///
/// ## Examples
///
/// ```gleam
/// min(Vec2(12, -34), Vec2(10, 21))
/// // -> Vec2(10, -34)
/// ```
///
pub fn min(a: Vec2(Int), b: Vec2(Int)) -> Vec2(Int) {
  a |> vec2.map2(b, int.min)
}

/// Compares two vectors, returning the larger of the two.
///
/// ## Examples
///
/// ```gleam
/// max(Vec2(12, -34), Vec2(14, -93))
/// // -> Vec2(14, -34)
/// ```
///
pub fn max(a: Vec2(Int), b: Vec2(Int)) -> Vec2(Int) {
  a |> vec2.map2(b, int.max)
}

/// Returns a new vector with all elements in absolute values.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> absolute_value()
/// // -> Vec2(12, 34)
/// ```
///
pub fn absolute_value(vector: Vec2(Int)) -> Vec2(Int) {
  vector |> vec2.map(int.absolute_value)
}

/// Takes an int vector and returns its value as a float vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> to_vec2f()
/// // -> Vec2(12.0, -34.0)
/// ```
///
pub fn to_vec2f(vector: Vec2(Int)) -> Vec2(Float) {
  vector |> vec2.map(int.to_float)
}

/// Returns a new vector with all elements negated.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> negate()
/// // -> Vec2(-12, 34)
/// ```
///
pub fn negate(vector: Vec2(Int)) -> Vec2(Int) {
  vector |> vec2.map(int.negate)
}

/// Sums a list of vectors.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec2(12, -34),
///   Vec2(21, 45),
///   Vec2(33, 0),
/// ]
/// |> sum()
/// // -> Vec2(66, 11)
/// ```
///
pub fn sum(vectors: List(Vec2(Int))) -> Vec2(Int) {
  vectors |> list.fold(vec2.splat(0), add)
}

/// Multiplies a list of vectors and returns the product.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec2(12, -34),
///   Vec2(21, -10),
///   Vec2(32, 20),
/// ]
/// |> product()
/// // -> Vec2(8064, 6800)
/// ```
///
pub fn product(vectors: List(Vec2(Int))) -> Vec2(Int) {
  vectors |> list.fold(vec2.splat(1), multiply)
}

/// Computes the remainder of an integer vector division of inputs as a
/// `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(13, -13) |> remainder(Vec2(3, 3))
/// // -> Ok(Vec2(1, -1))
/// ```
///
/// ```gleam
/// Vec2(12, -34) |> remainder(Vec2(0, 1))
/// // -> Error(Nil)
/// ```
///
pub fn remainder(
  dividend: Vec2(Int),
  by divisor: Vec2(Int),
) -> Result(Vec2(Int), Nil) {
  dividend |> vec2.map2(divisor, int.remainder) |> vec2.result()
}

/// Returns the modulo of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(13, 13) |> modulo(Vec2(3, -3))
/// // -> Ok(Vec2(1, -2))
/// ```
///
/// ```gleam
/// Vec2(-13, -13) |> modulo(Vec2(3, -3))
/// // -> Ok(Vec2(2, -1))
/// ```
///
pub fn modulo(
  dividend: Vec2(Int),
  by divisor: Vec2(Int),
) -> Result(Vec2(Int), Nil) {
  dividend |> vec2.map2(divisor, int.modulo) |> vec2.result()
}

/// Returns division of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> divide(Vec2(2, 5))
/// // -> Ok(Vec2(6, -6))
/// ```
///
/// ```gleam
/// Vec2(12, -34) |> divide(Vec2(0, 5))
/// // -> Error(Nil)
/// ```
///
pub fn divide(
  dividend: Vec2(Int),
  by divisor: Vec2(Int),
) -> Result(Vec2(Int), Nil) {
  dividend |> vec2.map2(divisor, int.divide) |> vec2.result()
}

/// Performs a *floored* integer vector division, which means that the result
/// will always be rounded towards negative infinity.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> floor_divide(Vec2(2, 5))
/// // -> Ok(Vec2(6, -7))
/// ```
///
/// ```gleam
/// Vec2(12, -34) |> floor_divide(Vec2(0, 5))
/// // -> Error(Nil)
/// ```
///
pub fn floor_divide(
  dividend: Vec2(Int),
  by divisor: Vec2(Int),
) -> Result(Vec2(Int), Nil) {
  dividend |> vec2.map2(divisor, int.floor_divide) |> vec2.result()
}

/// Adds two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> add(Vec2(21, 45))
/// // -> Vec2(33, 11)
/// ```
///
pub fn add(a: Vec2(Int), b: Vec2(Int)) -> Vec2(Int) {
  a |> vec2.map2(b, int.add)
}

/// Multiplies two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> multiply(Vec2(2, -3))
/// // -> Vec2(24, 102)
/// ```
///
pub fn multiply(a: Vec2(Int), b: Vec2(Int)) -> Vec2(Int) {
  a |> vec2.map2(b, int.multiply)
}

/// Subtracts one vector from another.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> subtract(Vec2(7, -45))
/// // -> Vec2(5, 11)
/// ```
///
pub fn subtract(a: Vec2(Int), b: Vec2(Int)) -> Vec2(Int) {
  a |> vec2.map2(b, int.subtract)
}

/// Returns the squared length (squared magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> length_squared()
/// // -> 1300
/// ```
///
pub fn length_squared(vector: Vec2(Int)) -> Int {
  vector
  |> vec2.to_list()
  |> list.map(fn(element) { element * element })
  |> int.sum()
}

/// Returns the length (magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> length()
/// // -> 36.06
/// ```
///
pub fn length(vector: Vec2(Int)) -> Float {
  let assert Ok(length) = vector |> length_squared() |> int.square_root()

  length
}

/// Compares two vector's lengths, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare_length(Vec2(12, -34), Vec2(2, 3))
/// // -> Gt
/// ```
///
pub fn compare_length(a: Vec2(Int), with b: Vec2(Int)) -> Order {
  int.compare(a |> length_squared(), b |> length_squared())
}

/// Returns the squared distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> distance_squared(Vec2(2, 3))
/// // -> 1469
/// ```
///
pub fn distance_squared(a: Vec2(Int), with b: Vec2(Int)) -> Int {
  a |> vec2.map2(b, int.subtract) |> length_squared()
}

/// Returns the distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> distance(Vec2(2, 3))
/// // -> 38.33
/// ```
///
pub fn distance(a: Vec2(Int), with b: Vec2(Int)) -> Float {
  let assert Ok(distance) = distance_squared(a, b) |> int.square_root()

  distance
}

/// Compares two vector's distances to a vector, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare_distance(Vec2(12, -34), Vec2(2, 3), Vec2(-25, 67))
/// // -> Gt
/// ```
///
pub fn compare_distance(
  a: Vec2(Int),
  with b: Vec2(Int),
  to vector: Vec2(Int),
) -> Order {
  int.compare(a |> distance_squared(vector), b |> distance_squared(vector))
}

/// Returns a new vector containing the elements multiplies by `scalar`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> scale(2)
/// // -> Vec2(24, -68)
/// ```
///
pub fn scale(vector: Vec2(Int), by scalar: Int) -> Vec2(Int) {
  vector |> vec2.map(int.multiply(_, scalar))
}

/// Returns the cross product of two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> cross(Vec2(2, 3))
/// // -> 104
/// ```
///
pub fn cross(a: Vec2(Int), b: Vec2(Int)) -> Int {
  a.x * b.y - b.x * a.y
}

/// Returns the dot product of two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> dot(Vec2(2, 3))
/// // -> -78
/// ```
///
pub fn dot(a: Vec2(Int), b: Vec2(Int)) -> Int {
  a |> multiply(b) |> vec2.to_list() |> int.sum()
}

/// Returns the projection of a vector on another vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> project(Vec2(2, 3))
/// // -> Vec2(-12, -18)
/// ```
///
pub fn project(a: Vec2(Int), on b: Vec2(Int)) -> Vec2(Int) {
  b |> scale(dot(a, b) / dot(b, b))
}

/// Returns a new vector resulting from sliding this vector along a plane
/// defined by the given normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> slide(Vec2(2, 3))
/// // -> -16
/// ```
///
pub fn slide(a: Vec2(Int), on b: Vec2(Int)) -> Vec2(Int) {
  a |> subtract(a |> project(b))
}

/// Returns the reflection of a vector through a plane defined by the given
/// normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> reflect(Vec2(2, 3))
/// // -> Vec2(-36, -2)
/// ```
///
pub fn reflect(vector: Vec2(Int), through normal: Vec2(Int)) -> Vec2(Int) {
  vector |> project(normal) |> scale(2) |> subtract(vector)
}

/// Returns the mirror of a vector through a plane defined by the given normal
/// vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> mirror(Vec2(2, 3))
/// // -> Vec2(36, 2)
/// ```
///
pub fn mirror(vector: Vec2(Int), through normal: Vec2(Int)) -> Vec2(Int) {
  vector |> reflect(normal) |> negate()
}

/// Rotate a vector by an angle (in 90 degree steps).
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34) |> rotate(1)
/// // -> Vec2(34, 12)
/// ```
///
pub fn rotate(vector: Vec2(Int), by angle: Int) -> Vec2(Int) {
  case angle |> int.modulo(4) {
    Ok(0) -> vector
    Ok(1) -> Vec2(-vector.y, vector.x)
    Ok(2) -> Vec2(-vector.x, -vector.y)
    Ok(3) -> Vec2(vector.y, -vector.x)
    _ -> panic
  }
}

/// Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34)
/// |> anchor_position(Vec2(10, 21), rotate(_, 1))
/// // -> Vec2(65, 23)
/// ```
///
pub fn anchor_position(
  vector: Vec2(Int),
  at position: Vec2(Int),
  then fun: fn(Vec2(Int)) -> Vec2(Int),
) -> Vec2(Int) {
  vector |> subtract(position) |> fun() |> add(position)
}

/// Return the equivalent of `vector |> rotate(-angle) |> fun() |> rotate(angle)`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(12, -34)
/// |> anchor_rotation(1, add(_, Vec2(6, 9)))
/// // -> Vec2(3, -28)
/// ```
///
pub fn anchor_rotation(
  vector: Vec2(Int),
  at angle: Int,
  then fun: fn(Vec2(Int)) -> Vec2(Int),
) -> Vec2(Int) {
  vector |> rotate(-angle) |> fun() |> rotate(angle)
}
