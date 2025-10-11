import gleam/float
import gleam/list
import gleam/order.{type Order}
import vec/internal
import vec/vec4.{type Vec4, Vec4}

/// Zero vector, a vector with all components set to `0.0`.
///
pub const zero = Vec4(0.0, 0.0, 0.0, 0.0)

/// One vector, a vector with all components set to `1.0`.
///
pub const one = Vec4(1.0, 1.0, 1.0, 1.0)

/// Returns a new vector with all components clamped between a lower and upper
/// bound.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> clamp(
///   Vec4(1.0, 2.1, -5.4, 7.5),
///   Vec4(1.4, 18.2, 32.3, 9.1),
/// )
/// // -> Vec4(1.2, 2.1, 32.3, 7.5)
/// ```
///
pub fn clamp(
  vector: Vec4(Float),
  start_bound: Vec4(Float),
  stop_bound: Vec4(Float),
) -> Vec4(Float) {
  Vec4(
    float.clamp(vector.x, start_bound.x, stop_bound.x),
    float.clamp(vector.y, start_bound.y, stop_bound.y),
    float.clamp(vector.z, start_bound.z, stop_bound.z),
    float.clamp(vector.w, start_bound.w, stop_bound.w),
  )
}

/// Checks for equality of two vectors within a tolerance, returning an `Bool`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69)
/// |> loosely_equals(Vec4(1.25, -3.43, 42.0001, 0.6999), tolerating: 0.1)
/// // -> True
/// ```
///
pub fn loosely_equals(
  a: Vec4(Float),
  with b: Vec4(Float),
  tolerating tolerance: Float,
) -> Bool {
  case a |> vec4.map2(b, fn(a, b) { float.loosely_equals(a, b, tolerance) }) {
    Vec4(True, True, True, True) -> True
    _ -> False
  }
}

/// Compares two vectors, returning the smaller of the two.
///
/// ## Examples
///
/// ```gleam
/// min(Vec4(1.2, -3.4, 42.0, 0.69), Vec4(1.0, 2.1, -5.4, 7.5))
/// // -> Vec4(1.0, -3.4, -5.4, 0.69)
/// ```
///
pub fn min(a: Vec4(Float), b: Vec4(Float)) -> Vec4(Float) {
  a |> vec4.map2(b, float.min)
}

/// Compares two vectors, returning the larger of the two.
///
/// ## Examples
///
/// ```gleam
/// max(Vec4(1.2, -3.4, 42.0, 0.69), Vec4(1.4, -9.3, 32.3, 9.1))
/// // -> Vec4(1.4, -3.4, 42.0, 9.1)
/// ```
///
pub fn max(a: Vec4(Float), b: Vec4(Float)) -> Vec4(Float) {
  a |> vec4.map2(b, float.max)
}

/// Returns a new vector with all elements rounded to the next highest whole
/// number as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.6, 42.0, 0.5) |> ceiling()
/// // -> Vec4(2.0, -3.0, 42.0, 1.0)
/// ```
///
pub fn ceiling(vector: Vec4(Float)) -> Vec4(Float) {
  vector |> vec4.map(float.ceiling)
}

/// Returns a new vector with all elements rounded to the next lowest whole
/// number as an `Float`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.6, 42.0, 0.5) |> floor()
/// // -> Vec4(1.0, -4.0, 42.0, 0.0)
/// ```
///
pub fn floor(vector: Vec4(Float)) -> Vec4(Float) {
  vector |> vec4.map(float.floor)
}

/// Returns a new vector with all elements rounded to the nearest whole number
/// as an `Int`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.6, 42.0, 0.5) |> round()
/// // -> Vec4(1, -4, 42, 1)
/// ```
///
pub fn round(vector: Vec4(Float)) -> Vec4(Int) {
  vector |> vec4.map(float.round)
}

/// Returns a new vector with all elements truncated as an `Int`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2323232827383238, -3.656565, 42.0, 0.5) |> truncate()
/// // -> Vec4(1, -3, 42, 0)
/// ```
///
pub fn truncate(vector: Vec4(Float)) -> Vec4(Int) {
  vector |> vec4.map(float.truncate)
}

/// Returns a new vector with all elements converted to a given precision.
///
/// ## Examples
///
/// ```gleam
/// Vec4(2.43434348473, -3.656565, 42.0, 0.5) |> to_precision(2)
/// // -> Vec4(2.43, -3.66, 42.0, 0.5)
/// ```
///
/// ```gleam
/// Vec4(547_890.453444, -3.656565, 42.0, 0.5) |> to_precision(-3)
/// // -> Vec4(548_000.0, 0.0, 0.0, 0.0)
/// ```
///
pub fn to_precision(vector: Vec4(Float), precision: Int) -> Vec4(Float) {
  vector |> vec4.map(float.to_precision(_, precision))
}

/// Returns a new vector with all elements in absolute values.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> absolute_value()
/// // -> Vec4(1.2, 3.4, 42.0, 0.69)
/// ```
///
pub fn absolute_value(vector: Vec4(Float)) -> Vec4(Float) {
  vector |> vec4.map(float.absolute_value)
}

/// Returns a new vector with all elements negated.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> negate()
/// // -> Vec4(-1.2, 3.4, -42.0, -0.69)
/// ```
///
pub fn negate(vector: Vec4(Float)) -> Vec4(Float) {
  vector |> vec4.map(float.negate)
}

/// Sums a list of vectors.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec4(1.2, -3.4, 42.0, 0.69),
///   Vec4(2.1, 4.5, -2.0, 9.01),
///   Vec4(3.3, 0.0, -20.0, 0.3),
/// ]
/// |> sum()
/// // -> Vec4(6.6, 1.1, 20.0, 10.0)
/// ```
///
pub fn sum(vectors: List(Vec4(Float))) -> Vec4(Float) {
  vectors |> list.fold(vec4.splat(0.0), add)
}

/// Multiplies a list of vectors and returns the product.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec4(1.2, -3.4, 42.0, 0.69),
///   Vec4(2.1, -1.0, 999.9, 2.0),
///   Vec4(3.2, 2.0, 0.0, 0.5),
/// ]
/// |> product()
/// // -> Vec4(8.064, 6.8, 0.0, 0.69)
/// ```
///
pub fn product(vectors: List(Vec4(Float))) -> Vec4(Float) {
  vectors |> list.fold(vec4.splat(1.0), multiply)
}

/// Returns the modulo of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(13.3, -13.3, 13.3, -13.3) |> modulo(Vec4(3.3, 3.3, -3.3, -3.3))
/// // -> Ok(Vec4(0.1, 3.2, -3.2, -0.1))
/// ```
///
pub fn modulo(
  dividend: Vec4(Float),
  by divisor: Vec4(Float),
) -> Result(Vec4(Float), Nil) {
  dividend |> vec4.map2(divisor, float.modulo) |> vec4.result()
}

/// Returns division of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> divide(Vec4(2.0, 0.5, 4.0, 1.0))
/// // -> Ok(Vec4(0.6, -6.8, 10.5, 0.69))
/// ```
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> divide(Vec4(0.0, 0.5, 4.0, 1.0))
/// // -> Error(Nil)
/// ```
///
pub fn divide(
  dividend: Vec4(Float),
  by divisor: Vec4(Float),
) -> Result(Vec4(Float), Nil) {
  dividend |> vec4.map2(divisor, float.divide) |> vec4.result()
}

/// Adds two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> add(Vec4(2.1, 4.5, -2.0, 9.01))
/// // -> Vec4(3.3, 1.1, 40.0, 9.7)
/// ```
///
pub fn add(a: Vec4(Float), b: Vec4(Float)) -> Vec4(Float) {
  a |> vec4.map2(b, float.add)
}

/// Multiplies two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> multiply(Vec4(2.1, -1.0, 0.0, 1.0))
/// // -> Vec4(2.52, 3.4, 0.0, 0.69)
/// ```
///
pub fn multiply(a: Vec4(Float), b: Vec4(Float)) -> Vec4(Float) {
  a |> vec4.map2(b, float.multiply)
}

/// Subtracts one vector from another.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> subtract(Vec4(0.7, -4.5, 2.0, 1.39))
/// // -> Vec4(0.5, 1.1, 40.0, -0.7)
/// ```
///
pub fn subtract(a: Vec4(Float), b: Vec4(Float)) -> Vec4(Float) {
  a |> vec4.map2(b, float.subtract)
}

/// Returns the squared length (squared magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> length_squared()
/// // -> 1777.47
/// ```
///
pub fn length_squared(vector: Vec4(Float)) -> Float {
  vector
  |> vec4.to_list()
  |> list.map(fn(element) { element *. element })
  |> float.sum()
}

/// Returns the length (magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> length()
/// // -> 42.16
/// ```
///
pub fn length(vector: Vec4(Float)) -> Float {
  let assert Ok(length) = vector |> length_squared() |> float.square_root()

  length
}

/// Compares two vector's lengths, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare_length(
///   Vec4(1.2, -3.4, 42.0, 0.69),
///   Vec4(1.0, 2.1, 3.2, 4.3),
/// )
/// // -> Gt
/// ```
///
pub fn compare_length(a: Vec4(Float), with b: Vec4(Float)) -> Order {
  float.compare(a |> length_squared(), b |> length_squared())
}

/// Compares two vector's lengths within a tolerance, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// loosely_compare_length(
///   Vec4(1.2, -3.4, 42.0, 0.69),
///   Vec4(-1.25, 3.43, -42.0001, -0.6999),
///   tolerating: 0.5,
/// )
/// // -> Eq
/// ```
///
pub fn loosely_compare_length(
  a: Vec4(Float),
  with b: Vec4(Float),
  tolerating tolerance: Float,
) -> Order {
  float.loosely_compare(a |> length_squared(), b |> length_squared(), tolerance)
}

/// Returns the squared distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> distance_squared(Vec4(1.0, 2.1, 3.2, 4.3))
/// // -> 1548.76
/// ```
///
pub fn distance_squared(a: Vec4(Float), b: Vec4(Float)) -> Float {
  a |> vec4.map2(b, float.subtract) |> length_squared()
}

/// Returns the distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> distance(Vec4(1.0, 2.1, 3.2, 4.3))
/// // -> 39.35
/// ```
///
pub fn distance(a: Vec4(Float), with b: Vec4(Float)) -> Float {
  let assert Ok(distance) = distance_squared(a, b) |> float.square_root()

  distance
}

/// Compares two vector's distances to a vector, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare_distance(
///   Vec4(1.2, -3.4, 42.0, 0.69),
///   Vec4(1.0, 2.1, 3.2, 4.3),
///   Vec4(-2.5, 6.7, 19.4, 0.0),
/// )
/// // -> Gt
/// ```
///
pub fn compare_distance(
  a: Vec4(Float),
  with b: Vec4(Float),
  to vector: Vec4(Float),
) -> Order {
  float.compare(a |> distance_squared(vector), b |> distance_squared(vector))
}

/// Compares two vector's distances to a vector within a tolerance, returning
/// an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// loosely_compare_distance(
///   Vec4(1.2, -3.4, 42.0, 0.69),
///   Vec4(1.25, -3.43, 42.0001, 0.6999),
///   Vec4(-2.5, 6.7, 19.4, 0.0),
///   tolerating: 1.0,
/// )
/// // -> Eq
/// ```
///
pub fn loosely_compare_distance(
  a: Vec4(Float),
  with b: Vec4(Float),
  to vector: Vec4(Float),
  tolerating tolerance: Float,
) -> Order {
  float.loosely_compare(
    a |> distance_squared(vector),
    b |> distance_squared(vector),
    tolerance,
  )
}

/// Returns a new vector containing the elements multiplies by `scalar`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> scale(2.5)
/// // -> Vec4(3.0, -8.5, 105.0, 1.72)
/// ```
///
pub fn scale(vector: Vec4(Float), by scalar: Float) -> Vec4(Float) {
  vector |> vec4.map(float.multiply(_, scalar))
}

/// Normalize the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> normalize()
/// // -> Vec4(0.03, -0.08, 1.0, 0.02)
/// ```
///
pub fn normalize(vector: Vec4(Float)) -> Vec4(Float) {
  vector |> scale(1.0 /. { vector |> length() })
}

/// Returns a normalized vector pointing from `a` to `b`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> direction(Vec4(1.0, 2.1, 3.2, 4.3))
/// // -> Vec4(-0.0, 0.14, -0.99, 0.092)
/// ```
///
pub fn direction(a: Vec4(Float), to b: Vec4(Float)) -> Vec4(Float) {
  b |> subtract(a) |> normalize()
}

/// Returns the dot product of two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> dot(Vec4(1.0, 2.1, 3.2, 4.3))
/// // -> 131.43
/// ```
///
pub fn dot(a: Vec4(Float), b: Vec4(Float)) -> Float {
  a |> multiply(b) |> vec4.to_list() |> float.sum()
}

/// Returns the projection of a vector on another vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> project(Vec4(1.0, 2.1, 3.2, 4.3))
/// // -> Vec4(3.85, 8.08, 12.32, 16.55)
/// ```
///
pub fn project(a: Vec4(Float), on b: Vec4(Float)) -> Vec4(Float) {
  b |> scale(dot(a, b) /. dot(b, b))
}

/// Returns a new vector resulting from sliding this vector along a plane
/// defined by the given normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> slide(Vec4(1.0, 2.1, 3.2, 4.3))
/// // -> Vec4(-2.65, -11.48, 29.68, -15.86)
/// ```
///
pub fn slide(a: Vec4(Float), on b: Vec4(Float)) -> Vec4(Float) {
  a |> subtract(a |> project(b))
}

/// Returns the reflection of a vector through a plane defined by the given
/// normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> reflect(Vec4(1.0, 2.1, 3.2, 4.3))
/// // -> Vec4(6.5, 19.57, -17.36, 32.42)
/// ```
///
pub fn reflect(vector: Vec4(Float), through normal: Vec4(Float)) -> Vec4(Float) {
  vector |> project(normal) |> scale(2.0) |> subtract(vector)
}

/// Returns the mirror of a vector through a plane defined by the given normal
/// vector.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> mirror(Vec4(1.0, 2.1, 3.2, 4.3))
/// // -> Vec4(-6.5, -19.57, 17.36, -32.42)
/// ```
///
pub fn mirror(vector: Vec4(Float), through normal: Vec4(Float)) -> Vec4(Float) {
  vector |> reflect(normal) |> negate()
}

/// Returns the angle (in radians) between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69) |> angle(Vec4(1.0, 2.1, 3.2, 4.3))
/// // -> 1.0
/// ```
///
pub fn angle(a: Vec4(Float), b: Vec4(Float)) -> Float {
  let assert Ok(angle) =
    dot(normalize(a), normalize(b)) |> float.clamp(-1.0, 1.0) |> internal.acos()

  angle
}

/// Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
///
/// ## Examples
///
/// ```gleam
/// Vec4(1.2, -3.4, 42.0, 0.69)
/// |> anchor_position(Vec4(1.0, 2.1, 3.2, 4.3), scale(_, 2.0))
/// // -> Vec4(1.4, -8.9, 80.8, -2.92)
/// ```
///
pub fn anchor_position(
  vector: Vec4(Float),
  at position: Vec4(Float),
  then fun: fn(Vec4(Float)) -> Vec4(Float),
) -> Vec4(Float) {
  vector |> subtract(position) |> fun() |> add(position)
}
