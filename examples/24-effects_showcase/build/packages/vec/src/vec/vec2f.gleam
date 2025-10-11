import gleam/float
import gleam/list
import gleam/order.{type Order}
import vec/internal
import vec/vec2.{type Vec2, Vec2}

/// Zero vector, a vector with all components set to `0.0`.
///
pub const zero = Vec2(0.0, 0.0)

/// One vector, a vector with all components set to `1.0`.
///
pub const one = Vec2(1.0, 1.0)

/// Returns a new vector with all components clamped between a lower and upper
/// bound.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> clamp(Vec2(1.0, 2.1), Vec2(1.4, 18.2))
/// // -> Vec2(1.2, 2.1)
/// ```
///
pub fn clamp(
  vector: Vec2(Float),
  start_bound: Vec2(Float),
  stop_bound: Vec2(Float),
) -> Vec2(Float) {
  Vec2(
    float.clamp(vector.x, start_bound.x, stop_bound.x),
    float.clamp(vector.y, start_bound.y, stop_bound.y),
  )
}

/// Checks for equality of two vectors within a tolerance, returning an `Bool`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4)
/// |> loosely_equals(Vec2(1.25, -3.43), tolerating: 0.1)
/// // -> True
/// ```
///
pub fn loosely_equals(
  a: Vec2(Float),
  with b: Vec2(Float),
  tolerating tolerance: Float,
) -> Bool {
  case a |> vec2.map2(b, fn(a, b) { float.loosely_equals(a, b, tolerance) }) {
    Vec2(True, True) -> True
    _ -> False
  }
}

/// Compares two vectors, returning the smaller of the two.
///
/// ## Examples
///
/// ```gleam
/// min(Vec2(1.2, -3.4), Vec2(1.0, 2.1))
/// // -> Vec2(1.0, -3.4)
/// ```
///
pub fn min(a: Vec2(Float), b: Vec2(Float)) -> Vec2(Float) {
  a |> vec2.map2(b, float.min)
}

/// Compares two vectors, returning the larger of the two.
///
/// ## Examples
///
/// ```gleam
/// max(Vec2(1.2, -3.4), Vec2(1.4, -9.3))
/// // -> Vec2(1.4, -3.4)
/// ```
///
pub fn max(a: Vec2(Float), b: Vec2(Float)) -> Vec2(Float) {
  a |> vec2.map2(b, float.max)
}

/// Returns a new vector with all elements rounded to the next highest whole
/// number as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.6) |> ceiling()
/// // -> Vec2(2.0, -3.0)
/// ```
///
pub fn ceiling(vector: Vec2(Float)) -> Vec2(Float) {
  vector |> vec2.map(float.ceiling)
}

/// Returns a new vector with all elements rounded to the next lowest whole
/// number as an `Float`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.6) |> floor()
/// // -> Vec2(1.0, -4.0)
/// ```
///
pub fn floor(vector: Vec2(Float)) -> Vec2(Float) {
  vector |> vec2.map(float.floor)
}

/// Returns a new vector with all elements rounded to the nearest whole number
/// as an `Int`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.6) |> round()
/// // -> Vec2(1, -4)
/// ```
///
pub fn round(vector: Vec2(Float)) -> Vec2(Int) {
  vector |> vec2.map(float.round)
}

/// Returns a new vector with all elements truncated as an `Int`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2323232827383238, -3.656565) |> truncate()
/// // -> Vec2(1, -3)
/// ```
///
pub fn truncate(vector: Vec2(Float)) -> Vec2(Int) {
  vector |> vec2.map(float.truncate)
}

/// Returns a new vector with all elements converted to a given precision.
///
/// ## Examples
///
/// ```gleam
/// Vec2(2.43434348473, -3.656565) |> to_precision(2)
/// // -> Vec2(2.43, -3.66)
/// ```
///
/// ```gleam
/// Vec2(547_890.453444, -3.656565) |> to_precision(-3)
/// // -> Vec2(548_000.0, 0.0)
/// ```
///
pub fn to_precision(vector: Vec2(Float), precision: Int) -> Vec2(Float) {
  vector |> vec2.map(float.to_precision(_, precision))
}

/// Returns a new vector with all elements in absolute values.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> absolute_value()
/// // -> Vec2(1.2, 3.4)
/// ```
///
pub fn absolute_value(vector: Vec2(Float)) -> Vec2(Float) {
  vector |> vec2.map(float.absolute_value)
}

/// Returns a new vector with all elements negated.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> negate()
/// // -> Vec2(-1.2, 3.4)
/// ```
///
pub fn negate(vector: Vec2(Float)) -> Vec2(Float) {
  vector |> vec2.map(float.negate)
}

/// Sums a list of vectors.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec2(1.2, -3.4),
///   Vec2(2.1, 4.5),
///   Vec2(3.3, 0.0),
/// ]
/// |> sum()
/// // -> Vec2(6.6, 1.1)
/// ```
///
pub fn sum(vectors: List(Vec2(Float))) -> Vec2(Float) {
  vectors |> list.fold(vec2.splat(0.0), add)
}

/// Multiplies a list of vectors and returns the product.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec2(1.2, -3.4),
///   Vec2(2.1, -1.0),
///   Vec2(3.2, 2.0),
/// ]
/// |> product()
/// // -> Vec2(8.064, 6.8)
/// ```
///
pub fn product(vectors: List(Vec2(Float))) -> Vec2(Float) {
  vectors |> list.fold(vec2.splat(1.0), multiply)
}

/// Returns the modulo of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(13.3, 13.3) |> modulo(Vec2(3.3, -3.3))
/// // -> Ok(Vec2(0.1, -3.2))
/// ```
///
/// ```gleam
/// Vec2(-13.3, -13.3) |> modulo(Vec2(3.3, -3.3))
/// // -> Ok(Vec2(3.2, -0.1))
/// ```
///
pub fn modulo(
  dividend: Vec2(Float),
  by divisor: Vec2(Float),
) -> Result(Vec2(Float), Nil) {
  dividend |> vec2.map2(divisor, float.modulo) |> vec2.result()
}

/// Returns division of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> divide(Vec2(2.0, 0.5))
/// // -> Ok(Vec2(0.6, -6.8))
/// ```
///
/// ```gleam
/// Vec2(1.2, -3.4) |> divide(Vec2(0.0, 0.5))
/// // -> Error(Nil)
/// ```
///
pub fn divide(
  dividend: Vec2(Float),
  by divisor: Vec2(Float),
) -> Result(Vec2(Float), Nil) {
  dividend |> vec2.map2(divisor, float.divide) |> vec2.result()
}

/// Adds two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> add(Vec2(2.1, 4.5))
/// // -> Vec2(3.3, 1.1)
/// ```
///
pub fn add(a: Vec2(Float), b: Vec2(Float)) -> Vec2(Float) {
  a |> vec2.map2(b, float.add)
}

/// Multiplies two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> multiply(Vec2(2.1, -1.0))
/// // -> Vec2(2.52, 3.4)
/// ```
///
pub fn multiply(a: Vec2(Float), b: Vec2(Float)) -> Vec2(Float) {
  a |> vec2.map2(b, float.multiply)
}

/// Subtracts one vector from another.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> subtract(Vec2(0.7, -4.5))
/// // -> Vec2(0.5, 1.1)
/// ```
///
pub fn subtract(a: Vec2(Float), b: Vec2(Float)) -> Vec2(Float) {
  a |> vec2.map2(b, float.subtract)
}

/// Returns the squared length (squared magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> length_squared()
/// // -> 13.0
/// ```
///
pub fn length_squared(vector: Vec2(Float)) -> Float {
  vector
  |> vec2.to_list()
  |> list.map(fn(element) { element *. element })
  |> float.sum()
}

/// Returns the length (magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> length()
/// // -> 3.61
/// ```
///
pub fn length(vector: Vec2(Float)) -> Float {
  let assert Ok(length) = vector |> length_squared() |> float.square_root()

  length
}

/// Compares two vector's lengths, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare_length(Vec2(1.2, -3.4), Vec2(1.0, 2.1))
/// // -> Gt
/// ```
///
pub fn compare_length(a: Vec2(Float), with b: Vec2(Float)) -> Order {
  float.compare(a |> length_squared(), b |> length_squared())
}

/// Compares two vector's lengths within a tolerance, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// loosely_compare_length(Vec2(1.2, -3.4), Vec2(-1.25, 3.43), tolerating: 0.5)
/// // -> Eq
/// ```
///
pub fn loosely_compare_length(
  a: Vec2(Float),
  with b: Vec2(Float),
  tolerating tolerance: Float,
) -> Order {
  float.loosely_compare(a |> length_squared(), b |> length_squared(), tolerance)
}

/// Returns the squared distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> distance_squared(Vec2(1.0, 2.1))
/// // -> 30.29
/// ```
///
pub fn distance_squared(a: Vec2(Float), with b: Vec2(Float)) -> Float {
  a |> vec2.map2(b, float.subtract) |> length_squared()
}

/// Returns the distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> distance(Vec2(1.0, 2.1))
/// // -> 5.5
/// ```
///
pub fn distance(a: Vec2(Float), with b: Vec2(Float)) -> Float {
  let assert Ok(distance) = distance_squared(a, b) |> float.square_root()

  distance
}

/// Compares two vector's distances to a vector, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare_distance(Vec2(1.2, -3.4), Vec2(1.0, 2.1), Vec2(-2.5, 6.7))
/// // -> Gt
/// ```
///
pub fn compare_distance(
  a: Vec2(Float),
  with b: Vec2(Float),
  to vector: Vec2(Float),
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
///   Vec2(1.2, -3.4),
///   Vec2(1.25, -3.43),
///   Vec2(-2.5, 6.7),
///   tolerating: 1.0,
/// )
/// // -> Eq
/// ```
///
pub fn loosely_compare_distance(
  a: Vec2(Float),
  with b: Vec2(Float),
  to vector: Vec2(Float),
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
/// Vec2(1.2, -3.4) |> scale(2.5)
/// // -> Vec2(3.0, -8.5)
/// ```
///
pub fn scale(vector: Vec2(Float), by scalar: Float) -> Vec2(Float) {
  vector |> vec2.map(float.multiply(_, scalar))
}

/// Normalize the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> normalize()
/// // -> Vec2(0.33, -0.94)
/// ```
///
pub fn normalize(vector: Vec2(Float)) -> Vec2(Float) {
  vector |> scale(1.0 /. { vector |> length() })
}

/// Returns a normalized vector pointing from `a` to `b`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> direction(Vec2(1.0, 2.1))
/// // -> Vec2(-0.04, 1.0)
/// ```
///
pub fn direction(a: Vec2(Float), to b: Vec2(Float)) -> Vec2(Float) {
  b |> subtract(a) |> normalize()
}

/// Returns the cross product of two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> cross(Vec2(1.0, 2.1))
/// // -> 5.92
/// ```
///
pub fn cross(a: Vec2(Float), b: Vec2(Float)) -> Float {
  a.x *. b.y -. b.x *. a.y
}

/// Returns the dot product of two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> dot(Vec2(1.0, 2.1))
/// // -> -5.94
/// ```
///
pub fn dot(a: Vec2(Float), b: Vec2(Float)) -> Float {
  a |> multiply(b) |> vec2.to_list() |> float.sum()
}

/// Returns the projection of a vector on another vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> project(Vec2(1.0, 2.1))
/// // -> Vec2(-1.1, -2.31)
/// ```
///
pub fn project(a: Vec2(Float), on b: Vec2(Float)) -> Vec2(Float) {
  b |> scale(dot(a, b) /. dot(b, b))
}

/// Returns a new vector resulting from sliding this vector along a plane
/// defined by the given normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> slide(Vec2(1.0, 2.1))
/// // -> Vec2(2.3, -1.09)
/// ```
///
pub fn slide(a: Vec2(Float), on b: Vec2(Float)) -> Vec2(Float) {
  a |> subtract(a |> project(b))
}

/// Returns the reflection of a vector through a plane defined by the given
/// normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> reflect(Vec2(1.0, 2.1))
/// // -> Vec2(-3.4, -1.21)
/// ```
///
pub fn reflect(vector: Vec2(Float), through normal: Vec2(Float)) -> Vec2(Float) {
  vector |> project(normal) |> scale(2.0) |> subtract(vector)
}

/// Returns the mirror of a vector through a plane defined by the given normal
/// vector.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> mirror(Vec2(1.0, 2.1))
/// // -> Vec2(3.34, 1.21)
/// ```
///
pub fn mirror(vector: Vec2(Float), through normal: Vec2(Float)) -> Vec2(Float) {
  vector |> reflect(normal) |> negate()
}

/// Returns the angle (in radians) between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> angle(Vec2(1.0, 2.1))
/// // -> 2.36
/// ```
///
pub fn angle(a: Vec2(Float), b: Vec2(Float)) -> Float {
  let assert Ok(angle) =
    dot(normalize(a), normalize(b)) |> float.clamp(-1.0, 1.0) |> internal.acos()

  angle
}

/// Rotate a vector by an angle (in radians).
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4) |> rotate(maths.pi() *. 0.25)
/// // -> Vec2(3.25, -1.56)
/// ```
///
pub fn rotate(vector: Vec2(Float), by angle: Float) -> Vec2(Float) {
  let cos_angle = internal.cos(angle)
  let sin_angle = internal.sin(angle)

  Vec2(
    vector.x *. cos_angle -. vector.y *. sin_angle,
    vector.x *. sin_angle +. vector.y *. cos_angle,
  )
}

/// Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4)
/// |> anchor_position(
///   Vec2(1.0, 2.1),
///   rotate(_, maths.pi() *. 0.25),
/// )
/// // -> Vec2(5.03, -1.65)
/// ```
///
pub fn anchor_position(
  vector: Vec2(Float),
  at position: Vec2(Float),
  then fun: fn(Vec2(Float)) -> Vec2(Float),
) -> Vec2(Float) {
  vector |> subtract(position) |> fun() |> add(position)
}

/// Return the equivalent of `vector |> rotate(float.negate(angle)) |> fun() |> rotate(angle)`.
///
/// ## Examples
///
/// ```gleam
/// Vec2(1.2, -3.4)
/// |> anchor_rotation(
///   maths.pi() *. 0.25,
///   add(_, Vec2(6.0, 9.0)),
/// )
/// // -> Vec2(-0.92, 7.21)
/// ```
///
pub fn anchor_rotation(
  vector: Vec2(Float),
  at angle: Float,
  then fun: fn(Vec2(Float)) -> Vec2(Float),
) -> Vec2(Float) {
  vector |> rotate(float.negate(angle)) |> fun() |> rotate(angle)
}
