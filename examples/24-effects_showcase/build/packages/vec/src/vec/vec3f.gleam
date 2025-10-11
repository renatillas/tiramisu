import gleam/float
import gleam/list
import gleam/order.{type Order}
import vec/internal
import vec/vec3.{type Vec3, Vec3}

/// Zero vector, a vector with all components set to `0.0`.
///
pub const zero = Vec3(0.0, 0.0, 0.0)

/// One vector, a vector with all components set to `1.0`.
///
pub const one = Vec3(1.0, 1.0, 1.0)

/// Returns a new vector with all components clamped between a lower and upper
/// bound.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> clamp(
///   Vec3(1.0, 2.1, -5.4),
///   Vec3(1.4, 18.2, 32.3),
/// )
/// // -> Vec3(1.2, 2.1, 32.3)
/// ```
///
pub fn clamp(
  vector: Vec3(Float),
  start_bound: Vec3(Float),
  stop_bound: Vec3(Float),
) -> Vec3(Float) {
  Vec3(
    float.clamp(vector.x, start_bound.x, stop_bound.x),
    float.clamp(vector.y, start_bound.y, stop_bound.y),
    float.clamp(vector.z, start_bound.z, stop_bound.z),
  )
}

/// Checks for equality of two vectors within a tolerance, returning an `Bool`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0)
/// |> loosely_equals(Vec3(1.25, -3.43, 42.0001), tolerating: 0.1)
/// // -> True
/// ```
///
pub fn loosely_equals(
  a: Vec3(Float),
  with b: Vec3(Float),
  tolerating tolerance: Float,
) -> Bool {
  case a |> vec3.map2(b, fn(a, b) { float.loosely_equals(a, b, tolerance) }) {
    Vec3(True, True, True) -> True
    _ -> False
  }
}

/// Compares two vectors, returning the smaller of the two.
///
/// ## Examples
///
/// ```gleam
/// min(Vec3(1.2, -3.4, 42.0), Vec3(1.0, 2.1, -5.4))
/// // -> Vec3(1.0, -3.4, -5.4)
/// ```
///
pub fn min(a: Vec3(Float), b: Vec3(Float)) -> Vec3(Float) {
  a |> vec3.map2(b, float.min)
}

/// Compares two vectors, returning the larger of the two.
///
/// ## Examples
///
/// ```gleam
/// max(Vec3(1.2, -3.4, 42.0), Vec3(1.4, -9.3, 32.3))
/// // -> Vec3(1.4, -3.4, 42.0)
/// ```
///
pub fn max(a: Vec3(Float), b: Vec3(Float)) -> Vec3(Float) {
  a |> vec3.map2(b, float.max)
}

/// Returns a new vector with all elements rounded to the next highest whole
/// number as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.6, 42.0) |> ceiling()
/// // -> Vec3(2.0, -3.0, 42.0)
/// ```
///
pub fn ceiling(vector: Vec3(Float)) -> Vec3(Float) {
  vector |> vec3.map(float.ceiling)
}

/// Returns a new vector with all elements rounded to the next lowest whole
/// number as an `Float`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.6, 42.0) |> floor()
/// // -> Vec3(1.0, -4.0, 42.0)
/// ```
///
pub fn floor(vector: Vec3(Float)) -> Vec3(Float) {
  vector |> vec3.map(float.floor)
}

/// Returns a new vector with all elements rounded to the nearest whole number
/// as an `Int`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.6, 42.0) |> round()
/// // -> Vec3(1, -4, 42)
/// ```
///
pub fn round(vector: Vec3(Float)) -> Vec3(Int) {
  vector |> vec3.map(float.round)
}

/// Returns a new vector with all elements truncated as an `Int`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2323232827383238, -3.656565, 42.0) |> truncate()
/// // -> Vec3(1, -3, 42)
/// ```
///
pub fn truncate(vector: Vec3(Float)) -> Vec3(Int) {
  vector |> vec3.map(float.truncate)
}

/// Returns a new vector with all elements converted to a given precision.
///
/// ## Examples
///
/// ```gleam
/// Vec3(2.43434348473, -3.656565, 42.0) |> to_precision(2)
/// // -> Vec3(2.43, -3.66, 42.0)
/// ```
///
/// ```gleam
/// Vec3(547_890.453444, -3.656565, 42.0) |> to_precision(-3)
/// // -> Vec3(548_000.0, 0.0, 0.0)
/// ```
///
pub fn to_precision(vector: Vec3(Float), precision: Int) -> Vec3(Float) {
  vector |> vec3.map(float.to_precision(_, precision))
}

/// Returns a new vector with all elements in absolute values.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> absolute_value()
/// // -> Vec3(1.2, 3.4, 42.0)
/// ```
///
pub fn absolute_value(vector: Vec3(Float)) -> Vec3(Float) {
  vector |> vec3.map(float.absolute_value)
}

/// Returns a new vector with all elements negated.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> negate()
/// // -> Vec3(-1.2, 3.4, -42.0)
/// ```
///
pub fn negate(vector: Vec3(Float)) -> Vec3(Float) {
  vector |> vec3.map(float.negate)
}

/// Sums a list of vectors.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec3(1.2, -3.4, 42.0),
///   Vec3(2.1, 4.5, -2.0),
///   Vec3(3.3, 0.0, -20.0),
/// ]
/// |> sum()
/// // -> Vec3(6.6, 1.1, 20.0)
/// ```
///
pub fn sum(vectors: List(Vec3(Float))) -> Vec3(Float) {
  vectors |> list.fold(vec3.splat(0.0), add)
}

/// Multiplies a list of vectors and returns the product.
///
/// ## Examples
///
/// ```gleam
/// [
///   Vec3(1.2, -3.4, 42.0),
///   Vec3(2.1, -1.0, 999.9),
///   Vec3(3.2, 2.0, 0.0),
/// ]
/// |> product()
/// // -> Vec3(8.064, 6.8, 0.0)
/// ```
///
pub fn product(vectors: List(Vec3(Float))) -> Vec3(Float) {
  vectors |> list.fold(vec3.splat(1.0), multiply)
}

/// Returns the modulo of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(13.3, -13.3, 13.3) |> modulo(Vec3(3.3, 3.3, -3.3))
/// // -> Ok(Vec3(0.1, 3.2, -3.2))
/// ```
///
pub fn modulo(
  dividend: Vec3(Float),
  by divisor: Vec3(Float),
) -> Result(Vec3(Float), Nil) {
  dividend |> vec3.map2(divisor, float.modulo) |> vec3.result()
}

/// Returns division of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> divide(Vec3(2.0, 0.5, 4.0))
/// // -> Ok(Vec3(0.6, -6.8, 10.5))
/// ```
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> divide(Vec3(0.0, 0.5, 4.0))
/// // -> Error(Nil)
/// ```
///
pub fn divide(
  dividend: Vec3(Float),
  by divisor: Vec3(Float),
) -> Result(Vec3(Float), Nil) {
  dividend |> vec3.map2(divisor, float.divide) |> vec3.result()
}

/// Adds two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> add(Vec3(2.1, 4.5, -2.0))
/// // -> Vec3(3.3, 1.1, 40.0)
/// ```
///
pub fn add(a: Vec3(Float), b: Vec3(Float)) -> Vec3(Float) {
  a |> vec3.map2(b, float.add)
}

/// Multiplies two vectors together.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> multiply(Vec3(2.1, -1.0, 0.0))
/// // -> Vec3(2.52, 3.4, 0.0)
/// ```
///
pub fn multiply(a: Vec3(Float), b: Vec3(Float)) -> Vec3(Float) {
  a |> vec3.map2(b, float.multiply)
}

/// Subtracts one vector from another.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> subtract(Vec3(0.7, -4.5, 2.0))
/// // -> Vec3(0.5, 1.1, 40.0)
/// ```
///
pub fn subtract(a: Vec3(Float), b: Vec3(Float)) -> Vec3(Float) {
  a |> vec3.map2(b, float.subtract)
}

/// Returns the squared length (squared magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> length_squared()
/// // -> 1777.0
/// ```
///
pub fn length_squared(vector: Vec3(Float)) -> Float {
  vector
  |> vec3.to_list()
  |> list.map(fn(element) { element *. element })
  |> float.sum()
}

/// Returns the length (magnitude) of the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> length()
/// // -> 42.15
/// ```
///
pub fn length(vector: Vec3(Float)) -> Float {
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
///   Vec3(1.2, -3.4, 42.0),
///   Vec3(1.0, 2.1, 3.2),
/// )
/// // -> Gt
/// ```
///
pub fn compare_length(a: Vec3(Float), with b: Vec3(Float)) -> Order {
  float.compare(a |> length_squared(), b |> length_squared())
}

/// Compares two vector's lengths within a tolerance, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// loosely_compare_length(
///   Vec3(1.2, -3.4, 42.0),
///   Vec3(-1.25, 3.43, -42.0001),
///   tolerating: 0.5,
/// )
/// // -> Eq
/// ```
///
pub fn loosely_compare_length(
  a: Vec3(Float),
  with b: Vec3(Float),
  tolerating tolerance: Float,
) -> Order {
  float.loosely_compare(a |> length_squared(), b |> length_squared(), tolerance)
}

/// Returns the squared distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> distance_squared(Vec3(1.0, 2.1, 3.2))
/// // -> 1535.73
/// ```
///
pub fn distance_squared(a: Vec3(Float), with b: Vec3(Float)) -> Float {
  a |> vec3.map2(b, float.subtract) |> length_squared()
}

/// Returns the distance between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> distance(Vec3(1.0, 2.1, 3.2))
/// // -> 39.19
/// ```
///
pub fn distance(a: Vec3(Float), with b: Vec3(Float)) -> Float {
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
///   Vec3(1.2, -3.4, 42.0),
///   Vec3(1.0, 2.1, 3.2),
///   Vec3(-2.5, 6.7, 19.4),
/// )
/// // -> Gt
/// ```
///
pub fn compare_distance(
  a: Vec3(Float),
  with b: Vec3(Float),
  to vector: Vec3(Float),
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
///   Vec3(1.2, -3.4, 42.0),
///   Vec3(1.25, -3.43, 42.0001),
///   Vec3(-2.5, 6.7, 19.4),
///   tolerating: 1.0,
/// )
/// // -> Eq
/// ```
///
pub fn loosely_compare_distance(
  a: Vec3(Float),
  with b: Vec3(Float),
  to vector: Vec3(Float),
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
/// Vec3(1.2, -3.4, 42.0) |> scale(2.5)
/// // -> Vec3(3.0, -8.5, 105.0)
/// ```
///
pub fn scale(vector: Vec3(Float), by scalar: Float) -> Vec3(Float) {
  vector |> vec3.map(float.multiply(_, scalar))
}

/// Normalize the vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> normalize()
/// // -> Vec3(0.03, -0.08, 1.0)
/// ```
///
pub fn normalize(vector: Vec3(Float)) -> Vec3(Float) {
  vector |> scale(1.0 /. { vector |> length() })
}

/// Returns a normalized vector pointing from `a` to `b`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> direction(Vec3(1.0, 2.1, 3.2))
/// // -> Vec3(-0.01, 0.14, -1.0)
/// ```
///
pub fn direction(a: Vec3(Float), to b: Vec3(Float)) -> Vec3(Float) {
  b |> subtract(a) |> normalize()
}

/// Returns the cross product of two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> dot(Vec3(1.0, 2.1, 3.2))
/// // -> Vec3(-99.08, 38.16, 5.92)
/// ```
///
pub fn cross(a: Vec3(Float), b: Vec3(Float)) -> Vec3(Float) {
  Vec3(
    a.y *. b.z -. a.z *. b.y,
    a.z *. b.x -. a.x *. b.z,
    a.x *. b.y -. a.y *. b.x,
  )
}

/// Returns the dot product of two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> dot(Vec3(1.0, 2.1, 3.2))
/// // -> 128.46
/// ```
///
pub fn dot(a: Vec3(Float), b: Vec3(Float)) -> Float {
  a |> multiply(b) |> vec3.to_list() |> float.sum()
}

/// Returns the projection of a vector on another vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> project(Vec3(1.0, 2.1, 3.2))
/// // -> Vec3(8.21, 17.24, 26.27)
/// ```
///
pub fn project(a: Vec3(Float), on b: Vec3(Float)) -> Vec3(Float) {
  b |> scale(dot(a, b) /. dot(b, b))
}

/// Returns a new vector resulting from sliding this vector along a plane
/// defined by the given normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> slide(Vec3(1.0, 2.1, 3.2))
/// // -> Vec3(-7.01, -20.64, 15.73)
/// ```
///
pub fn slide(a: Vec3(Float), on b: Vec3(Float)) -> Vec3(Float) {
  a |> subtract(a |> project(b))
}

/// Returns the reflection of a vector through a plane defined by the given
/// normal vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> reflect(Vec3(1.0, 2.1, 3.2))
/// // -> Vec3(15.22, 37.87, 10.53)
/// ```
///
pub fn reflect(vector: Vec3(Float), through normal: Vec3(Float)) -> Vec3(Float) {
  vector |> project(normal) |> scale(2.0) |> subtract(vector)
}

/// Returns the mirror of a vector through a plane defined by the given normal
/// vector.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> mirror(Vec3(1.0, 2.1, 3.2))
/// // -> Vec3(-15.22, -37.87, -10.53)
/// ```
///
pub fn mirror(vector: Vec3(Float), through normal: Vec3(Float)) -> Vec3(Float) {
  vector |> reflect(normal) |> negate()
}

/// Returns the angle (in radians) between two vectors.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0) |> angle(Vec3(1.0, 2.1, 3.2))
/// // -> 0.69
/// ```
///
pub fn angle(a: Vec3(Float), b: Vec3(Float)) -> Float {
  let assert Ok(angle) =
    dot(normalize(a), normalize(b)) |> float.clamp(-1.0, 1.0) |> internal.acos()

  angle
}

/// Rotate a vector around a given axis by an angle (in radians).
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0)
/// |> rotate(around: Vec3(1.0, 2.1, 3.2), by: maths.pi() *. 0.25)
/// // -> Vec3(20.96, -4.18, 36.33)
/// ```
///
pub fn rotate(
  vector: Vec3(Float),
  around axis: Vec3(Float),
  by angle: Float,
) -> Vec3(Float) {
  let axis = axis |> normalize()

  let cos_angle = internal.cos(angle)
  let sin_angle = internal.sin(angle)

  Vec3(
    vector.x
      *. { cos_angle +. axis.x *. axis.x *. { 1.0 -. cos_angle } }
      +. vector.y
      *. { axis.x *. axis.y *. { 1.0 -. cos_angle } -. axis.z *. sin_angle }
      +. vector.z
      *. { axis.x *. axis.z *. { 1.0 -. cos_angle } +. axis.y *. sin_angle },
    vector.x
      *. { axis.y *. axis.x *. { 1.0 -. cos_angle } +. axis.z *. sin_angle }
      +. vector.y
      *. { cos_angle +. axis.y *. axis.y *. { 1.0 -. cos_angle } }
      +. vector.z
      *. { axis.y *. axis.z *. { 1.0 -. cos_angle } -. axis.x *. sin_angle },
    vector.x
      *. { axis.z *. axis.x *. { 1.0 -. cos_angle } -. axis.y *. sin_angle }
      +. vector.y
      *. { axis.z *. axis.y *. { 1.0 -. cos_angle } +. axis.x *. sin_angle }
      +. vector.z
      *. { cos_angle +. axis.z *. axis.z *. { 1.0 -. cos_angle } },
  )
}

/// Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0)
/// |> anchor_position(
///   Vec3(2.0, 4.0, 0.0),
///   rotate(_, Vec3(6.0, 9.0, 1.0), maths.pi() *. 0.25),
/// )
/// // -> Vec3(26.08, -18.35, 27.2)
/// ```
///
pub fn anchor_position(
  vector: Vec3(Float),
  at position: Vec3(Float),
  then fun: fn(Vec3(Float)) -> Vec3(Float),
) -> Vec3(Float) {
  vector |> subtract(position) |> fun() |> add(position)
}

/// Return the equivalent of `vector |> rotate(axis, float.negate(angle)) |> fun() |> rotate(axis, angle)`.
///
/// ## Examples
///
/// ```gleam
/// Vec3(1.2, -3.4, 42.0)
/// |> anchor_rotation(
///   Vec3(6.0, 9.0, 1.0),
///   maths.pi() *. 0.25,
///   add(_, Vec3(2.0, 4.0, 0.0)),
/// )
/// // -> Vec3(3.07, 0.63, 42.51)
/// ```
///
pub fn anchor_rotation(
  vector: Vec3(Float),
  around axis: Vec3(Float),
  at angle: Float,
  then fun: fn(Vec3(Float)) -> Vec3(Float),
) -> Vec3(Float) {
  vector |> rotate(axis, float.negate(angle)) |> fun() |> rotate(axis, angle)
}
