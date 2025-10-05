import gleam/float
import tiramisu/math/vec3

// Test: zero vector
pub fn zero_test() {
  let v = vec3.zero()
  assert v.x == 0.0
  assert v.y == 0.0
  assert v.z == 0.0
}

// Test: one vector
pub fn one_test() {
  let v = vec3.one()
  assert v.x == 1.0
  assert v.y == 1.0
  assert v.z == 1.0
}

// Test: vector addition
pub fn add_test() {
  let a = vec3.Vec3(1.0, 2.0, 3.0)
  let b = vec3.Vec3(4.0, 5.0, 6.0)
  let result = vec3.add(a, b)
  assert result.x == 5.0
  assert result.y == 7.0
  assert result.z == 9.0
}

// Test: vector subtraction
pub fn subtract_test() {
  let a = vec3.Vec3(5.0, 7.0, 9.0)
  let b = vec3.Vec3(1.0, 2.0, 3.0)
  let result = vec3.subtract(a, b)
  assert result.x == 4.0
  assert result.y == 5.0
  assert result.z == 6.0
}

// Test: vector scaling
pub fn scale_test() {
  let v = vec3.Vec3(1.0, 2.0, 3.0)
  let result = vec3.scale(v, 2.0)
  assert result.x == 2.0
  assert result.y == 4.0
  assert result.z == 6.0
}

// Test: magnitude calculation
pub fn magnitude_test() {
  let v = vec3.Vec3(3.0, 4.0, 0.0)
  let mag = vec3.magnitude(v)
  assert mag == 5.0
}

// Test: magnitude of unit vector
pub fn magnitude_unit_test() {
  let v = vec3.Vec3(1.0, 0.0, 0.0)
  let mag = vec3.magnitude(v)
  assert mag == 1.0
}

// Test: normalization
pub fn normalize_test() {
  let v = vec3.Vec3(3.0, 4.0, 0.0)
  let normalized = vec3.normalize(v)
  // Allow small floating point error
  assert float.absolute_value(normalized.x -. 0.6) <. 0.0001
  assert float.absolute_value(normalized.y -. 0.8) <. 0.0001
  assert normalized.z == 0.0
}

// Test: normalize zero vector
pub fn normalize_zero_test() {
  let v = vec3.zero()
  let normalized = vec3.normalize(v)
  assert normalized.x == 0.0
  assert normalized.y == 0.0
  assert normalized.z == 0.0
}

// Test: normalized vector has magnitude 1
pub fn normalized_magnitude_test() {
  let v = vec3.Vec3(5.0, 12.0, 84.0)
  let normalized = vec3.normalize(v)
  let mag = vec3.magnitude(normalized)
  // Allow small floating point error
  let diff = float.absolute_value(mag -. 1.0)
  assert diff <. 0.0001
}

// Test: dot product
pub fn dot_product_test() {
  let a = vec3.Vec3(1.0, 2.0, 3.0)
  let b = vec3.Vec3(4.0, 5.0, 6.0)
  let result = vec3.dot(a, b)
  // 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
  assert result == 32.0
}

// Test: dot product of perpendicular vectors is 0
pub fn dot_product_perpendicular_test() {
  let a = vec3.Vec3(1.0, 0.0, 0.0)
  let b = vec3.Vec3(0.0, 1.0, 0.0)
  let result = vec3.dot(a, b)
  assert result == 0.0
}

// Test: cross product
pub fn cross_product_test() {
  let a = vec3.Vec3(1.0, 0.0, 0.0)
  let b = vec3.Vec3(0.0, 1.0, 0.0)
  let result = vec3.cross(a, b)
  // i x j = k
  assert result.x == 0.0
  assert result.y == 0.0
  assert result.z == 1.0
}

// Test: cross product anti-commutative
pub fn cross_product_anticommutative_test() {
  let a = vec3.Vec3(1.0, 2.0, 3.0)
  let b = vec3.Vec3(4.0, 5.0, 6.0)
  let ab = vec3.cross(a, b)
  let ba = vec3.cross(b, a)
  // a x b = -(b x a)
  assert ab.x == 0.0 -. ba.x
  assert ab.y == 0.0 -. ba.y
  assert ab.z == 0.0 -. ba.z
}

// Test: distance
pub fn distance_test() {
  let a = vec3.Vec3(0.0, 0.0, 0.0)
  let b = vec3.Vec3(3.0, 4.0, 0.0)
  let dist = vec3.distance(a, b)
  assert dist == 5.0
}

// Test: distance is symmetric
pub fn distance_symmetric_test() {
  let a = vec3.Vec3(1.0, 2.0, 3.0)
  let b = vec3.Vec3(4.0, 6.0, 8.0)
  let dist_ab = vec3.distance(a, b)
  let dist_ba = vec3.distance(b, a)
  assert dist_ab == dist_ba
}

// Test: lerp at t=0 returns first vector
pub fn lerp_zero_test() {
  let a = vec3.Vec3(1.0, 2.0, 3.0)
  let b = vec3.Vec3(4.0, 5.0, 6.0)
  let result = vec3.lerp(a, b, 0.0)
  assert result == a
}

// Test: lerp at t=1 returns second vector
pub fn lerp_one_test() {
  let a = vec3.Vec3(1.0, 2.0, 3.0)
  let b = vec3.Vec3(4.0, 5.0, 6.0)
  let result = vec3.lerp(a, b, 1.0)
  assert result == b
}

// Test: lerp at t=0.5 returns midpoint
pub fn lerp_half_test() {
  let a = vec3.Vec3(0.0, 0.0, 0.0)
  let b = vec3.Vec3(10.0, 20.0, 30.0)
  let result = vec3.lerp(a, b, 0.5)
  assert result.x == 5.0
  assert result.y == 10.0
  assert result.z == 15.0
}

// Test: angle between same vectors is 0
pub fn angle_same_test() {
  let v = vec3.Vec3(1.0, 2.0, 3.0)
  let assert Ok(angle) = vec3.angle(v, v)
  assert angle == 0.0
}

// Test: angle between perpendicular vectors
pub fn angle_perpendicular_test() {
  let a = vec3.Vec3(1.0, 0.0, 0.0)
  let b = vec3.Vec3(0.0, 1.0, 0.0)
  let assert Ok(angle) = vec3.angle(a, b)
  // Should be π/2 radians (90 degrees)
  let pi = 3.141592653589793
  let diff = float.absolute_value(angle -. pi /. 2.0)
  assert diff <. 0.0001
}

// Test: angle between opposite vectors
pub fn angle_opposite_test() {
  let a = vec3.Vec3(1.0, 0.0, 0.0)
  let b = vec3.Vec3(-1.0, 0.0, 0.0)
  let assert Ok(angle) = vec3.angle(a, b)
  // Should be π radians (180 degrees)
  let pi = 3.141592653589793
  let diff = float.absolute_value(angle -. pi)
  assert diff <. 0.0001
}
