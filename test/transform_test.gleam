import gleam/float
import gleam/option
import gleam_community/maths
import quaternion
import tiramisu/transform
import vec/vec3

pub fn identity_test() {
  let t = transform.identity

  assert transform.position(t) == vec3.Vec3(0.0, 0.0, 0.0)
  assert transform.rotation(t) == vec3.Vec3(0.0, 0.0, 0.0)
  assert transform.scale(t) == vec3.Vec3(1.0, 1.0, 1.0)
}

pub fn at_test() {
  let pos = vec3.Vec3(5.0, 10.0, 15.0)
  let t = transform.at(pos)

  assert transform.position(t) == pos
  assert transform.rotation(t) == vec3.Vec3(0.0, 0.0, 0.0)
  assert transform.scale(t) == vec3.Vec3(1.0, 1.0, 1.0)
}

pub fn with_euler_rotation_test() {
  let rot = vec3.Vec3(1.0, 2.0, 3.0)
  let t = transform.with_euler_rotation(transform.identity, rot)

  assert transform.position(t) == vec3.Vec3(0.0, 0.0, 0.0)
  // Compare quaternions instead of Euler angles to avoid representation ambiguity
  let expected_quat = quaternion.from_euler(rot)
  let actual_quat = transform.rotation_quaternion(t)
  assert float.loosely_equals(
    expected_quat.x,
    actual_quat.x,
    tolerating: 0.0001,
  )
  assert float.loosely_equals(
    expected_quat.y,
    actual_quat.y,
    tolerating: 0.0001,
  )
  assert float.loosely_equals(
    expected_quat.z,
    actual_quat.z,
    tolerating: 0.0001,
  )
  assert float.loosely_equals(
    expected_quat.w,
    actual_quat.w,
    tolerating: 0.0001,
  )
  assert transform.scale(t) == vec3.Vec3(1.0, 1.0, 1.0)
}

pub fn with_scale_test() {
  let scale = vec3.Vec3(2.0, 3.0, 4.0)
  let t = transform.with_scale(transform.identity, scale)

  assert transform.position(t) == vec3.Vec3(0.0, 0.0, 0.0)
  assert transform.rotation(t) == vec3.Vec3(0.0, 0.0, 0.0)
  assert transform.scale(t) == scale
}

pub fn set_position_test() {
  let pos = vec3.Vec3(1.0, 2.0, 3.0)
  let t = transform.with_position(transform.identity, pos)

  assert transform.position(t) == pos
}

pub fn set_scale_test() {
  let scale = vec3.Vec3(0.5, 2.0, 3.5)
  let t = transform.with_scale(transform.identity, scale)

  assert transform.scale(t) == scale
}

pub fn translate_test() {
  let t =
    transform.identity
    |> transform.translate(by: vec3.Vec3(1.0, 2.0, 3.0))
    |> transform.translate(by: vec3.Vec3(4.0, 5.0, 6.0))

  assert transform.position(t) == vec3.Vec3(5.0, 7.0, 9.0)
}

pub fn rotate_test() {
  // Test that rotate_by correctly composes rotations using quaternions
  let t =
    transform.identity
    |> transform.rotate_by(vec3.Vec3(0.0, maths.pi() /. 2.0, 0.0))
    // 90° around Y
    |> transform.rotate_by(vec3.Vec3(0.0, maths.pi() /. 2.0, 0.0))
  // Another 90° around Y

  // After two 90° rotations around Y, we should have a 180° Y rotation
  // Compare quaternions since Euler angles can have multiple representations
  let expected_quat = quaternion.from_euler(vec3.Vec3(0.0, maths.pi(), 0.0))
  let actual_quat = transform.rotation_quaternion(t)

  // Allow some tolerance for floating point math
  assert float.loosely_equals(expected_quat.x, actual_quat.x, tolerating: 0.01)
  assert float.loosely_equals(expected_quat.y, actual_quat.y, tolerating: 0.01)
  assert float.loosely_equals(expected_quat.z, actual_quat.z, tolerating: 0.01)
  assert float.loosely_equals(expected_quat.w, actual_quat.w, tolerating: 0.01)
}

pub fn scale_by_test() {
  let t =
    transform.identity
    |> transform.scale_by(vec3.Vec3(2.0, 3.0, 4.0))
    |> transform.scale_by(vec3.Vec3(0.5, 2.0, 0.25))

  assert transform.scale(t) == vec3.Vec3(1.0, 6.0, 1.0)
}

pub fn builder_pattern_test() {
  let t =
    transform.identity
    |> transform.with_position(vec3.Vec3(10.0, 20.0, 30.0))
    |> transform.with_quaternion_rotation(quaternion.Quaternion(
      0.0,
      0.0,
      0.0,
      1.0,
    ))
    |> transform.with_scale(vec3.Vec3(2.0, 2.0, 2.0))

  assert transform.position(t) == vec3.Vec3(10.0, 20.0, 30.0)
  assert quaternion.loosely_equals(
    transform.rotation_quaternion(t),
    quaternion.Quaternion(x: 0.0, y: 0.0, z: 0.0, w: 1.0),
    tolerating: 0.01,
  )
  assert transform.scale(t) == vec3.Vec3(2.0, 2.0, 2.0)
}

// ============================================================================
// Lerp Tests
// ============================================================================

pub fn lerp_position_test() {
  let from = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
  let to = transform.at(position: vec3.Vec3(10.0, 20.0, 30.0))

  let halfway = transform.lerp(from, to: to, with: 0.5)
  let pos = transform.position(halfway)

  assert float.loosely_equals(pos.x, 5.0, tolerating: 0.001)
  assert float.loosely_equals(pos.y, 10.0, tolerating: 0.001)
  assert float.loosely_equals(pos.z, 15.0, tolerating: 0.001)
}

pub fn lerp_at_zero_test() {
  let from = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
  let to = transform.at(position: vec3.Vec3(10.0, 20.0, 30.0))

  let result = transform.lerp(from, to: to, with: 0.0)
  assert transform.position(result) == vec3.Vec3(0.0, 0.0, 0.0)
}

pub fn lerp_at_one_test() {
  let from = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
  let to = transform.at(position: vec3.Vec3(10.0, 20.0, 30.0))

  let result = transform.lerp(from, to: to, with: 1.0)
  assert transform.position(result) == vec3.Vec3(10.0, 20.0, 30.0)
}

pub fn lerp_scale_test() {
  let from =
    transform.identity
    |> transform.with_scale(vec3.Vec3(1.0, 1.0, 1.0))
  let to =
    transform.identity
    |> transform.with_scale(vec3.Vec3(3.0, 5.0, 7.0))

  let halfway = transform.lerp(from, to: to, with: 0.5)
  let scale = transform.scale(halfway)

  assert float.loosely_equals(scale.x, 2.0, tolerating: 0.001)
  assert float.loosely_equals(scale.y, 3.0, tolerating: 0.001)
  assert float.loosely_equals(scale.z, 4.0, tolerating: 0.001)
}

// ============================================================================
// Compose Tests
// ============================================================================

pub fn compose_positions_test() {
  let first = transform.at(position: vec3.Vec3(5.0, 0.0, 0.0))
  let second = transform.at(position: vec3.Vec3(0.0, 3.0, 0.0))

  let composed = transform.compose(first, second)
  let pos = transform.position(composed)

  assert float.loosely_equals(pos.x, 5.0, tolerating: 0.001)
  assert float.loosely_equals(pos.y, 3.0, tolerating: 0.001)
  assert float.loosely_equals(pos.z, 0.0, tolerating: 0.001)
}

pub fn compose_scales_test() {
  let first =
    transform.identity
    |> transform.with_scale(vec3.Vec3(2.0, 3.0, 4.0))
  let second =
    transform.identity
    |> transform.with_scale(vec3.Vec3(0.5, 2.0, 0.25))

  let composed = transform.compose(first, second)
  let scale = transform.scale(composed)

  assert float.loosely_equals(scale.x, 1.0, tolerating: 0.001)
  assert float.loosely_equals(scale.y, 6.0, tolerating: 0.001)
  assert float.loosely_equals(scale.z, 1.0, tolerating: 0.001)
}

pub fn compose_with_identity_test() {
  let t = transform.at(position: vec3.Vec3(5.0, 10.0, 15.0))
  let composed = transform.compose(t, transform.identity)

  assert transform.position(composed) == vec3.Vec3(5.0, 10.0, 15.0)
  assert transform.scale(composed) == vec3.Vec3(1.0, 1.0, 1.0)
}

// ============================================================================
// Scale Uniform Tests
// ============================================================================

pub fn scale_uniform_test() {
  let t =
    transform.identity
    |> transform.scale_uniform(2.0)

  assert transform.scale(t) == vec3.Vec3(2.0, 2.0, 2.0)
}

pub fn scale_uniform_from_existing_test() {
  let t =
    transform.identity
    |> transform.with_scale(vec3.Vec3(3.0, 4.0, 5.0))
    |> transform.scale_uniform(0.5)

  // scale_uniform replaces scale, doesn't multiply
  assert transform.scale(t) == vec3.Vec3(0.5, 0.5, 0.5)
}

// ============================================================================
// Rotate X/Y/Z Tests
// ============================================================================

pub fn rotate_x_test() {
  let t =
    transform.identity
    |> transform.rotate_x(maths.pi() /. 2.0)

  // 90 degrees around X axis
  let expected_quat =
    quaternion.from_euler(vec3.Vec3(maths.pi() /. 2.0, 0.0, 0.0))
  let actual_quat = transform.rotation_quaternion(t)

  assert quaternion.loosely_equals(expected_quat, actual_quat, tolerating: 0.01)
}

pub fn rotate_y_test() {
  let t =
    transform.identity
    |> transform.rotate_y(maths.pi() /. 2.0)

  // 90 degrees around Y axis
  let expected_quat =
    quaternion.from_euler(vec3.Vec3(0.0, maths.pi() /. 2.0, 0.0))
  let actual_quat = transform.rotation_quaternion(t)

  assert quaternion.loosely_equals(expected_quat, actual_quat, tolerating: 0.01)
}

pub fn rotate_z_test() {
  let t =
    transform.identity
    |> transform.rotate_z(maths.pi() /. 2.0)

  // 90 degrees around Z axis
  let expected_quat =
    quaternion.from_euler(vec3.Vec3(0.0, 0.0, maths.pi() /. 2.0))
  let actual_quat = transform.rotation_quaternion(t)

  assert quaternion.loosely_equals(expected_quat, actual_quat, tolerating: 0.01)
}

// ============================================================================
// Look At Tests
// ============================================================================

pub fn look_at_basic_test() {
  let from = transform.at(position: vec3.Vec3(0.0, 0.0, 10.0))
  let to = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))

  let result = transform.look_at(from: from, to: to, up: option.None)

  // Position should be preserved from the 'from' transform
  assert transform.position(result) == vec3.Vec3(0.0, 0.0, 10.0)

  // Scale should be preserved
  assert transform.scale(result) == vec3.Vec3(1.0, 1.0, 1.0)
}

pub fn look_at_preserves_position_test() {
  let from =
    transform.at(position: vec3.Vec3(5.0, 10.0, 15.0))
    |> transform.with_scale(vec3.Vec3(2.0, 2.0, 2.0))
  let to = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))

  let result = transform.look_at(from: from, to: to, up: option.None)

  // Position should be preserved
  assert transform.position(result) == vec3.Vec3(5.0, 10.0, 15.0)

  // Scale should be preserved
  assert transform.scale(result) == vec3.Vec3(2.0, 2.0, 2.0)
}
