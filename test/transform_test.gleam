import gleam/float
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
