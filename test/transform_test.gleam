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

pub fn with_rotation_test() {
  let rot = vec3.Vec3(1.0, 2.0, 3.0)
  let t = transform.with_rotation(transform.identity, rot)

  assert transform.position(t) == vec3.Vec3(0.0, 0.0, 0.0)
  assert transform.rotation(t) == rot
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

pub fn set_rotation_test() {
  let rot = vec3.Vec3(0.5, 1.0, 1.5)
  let t = transform.with_rotation(transform.identity, rot)

  assert transform.rotation(t) == rot
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
    |> transform.rotate_by(vec3.Vec3(0.0, 1.57, 0.0))  // 90° around Y
    |> transform.rotate_by(vec3.Vec3(0.0, 1.57, 0.0))  // Another 90° around Y

  // After two 90° rotations around Y, we should be at ~180° (3.14 radians)
  let epsilon = 0.01
  let rotation = transform.rotation(t)
  assert rotation.y >. 3.14 -. epsilon
  assert rotation.y <. 3.14 +. epsilon
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
    |> transform.with_rotation(vec3.Vec3(0.5, 1.0, 1.5))
    |> transform.with_scale(vec3.Vec3(2.0, 2.0, 2.0))

  assert transform.position(t) == vec3.Vec3(10.0, 20.0, 30.0)
  assert transform.rotation(t) == vec3.Vec3(0.5, 1.0, 1.5)
  assert transform.scale(t) == vec3.Vec3(2.0, 2.0, 2.0)
}

pub fn chaining_test() {
  let t =
    transform.at(vec3.Vec3(1.0, 2.0, 3.0))
    |> transform.translate(by: vec3.Vec3(5.0, 5.0, 5.0))
    |> transform.with_rotation(vec3.Vec3(0.1, 0.2, 0.3))
    |> transform.scale_by(vec3.Vec3(2.0, 2.0, 2.0))

  // Position should be summed
  assert transform.position(t) == vec3.Vec3(6.0, 7.0, 8.0)

  // Rotation should match what we set (not rotate_by, so no composition)
  // Use epsilon for floating point precision in quaternion<->Euler conversion
  let epsilon = 0.0001
  let rotation = transform.rotation(t)
  assert rotation.x >. 0.1 -. epsilon
  assert rotation.x <. 0.1 +. epsilon
  assert rotation.y >. 0.2 -. epsilon
  assert rotation.y <. 0.2 +. epsilon
  assert rotation.z >. 0.3 -. epsilon
  assert rotation.z <. 0.3 +. epsilon

  // Scale should be multiplied
  assert transform.scale(t) == vec3.Vec3(2.0, 2.0, 2.0)
}
