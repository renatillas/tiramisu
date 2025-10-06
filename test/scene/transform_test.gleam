import tiramisu/transform
import tiramisu/vec3

// Test: identity transform
pub fn identity_transform_test() {
  let t = transform.identity()
  assert t.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

// Test: transform_at helper
pub fn transform_at_test() {
  let t = transform.at(position: vec3.Vec3(1.0, 2.0, 3.0))
  assert t.position == vec3.Vec3(1.0, 2.0, 3.0)
  assert t.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

// Test: set_position helper
pub fn set_position_test() {
  let t = transform.identity()
  let new_pos = vec3.Vec3(5.0, 10.0, 15.0)
  let updated = transform.set_position(t, new_pos)

  assert updated.position == new_pos
  assert updated.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert updated.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

// Test: set_rotation helper
pub fn set_rotation_test() {
  let t = transform.identity()
  let new_rot = vec3.Vec3(1.5, 0.5, 2.0)
  let updated = transform.set_rotation(t, new_rot)

  assert updated.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert updated.rotation == new_rot
  assert updated.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

// Test: set_scale helper
pub fn set_scale_test() {
  let t = transform.identity()
  let new_scale = vec3.Vec3(2.0, 3.0, 4.0)
  let updated = transform.set_scale(t, new_scale)

  assert updated.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert updated.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert updated.scale == new_scale
}

// Test: chaining transform updates
pub fn chaining_test() {
  let t =
    transform.identity()
    |> transform.set_position(vec3.Vec3(1.0, 2.0, 3.0))
    |> transform.set_rotation(vec3.Vec3(0.5, 1.0, 1.5))
    |> transform.set_scale(vec3.Vec3(2.0, 2.0, 2.0))

  assert t.position == vec3.Vec3(1.0, 2.0, 3.0)
  assert t.rotation == vec3.Vec3(0.5, 1.0, 1.5)
  assert t.scale == vec3.Vec3(2.0, 2.0, 2.0)
}

// Test: immutability - original unchanged after updates
pub fn immutability_test() {
  let original = transform.identity()
  let _updated = transform.set_position(original, vec3.Vec3(10.0, 20.0, 30.0))

  // Original should be unchanged
  assert original.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert original.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert original.scale == vec3.Vec3(1.0, 1.0, 1.0)
}
