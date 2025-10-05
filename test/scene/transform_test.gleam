import tiramisu/math/vec3
import tiramisu/scene

// Test: identity transform
pub fn identity_transform_test() {
  let t = scene.identity_transform()
  assert t.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

// Test: transform_at helper
pub fn transform_at_test() {
  let t = scene.transform_at(1.0, 2.0, 3.0)
  assert t.position == vec3.Vec3(1.0, 2.0, 3.0)
  assert t.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

// Test: set_position helper
pub fn set_position_test() {
  let t = scene.identity_transform()
  let new_pos = vec3.Vec3(5.0, 10.0, 15.0)
  let updated = scene.set_position(t, new_pos)

  assert updated.position == new_pos
  assert updated.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert updated.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

// Test: set_rotation helper
pub fn set_rotation_test() {
  let t = scene.identity_transform()
  let new_rot = vec3.Vec3(1.5, 0.5, 2.0)
  let updated = scene.set_rotation(t, new_rot)

  assert updated.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert updated.rotation == new_rot
  assert updated.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

// Test: set_scale helper
pub fn set_scale_test() {
  let t = scene.identity_transform()
  let new_scale = vec3.Vec3(2.0, 3.0, 4.0)
  let updated = scene.set_scale(t, new_scale)

  assert updated.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert updated.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert updated.scale == new_scale
}

// Test: chaining transform updates
pub fn chaining_test() {
  let t =
    scene.identity_transform()
    |> scene.set_position(vec3.Vec3(1.0, 2.0, 3.0))
    |> scene.set_rotation(vec3.Vec3(0.5, 1.0, 1.5))
    |> scene.set_scale(vec3.Vec3(2.0, 2.0, 2.0))

  assert t.position == vec3.Vec3(1.0, 2.0, 3.0)
  assert t.rotation == vec3.Vec3(0.5, 1.0, 1.5)
  assert t.scale == vec3.Vec3(2.0, 2.0, 2.0)
}

// Test: immutability - original unchanged after updates
pub fn immutability_test() {
  let original = scene.identity_transform()
  let _updated = scene.set_position(original, vec3.Vec3(10.0, 20.0, 30.0))

  // Original should be unchanged
  assert original.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert original.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert original.scale == vec3.Vec3(1.0, 1.0, 1.0)
}
