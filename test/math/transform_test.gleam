import gleam/float
import gleam/list
import gleam_community/maths
import tiramisu/transform
import vec/vec3
import vec/vec3f

// Test: identity transform
pub fn identity_test() {
  let t = transform.identity()
  assert t.position == vec3f.zero
  assert t.rotation == vec3f.zero
  assert t.scale == vec3f.one
}

// Test: transform at position
pub fn at_test() {
  let pos = vec3.Vec3(1.0, 2.0, 3.0)
  let t = transform.at(position: pos)
  assert t.position == pos
  assert t.rotation == vec3f.zero
  assert t.scale == vec3f.one
}

// Test: set position
pub fn set_position_test() {
  let t = transform.identity()
  let new_pos = vec3.Vec3(5.0, 6.0, 7.0)
  let updated = transform.set_position(t, new_pos)
  assert updated.position == new_pos
  assert updated.rotation == vec3f.zero
  assert updated.scale == vec3f.one
}

// Test: set rotation
pub fn set_rotation_test() {
  let t = transform.identity()
  let new_rot = vec3.Vec3(0.5, 1.0, 1.5)
  let updated = transform.set_rotation(t, new_rot)
  assert updated.position == vec3f.zero
  assert updated.rotation == new_rot
  assert updated.scale == vec3f.one
}

// Test: set scale
pub fn set_scale_test() {
  let t = transform.identity()
  let new_scale = vec3.Vec3(2.0, 3.0, 4.0)
  let updated = transform.set_scale(t, new_scale)
  assert updated.position == vec3f.zero
  assert updated.rotation == vec3f.zero
  assert updated.scale == new_scale
}

// Test: lerp at t=0 returns first transform
pub fn lerp_zero_test() {
  let a = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
  let b = transform.at(position: vec3.Vec3(10.0, 10.0, 10.0))
  let result = transform.lerp(a, to: b, with: 0.0)
  assert result.position == a.position
}

// Test: lerp at t=1 returns second transform
pub fn lerp_one_test() {
  let a = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
  let b = transform.at(position: vec3.Vec3(10.0, 10.0, 10.0))
  let result = transform.lerp(a, to: b, with: 1.0)
  assert result.position == b.position
}

// Test: lerp at t=0.5 returns midpoint
pub fn lerp_half_test() {
  let a = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
  let b = transform.at(position: vec3.Vec3(10.0, 20.0, 30.0))
  let result = transform.lerp(a, to: b, with: 0.5)
  assert result.position.x == 5.0
  assert result.position.y == 10.0
  assert result.position.z == 15.0
}

// Test: compose transforms - position addition
pub fn compose_position_test() {
  let t1 = transform.at(position: vec3.Vec3(1.0, 2.0, 3.0))
  let t2 = transform.at(position: vec3.Vec3(4.0, 5.0, 6.0))
  let result = transform.compose(t1, t2)
  assert result.position.x == 5.0
  assert result.position.y == 7.0
  assert result.position.z == 9.0
}

// Test: compose transforms - scale multiplication
pub fn compose_scale_test() {
  let t1 =
    transform.identity()
    |> transform.set_scale(vec3.Vec3(2.0, 3.0, 4.0))
  let t2 =
    transform.identity()
    |> transform.set_scale(vec3.Vec3(5.0, 6.0, 7.0))
  let result = transform.compose(t1, t2)
  assert result.scale.x == 10.0
  assert result.scale.y == 18.0
  assert result.scale.z == 28.0
}

// Test: look_at facing forward (+Z)
pub fn look_at_forward_test() {
  let from = vec3.Vec3(0.0, 0.0, 0.0)
  let to = vec3.Vec3(0.0, 0.0, 10.0)
  let t = transform.look_at(from: from, to: to)

  // Looking straight forward should have minimal rotation
  assert t.position == from
  assert float.absolute_value(t.rotation.x) <. 0.0001
  // No pitch
  assert float.absolute_value(t.rotation.y) <. 0.0001
  // No yaw
  assert t.rotation.z == 0.0
  // No roll
}

// Test: look_at facing backward (-Z)
pub fn look_at_backward_test() {
  let from = vec3.Vec3(0.0, 0.0, 0.0)
  let to = vec3.Vec3(0.0, 0.0, -10.0)
  let t = transform.look_at(from: from, to: to)

  let pi = maths.pi()
  assert t.position == from
  assert float.absolute_value(t.rotation.x) <. 0.0001
  // No pitch
  // Yaw should be approximately π or -π
  let yaw_diff =
    float.min(
      float.absolute_value(t.rotation.y -. pi),
      float.absolute_value(t.rotation.y +. pi),
    )
  assert yaw_diff <. 0.0001
  assert t.rotation.z == 0.0
  // No roll
}

// Test: look_at facing right (+X)
pub fn look_at_right_test() {
  let from = vec3.Vec3(0.0, 0.0, 0.0)
  let to = vec3.Vec3(10.0, 0.0, 0.0)
  let t = transform.look_at(from: from, to: to)

  let pi = maths.pi()
  assert t.position == from
  assert float.absolute_value(t.rotation.x) <. 0.0001
  // No pitch
  // Yaw should be approximately π/2
  assert float.absolute_value(t.rotation.y -. pi /. 2.0) <. 0.0001
  assert t.rotation.z == 0.0
  // No roll
}

// Test: look_at facing left (-X)
pub fn look_at_left_test() {
  let from = vec3.Vec3(0.0, 0.0, 0.0)
  let to = vec3.Vec3(-10.0, 0.0, 0.0)
  let t = transform.look_at(from: from, to: to)

  let pi = maths.pi()
  assert t.position == from
  assert float.absolute_value(t.rotation.x) <. 0.0001
  // No pitch
  // Yaw should be approximately -π/2
  assert float.absolute_value(t.rotation.y +. pi /. 2.0) <. 0.0001
  assert t.rotation.z == 0.0
  // No roll
}

// Test: look_at facing up (+Y)
pub fn look_at_up_test() {
  let from = vec3.Vec3(0.0, 0.0, 0.0)
  let to = vec3.Vec3(0.0, 10.0, 0.0)
  let t = transform.look_at(from: from, to: to)

  let pi = maths.pi()
  assert t.position == from
  // Pitch should be approximately π/2 (looking straight up)
  assert float.absolute_value(t.rotation.x -. pi /. 2.0) <. 0.0001
  assert t.rotation.z == 0.0
  // No roll
}

// Test: look_at facing down (-Y)
pub fn look_at_down_test() {
  let from = vec3.Vec3(0.0, 0.0, 0.0)
  let to = vec3.Vec3(0.0, -10.0, 0.0)
  let t = transform.look_at(from: from, to: to)

  let pi = maths.pi()
  assert t.position == from
  // Pitch should be approximately -π/2 (looking straight down)
  assert float.absolute_value(t.rotation.x +. pi /. 2.0) <. 0.0001
  assert t.rotation.z == 0.0
  // No roll
}

// Test: look_at diagonal direction
pub fn look_at_diagonal_test() {
  let from = vec3.Vec3(0.0, 0.0, 0.0)
  let to = vec3.Vec3(10.0, 10.0, 10.0)
  let t = transform.look_at(from: from, to: to)

  assert t.position == from
  // Should have both pitch and yaw (not straight in any axis)
  assert float.absolute_value(t.rotation.x) >. 0.01
  // Has pitch
  assert float.absolute_value(t.rotation.y) >. 0.01
  // Has yaw
  assert t.rotation.z == 0.0
  // No roll
}

// Test: look_at from non-origin position
pub fn look_at_offset_test() {
  let from = vec3.Vec3(5.0, 5.0, 5.0)
  let to = vec3.Vec3(15.0, 15.0, 15.0)
  let t = transform.look_at(from: from, to: to)

  // Position should be the 'from' position
  assert t.position == from
  // Direction is the same as (0,0,0) -> (10,10,10), so same rotation
  assert float.absolute_value(t.rotation.x) >. 0.01
  assert float.absolute_value(t.rotation.y) >. 0.01
  assert t.rotation.z == 0.0
}

// Test: look_at degenerate case (from == to)
pub fn look_at_degenerate_test() {
  let pos = vec3.Vec3(5.0, 5.0, 5.0)
  let t = transform.look_at(from: pos, to: pos)

  // Should handle gracefully and return some default orientation
  assert t.position == pos
  // Should have some rotation values (default forward direction)
  assert t.rotation.z == 0.0
  // Roll should still be 0
}

// Test: look_at very close positions
pub fn look_at_close_positions_test() {
  let from = vec3.Vec3(0.0, 0.0, 0.0)
  let to = vec3.Vec3(0.00001, 0.00001, 0.00001)
  let t = transform.look_at(from: from, to: to)

  // Should handle without crashing
  assert t.position == from
  assert t.rotation.z == 0.0
  // Roll should be 0
}

// Test: look_at always has zero roll
pub fn look_at_zero_roll_test() {
  // Test multiple directions to ensure roll is always 0
  let directions = [
    vec3.Vec3(1.0, 0.0, 0.0),
    vec3.Vec3(0.0, 1.0, 0.0),
    vec3.Vec3(0.0, 0.0, 1.0),
    vec3.Vec3(1.0, 1.0, 1.0),
    vec3.Vec3(-1.0, 2.0, -3.0),
  ]

  let from = vec3f.zero

  directions
  |> list.each(fn(to) {
    let t = transform.look_at(from: from, to: to)
    assert t.rotation.z == 0.0
  })
}
