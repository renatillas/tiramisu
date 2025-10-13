import tiramisu/transform
import vec/vec3

pub fn identity_test() {
  let t = transform.identity

  assert t.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

pub fn at_test() {
  let pos = vec3.Vec3(5.0, 10.0, 15.0)
  let t = transform.at(pos)

  assert t.position == pos
  assert t.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

pub fn with_rotation_test() {
  let rot = vec3.Vec3(1.0, 2.0, 3.0)
  let t = transform.with_rotation(transform.identity, rot)

  assert t.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.rotation == rot
  assert t.scale == vec3.Vec3(1.0, 1.0, 1.0)
}

pub fn with_scale_test() {
  let scale = vec3.Vec3(2.0, 3.0, 4.0)
  let t = transform.with_scale(transform.identity, scale)

  assert t.position == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.rotation == vec3.Vec3(0.0, 0.0, 0.0)
  assert t.scale == scale
}

pub fn set_position_test() {
  let pos = vec3.Vec3(1.0, 2.0, 3.0)
  let t = transform.with_position(transform.identity, pos)

  assert t.position == pos
}

pub fn set_rotation_test() {
  let rot = vec3.Vec3(0.5, 1.0, 1.5)
  let t = transform.with_rotation(transform.identity, rot)

  assert t.rotation == rot
}

pub fn set_scale_test() {
  let scale = vec3.Vec3(0.5, 2.0, 3.5)
  let t = transform.with_scale(transform.identity, scale)

  assert t.scale == scale
}

pub fn translate_test() {
  let t =
    transform.identity
    |> transform.translate(by: vec3.Vec3(1.0, 2.0, 3.0))
    |> transform.translate(by: vec3.Vec3(4.0, 5.0, 6.0))

  assert t.position == vec3.Vec3(5.0, 7.0, 9.0)
}

pub fn rotate_test() {
  let t =
    transform.identity
    |> transform.rotate_by(vec3.Vec3(0.1, 0.2, 0.3))
    |> transform.rotate_by(vec3.Vec3(0.4, 0.5, 0.6))

  // Check with small tolerance for floating point arithmetic
  let epsilon = 0.0001
  assert t.rotation.x >. 0.5 -. epsilon
  assert t.rotation.x <. 0.5 +. epsilon
  assert t.rotation.y >. 0.7 -. epsilon
  assert t.rotation.y <. 0.7 +. epsilon
  assert t.rotation.z >. 0.9 -. epsilon
  assert t.rotation.z <. 0.9 +. epsilon
}

pub fn scale_by_test() {
  let t =
    transform.identity
    |> transform.scale_by(vec3.Vec3(2.0, 3.0, 4.0))
    |> transform.scale_by(vec3.Vec3(0.5, 2.0, 0.25))

  assert t.scale == vec3.Vec3(1.0, 6.0, 1.0)
}

pub fn builder_pattern_test() {
  let t =
    transform.identity
    |> transform.with_position(vec3.Vec3(10.0, 20.0, 30.0))
    |> transform.with_rotation(vec3.Vec3(0.5, 1.0, 1.5))
    |> transform.with_scale(vec3.Vec3(2.0, 2.0, 2.0))

  assert t.position == vec3.Vec3(10.0, 20.0, 30.0)
  assert t.rotation == vec3.Vec3(0.5, 1.0, 1.5)
  assert t.scale == vec3.Vec3(2.0, 2.0, 2.0)
}

pub fn chaining_test() {
  let t =
    transform.at(vec3.Vec3(1.0, 2.0, 3.0))
    |> transform.translate(by: vec3.Vec3(5.0, 5.0, 5.0))
    |> transform.rotate_by(vec3.Vec3(0.1, 0.2, 0.3))
    |> transform.scale_by(vec3.Vec3(2.0, 2.0, 2.0))

  assert t.position == vec3.Vec3(6.0, 7.0, 8.0)
  assert t.rotation == vec3.Vec3(0.1, 0.2, 0.3)
  assert t.scale == vec3.Vec3(2.0, 2.0, 2.0)
}
