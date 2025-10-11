import tiramisu/camera

pub fn perspective_invalid_fov_test() {
  let assert Error(camera.InvalidFieldOfView(0.0)) =
    camera.perspective(field_of_view: 0.0, near: 0.1, far: 1000.0)
}

pub fn perspective_invalid_near_test() {
  let assert Error(camera.InvalidNearPlane(0.0)) =
    camera.perspective(field_of_view: 75.0, near: 0.0, far: 1000.0)
}

pub fn perspective_invalid_far_test() {
  let assert Error(camera.NearFarConflict(0.1, 0.1)) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 0.1)
}

pub fn camera_2d_test() {
  let camera = camera.camera_2d(width: 800, height: 600)
  let assert camera.Orthographic(
    left: -400.0,
    right: 400.0,
    top: 300.0,
    bottom: -300.0,
    near: 0.1,
    far: 1000.0,
  ) = camera.get_projection(camera)
}
