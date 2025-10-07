import tiramisu/camera

// Test: valid camera creation succeeds
pub fn valid_camera_test() {
  let result = camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
  assert case result {
    Ok(_) -> True
    Error(_) -> False
  }
}

// Test: invalid field of view (too low)
pub fn invalid_fov_low_test() {
  let result = camera.perspective(field_of_view: 0.0, near: 0.1, far: 1000.0)
  assert case result {
    Error(camera.InvalidFieldOfView(0.0)) -> True
    _ -> False
  }
}

// Test: invalid field of view (too high)
pub fn invalid_fov_high_test() {
  let result = camera.perspective(field_of_view: 180.0, near: 0.1, far: 1000.0)
  assert case result {
    Error(camera.InvalidFieldOfView(180.0)) -> True
    _ -> False
  }
}

// Test: invalid near plane
pub fn invalid_near_test() {
  let result = camera.perspective(field_of_view: 75.0, near: 0.0, far: 1000.0)
  assert case result {
    Error(camera.InvalidNearPlane(0.0)) -> True
    _ -> False
  }
}

// Test: invalid far plane
pub fn invalid_far_test() {
  let result = camera.perspective(field_of_view: 75.0, near: 0.1, far: 0.0)
  assert case result {
    Error(camera.InvalidFarPlane(0.0)) -> True
    _ -> False
  }
}

// Test: near >= far conflict
pub fn near_far_conflict_test() {
  let result = camera.perspective(field_of_view: 75.0, near: 100.0, far: 10.0)
  assert case result {
    Error(camera.NearFarConflict(100.0, 10.0)) -> True
    _ -> False
  }
}
