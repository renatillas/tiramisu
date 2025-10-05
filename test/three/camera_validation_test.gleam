import tiramisu/camera

// Test: valid camera creation succeeds
pub fn valid_camera_test() {
  let result = camera.perspective(75.0, 16.0 /. 9.0, 0.1, 1000.0)
  assert case result {
    Ok(_) -> True
    Error(_) -> False
  }
}

// Test: invalid field of view (too low)
pub fn invalid_fov_low_test() {
  let result = camera.perspective(0.0, 16.0 /. 9.0, 0.1, 1000.0)
  assert case result {
    Error(camera.InvalidFieldOfView(0.0)) -> True
    _ -> False
  }
}

// Test: invalid field of view (too high)
pub fn invalid_fov_high_test() {
  let result = camera.perspective(180.0, 16.0 /. 9.0, 0.1, 1000.0)
  assert case result {
    Error(camera.InvalidFieldOfView(180.0)) -> True
    _ -> False
  }
}

// Test: invalid aspect ratio
pub fn invalid_aspect_test() {
  let result = camera.perspective(75.0, 0.0, 0.1, 1000.0)
  assert case result {
    Error(camera.InvalidAspectRatio(0.0)) -> True
    _ -> False
  }
}

// Test: invalid near plane
pub fn invalid_near_test() {
  let result = camera.perspective(75.0, 16.0 /. 9.0, 0.0, 1000.0)
  assert case result {
    Error(camera.InvalidNearPlane(0.0)) -> True
    _ -> False
  }
}

// Test: invalid far plane
pub fn invalid_far_test() {
  let result = camera.perspective(75.0, 16.0 /. 9.0, 0.1, 0.0)
  assert case result {
    Error(camera.InvalidFarPlane(0.0)) -> True
    _ -> False
  }
}

// Test: near >= far conflict
pub fn near_far_conflict_test() {
  let result = camera.perspective(75.0, 16.0 /. 9.0, 100.0, 10.0)
  assert case result {
    Error(camera.NearFarConflict(100.0, 10.0)) -> True
    _ -> False
  }
}
