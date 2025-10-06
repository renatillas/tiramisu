import gleam/option
import tiramisu/scene

// Test: valid box geometry
pub fn valid_box_test() {
  let result = scene.box(1.0, 2.0, 3.0)
  assert case result {
    Ok(scene.BoxGeometry(1.0, 2.0, 3.0)) -> True
    _ -> False
  }
}

// Test: invalid box width
pub fn invalid_box_width_test() {
  let result = scene.box(0.0, 2.0, 3.0)
  assert case result {
    Error(scene.InvalidDimension("width", 0.0)) -> True
    _ -> False
  }
}

// Test: invalid box height
pub fn invalid_box_height_test() {
  let result = scene.box(1.0, -1.0, 3.0)
  assert case result {
    Error(scene.InvalidDimension("height", _)) -> True
    _ -> False
  }
}

// Test: valid sphere geometry
pub fn valid_sphere_test() {
  let result = scene.sphere(1.5, 32, 16)
  assert case result {
    Ok(scene.SphereGeometry(1.5, 32, 16)) -> True
    _ -> False
  }
}

// Test: invalid sphere radius
pub fn invalid_sphere_radius_test() {
  let result = scene.sphere(-1.0, 32, 16)
  assert case result {
    Error(scene.InvalidDimension("radius", _)) -> True
    _ -> False
  }
}

// Test: invalid sphere width segments (too few)
pub fn invalid_sphere_width_segments_test() {
  let result = scene.sphere(1.0, 2, 16)
  assert case result {
    Error(scene.InvalidSegmentCount("width_segments", 2)) -> True
    _ -> False
  }
}

// Test: invalid sphere height segments (too few)
pub fn invalid_sphere_height_segments_test() {
  let result = scene.sphere(1.0, 32, 1)
  assert case result {
    Error(scene.InvalidSegmentCount("height_segments", 1)) -> True
    _ -> False
  }
}

// Test: valid basic material
pub fn valid_basic_material_test() {
  let result = scene.basic_material(0xff0000, False, 1.0)
  assert case result {
    Ok(scene.BasicMaterial(0xff0000, False, 1.0, option.None)) -> True
    _ -> False
  }
}

// Test: invalid opacity (too high)
pub fn invalid_opacity_high_test() {
  let result = scene.basic_material(0xff0000, False, 1.5)
  assert case result {
    Error(scene.InvalidOpacity(1.5)) -> True
    _ -> False
  }
}

// Test: invalid opacity (negative)
pub fn invalid_opacity_negative_test() {
  let result = scene.basic_material(0xff0000, False, -0.1)
  assert case result {
    Error(scene.InvalidOpacity(_)) -> True
    _ -> False
  }
}

// Test: valid standard material
pub fn valid_standard_material_test() {
  let result = scene.standard_material(0xff0000, 0.5, 0.8)
  assert case result {
    Ok(scene.StandardMaterial(0xff0000, 0.5, 0.8, option.None, option.None)) ->
      True
    _ -> False
  }
}

// Test: invalid metalness
pub fn invalid_metalness_test() {
  let result = scene.standard_material(0xff0000, 1.5, 0.8)
  assert case result {
    Error(scene.InvalidMetalness(1.5)) -> True
    _ -> False
  }
}

// Test: invalid roughness
pub fn invalid_roughness_test() {
  let result = scene.standard_material(0xff0000, 0.5, -0.1)
  assert case result {
    Error(scene.InvalidRoughness(_)) -> True
    _ -> False
  }
}
