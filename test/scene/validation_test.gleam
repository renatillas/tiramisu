import gleam/option
import tiramisu/scene

// Test: valid box geometry
pub fn valid_box_test() {
  let result = scene.box(width: 1.0, height: 2.0, depth: 3.0)
  assert case result {
    Ok(_) -> True
    _ -> False
  }
}

// Test: invalid box width
pub fn invalid_box_width_test() {
  let assert Error(scene.InvalidGeometryWidth(0.0)) =
    scene.box(width: 0.0, height: 2.0, depth: 3.0)
}

// Test: invalid box height
pub fn invalid_box_height_test() {
  let assert Error(scene.InvalidGeometryHeight(-1.0)) =
    scene.box(width: 1.0, height: -1.0, depth: 3.0)
}

// Test: valid sphere geometry
pub fn valid_sphere_test() {
  let result =
    scene.sphere(radius: 1.5, width_segments: 32, height_segments: 16)
  assert case result {
    Ok(_) -> True
    _ -> False
  }
}

// Test: invalid sphere radius
pub fn invalid_sphere_radius_test() {
  let assert Error(scene.InvalidGeometryRadius(-1.0)) =
    scene.sphere(radius: -1.0, width_segments: 32, height_segments: 16)
}

// Test: invalid sphere width segments (too few)
pub fn invalid_sphere_width_segments_test() {
  let assert Error(scene.InvalidGeometrySegmentCountWidth(2)) =
    scene.sphere(radius: 1.0, width_segments: 2, height_segments: 16)
}

// Test: invalid sphere height segments (too few)
pub fn invalid_sphere_height_segments_test() {
  let assert Error(scene.InvalidGeometrySegmentCountHeight(1)) =
    scene.sphere(radius: 1.0, width_segments: 32, height_segments: 1)
}

// Test: valid basic material
pub fn valid_basic_material_test() {
  let assert Ok(_) =
    scene.basic_material(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
}

// Test: invalid opacity (too high)
pub fn invalid_opacity_high_test() {
  let assert Error(scene.InvalidMaterialOpacity(1.5)) =
    scene.basic_material(
      color: 0xff0000,
      transparent: False,
      opacity: 1.5,
      map: option.None,
      normal_map: option.None,
    )
}

// Test: invalid opacity (negative)
pub fn invalid_opacity_negative_test() {
  let assert Error(scene.InvalidMaterialOpacity(-0.1)) =
    scene.basic_material(
      color: 0xff0000,
      transparent: False,
      opacity: -0.1,
      map: option.None,
      normal_map: option.None,
    )
}

// Test: valid standard material
pub fn valid_standard_material_test() {
  let result =
    scene.standard_material(
      color: 0xff0000,
      metalness: 0.5,
      roughness: 0.8,
      map: option.None,
      normal_map: option.None,
    )
  assert case result {
    Ok(_) -> True
    _ -> False
  }
}

// Test: invalid metalness
pub fn invalid_metalness_test() {
  let assert Error(scene.InvalidMaterialMetalness(1.5)) =
    scene.standard_material(
      color: 0xff0000,
      metalness: 1.5,
      roughness: 0.8,
      map: option.None,
      normal_map: option.None,
    )
}

// Test: invalid roughness
pub fn invalid_roughness_test() {
  let assert Error(scene.InvalidMaterialRoughness(-0.1)) =
    scene.standard_material(
      color: 0xff0000,
      metalness: 0.5,
      roughness: -0.1,
      map: option.None,
      normal_map: option.None,
    )
}
