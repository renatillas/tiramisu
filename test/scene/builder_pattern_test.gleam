import gleam/list
import gleam/option
import tiramisu/scene
import tiramisu/transform

// --- Material Preset Tests ---

// Test: plastic material preset
pub fn plastic_preset_test() {
  let assert Ok(material) = scene.plastic(0xff0000)

  // Should successfully create a valid material
  let previous = []
  let current = [
    scene.Mesh(
      id: "plastic_mesh",
      geometry: {
        let assert Ok(geom) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
        geom
      },
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: metal material preset
pub fn metal_preset_test() {
  let assert Ok(material) = scene.metal(0xffd700)

  let previous = []
  let current = [
    scene.Mesh(
      id: "metal_mesh",
      geometry: {
        let assert Ok(geom) = scene.sphere(radius: 1.0, width_segments: 32, height_segments: 16)
        geom
      },
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: rough_metal material preset
pub fn rough_metal_preset_test() {
  let assert Ok(material) = scene.rough_metal(0x8b4513)

  let previous = []
  let current = [
    scene.Mesh(
      id: "rough_mesh",
      geometry: {
        let assert Ok(geom) = scene.plane(width: 10.0, height: 10.0)
        geom
      },
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: glossy material preset
pub fn glossy_preset_test() {
  let assert Ok(material) = scene.glossy(0x4444ff)

  let previous = []
  let current = [
    scene.Mesh(
      id: "glossy_mesh",
      geometry: {
        let assert Ok(geom) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
        geom
      },
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: matte material preset
pub fn matte_preset_test() {
  let assert Ok(material) = scene.matte(0x808080)

  let previous = []
  let current = [
    scene.Mesh(
      id: "matte_mesh",
      geometry: {
        let assert Ok(geom) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
        geom
      },
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// --- Material Builder Tests ---

// Test: material builder with default values
pub fn material_builder_defaults_test() {
  let assert Ok(material) =
    scene.new_standard_material()
    |> scene.build_material()

  // Should successfully build with defaults
  let previous = []
  let current = [
    scene.Mesh(
      id: "builder_mesh",
      geometry: {
        let assert Ok(geom) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
        geom
      },
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: material builder with color
pub fn material_builder_color_test() {
  let assert Ok(material) =
    scene.new_standard_material()
    |> scene.mat_color(0xff0000)
    |> scene.build_material()

  let previous = []
  let current = [
    scene.Mesh(
      id: "red_mesh",
      geometry: {
        let assert Ok(geom) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
        geom
      },
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: material builder with full customization
pub fn material_builder_full_test() {
  let assert Ok(material) =
    scene.new_standard_material()
    |> scene.mat_color(0xff6600)
    |> scene.mat_metalness(0.9)
    |> scene.mat_roughness(0.2)
    |> scene.build_material()

  let previous = []
  let current = [
    scene.Mesh(
      id: "custom_mesh",
      geometry: {
        let assert Ok(geom) = scene.sphere(radius: 1.0, width_segments: 32, height_segments: 16)
        geom
      },
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: material builder validates parameters
pub fn material_builder_invalid_color_test() {
  let result =
    scene.new_standard_material()
    |> scene.mat_color(0x1ffffff)  // Invalid color (too large)
    |> scene.build_material()

  assert case result {
    Error(scene.InvalidMaterialColor(_)) -> True
    _ -> False
  }
}

// Test: material builder validates roughness
pub fn material_builder_invalid_roughness_test() {
  let result =
    scene.new_standard_material()
    |> scene.mat_roughness(1.5)  // Invalid roughness (> 1.0)
    |> scene.build_material()

  assert case result {
    Error(scene.InvalidMaterialRoughness(_)) -> True
    _ -> False
  }
}

// Test: material builder validates metalness
pub fn material_builder_invalid_metalness_test() {
  let result =
    scene.new_standard_material()
    |> scene.mat_metalness(-0.1)  // Invalid metalness (< 0.0)
    |> scene.build_material()

  assert case result {
    Error(scene.InvalidMaterialMetalness(_)) -> True
    _ -> False
  }
}

// --- Geometry Builder Tests ---

// Test: box builder with defaults
pub fn box_builder_defaults_test() {
  let assert Ok(geometry) =
    scene.new_box()
    |> scene.build_box()

  // Should successfully build a 1x1x1 cube
  let previous = []
  let current = [
    scene.Mesh(
      id: "default_box",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.plastic(0xffffff)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: box builder with custom dimensions
pub fn box_builder_custom_test() {
  let assert Ok(geometry) =
    scene.new_box()
    |> scene.box_width(10.0)
    |> scene.box_height(3.0)
    |> scene.box_depth(0.5)
    |> scene.build_box()

  let previous = []
  let current = [
    scene.Mesh(
      id: "wall",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.plastic(0xcccccc)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: box builder with size setter
pub fn box_builder_size_test() {
  let assert Ok(geometry) =
    scene.new_box()
    |> scene.box_size(width: 5.0, height: 2.0, depth: 3.0)
    |> scene.build_box()

  let previous = []
  let current = [
    scene.Mesh(
      id: "sized_box",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.plastic(0xff0000)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: box builder cube
pub fn box_builder_cube_test() {
  let assert Ok(geometry) =
    scene.new_box()
    |> scene.box_cube(2.5)
    |> scene.build_box()

  let previous = []
  let current = [
    scene.Mesh(
      id: "cube",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.metal(0xffd700)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: sphere builder with defaults
pub fn sphere_builder_defaults_test() {
  let assert Ok(geometry) =
    scene.new_sphere()
    |> scene.build_sphere()

  // Should build with radius 1.0, 32x16 segments
  let previous = []
  let current = [
    scene.Mesh(
      id: "default_sphere",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.plastic(0x0000ff)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: sphere builder with custom radius
pub fn sphere_builder_radius_test() {
  let assert Ok(geometry) =
    scene.new_sphere()
    |> scene.sphere_radius(2.5)
    |> scene.build_sphere()

  let previous = []
  let current = [
    scene.Mesh(
      id: "big_sphere",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.glossy(0x00ff00)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: sphere builder with custom segments
pub fn sphere_builder_segments_test() {
  let assert Ok(geometry) =
    scene.new_sphere()
    |> scene.sphere_segments(64)
    |> scene.build_sphere()

  // Should set width_segments to 64, height_segments to 32
  let previous = []
  let current = [
    scene.Mesh(
      id: "high_poly_sphere",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.metal(0xc0c0c0)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: plane builder with defaults
pub fn plane_builder_defaults_test() {
  let assert Ok(geometry) =
    scene.new_plane()
    |> scene.build_plane()

  // Should build a 1x1 plane
  let previous = []
  let current = [
    scene.Mesh(
      id: "default_plane",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.matte(0x808080)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: plane builder with custom dimensions
pub fn plane_builder_dimensions_test() {
  let assert Ok(geometry) =
    scene.new_plane()
    |> scene.plane_width(20.0)
    |> scene.plane_height(15.0)
    |> scene.build_plane()

  let previous = []
  let current = [
    scene.Mesh(
      id: "floor",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.plastic(0x228b22)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: plane builder with size setter
pub fn plane_builder_size_test() {
  let assert Ok(geometry) =
    scene.new_plane()
    |> scene.plane_size(width: 10.0, height: 8.0)
    |> scene.build_plane()

  let previous = []
  let current = [
    scene.Mesh(
      id: "sized_plane",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.plastic(0xaaaaaa)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}

// Test: plane builder square
pub fn plane_builder_square_test() {
  let assert Ok(geometry) =
    scene.new_plane()
    |> scene.plane_square(5.0)
    |> scene.build_plane()

  let previous = []
  let current = [
    scene.Mesh(
      id: "square_plane",
      geometry: geometry,
      material: {
        let assert Ok(mat) = scene.rough_metal(0x8b4513)
        mat
      },
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)
  assert patches |> list.length == 1
}
