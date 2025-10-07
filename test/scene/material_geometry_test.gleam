import gleam/list
import gleam/option
import tiramisu/scene
import tiramisu/transform
import vec/vec3

// Test: Update StandardMaterial properties
pub fn update_standard_material_test() {
  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        0xff0000,
        0.5,
        0.5,
        option.None,
        option.None,
      ),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        0xff0000,
        0.8,
        0.2,
        option.None,
        option.None,
      ),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateMaterial(
      "mesh1",
      scene.StandardMaterial(0xff0000, 0.8, 0.2, option.None, option.None),
    )) -> True
    _ -> False
  }
}

// Test: Change material type (BasicMaterial to StandardMaterial)
pub fn change_material_type_test() {
  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        0xff0000,
        0.5,
        0.5,
        option.None,
        option.None,
      ),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateMaterial(
      "mesh1",
      scene.StandardMaterial(_, _, _, _, option.None),
    )) -> True
    _ -> False
  }
}

// Test: PhongMaterial update
pub fn phong_material_test() {
  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.PhongMaterial(0xff0000, 30.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.PhongMaterial(0xff0000, 60.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateMaterial(
      "mesh1",
      scene.PhongMaterial(0xff0000, 60.0, option.None),
    )) -> True
    _ -> False
  }
}

// Test: LambertMaterial
pub fn lambert_material_test() {
  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.LambertMaterial(0xff0000, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.LambertMaterial(0x00ff00, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: ToonMaterial
pub fn toon_material_test() {
  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.ToonMaterial(0xff0000, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.ToonMaterial(0x00ff00, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: Different geometry types
pub fn cone_geometry_test() {
  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: scene.ConeGeometry(1.0, 2.0, 32),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateGeometry("mesh1", scene.ConeGeometry(1.0, 2.0, 32))) -> True
    _ -> False
  }
}

// Test: PlaneGeometry
pub fn plane_geometry_test() {
  let previous = []
  let current = [
    scene.Mesh(
      id: "plane",
      geometry: scene.PlaneGeometry(10.0, 10.0),
      material: scene.BasicMaterial(0x808080, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.AddNode(
      "plane",
      scene.Mesh(_, scene.PlaneGeometry(10.0, 10.0), _, _, _),
      _,
    )) -> True
    _ -> False
  }
}

// Test: CircleGeometry
pub fn circle_geometry_test() {
  let previous = []
  let current = [
    scene.Mesh(
      id: "circle",
      geometry: scene.CircleGeometry(1.5, 64),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: CylinderGeometry
pub fn cylinder_geometry_test() {
  let previous = []
  let current = [
    scene.Mesh(
      id: "cylinder",
      geometry: scene.CylinderGeometry(1.0, 1.0, 2.0, 32),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: TorusGeometry
pub fn torus_geometry_test() {
  let previous = []
  let current = [
    scene.Mesh(
      id: "torus",
      geometry: scene.TorusGeometry(1.0, 0.3, 16, 100),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: TetrahedronGeometry
pub fn tetrahedron_geometry_test() {
  let previous = []
  let current = [
    scene.Mesh(
      id: "tetra",
      geometry: scene.TetrahedronGeometry(1.0, 0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: IcosahedronGeometry
pub fn icosahedron_geometry_test() {
  let previous = []
  let current = [
    scene.Mesh(
      id: "ico",
      geometry: scene.IcosahedronGeometry(1.0, 0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: DirectionalLight
pub fn directional_light_test() {
  let previous = []
  let current = [
    scene.Light(
      id: "dir_light",
      light_type: scene.DirectionalLight(0xffffff, 1.0),
      transform: transform.identity(),
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: PointLight
pub fn point_light_test() {
  let previous = [
    scene.Light(
      id: "light1",
      light_type: scene.PointLight(0xffffff, 1.0, 10.0),
      transform: transform.identity(),
    ),
  ]
  let current = [
    scene.Light(
      id: "light1",
      light_type: scene.PointLight(0xffffff, 2.0, 20.0),
      transform: transform.identity(),
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateLight("light1", scene.PointLight(0xffffff, 2.0, 20.0))) ->
      True
    _ -> False
  }
}

// Test: SpotLight
pub fn spot_light_test() {
  let previous = []
  let current = [
    scene.Light(
      id: "spot",
      light_type: scene.SpotLight(0xffffff, 1.0, 10.0, 0.5, 0.1),
      transform: transform.identity(),
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: HemisphereLight
pub fn hemisphere_light_test() {
  let previous = []
  let current = [
    scene.Light(
      id: "hemi",
      light_type: scene.HemisphereLight(0x0000ff, 0x00ff00, 1.0),
      transform: transform.identity(),
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: Complex scene with various material and geometry types
pub fn complex_scene_test() {
  let previous = []
  let current = [
    scene.Mesh(
      id: "box",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        0xff0000,
        0.5,
        0.5,
        option.None,
        option.None,
      ),
      transform: transform.identity(),
      physics: option.None,
    ),
    scene.Mesh(
      id: "sphere",
      geometry: scene.SphereGeometry(1.0, 32, 32),
      material: scene.PhongMaterial(0x00ff00, 30.0, option.None),
      transform: transform.at(position: vec3.Vec3(2.0, 0.0, 0.0)),
      physics: option.None,
    ),
    scene.Mesh(
      id: "torus",
      geometry: scene.TorusGeometry(1.0, 0.3, 16, 100),
      material: scene.ToonMaterial(0x0000ff, option.None),
      transform: transform.at(position: vec3.Vec3(-2.0, 0.0, 0.0)),
      physics: option.None,
    ),
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(0x404040, 0.5),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "point",
      light_type: scene.PointLight(0xffffff, 1.0, 10.0),
      transform: transform.at(vec3.Vec3(0.0, 5.0, 0.0)),
    ),
  ]
  let patches = scene.diff(previous, current)

  // Should add 5 nodes
  assert list.length(patches) == 5
}
