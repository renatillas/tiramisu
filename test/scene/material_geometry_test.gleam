import gleam/list
import gleam/option
import tiramisu/scene
import tiramisu/transform
import vec/vec3

// Test: Update StandardMaterial properties
pub fn update_standard_material_test() {
  let assert Ok(geometry1) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material1) =
    scene.standard_material(
      color: 0xff0000,
      metalness: 0.5,
      roughness: 0.5,
      map: option.None,
      normal_map: option.None,
      roughness_map: option.None,
      metalness_map: option.None,
      ambient_oclusion_map: option.None,
    )
  let assert Ok(material2) =
    scene.standard_material(
      color: 0xff0000,
      metalness: 0.8,
      roughness: 0.2,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
      roughness_map: option.None,
      metalness_map: option.None,
    )

  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material2,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateMaterial("mesh1", _)) -> True
    _ -> False
  }
}

// Test: Change material type (BasicMaterial to StandardMaterial)
pub fn change_material_type_test() {
  let assert Ok(geometry1) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material1) =
    scene.basic_material(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let assert Ok(material2) =
    scene.standard_material(
      color: 0xff0000,
      metalness: 0.5,
      roughness: 0.5,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
      roughness_map: option.None,
      metalness_map: option.None,
    )

  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material2,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateMaterial("mesh1", _)) -> True
    _ -> False
  }
}

// Test: PhongMaterial update
pub fn phong_material_test() {
  let assert Ok(geometry1) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material1) =
    scene.phong_material(0xff0000, 30.0, option.None, option.None, option.None)
  let assert Ok(material2) =
    scene.phong_material(0xff0000, 60.0, option.None, option.None, option.None)

  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material2,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateMaterial("mesh1", _)) -> True
    _ -> False
  }
}

// Test: LambertMaterial
pub fn lambert_material_test() {
  let assert Ok(geometry1) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material1) =
    scene.lambert_material(0xff0000, option.None, option.None, option.None)
  let assert Ok(material2) =
    scene.lambert_material(0x00ff00, option.None, option.None, option.None)

  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material2,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: ToonMaterial
pub fn toon_material_test() {
  let assert Ok(geometry1) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material1) =
    scene.toon_material(0xff0000, option.None, option.None, option.None)
  let assert Ok(material2) =
    scene.toon_material(0x00ff00, option.None, option.None, option.None)

  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material2,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: Different geometry types
pub fn cone_geometry_test() {
  let assert Ok(geometry1) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(geometry2) = scene.cone(radius: 1.0, height: 2.0, segments: 32)
  let assert Ok(material1) =
    scene.basic_material(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )

  let previous = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "mesh1",
      geometry: geometry2,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateGeometry("mesh1", _)) -> True
    _ -> False
  }
}

// Test: PlaneGeometry
pub fn plane_geometry_test() {
  let assert Ok(geometry1) = scene.plane(width: 10.0, height: 10.0)
  let assert Ok(material1) =
    scene.basic_material(
      color: 0x808080,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )

  let previous = []
  let current = [
    scene.Mesh(
      id: "plane",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.AddNode("plane", scene.Mesh(_, _, _, _, _), _)) -> True
    _ -> False
  }
}

// Test: CircleGeometry
pub fn circle_geometry_test() {
  let assert Ok(geometry1) = scene.circle(radius: 1.5, segments: 64)
  let assert Ok(material1) =
    scene.basic_material(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )

  let previous = []
  let current = [
    scene.Mesh(
      id: "circle",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: CylinderGeometry
pub fn cylinder_geometry_test() {
  let assert Ok(geometry1) =
    scene.cylinder(
      radius_top: 1.0,
      radius_bottom: 1.0,
      height: 2.0,
      radial_segments: 32,
    )
  let assert Ok(material1) =
    scene.basic_material(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )

  let previous = []
  let current = [
    scene.Mesh(
      id: "cylinder",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: TorusGeometry
pub fn torus_geometry_test() {
  let assert Ok(geometry1) =
    scene.torus(
      radius: 1.0,
      tube: 0.3,
      radial_segments: 16,
      tubular_segments: 100,
    )
  let assert Ok(material1) =
    scene.basic_material(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )

  let previous = []
  let current = [
    scene.Mesh(
      id: "torus",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: TetrahedronGeometry
pub fn tetrahedron_geometry_test() {
  let assert Ok(geometry1) = scene.tetrahedron(radius: 1.0, detail: 0)
  let assert Ok(material1) =
    scene.basic_material(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )

  let previous = []
  let current = [
    scene.Mesh(
      id: "tetra",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: IcosahedronGeometry
pub fn icosahedron_geometry_test() {
  let assert Ok(geometry1) = scene.icosahedron(radius: 1.0, detail: 0)
  let assert Ok(material1) =
    scene.basic_material(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )

  let previous = []
  let current = [
    scene.Mesh(
      id: "ico",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
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
      light: {
        let assert Ok(light) =
          scene.directional_light(intensity: 1.0, color: 0xffffff)
        light
      },
      transform: transform.identity,
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
      light: {
        let assert Ok(light) =
          scene.point_light(intensity: 1.0, color: 0xffffff, distance: 10.0)
        light
      },
      transform: transform.identity,
    ),
  ]
  let current = [
    scene.Light(
      id: "light1",
      light: {
        let assert Ok(light) =
          scene.point_light(intensity: 2.0, color: 0xffffff, distance: 20.0)
        light
      },
      transform: transform.identity,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateLight("light1", _)) -> True
    _ -> False
  }
}

// Test: SpotLight
pub fn spot_light_test() {
  let previous = []
  let current = [
    scene.Light(
      id: "spot",
      light: {
        let assert Ok(light) =
          scene.spotlight(
            intensity: 1.0,
            color: 0xffffff,
            distance: 10.0,
            angle: 0.5,
            penumbra: 0.1,
          )
        light
      },
      transform: transform.identity,
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
      light: {
        let assert Ok(light) =
          scene.hemisphere_light(
            intensity: 1.0,
            sky_color: 0x0000ff,
            ground_color: 0x00ff00,
          )
        light
      },
      transform: transform.identity,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
}

// Test: Complex scene with various material and geometry types
pub fn complex_scene_test() {
  let assert Ok(geometry1) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(geometry2) =
    scene.sphere(radius: 1.0, width_segments: 32, height_segments: 32)
  let assert Ok(geometry3) =
    scene.torus(
      radius: 1.0,
      tube: 0.3,
      radial_segments: 16,
      tubular_segments: 100,
    )
  let assert Ok(material1) =
    scene.standard_material(
      color: 0xff0000,
      metalness: 0.5,
      roughness: 0.5,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
      roughness_map: option.None,
      metalness_map: option.None,
    )
  let assert Ok(material2) =
    scene.phong_material(0x00ff00, 30.0, option.None, option.None, option.None)
  let assert Ok(material3) =
    scene.toon_material(0x0000ff, option.None, option.None, option.None)

  let previous = []
  let current = [
    scene.Mesh(
      id: "box",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
    scene.Mesh(
      id: "sphere",
      geometry: geometry2,
      material: material2,
      transform: transform.at(position: vec3.Vec3(2.0, 0.0, 0.0)),
      physics: option.None,
    ),
    scene.Mesh(
      id: "torus",
      geometry: geometry3,
      material: material3,
      transform: transform.at(position: vec3.Vec3(-2.0, 0.0, 0.0)),
      physics: option.None,
    ),
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) =
          scene.ambient_light(intensity: 0.5, color: 0x404040)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "point",
      light: {
        let assert Ok(light) =
          scene.point_light(intensity: 1.0, color: 0xffffff, distance: 10.0)
        light
      },
      transform: transform.at(vec3.Vec3(0.0, 5.0, 0.0)),
    ),
  ]
  let patches = scene.diff(previous, current)

  // Should add 5 nodes
  assert list.length(patches) == 5
}
