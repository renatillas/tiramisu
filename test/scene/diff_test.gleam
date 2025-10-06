import gleam/list
import gleam/option
import tiramisu/math/vec3
import tiramisu/scene
import tiramisu/transform

// Test: empty to empty diff produces no patches
pub fn empty_to_empty_test() {
  let patches = scene.diff([], [])
  assert patches == []
}

// Test: adding a single mesh
pub fn add_mesh_test() {
  let previous = []
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(scene.AddNode("cube1", _, _)) -> True
    _ -> False
  }
}

// Test: removing a single mesh
pub fn remove_mesh_test() {
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let current = []
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(scene.RemoveNode("cube1")) -> True
    _ -> False
  }
}

// Test: no changes produces no patches
pub fn no_changes_test() {
  let nodes = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(nodes, nodes)
  assert patches == []
}

// Test: updating transform
pub fn update_transform_test() {
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.at(position: vec3.Vec3(1.0, 2.0, 3.0)),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(scene.UpdateTransform("cube1", transform)) -> {
      transform.position.x == 1.0
      && transform.position.y == 2.0
      && transform.position.z == 3.0
    }
    _ -> False
  }
}

// Test: updating material
pub fn update_material_test() {
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(scene.UpdateMaterial(
      "cube1",
      scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
    )) -> True
    _ -> False
  }
}

// Test: updating geometry
pub fn update_geometry_test() {
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.SphereGeometry(1.5, 32, 32),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(scene.UpdateGeometry("cube1", scene.SphereGeometry(1.5, 32, 32))) -> True
    _ -> False
  }
}

// Test: adding light
pub fn add_light_test() {
  let previous = []
  let current = [
    scene.Light(
      id: "light1",
      light_type: scene.AmbientLight(0xffffff, 1.0),
      transform: transform.identity(),
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(scene.AddNode("light1", _, _)) -> True
    _ -> False
  }
}

// Test: updating light type
pub fn update_light_test() {
  let previous = [
    scene.Light(
      id: "light1",
      light_type: scene.AmbientLight(0xffffff, 0.5),
      transform: transform.identity(),
    ),
  ]
  let current = [
    scene.Light(
      id: "light1",
      light_type: scene.AmbientLight(0xffffff, 1.0),
      transform: transform.identity(),
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(scene.UpdateLight("light1", scene.AmbientLight(0xffffff, 1.0))) -> True
    _ -> False
  }
}

// Test: multiple changes
pub fn multiple_changes_test() {
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
    scene.Mesh(
      id: "cube2",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.at(vec3.Vec3(1.0, 0.0, 0.0)),
      physics: option.None,
    ),
    scene.Mesh(
      id: "cube3",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0x0000ff, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  // Should have: remove cube2, add cube3, update cube1 transform
  assert list.length(patches) == 3
}

// Test: group node
pub fn group_test() {
  let previous = []
  let current = [
    scene.Group(id: "group1", transform: transform.identity(), children: [
      scene.Mesh(
        id: "child1",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity(),
        physics: option.None,
      ),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Should add group and child
  assert list.length(patches) == 2
}
