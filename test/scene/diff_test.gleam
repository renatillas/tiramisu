import gleam/list
import gleam/option
import tiramisu/scene
import tiramisu/scene/diff

// Test: empty to empty diff produces no patches
pub fn empty_to_empty_test() {
  let patches = diff.diff([], [])
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
      transform: scene.identity_transform(),
    ),
  ]
  let patches = diff.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(diff.AddNode("cube1", _, _)) -> True
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
      transform: scene.identity_transform(),
    ),
  ]
  let current = []
  let patches = diff.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(diff.RemoveNode("cube1")) -> True
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
      transform: scene.identity_transform(),
    ),
  ]
  let patches = diff.diff(nodes, nodes)
  assert patches == []
}

// Test: updating transform
pub fn update_transform_test() {
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: scene.transform_at(0.0, 0.0, 0.0),
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: scene.transform_at(1.0, 2.0, 3.0),
    ),
  ]
  let patches = diff.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(diff.UpdateTransform("cube1", transform)) -> {
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
      transform: scene.identity_transform(),
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
      transform: scene.identity_transform(),
    ),
  ]
  let patches = diff.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(diff.UpdateMaterial("cube1", scene.BasicMaterial(0x00ff00, False, 1.0, option.None))) ->
      True
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
      transform: scene.identity_transform(),
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.SphereGeometry(1.5, 32, 32),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: scene.identity_transform(),
    ),
  ]
  let patches = diff.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(diff.UpdateGeometry("cube1", scene.SphereGeometry(1.5, 32, 32))) -> True
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
      transform: scene.identity_transform(),
    ),
  ]
  let patches = diff.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(diff.AddNode("light1", _, _)) -> True
    _ -> False
  }
}

// Test: updating light type
pub fn update_light_test() {
  let previous = [
    scene.Light(
      id: "light1",
      light_type: scene.AmbientLight(0xffffff, 0.5),
      transform: scene.identity_transform(),
    ),
  ]
  let current = [
    scene.Light(
      id: "light1",
      light_type: scene.AmbientLight(0xffffff, 1.0),
      transform: scene.identity_transform(),
    ),
  ]
  let patches = diff.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(diff.UpdateLight("light1", scene.AmbientLight(0xffffff, 1.0))) -> True
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
      transform: scene.identity_transform(),
    ),
    scene.Mesh(
      id: "cube2",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
      transform: scene.identity_transform(),
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: scene.transform_at(1.0, 0.0, 0.0),
    ),
    scene.Mesh(
      id: "cube3",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0x0000ff, False, 1.0, option.None),
      transform: scene.identity_transform(),
    ),
  ]
  let patches = diff.diff(previous, current)

  // Should have: remove cube2, add cube3, update cube1 transform
  assert list.length(patches) == 3
}

// Test: group node
pub fn group_test() {
  let previous = []
  let current = [
    scene.Group(
      id: "group1",
      transform: scene.identity_transform(),
      children: [
        scene.Mesh(
          id: "child1",
          geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
          material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
          transform: scene.identity_transform(),
        ),
      ],
    ),
  ]
  let patches = diff.diff(previous, current)

  // Should add group and child
  assert list.length(patches) == 2
}
