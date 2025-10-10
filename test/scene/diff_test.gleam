import gleam/list
import gleam/option
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

// Test: empty to empty diff produces no patches
pub fn empty_to_empty_test() {
  let patches = scene.diff([], [])
  assert patches == []
}

// Test: adding a single mesh
pub fn add_mesh_test() {
  let previous = []
  let assert Ok(geometry) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: geometry,
      material: material,
      transform: transform.identity,
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
  let assert Ok(geometry) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: geometry,
      material: material,
      transform: transform.identity,
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
  let assert Ok(geometry) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let nodes = [
    scene.Mesh(
      id: "cube1",
      geometry: geometry,
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(nodes, nodes)
  assert patches == []
}

// Test: updating transform
pub fn update_transform_test() {
  let assert Ok(geometry) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: geometry,
      material: material,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: geometry,
      material: material,
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
  let assert Ok(geometry) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material1) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let assert Ok(material2) =
    material.basic(
      color: 0x00ff00,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: geometry,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: geometry,
      material: material2,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(scene.UpdateMaterial("cube1", _)) -> True
    _ -> False
  }
}

// Test: updating geometry
pub fn update_geometry_test() {
  let assert Ok(box_geometry) =
    geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(sphere_geometry) =
    geometry.sphere(radius: 1.5, width_segments: 32, height_segments: 32)
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: box_geometry,
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: sphere_geometry,
      material: material,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
  assert case list.first(patches) {
    Ok(scene.UpdateGeometry("cube1", _)) -> True
    _ -> False
  }
}

// Test: adding light
pub fn add_light_test() {
  let previous = []
  let current = [
    scene.Light(
      id: "light1",
      light: {
        let assert Ok(light) = light.ambient(intensity: 1.0, color: 0xffffff)
        light
      },
      transform: transform.identity,
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
      light: {
        let assert Ok(light) = light.ambient(intensity: 0.5, color: 0xffffff)
        light
      },
      transform: transform.identity,
    ),
  ]
  let current = [
    scene.Light(
      id: "light1",
      light: {
        let assert Ok(light) = light.ambient(intensity: 1.0, color: 0xffffff)
        light
      },
      transform: transform.identity,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1
  let assert Ok(ambient_light) = light.ambient(intensity: 1.0, color: 0xffffff)
  assert case list.first(patches) {
    Ok(scene.UpdateLight("light1", light)) -> light == ambient_light
    _ -> False
  }
}

// Test: multiple changes
pub fn multiple_changes_test() {
  let assert Ok(geometry) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material_red) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let assert Ok(material_green) =
    material.basic(
      color: 0x00ff00,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let assert Ok(material_blue) =
    material.basic(
      color: 0x0000ff,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let previous = [
    scene.Mesh(
      id: "cube1",
      geometry: geometry,
      material: material_red,
      transform: transform.identity,
      physics: option.None,
    ),
    scene.Mesh(
      id: "cube2",
      geometry: geometry,
      material: material_green,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let current = [
    scene.Mesh(
      id: "cube1",
      geometry: geometry,
      material: material_red,
      transform: transform.at(vec3.Vec3(1.0, 0.0, 0.0)),
      physics: option.None,
    ),
    scene.Mesh(
      id: "cube3",
      geometry: geometry,
      material: material_blue,
      transform: transform.identity,
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
  let assert Ok(geometry) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let current = [
    scene.Group(id: "group1", transform: transform.identity, children: [
      scene.Mesh(
        id: "child1",
        geometry: geometry,
        material: material,
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Should add group and child
  assert list.length(patches) == 2
}
