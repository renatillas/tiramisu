import gleam/list
import gleam/option
import tiramisu/geometry
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

// Test: Parent group is added before children in patches
pub fn parent_before_children_test() {
  let assert Ok(geometry1) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(geometry2) =
    geometry.sphere(radius: 1.0, width_segments: 32, height_segments: 32)
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

  let previous = []
  let current = [
    scene.Group(id: "parent", transform: transform.identity, children: [
      scene.Mesh(
        id: "child1",
        geometry: geometry1,
        material: material1,
        transform: transform.identity,
        physics: option.None,
      ),
      scene.Mesh(
        id: "child2",
        geometry: geometry2,
        material: material2,
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Find indices of parent and children
  let parent_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("parent", _, _) -> idx
        _ -> acc
      }
    })

  let child1_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("child1", _, _) -> idx
        _ -> acc
      }
    })

  let child2_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("child2", _, _) -> idx
        _ -> acc
      }
    })

  // Parent must come before both children
  assert parent_index >= 0
  assert child1_index >= 0
  assert child2_index >= 0
  assert parent_index < child1_index
  assert parent_index < child2_index
}

// Test: Deeply nested hierarchy is added in correct order
pub fn deeply_nested_ordering_test() {
  let assert Ok(geometry1) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material1) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )

  let previous = []
  let current = [
    scene.Group(id: "root", transform: transform.identity, children: [
      scene.Group(id: "level1", transform: transform.identity, children: [
        scene.Group(id: "level2", transform: transform.identity, children: [
          scene.Mesh(
            id: "leaf",
            geometry: geometry1,
            material: material1,
            transform: transform.identity,
            physics: option.None,
          ),
        ]),
      ]),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Find indices
  let root_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("root", _, _) -> idx
        _ -> acc
      }
    })

  let level1_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("level1", _, _) -> idx
        _ -> acc
      }
    })

  let level2_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("level2", _, _) -> idx
        _ -> acc
      }
    })

  let leaf_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("leaf", _, _) -> idx
        _ -> acc
      }
    })

  // Each level must come before the next
  assert root_index >= 0
  assert level1_index >= 0
  assert level2_index >= 0
  assert leaf_index >= 0
  assert root_index < level1_index
  assert level1_index < level2_index
  assert level2_index < leaf_index
}

// Test: Multiple groups at same level can be in any order
pub fn sibling_groups_ordering_test() {
  let assert Ok(geometry1) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
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

  let previous = []
  let current = [
    scene.Group(id: "group1", transform: transform.identity, children: [
      scene.Mesh(
        id: "child1",
        geometry: geometry1,
        material: material1,
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
    scene.Group(id: "group2", transform: transform.identity, children: [
      scene.Mesh(
        id: "child2",
        geometry: geometry1,
        material: material2,
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Find indices
  let group1_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("group1", _, _) -> idx
        _ -> acc
      }
    })

  let group2_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("group2", _, _) -> idx
        _ -> acc
      }
    })

  let child1_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("child1", _, _) -> idx
        _ -> acc
      }
    })

  let child2_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("child2", _, _) -> idx
        _ -> acc
      }
    })

  // Each group must come before its own child
  assert group1_index >= 0
  assert group2_index >= 0
  assert child1_index >= 0
  assert child2_index >= 0
  assert group1_index < child1_index
  assert group2_index < child2_index
}

// Test: Complex hierarchy with moon orbiting planet orbiting sun
pub fn solar_system_ordering_test() {
  let assert Ok(geometry1) =
    geometry.sphere(radius: 1.5, width_segments: 32, height_segments: 32)
  let assert Ok(geometry2) =
    geometry.sphere(radius: 0.5, width_segments: 32, height_segments: 32)
  let assert Ok(geometry3) =
    geometry.sphere(radius: 0.2, width_segments: 16, height_segments: 16)
  let assert Ok(material1) =
    material.basic(
      color: 0xffff00,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let assert Ok(material2) =
    material.basic(
      color: 0x4ecdc4,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let assert Ok(material3) =
    material.basic(
      color: 0xcccccc,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )

  let previous = []
  let current = [
    scene.Mesh(
      id: "sun",
      geometry: geometry1,
      material: material1,
      transform: transform.identity,
      physics: option.None,
    ),
    scene.Group(id: "planet_orbit", transform: transform.identity, children: [
      scene.Mesh(
        id: "planet",
        geometry: geometry2,
        material: material2,
        transform: transform.at(position: vec3.Vec3(4.0, 0.0, 0.0)),
        physics: option.None,
      ),
      scene.Group(
        id: "moon_orbit",
        transform: transform.at(position: vec3.Vec3(4.0, 0.0, 0.0)),
        children: [
          scene.Mesh(
            id: "moon",
            geometry: geometry3,
            material: material3,
            transform: transform.at(position: vec3.Vec3(1.0, 0.0, 0.0)),
            physics: option.None,
          ),
        ],
      ),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Find indices
  let planet_orbit_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("planet_orbit", _, _) -> idx
        _ -> acc
      }
    })

  let planet_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("planet", _, _) -> idx
        _ -> acc
      }
    })

  let moon_orbit_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("moon_orbit", _, _) -> idx
        _ -> acc
      }
    })

  let moon_index =
    list.index_fold(patches, -1, fn(acc, patch, idx) {
      case patch {
        scene.AddNode("moon", _, _) -> idx
        _ -> acc
      }
    })

  // Verify correct ordering
  assert planet_orbit_index >= 0
  assert planet_index >= 0
  assert moon_orbit_index >= 0
  assert moon_index >= 0

  // planet_orbit must come before planet and moon_orbit
  assert planet_orbit_index < planet_index
  assert planet_orbit_index < moon_orbit_index

  // moon_orbit must come before moon
  assert moon_orbit_index < moon_index
}
