import gleam/list
import gleam/option
import tiramisu/math/vec3
import tiramisu/scene
import tiramisu/transform

// Test: Parent group is added before children in patches
pub fn parent_before_children_test() {
  let previous = []
  let current = [
    scene.Group(id: "parent", transform: transform.identity(), children: [
      scene.Mesh(
        id: "child1",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity(),
        physics: option.None,
      ),
      scene.Mesh(
        id: "child2",
        geometry: scene.SphereGeometry(1.0, 32, 32),
        material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
        transform: transform.identity(),
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
  let previous = []
  let current = [
    scene.Group(id: "root", transform: transform.identity(), children: [
      scene.Group(id: "level1", transform: transform.identity(), children: [
        scene.Group(id: "level2", transform: transform.identity(), children: [
          scene.Mesh(
            id: "leaf",
            geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
            material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
            transform: transform.identity(),
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
    scene.Group(id: "group2", transform: transform.identity(), children: [
      scene.Mesh(
        id: "child2",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
        transform: transform.identity(),
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
  let previous = []
  let current = [
    scene.Mesh(
      id: "sun",
      geometry: scene.SphereGeometry(1.5, 32, 32),
      material: scene.BasicMaterial(0xffff00, False, 1.0, option.None),
      transform: transform.identity(),
      physics: option.None,
    ),
    scene.Group(id: "planet_orbit", transform: transform.identity(), children: [
      scene.Mesh(
        id: "planet",
        geometry: scene.SphereGeometry(0.5, 32, 32),
        material: scene.BasicMaterial(0x4ecdc4, False, 1.0, option.None),
        transform: transform.at(position: vec3.Vec3(4.0, 0.0, 0.0)),
        physics: option.None,
      ),
      scene.Group(
        id: "moon_orbit",
        transform: transform.at(position: vec3.Vec3(4.0, 0.0, 0.0)),
        children: [
          scene.Mesh(
            id: "moon",
            geometry: scene.SphereGeometry(0.2, 16, 16),
            material: scene.BasicMaterial(0xcccccc, False, 1.0, option.None),
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
