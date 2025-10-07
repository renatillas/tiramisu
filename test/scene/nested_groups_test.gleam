import gleam/list
import gleam/option
import tiramisu/scene
import tiramisu/transform

// Test: nested group structure tracks parent IDs correctly
pub fn nested_group_parent_test() {
  let previous = []
  let current = [
    scene.Group(id: "parent_group", transform: transform.identity, children: [
      scene.Mesh(
        id: "child_mesh",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Should have 2 patches: add parent group, add child mesh
  assert list.length(patches) == 2

  // Find the child mesh patch
  let child_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("child_mesh", _, _) -> True
        _ -> False
      }
    })

  // Verify child has parent_group as parent
  assert case child_patch {
    Ok(scene.AddNode(_, _, option.Some("parent_group"))) -> True
    _ -> False
  }
}

// Test: deeply nested groups
pub fn deeply_nested_groups_test() {
  let previous = []
  let current = [
    scene.Group(id: "root", transform: transform.identity, children: [
      scene.Group(id: "level1", transform: transform.identity, children: [
        scene.Mesh(
          id: "leaf",
          geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
          material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
          transform: transform.identity,
          physics: option.None,
        ),
      ]),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Should have 3 patches: root, level1, leaf
  assert list.length(patches) == 3

  // Verify level1 has root as parent
  let level1_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("level1", _, _) -> True
        _ -> False
      }
    })

  assert case level1_patch {
    Ok(scene.AddNode(_, _, option.Some("root"))) -> True
    _ -> False
  }

  // Verify leaf has level1 as parent
  let leaf_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("leaf", _, _) -> True
        _ -> False
      }
    })

  assert case leaf_patch {
    Ok(scene.AddNode(_, _, option.Some("level1"))) -> True
    _ -> False
  }
}

// Test: multiple children in same group
pub fn multiple_children_test() {
  let previous = []
  let current = [
    scene.Group(id: "container", transform: transform.identity, children: [
      scene.Mesh(
        id: "child1",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
      scene.Mesh(
        id: "child2",
        geometry: scene.SphereGeometry(1.0, 32, 32),
        material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Should have 3 patches: container, child1, child2
  assert list.length(patches) == 3

  // Both children should have container as parent
  let child1_has_correct_parent =
    list.any(patches, fn(patch) {
      case patch {
        scene.AddNode("child1", _, option.Some("container")) -> True
        _ -> False
      }
    })

  let child2_has_correct_parent =
    list.any(patches, fn(patch) {
      case patch {
        scene.AddNode("child2", _, option.Some("container")) -> True
        _ -> False
      }
    })

  assert child1_has_correct_parent
  assert child2_has_correct_parent
}

// Test: root level nodes have no parent
pub fn root_level_no_parent_test() {
  let previous = []
  let current = [
    scene.Mesh(
      id: "root_mesh",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.AddNode("root_mesh", _, option.None)) -> True
    _ -> False
  }
}
