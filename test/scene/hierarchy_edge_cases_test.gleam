import gleam/int
import gleam/list
import gleam/option
import tiramisu/scene
import tiramisu/transform
import vec/vec3

// Test: Empty groups should be created correctly
pub fn empty_group_test() {
  let previous = []
  let current = [
    scene.Group(id: "empty_group", transform: transform.identity, children: []),
  ]
  let patches = scene.diff(previous, current)

  // Should have 1 patch: add empty group
  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.AddNode("empty_group", _, option.None)) -> True
    _ -> False
  }
}

// Test: Very deep nesting (10 levels)
pub fn very_deep_nesting_test() {
  let previous = []

  // Create 10-level deep hierarchy
  let level9 =
    scene.Mesh(
      id: "level9",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.identity,
      physics: option.None,
    )

  let level8 =
    scene.Group(id: "level8", transform: transform.identity, children: [
      level9,
    ])
  let level7 =
    scene.Group(id: "level7", transform: transform.identity, children: [
      level8,
    ])
  let level6 =
    scene.Group(id: "level6", transform: transform.identity, children: [
      level7,
    ])
  let level5 =
    scene.Group(id: "level5", transform: transform.identity, children: [
      level6,
    ])
  let level4 =
    scene.Group(id: "level4", transform: transform.identity, children: [
      level5,
    ])
  let level3 =
    scene.Group(id: "level3", transform: transform.identity, children: [
      level4,
    ])
  let level2 =
    scene.Group(id: "level2", transform: transform.identity, children: [
      level3,
    ])
  let level1 =
    scene.Group(id: "level1", transform: transform.identity, children: [
      level2,
    ])
  let level0 =
    scene.Group(id: "level0", transform: transform.identity, children: [
      level1,
    ])

  let current = [level0]
  let patches = scene.diff(previous, current)

  // Should have 10 patches: one for each level
  assert list.length(patches) == 10

  // Verify that level9 has level8 as parent
  let level9_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("level9", _, _) -> True
        _ -> False
      }
    })

  assert case level9_patch {
    Ok(scene.AddNode(_, _, option.Some("level8"))) -> True
    _ -> False
  }
}

// Test: Wide hierarchy (many children in one group)
pub fn wide_hierarchy_test() {
  let previous = []

  // Create 20 children in one group
  let children =
    list.range(0, 19)
    |> list.map(fn(i) {
      scene.Mesh(
        id: "child" <> int.to_string(i),
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      )
    })

  let current = [
    scene.Group(
      id: "wide_parent",
      transform: transform.identity,
      children: children,
    ),
  ]

  let patches = scene.diff(previous, current)

  // Should have 21 patches: 1 parent + 20 children
  assert list.length(patches) == 21

  // Verify all children have wide_parent as parent
  let child_patches =
    list.filter(patches, fn(patch) {
      case patch {
        scene.AddNode(id, _, _) -> id != "wide_parent"
        _ -> False
      }
    })

  assert list.length(child_patches) == 20

  // Check that all children have correct parent
  let all_have_parent =
    list.all(child_patches, fn(patch) {
      case patch {
        scene.AddNode(_, _, option.Some("wide_parent")) -> True
        _ -> False
      }
    })

  assert all_have_parent
}

// Test: Mixed node types as children
pub fn mixed_node_types_test() {
  let previous = []

  let current = [
    scene.Group(id: "mixed_group", transform: transform.identity, children: [
      scene.Mesh(
        id: "mesh_child",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
      scene.Light(
        id: "light_child",
        light_type: scene.PointLight(0xffffff, 1.0, 10.0),
        transform: transform.identity,
      ),
      scene.Group(
        id: "group_child",
        transform: transform.identity,
        children: [],
      ),
    ]),
  ]

  let patches = scene.diff(previous, current)

  // Should have 4 patches: 1 parent + 3 children of different types
  assert list.length(patches) == 4

  // Verify all children have mixed_group as parent
  let child_ids = ["mesh_child", "light_child", "group_child"]
  let all_children_present =
    list.all(child_ids, fn(id) {
      list.any(patches, fn(patch) {
        case patch {
          scene.AddNode(patch_id, _, option.Some("mixed_group")) ->
            patch_id == id
          _ -> False
        }
      })
    })

  assert all_children_present
}

// Test: Removing node from middle of hierarchy orphans children
pub fn remove_middle_node_test() {
  let previous = [
    scene.Group(id: "grandparent", transform: transform.identity, children: [
      scene.Group(id: "parent", transform: transform.identity, children: [
        scene.Mesh(
          id: "child",
          geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
          material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
          transform: transform.identity,
          physics: option.None,
        ),
      ]),
    ]),
  ]

  // Remove the middle node (parent)
  let current = [
    scene.Group(id: "grandparent", transform: transform.identity, children: []),
  ]

  let patches = scene.diff(previous, current)

  // Should remove both parent and child
  let remove_patches =
    list.filter(patches, fn(patch) {
      case patch {
        scene.RemoveNode(_) -> True
        _ -> False
      }
    })

  assert list.length(remove_patches) == 2

  // Verify both parent and child are removed
  let parent_removed =
    list.any(remove_patches, fn(patch) {
      case patch {
        scene.RemoveNode("parent") -> True
        _ -> False
      }
    })

  let child_removed =
    list.any(remove_patches, fn(patch) {
      case patch {
        scene.RemoveNode("child") -> True
        _ -> False
      }
    })

  assert parent_removed
  assert child_removed
}

// Test: Moving node between parents (uses RemoveNode + AddNode)
pub fn move_node_between_parents_test() {
  let previous = [
    scene.Group(id: "parent1", transform: transform.identity, children: [
      scene.Mesh(
        id: "movable",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
    scene.Group(id: "parent2", transform: transform.identity, children: []),
  ]

  let current = [
    scene.Group(id: "parent1", transform: transform.identity, children: []),
    scene.Group(id: "parent2", transform: transform.identity, children: [
      scene.Mesh(
        id: "movable",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]

  let patches = scene.diff(previous, current)

  // When a node moves between parents, it's removed and re-added
  let remove_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.RemoveNode("movable") -> True
        _ -> False
      }
    })

  let add_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("movable", _, _) -> True
        _ -> False
      }
    })

  assert case remove_patch {
    Ok(scene.RemoveNode(_)) -> True
    _ -> False
  }

  assert case add_patch {
    Ok(scene.AddNode(_, _, option.Some("parent2"))) -> True
    _ -> False
  }
}

// Test: Adding children to existing group
pub fn add_children_to_existing_group_test() {
  let previous = [
    scene.Group(id: "parent", transform: transform.identity, children: [
      scene.Mesh(
        id: "child1",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]

  let current = [
    scene.Group(id: "parent", transform: transform.identity, children: [
      scene.Mesh(
        id: "child1",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
      scene.Mesh(
        id: "child2",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]

  let patches = scene.diff(previous, current)

  // Should have AddNode patch for child2
  let add_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("child2", _, _) -> True
        _ -> False
      }
    })

  assert case add_patch {
    Ok(scene.AddNode(_, _, option.Some("parent"))) -> True
    _ -> False
  }
}

// Test: Removing all children from group
pub fn remove_all_children_test() {
  let previous = [
    scene.Group(id: "parent", transform: transform.identity, children: [
      scene.Mesh(
        id: "child1",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
      scene.Mesh(
        id: "child2",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]

  let current = [
    scene.Group(id: "parent", transform: transform.identity, children: []),
  ]

  let patches = scene.diff(previous, current)

  // Should have 2 RemoveNode patches
  let remove_patches =
    list.filter(patches, fn(patch) {
      case patch {
        scene.RemoveNode(_) -> True
        _ -> False
      }
    })

  assert list.length(remove_patches) == 2
}

// Test: Transform updates propagate through nested groups
pub fn nested_transform_updates_test() {
  let previous = [
    scene.Group(id: "parent", transform: transform.identity, children: [
      scene.Mesh(
        id: "child",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]

  let current = [
    scene.Group(
      id: "parent",
      transform: transform.at(position: vec3.Vec3(1.0, 2.0, 3.0)),
      children: [
        scene.Mesh(
          id: "child",
          geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
          material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
          transform: transform.identity,
          physics: option.None,
        ),
      ],
    ),
  ]

  let patches = scene.diff(previous, current)

  // Should have UpdateTransform patch for parent
  let update_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.UpdateTransform("parent", _) -> True
        _ -> False
      }
    })

  assert case update_patch {
    Ok(scene.UpdateTransform(_, _)) -> True
    _ -> False
  }
}

// Test: Sibling groups with different children
pub fn sibling_groups_test() {
  let previous = []

  let current = [
    scene.Group(id: "sibling1", transform: transform.identity, children: [
      scene.Mesh(
        id: "child1",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
    scene.Group(id: "sibling2", transform: transform.identity, children: [
      scene.Mesh(
        id: "child2",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]

  let patches = scene.diff(previous, current)

  // Should have 4 patches: 2 parents + 2 children
  assert list.length(patches) == 4

  // Verify child1 has sibling1 as parent
  let child1_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("child1", _, _) -> True
        _ -> False
      }
    })

  assert case child1_patch {
    Ok(scene.AddNode(_, _, option.Some("sibling1"))) -> True
    _ -> False
  }

  // Verify child2 has sibling2 as parent
  let child2_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("child2", _, _) -> True
        _ -> False
      }
    })

  assert case child2_patch {
    Ok(scene.AddNode(_, _, option.Some("sibling2"))) -> True
    _ -> False
  }
}

// Test: Group containing nested groups with same ID pattern
pub fn same_id_pattern_test() {
  let previous = []

  let current = [
    scene.Group(id: "container", transform: transform.identity, children: [
      scene.Group(id: "group_1", transform: transform.identity, children: [
        scene.Mesh(
          id: "mesh_1",
          geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
          material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
          transform: transform.identity,
          physics: option.None,
        ),
      ]),
      scene.Group(id: "group_2", transform: transform.identity, children: [
        scene.Mesh(
          id: "mesh_2",
          geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
          material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
          transform: transform.identity,
          physics: option.None,
        ),
      ]),
    ]),
  ]

  let patches = scene.diff(previous, current)

  // Should have 5 patches: 1 container + 2 groups + 2 meshes
  assert list.length(patches) == 5

  // Verify mesh_1 has group_1 as parent
  let mesh1_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("mesh_1", _, _) -> True
        _ -> False
      }
    })

  assert case mesh1_patch {
    Ok(scene.AddNode(_, _, option.Some("group_1"))) -> True
    _ -> False
  }

  // Verify group_1 has container as parent
  let group1_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("group_1", _, _) -> True
        _ -> False
      }
    })

  assert case group1_patch {
    Ok(scene.AddNode(_, _, option.Some("container"))) -> True
    _ -> False
  }
}

// Test: Removing parent but keeping grandparent and moving children up
pub fn restructure_hierarchy_test() {
  let previous = [
    scene.Group(id: "grandparent", transform: transform.identity, children: [
      scene.Group(id: "parent", transform: transform.identity, children: [
        scene.Mesh(
          id: "child",
          geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
          material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
          transform: transform.identity,
          physics: option.None,
        ),
      ]),
    ]),
  ]

  let current = [
    scene.Group(id: "grandparent", transform: transform.identity, children: [
      scene.Mesh(
        id: "child",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]

  let patches = scene.diff(previous, current)

  // Should remove parent
  let remove_parent_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.RemoveNode("parent") -> True
        _ -> False
      }
    })

  assert case remove_parent_patch {
    Ok(scene.RemoveNode(_)) -> True
    _ -> False
  }

  // Child is removed and re-added with new parent (grandparent)
  let remove_child_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.RemoveNode("child") -> True
        _ -> False
      }
    })

  let add_child_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("child", _, _) -> True
        _ -> False
      }
    })

  assert case remove_child_patch {
    Ok(scene.RemoveNode(_)) -> True
    _ -> False
  }

  assert case add_child_patch {
    Ok(scene.AddNode(_, _, option.Some("grandparent"))) -> True
    _ -> False
  }
}
