import gleam/list
import gleam/option
import tiramisu/scene
import tiramisu/transform
import vec/vec3

// Test: Removing a group removes all its children
pub fn remove_group_with_children_test() {
  let previous = [
    scene.Group(id: "group1", transform: transform.identity, children: [
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
  let current = []
  let patches = scene.diff(previous, current)

  // Should remove group and both children (3 RemoveNode patches)
  assert list.length(patches) == 3

  let has_group_removal =
    list.any(patches, fn(patch) {
      case patch {
        scene.RemoveNode("group1") -> True
        _ -> False
      }
    })

  let has_child1_removal =
    list.any(patches, fn(patch) {
      case patch {
        scene.RemoveNode("child1") -> True
        _ -> False
      }
    })

  let has_child2_removal =
    list.any(patches, fn(patch) {
      case patch {
        scene.RemoveNode("child2") -> True
        _ -> False
      }
    })

  assert has_group_removal
  assert has_child1_removal
  assert has_child2_removal
}

// Test: Removing nested groups removes all descendants
pub fn remove_nested_groups_test() {
  let previous = [
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
  let current = []
  let patches = scene.diff(previous, current)

  // Should remove root, level1, and leaf
  assert list.length(patches) == 3
}

// Test: Update group transform
pub fn update_group_transform_test() {
  let previous = [
    scene.Group(id: "group1", transform: transform.identity, children: [
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
    scene.Group(
      id: "group1",
      transform: transform.at(position: vec3.Vec3(5.0, 0.0, 0.0)),
      children: [
        scene.Mesh(
          id: "child1",
          geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
          material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
          transform: transform.identity,
          physics: option.None,
        ),
      ],
    ),
  ]
  let patches = scene.diff(previous, current)

  // Should have 1 patch: UpdateTransform for group1
  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.UpdateTransform("group1", transform)) ->
      transform.position.x == 5.0
    _ -> False
  }
}

// Test: Add child to existing group
pub fn add_child_to_existing_group_test() {
  let previous = [
    scene.Group(id: "group1", transform: transform.identity, children: [
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
    scene.Group(id: "group1", transform: transform.identity, children: [
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

  // Should have 1 patch: AddNode for child2
  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.AddNode("child2", _, option.Some("group1"))) -> True
    _ -> False
  }
}

// Test: Remove child from group (group remains)
pub fn remove_child_from_group_test() {
  let previous = [
    scene.Group(id: "group1", transform: transform.identity, children: [
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
    scene.Group(id: "group1", transform: transform.identity, children: [
      scene.Mesh(
        id: "child1",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Should have 1 patch: RemoveNode for child2
  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.RemoveNode("child2")) -> True
    _ -> False
  }
}

// Test: Moving a node from one group to another
pub fn move_node_between_groups_test() {
  let previous = [
    scene.Group(id: "group1", transform: transform.identity, children: [
      scene.Mesh(
        id: "movable",
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity,
        physics: option.None,
      ),
    ]),
    scene.Group(id: "group2", transform: transform.identity, children: []),
  ]
  let current = [
    scene.Group(id: "group1", transform: transform.identity, children: []),
    scene.Group(id: "group2", transform: transform.identity, children: [
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

  // Moving a node between groups should result in remove + add
  // Because the parent_id changes
  assert list.length(patches) == 2

  let has_removal =
    list.any(patches, fn(patch) {
      case patch {
        scene.RemoveNode("movable") -> True
        _ -> False
      }
    })

  let has_addition =
    list.any(patches, fn(patch) {
      case patch {
        scene.AddNode("movable", _, option.Some("group2")) -> True
        _ -> False
      }
    })

  assert has_removal
  assert has_addition
}

// Test: Empty group (group with no children)
pub fn empty_group_test() {
  let previous = []
  let current = [
    scene.Group(id: "empty_group", transform: transform.identity, children: []),
  ]
  let patches = scene.diff(previous, current)

  // Should add just the group
  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.AddNode("empty_group", scene.Group(_, _, []), option.None)) -> True
    _ -> False
  }
}

// Test: Group with only lights
pub fn group_with_lights_test() {
  let previous = []
  let current = [
    scene.Group(id: "light_group", transform: transform.identity, children: [
      scene.Light(
        id: "light1",
        light_type: scene.PointLight(0xffffff, 1.0, 10.0),
        transform: transform.identity,
      ),
      scene.Light(
        id: "light2",
        light_type: scene.AmbientLight(0x404040, 0.5),
        transform: transform.identity,
      ),
    ]),
  ]
  let patches = scene.diff(previous, current)

  // Should add group and both lights
  assert list.length(patches) == 3

  let has_group =
    list.any(patches, fn(patch) {
      case patch {
        scene.AddNode("light_group", scene.Group(_, _, _), _) -> True
        _ -> False
      }
    })

  let has_light1 =
    list.any(patches, fn(patch) {
      case patch {
        scene.AddNode(
          "light1",
          scene.Light(_, _, _),
          option.Some("light_group"),
        ) -> True
        _ -> False
      }
    })

  let has_light2 =
    list.any(patches, fn(patch) {
      case patch {
        scene.AddNode(
          "light2",
          scene.Light(_, _, _),
          option.Some("light_group"),
        ) -> True
        _ -> False
      }
    })

  assert has_group
  assert has_light1
  assert has_light2
}
