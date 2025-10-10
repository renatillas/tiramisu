import gleam/list
import gleam/option
import tiramisu/camera
import tiramisu/geometry
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

/// Test: camera in nested group has correct parent ID in patches
/// This prevents regression of the bug where cameras ignored parent transforms
pub fn camera_in_nested_group_test() {
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let previous = []
  let current = [
    scene.Group(id: "moving_group", transform: transform.identity, children: [
      scene.Camera(
        id: "nested_camera",
        camera: cam,
        transform: transform.identity,
        look_at: option.None,
        active: True,
        viewport: option.None,
      ),
    ]),
  ]

  let patches = scene.diff(previous, current)

  // Should have 2 patches: add group, add camera
  assert list.length(patches) == 2

  // Verify camera has moving_group as parent
  let camera_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("nested_camera", _, _) -> True
        _ -> False
      }
    })

  assert case camera_patch {
    Ok(scene.AddNode(_, _, option.Some("moving_group"))) -> True
    _ -> False
  }
}

/// Test: camera in deeply nested groups maintains correct hierarchy
pub fn camera_deeply_nested_test() {
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let previous = []
  let current = [
    scene.Group(id: "root_group", transform: transform.identity, children: [
      scene.Group(id: "child_group", transform: transform.identity, children: [
        scene.Camera(
          id: "deep_camera",
          camera: cam,
          transform: transform.identity,
          look_at: option.None,
          active: True,
          viewport: option.None,
        ),
      ]),
    ]),
  ]

  let patches = scene.diff(previous, current)

  // Should have 3 patches: root, child, camera
  assert list.length(patches) == 3

  // Verify child_group has root_group as parent
  let child_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("child_group", _, _) -> True
        _ -> False
      }
    })

  assert case child_patch {
    Ok(scene.AddNode(_, _, option.Some("root_group"))) -> True
    _ -> False
  }

  // Verify camera has child_group as parent
  let camera_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("deep_camera", _, _) -> True
        _ -> False
      }
    })

  assert case camera_patch {
    Ok(scene.AddNode(_, _, option.Some("child_group"))) -> True
    _ -> False
  }
}

/// Test: camera with transform inside group generates correct patches
pub fn camera_with_transform_in_group_test() {
  let assert Ok(cam_result) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let camera_transform =
    transform.identity
    |> transform.set_position(vec3.Vec3(1.0, 2.0, 3.0))

  let previous = []
  let current = [
    scene.Group(
      id: "vehicle",
      transform: transform.at(position: vec3.Vec3(10.0, 0.0, 0.0)),
      children: [
        scene.Camera(
          id: "third_person_camera",
          camera: cam_result,
          transform: camera_transform,
          look_at: option.None,
          active: True,
          viewport: option.None,
        ),
      ],
    ),
  ]

  let patches = scene.diff(previous, current)

  // Should have 2 patches: add vehicle group, add camera
  assert list.length(patches) == 2

  // Verify camera patch exists with correct parent and has a Camera node
  let camera_patch =
    list.find(patches, fn(patch) {
      case patch {
        scene.AddNode("third_person_camera", scene.Camera(..), _) -> True
        _ -> False
      }
    })

  assert case camera_patch {
    Ok(scene.AddNode(
      "third_person_camera",
      scene.Camera(camera: _, transform: t, ..),
      option.Some("vehicle"),
    )) -> {
      // Verify the transform was preserved
      t.position.x == 1.0 && t.position.y == 2.0 && t.position.z == 3.0
    }
    _ -> False
  }
}

/// Test: moving a camera between groups generates correct patches
pub fn camera_move_between_groups_test() {
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let camera_node =
    scene.Camera(
      id: "moving_cam",
      camera: cam,
      transform: transform.identity,
      look_at: option.None,
      active: True,
      viewport: option.None,
    )

  let previous = [
    scene.Group(id: "group_a", transform: transform.identity, children: [
      camera_node,
    ]),
  ]

  let current = [
    scene.Group(id: "group_b", transform: transform.identity, children: [
      camera_node,
    ]),
  ]

  let patches = scene.diff(previous, current)

  // Should remove from group_a and add to group_b
  let has_remove_group_a =
    list.any(patches, fn(patch) {
      case patch {
        scene.RemoveNode("group_a") -> True
        _ -> False
      }
    })

  let has_add_to_group_b =
    list.any(patches, fn(patch) {
      case patch {
        scene.AddNode("moving_cam", _, option.Some("group_b")) -> True
        _ -> False
      }
    })

  assert has_remove_group_a
  assert has_add_to_group_b
}

/// Test: UpdateCamera patch preserves parent relationship
pub fn update_camera_in_group_test() {
  let assert Ok(cam1) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let assert Ok(cam2_result) =
    camera.perspective(field_of_view: 90.0, near: 0.1, far: 1000.0)

  let previous = [
    scene.Group(id: "camera_rig", transform: transform.identity, children: [
      scene.Camera(
        id: "game_cam",
        camera: cam1,
        transform: transform.identity,
        look_at: option.None,
        active: True,
        viewport: option.None,
      ),
    ]),
  ]

  let current = [
    scene.Group(id: "camera_rig", transform: transform.identity, children: [
      scene.Camera(
        id: "game_cam",
        camera: cam2_result,
        transform: transform.identity,
        look_at: option.None,
        active: True,
        viewport: option.None,
      ),
    ]),
  ]

  let patches = scene.diff(previous, current)

  // Should generate UpdateCamera patch
  let has_update_camera =
    list.any(patches, fn(patch) {
      case patch {
        scene.UpdateCamera("game_cam", _, _) -> True
        _ -> False
      }
    })

  assert has_update_camera

  // Should NOT generate RemoveNode or AddNode for the camera
  let has_remove_camera =
    list.any(patches, fn(patch) {
      case patch {
        scene.RemoveNode("game_cam") -> True
        _ -> False
      }
    })

  let has_add_camera =
    list.any(patches, fn(patch) {
      case patch {
        scene.AddNode("game_cam", _, _) -> True
        _ -> False
      }
    })

  assert !has_remove_camera
  assert !has_add_camera
}

/// Test: camera and mesh as siblings in same group
pub fn camera_and_mesh_siblings_test() {
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let assert Ok(geometry1) = geometry.box(width: 2.0, height: 1.0, depth: 3.0)
  let assert Ok(material1) =
    material.basic(
      color: 0x00ff00,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )

  let previous = []
  let current = [
    scene.Group(id: "helicopter", transform: transform.identity, children: [
      scene.Mesh(
        id: "heli_body",
        geometry: geometry1,
        material: material1,
        transform: transform.identity,
        physics: option.None,
      ),
      scene.Camera(
        id: "cockpit_cam",
        camera: cam,
        transform: transform.at(position: vec3.Vec3(0.0, 1.0, 0.0)),
        look_at: option.None,
        active: True,
        viewport: option.None,
      ),
    ]),
  ]

  let patches = scene.diff(previous, current)

  // Should have 3 patches: helicopter, heli_body, cockpit_cam
  assert list.length(patches) == 3

  // Both mesh and camera should have helicopter as parent
  let mesh_has_correct_parent =
    list.any(patches, fn(patch) {
      case patch {
        scene.AddNode("heli_body", _, option.Some("helicopter")) -> True
        _ -> False
      }
    })

  let camera_has_correct_parent =
    list.any(patches, fn(patch) {
      case patch {
        scene.AddNode("cockpit_cam", _, option.Some("helicopter")) -> True
        _ -> False
      }
    })

  assert mesh_has_correct_parent
  assert camera_has_correct_parent
}

/// Test: camera at root level has no parent
pub fn camera_at_root_level_test() {
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let previous = []
  let current = [
    scene.Camera(
      id: "main_camera",
      camera: cam,
      transform: transform.identity,
      look_at: option.None,
      active: True,
      viewport: option.None,
    ),
  ]

  let patches = scene.diff(previous, current)

  assert list.length(patches) == 1

  assert case list.first(patches) {
    Ok(scene.AddNode("main_camera", scene.Camera(..), option.None)) -> True
    _ -> False
  }
}
