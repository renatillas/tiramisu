import gleam/list
import gleam/option
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec2
import vec/vec3

pub fn diff_empty_to_single_node_test() {
  let assert Ok(geom) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev = option.None
  let next =
    option.Some(scene.mesh(
      id: "cube1",
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.AddNode(id: "cube1", node: _, parent_id: option.None)) =
    list.first(patches)
}

pub fn diff_single_node_to_empty_test() {
  let assert Ok(geom) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev =
    option.Some(scene.mesh(
      id: "cube1",
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))
  let next = option.None

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.RemoveNode(id: "cube1")) = list.first(patches)
}

pub fn diff_transform_change_test() {
  let assert Ok(geom) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev =
    option.Some(scene.mesh(
      id: "cube1",
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))
  let next =
    option.Some(scene.mesh(
      id: "cube1",
      geometry: geom,
      material: mat,
      transform: transform.at(vec3.Vec3(5.0, 0.0, 0.0)),
      physics: option.None,
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.UpdateTransform(id: "cube1", transform: t)) =
    list.first(patches)

  assert transform.position(t) == vec3.Vec3(5.0, 0.0, 0.0)
}

pub fn diff_material_change_test() {
  let assert Ok(geom) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(mat1) = material.basic(0xff0000, False, 1.0, option.None)
  let assert Ok(mat2) = material.basic(0x00ff00, False, 1.0, option.None)

  let prev =
    option.Some(scene.mesh(
      id: "cube1",
      geometry: geom,
      material: mat1,
      transform: transform.identity,
      physics: option.None,
    ))
  let next =
    option.Some(scene.mesh(
      id: "cube1",
      geometry: geom,
      material: mat2,
      transform: transform.identity,
      physics: option.None,
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.UpdateMaterial(id: "cube1", material: updated_mat)) =
    list.first(patches)
  assert updated_mat == option.Some(mat2)
}

pub fn diff_geometry_change_test() {
  let assert Ok(geom1) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(geom2) = geometry.sphere(radius: 2.0, segments: vec2.Vec2(32, 16))
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev =
    option.Some(scene.mesh(
      id: "cube1",
      geometry: geom1,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))
  let next =
    option.Some(scene.mesh(
      id: "cube1",
      geometry: geom2,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.UpdateGeometry(id: "cube1", geometry: updated_geom)) =
    list.first(patches)
  assert updated_geom == geom2
}

pub fn diff_multiple_changes_test() {
  let assert Ok(geom) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)
  let assert Ok(light_obj) = light.directional(1.0, 0xffffff)

  let prev =
    option.Some(
      scene.empty(id: "root", transform: transform.identity, children: [
        scene.mesh(
          id: "cube1",
          geometry: geom,
          material: mat,
          transform: transform.identity,
          physics: option.None,
        ),
      ]),
    )
  let next =
    option.Some(
      scene.empty(id: "root", transform: transform.identity, children: [
        scene.mesh(
          id: "cube1",
          geometry: geom,
          material: mat,
          transform: transform.at(vec3.Vec3(10.0, 0.0, 0.0)),
          physics: option.None,
        ),
        scene.light(
          id: "light1",
          light: light_obj,
          transform: transform.identity,
        ),
      ]),
    )

  let #(patches, _) = scene.diff(prev, next, option.None)

  // Should have 2 patches: UpdateTransform and AddNode
  assert list.length(patches) == 2
}

pub fn diff_no_changes_test() {
  let assert Ok(geom) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev =
    option.Some(scene.mesh(
      id: "cube1",
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))
  let next =
    option.Some(scene.mesh(
      id: "cube1",
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  assert list.is_empty(patches)
}

// Test that patches for hierarchical scenes are ordered correctly:
// Parents must be added before their children
pub fn diff_hierarchy_ordering_test() {
  let assert Ok(geom) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev = option.None
  let next =
    option.Some(
      scene.empty(id: "parent", transform: transform.identity, children: [
        scene.mesh(
          id: "child1",
          geometry: geom,
          material: mat,
          transform: transform.identity,
          physics: option.None,
        ),
        scene.mesh(
          id: "child2",
          geometry: geom,
          material: mat,
          transform: transform.identity,
          physics: option.None,
        ),
      ]),
    )

  let #(patches, _) = scene.diff(prev, next, option.None)

  // Should have 3 AddNode patches
  assert list.length(patches) == 3

  // First patch should be the Parent (depth 0)
  let assert Ok(scene.AddNode(id: "parent", node: _, parent_id: option.None)) =
    list.first(patches)

  // Second and third patches should be children (depth 1)
  // Both should have Parent as parent_id
  let assert Ok(rest) = list.rest(patches)
  let assert Ok(scene.AddNode(
    id: child1_id,
    node: _,
    parent_id: option.Some("parent"),
  )) = list.first(rest)

  let assert Ok(rest2) = list.rest(rest)
  let assert Ok(scene.AddNode(
    id: child2_id,
    node: _,
    parent_id: option.Some("parent"),
  )) = list.first(rest2)

  // Both children should be present (order between children doesn't matter)
  assert child1_id == "child1" || child1_id == "child2"
  assert child2_id == "child1" || child2_id == "child2"
  assert child1_id != child2_id
}

// Test deeper hierarchy with grandchildren
pub fn diff_deep_hierarchy_ordering_test() {
  let assert Ok(geom) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev = option.None
  let next =
    option.Some(
      scene.empty(id: "parent", transform: transform.identity, children: [
        scene.empty(id: "child1", transform: transform.identity, children: [
          scene.mesh(
            id: "grandchild",
            geometry: geom,
            material: mat,
            transform: transform.identity,
            physics: option.None,
          ),
        ]),
      ]),
    )

  let #(patches, _) = scene.diff(prev, next, option.None)

  // Should have 3 AddNode patches
  assert list.length(patches) == 3

  // Verify order: Parent (depth 0), Child1 (depth 1), Grandchild (depth 2)
  let assert Ok(scene.AddNode(id: "parent", node: _, parent_id: option.None)) =
    list.first(patches)

  let assert Ok(rest) = list.rest(patches)
  let assert Ok(scene.AddNode(
    id: "child1",
    node: _,
    parent_id: option.Some("parent"),
  )) = list.first(rest)

  let assert Ok(rest2) = list.rest(rest)
  let assert Ok(scene.AddNode(
    id: "grandchild",
    node: _,
    parent_id: option.Some("child1"),
  )) = list.first(rest2)
}
