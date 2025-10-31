import gleam/list
import gleam/option
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type TestId {
  Cube1
  Light1
  Root
}

pub fn diff_empty_to_single_node_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev = option.None
  let next =
    option.Some(scene.mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))

  let patches = scene.diff(prev, next)

  let assert Ok(scene.AddNode(id: Cube1, node: _, parent_id: option.None)) =
    list.first(patches)
}

pub fn diff_single_node_to_empty_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev =
    option.Some(scene.mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))
  let next = option.None

  let patches = scene.diff(prev, next)

  let assert Ok(scene.RemoveNode(id: Cube1)) = list.first(patches)
}

pub fn diff_transform_change_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev =
    option.Some(scene.mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))
  let next =
    option.Some(scene.mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.at(vec3.Vec3(5.0, 0.0, 0.0)),
      physics: option.None,
    ))

  let patches = scene.diff(prev, next)

  let assert Ok(scene.UpdateTransform(id: Cube1, transform: t)) =
    list.first(patches)

  assert transform.position(t) == vec3.Vec3(5.0, 0.0, 0.0)
}

pub fn diff_material_change_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat1) = material.basic(0xff0000, False, 1.0, option.None)
  let assert Ok(mat2) = material.basic(0x00ff00, False, 1.0, option.None)

  let prev =
    option.Some(scene.mesh(
      id: Cube1,
      geometry: geom,
      material: mat1,
      transform: transform.identity,
      physics: option.None,
    ))
  let next =
    option.Some(scene.mesh(
      id: Cube1,
      geometry: geom,
      material: mat2,
      transform: transform.identity,
      physics: option.None,
    ))

  let patches = scene.diff(prev, next)

  let assert Ok(scene.UpdateMaterial(id: Cube1, material: updated_mat)) =
    list.first(patches)
  assert updated_mat == option.Some(mat2)
}

pub fn diff_geometry_change_test() {
  let assert Ok(geom1) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(geom2) = geometry.sphere(2.0, 32, 16)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev =
    option.Some(scene.mesh(
      id: Cube1,
      geometry: geom1,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))
  let next =
    option.Some(scene.mesh(
      id: Cube1,
      geometry: geom2,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))

  let patches = scene.diff(prev, next)

  let assert Ok(scene.UpdateGeometry(id: Cube1, geometry: updated_geom)) =
    list.first(patches)
  assert updated_geom == geom2
}

pub fn diff_multiple_changes_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)
  let assert Ok(light_obj) = light.directional(1.0, 0xffffff)

  let prev =
    option.Some(
      scene.empty(id: Root, transform: transform.identity, children: [
        scene.mesh(
          id: Cube1,
          geometry: geom,
          material: mat,
          transform: transform.identity,
          physics: option.None,
        ),
      ]),
    )
  let next =
    option.Some(
      scene.empty(id: Root, transform: transform.identity, children: [
        scene.mesh(
          id: Cube1,
          geometry: geom,
          material: mat,
          transform: transform.at(vec3.Vec3(10.0, 0.0, 0.0)),
          physics: option.None,
        ),
        scene.light(id: Light1, light: light_obj, transform: transform.identity),
      ]),
    )

  let patches = scene.diff(prev, next)

  // Should have 2 patches: UpdateTransform and AddNode
  assert list.length(patches) == 2
}

pub fn diff_no_changes_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev =
    option.Some(scene.mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))
  let next =
    option.Some(scene.mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ))

  let patches = scene.diff(prev, next)

  assert list.is_empty(patches)
}
