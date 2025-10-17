import gleam/option
import gleeunit/should
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

type TestId {
  Cube1
  Light1
}

pub fn diff_empty_to_single_node_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev = []
  let next = [
    scene.Mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [scene.AddNode(id: Cube1, node: _, parent_id: option.None)] -> Nil
    _ -> should.fail()
  }
}

pub fn diff_single_node_to_empty_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev = [
    scene.Mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let next = []

  let patches = scene.diff(prev, next)

  case patches {
    [scene.RemoveNode(id: Cube1)] -> Nil
    _ -> should.fail()
  }
}

pub fn diff_transform_change_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev = [
    scene.Mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let next = [
    scene.Mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.at(vec3.Vec3(5.0, 0.0, 0.0)),
      physics: option.None,
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [scene.UpdateTransform(id: Cube1, transform: t)] -> {
      assert transform.position(t) == vec3.Vec3(5.0, 0.0, 0.0)
    }
    _ -> should.fail()
  }
}

pub fn diff_material_change_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat1) = material.basic(0xff0000, False, 1.0, option.None)
  let assert Ok(mat2) = material.basic(0x00ff00, False, 1.0, option.None)

  let prev = [
    scene.Mesh(
      id: Cube1,
      geometry: geom,
      material: mat1,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let next = [
    scene.Mesh(
      id: Cube1,
      geometry: geom,
      material: mat2,
      transform: transform.identity,
      physics: option.None,
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [scene.UpdateMaterial(id: Cube1, material: _)] -> Nil
    _ -> should.fail()
  }
}

pub fn diff_geometry_change_test() {
  let assert Ok(geom1) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(geom2) = geometry.sphere(2.0, 32, 16)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev = [
    scene.Mesh(
      id: Cube1,
      geometry: geom1,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let next = [
    scene.Mesh(
      id: Cube1,
      geometry: geom2,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [scene.UpdateGeometry(id: Cube1, geometry: _)] -> Nil
    _ -> should.fail()
  }
}

pub fn diff_multiple_changes_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)
  let assert Ok(light_obj) = light.directional(1.0, 0xffffff)

  let prev = [
    scene.Mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let next = [
    scene.Mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.at(vec3.Vec3(10.0, 0.0, 0.0)),
      physics: option.None,
    ),
    scene.Light(id: Light1, light: light_obj, transform: transform.identity),
  ]

  let patches = scene.diff(prev, next)

  // Should have 2 patches: UpdateTransform and AddNode
  case patches {
    [_, _] -> Nil
    _ -> should.fail()
  }
}

pub fn diff_no_changes_test() {
  let assert Ok(geom) = geometry.box(1.0, 1.0, 1.0)
  let assert Ok(mat) = material.basic(0xff0000, False, 1.0, option.None)

  let prev = [
    scene.Mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ),
  ]
  let next = [
    scene.Mesh(
      id: Cube1,
      geometry: geom,
      material: mat,
      transform: transform.identity,
      physics: option.None,
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [] -> Nil
    _ -> should.fail()
  }
}
