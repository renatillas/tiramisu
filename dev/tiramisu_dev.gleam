import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleamy/bench
import tiramisu/geometry
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub fn main() {
  bench.run(
    [
      bench.Input(
        "Small (10 nodes)",
        ScenePair(
          wrap_in_group("root", create_flat_scene(10)),
          wrap_in_group("root", create_flat_scene(10)),
        ),
      ),
      bench.Input(
        "Medium (100 nodes)",
        ScenePair(
          wrap_in_group("root", create_flat_scene(100)),
          wrap_in_group("root", create_flat_scene(100)),
        ),
      ),
      bench.Input(
        "Large (1000 nodes)",
        ScenePair(
          wrap_in_group("root", create_flat_scene(1000)),
          wrap_in_group("root", create_flat_scene(1000)),
        ),
      ),
      bench.Input(
        "Nested (10 levels)",
        ScenePair(
          wrap_in_group("root", create_nested_scene(10)),
          wrap_in_group("root", create_nested_scene(10)),
        ),
      ),
      bench.Input("No changes (100 nodes)", {
        let node = wrap_in_group("root", create_flat_scene(100))
        ScenePair(node, node)
      }),
      bench.Input(
        "All changed (100 nodes)",
        ScenePair(
          wrap_in_group("root", create_flat_scene(100)),
          wrap_in_group("root", create_flat_scene_offset(100, 10.0)),
        ),
      ),
    ],
    [
      bench.Function("scene.diff", fn(pair: ScenePair(String)) {
        scene.diff(pair.previous, pair.current, option.None).0
      }),
    ],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean, bench.Max, bench.P(99)])
  |> io.println
}

pub type ScenePair(id) {
  ScenePair(
    previous: option.Option(scene.Node),
    current: option.Option(scene.Node),
  )
}

// --- Helper Functions ---

fn wrap_in_group(
  id: String,
  children: List(scene.Node),
) -> option.Option(scene.Node) {
  option.Some(scene.empty(
    id: id,
    transform: transform.identity,
    children: children,
  ))
}

fn create_flat_scene(count: Int) -> List(scene.Node) {
  let assert Ok(box_geo) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(red_material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      side: material.FrontSide,
      alpha_test: 0.0,
      depth_write: True,
    )

  list.range(0, count - 1)
  |> list.map(fn(i) {
    let id = "node_" <> int.to_string(i)
    let x = int.to_float(i)

    scene.mesh(
      id: id,
      geometry: box_geo,
      material: red_material,
      transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
      physics: option.None,
    )
  })
}

fn create_flat_scene_offset(count: Int, offset: Float) -> List(scene.Node) {
  let assert Ok(box_geo) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(red_material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      side: material.FrontSide,
      alpha_test: 0.0,
      depth_write: True,
    )

  list.range(0, count - 1)
  |> list.map(fn(i) {
    let id = "node_" <> int.to_string(i)
    let x = int.to_float(i) +. offset

    scene.mesh(
      id: id,
      geometry: box_geo,
      material: red_material,
      transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
      physics: option.None,
    )
  })
}

fn create_nested_scene(depth: Int) -> List(scene.Node) {
  [create_nested_group(depth, 0)]
}

fn create_nested_group(depth: Int, current: Int) -> scene.Node {
  let assert Ok(box_geo) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(red_material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      side: material.FrontSide,
      alpha_test: 0.0,
      depth_write: True,
    )

  case current >= depth {
    True ->
      scene.mesh(
        id: "leaf_" <> int.to_string(current),
        geometry: box_geo,
        material: red_material,
        transform: transform.identity,
        physics: option.None,
      )

    False ->
      scene.empty(
        id: "group_" <> int.to_string(current),
        transform: transform.identity,
        children: [create_nested_group(depth, current + 1)],
      )
  }
}
