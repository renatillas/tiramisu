import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleamy/bench
import tiramisu/animation
import tiramisu/geometry
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub fn main() {
  // Benchmark Vec3 operations that return Vec3
  bench.run(
    [
      bench.Input(
        "Small (10 nodes)",
        ScenePair(create_flat_scene(10), create_flat_scene(10)),
      ),
      bench.Input(
        "Medium (100 nodes)",
        ScenePair(create_flat_scene(100), create_flat_scene(100)),
      ),
      bench.Input(
        "Large (1000 nodes)",
        ScenePair(create_flat_scene(1000), create_flat_scene(1000)),
      ),
      bench.Input(
        "Nested (10 levels)",
        ScenePair(create_nested_scene(10), create_nested_scene(10)),
      ),
      bench.Input("No changes (100 nodes)", {
        let nodes = create_flat_scene(100)
        ScenePair(nodes, nodes)
      }),
      bench.Input(
        "All changed (100 nodes)",
        ScenePair(create_flat_scene(100), create_flat_scene_offset(100, 10.0)),
      ),
    ],
    [
      bench.Function("scene.diff", fn(pair: ScenePair(String)) {
        scene.diff(pair.previous, pair.current)
      }),
    ],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean, bench.Max, bench.P(99)])
  |> io.println

  // Benchmark easing functions
  io.println("=== Easing Functions ===")
  bench.run(
    [bench.Input("t=0.5", 0.5)],
    [
      bench.Function("Linear", fn(t) { animation.ease(animation.Linear, t) }),
      bench.Function("EaseInQuad", fn(t) {
        animation.ease(animation.EaseInQuad, t)
      }),
      bench.Function("EaseOutQuad", fn(t) {
        animation.ease(animation.EaseOutQuad, t)
      }),
      bench.Function("EaseInOutQuad", fn(t) {
        animation.ease(animation.EaseInOutQuad, t)
      }),
      bench.Function("EaseInCubic", fn(t) {
        animation.ease(animation.EaseInCubic, t)
      }),
      bench.Function("EaseOutCubic", fn(t) {
        animation.ease(animation.EaseOutCubic, t)
      }),
      bench.Function("EaseInOutCubic", fn(t) {
        animation.ease(animation.EaseInOutCubic, t)
      }),
      bench.Function("EaseInSine", fn(t) {
        animation.ease(animation.EaseInSine, t)
      }),
      bench.Function("EaseOutSine", fn(t) {
        animation.ease(animation.EaseOutSine, t)
      }),
      bench.Function("EaseInOutSine", fn(t) {
        animation.ease(animation.EaseInOutSine, t)
      }),
    ],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean])
  |> io.println

  // Benchmark float tween update operation
  io.println("\n=== Float Tween Update ===")
  bench.run(
    [
      bench.Input(
        "Float tween",
        animation.tween_float(0.0, 100.0, 1.0, animation.Linear),
      ),
    ],
    [
      bench.Function("update_tween", fn(tween) {
        animation.update_tween(tween, 0.016)
      }),
    ],
    [bench.Duration(100), bench.Warmup(10)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean])
  |> io.println
}

pub type ScenePair(id) {
  ScenePair(previous: List(scene.Node(id)), current: List(scene.Node(id)))
}

// --- Helper Functions ---

fn create_flat_scene(count: Int) -> List(scene.Node(String)) {
  let assert Ok(box_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(red_material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
    )

  list.range(0, count - 1)
  |> list.map(fn(i) {
    let id = "node_" <> int.to_string(i)
    let x = int.to_float(i)

    scene.Mesh(
      id: id,
      geometry: box_geo,
      material: red_material,
      transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
      physics: option.None,
    )
  })
}

fn create_flat_scene_offset(
  count: Int,
  offset: Float,
) -> List(scene.Node(String)) {
  let assert Ok(box_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(red_material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
    )

  list.range(0, count - 1)
  |> list.map(fn(i) {
    let id = "node_" <> int.to_string(i)
    let x = int.to_float(i) +. offset

    scene.Mesh(
      id: id,
      geometry: box_geo,
      material: red_material,
      transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
      physics: option.None,
    )
  })
}

fn create_nested_scene(depth: Int) -> List(scene.Node(String)) {
  [create_nested_group(depth, 0)]
}

fn create_nested_group(depth: Int, current: Int) -> scene.Node(String) {
  let assert Ok(box_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(red_material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
    )

  case current >= depth {
    True ->
      scene.Mesh(
        id: "leaf_" <> int.to_string(current),
        geometry: box_geo,
        material: red_material,
        transform: transform.identity,
        physics: option.None,
      )

    False ->
      scene.Group(
        id: "group_" <> int.to_string(current),
        transform: transform.identity,
        children: [create_nested_group(depth, current + 1)],
      )
  }
}
