import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleamy/bench
import tiramisu/animation
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub type Vec3Pair {
  Vec3Pair(a: vec3.Vec3, b: vec3.Vec3)
}

pub fn main() {
  // Benchmark Vec3 operations that return Vec3
  io.println("=== Vec3 Operations (return Vec3) ===")
  bench.run(
    [
      bench.Input(
        "Vec3 pair",
        Vec3Pair(vec3.Vec3(1.0, 2.0, 3.0), vec3.Vec3(4.0, 5.0, 6.0)),
      ),
    ],
    [
      bench.Function("add", fn(pair: Vec3Pair) { vec3.add(pair.a, pair.b) }),
      bench.Function("subtract", fn(pair: Vec3Pair) {
        vec3.subtract(pair.a, pair.b)
      }),
      bench.Function("scale", fn(pair: Vec3Pair) { vec3.scale(pair.a, 2.5) }),
      bench.Function("normalize", fn(pair: Vec3Pair) { vec3.normalize(pair.a) }),
      bench.Function("cross", fn(pair: Vec3Pair) { vec3.cross(pair.a, pair.b) }),
      bench.Function("lerp", fn(pair: Vec3Pair) {
        vec3.lerp(pair.a, pair.b, 0.5)
      }),
    ],
    [bench.Duration(100), bench.Warmup(10)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean])
  |> io.println

  // Benchmark Vec3 operations that return Float
  io.println("\n=== Vec3 Operations (return Float) ===")
  bench.run(
    [
      bench.Input(
        "Vec3 pair",
        Vec3Pair(vec3.Vec3(1.0, 2.0, 3.0), vec3.Vec3(4.0, 5.0, 6.0)),
      ),
    ],
    [
      bench.Function("dot", fn(pair: Vec3Pair) { vec3.dot(pair.a, pair.b) }),
      bench.Function("distance", fn(pair: Vec3Pair) {
        vec3.distance(pair.a, pair.b)
      }),
    ],
    [bench.Duration(100), bench.Warmup(10)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean])
  |> io.println

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
      bench.Function("scene.diff", fn(pair: ScenePair) {
        scene.diff(pair.previous, pair.current)
      }),
    ],
    [bench.Duration(100), bench.Warmup(10)],
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
    [bench.Duration(100), bench.Warmup(10)],
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

pub type ScenePair {
  ScenePair(previous: List(scene.SceneNode), current: List(scene.SceneNode))
}

// --- Helper Functions ---

fn create_flat_scene(count: Int) -> List(scene.SceneNode) {
  list.range(0, count - 1)
  |> list.map(fn(i) {
    let id = "node_" <> int.to_string(i)
    let x = int.to_float(i)

    scene.Mesh(
      id: id,
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
      physics: option.None,
    )
  })
}

fn create_flat_scene_offset(count: Int, offset: Float) -> List(scene.SceneNode) {
  list.range(0, count - 1)
  |> list.map(fn(i) {
    let id = "node_" <> int.to_string(i)
    let x = int.to_float(i) +. offset

    scene.Mesh(
      id: id,
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
      transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
      physics: option.None,
    )
  })
}

fn create_nested_scene(depth: Int) -> List(scene.SceneNode) {
  [create_nested_group(depth, 0)]
}

fn create_nested_group(depth: Int, current: Int) -> scene.SceneNode {
  case current >= depth {
    True ->
      scene.Mesh(
        id: "leaf_" <> int.to_string(current),
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
        transform: transform.identity(),
        physics: option.None,
      )

    False ->
      scene.Group(
        id: "group_" <> int.to_string(current),
        transform: transform.identity(),
        children: [create_nested_group(depth, current + 1)],
      )
  }
}
