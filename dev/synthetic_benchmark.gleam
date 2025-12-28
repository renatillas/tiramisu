// Synthetic Benchmark for Performance Testing
// Run with: gleam run -m dev/synthetic_benchmark
//
// This benchmark measures the performance of key systems before/after optimizations:
// - Scene diffing (flatten, compare, patch generation)
// - Input state capture
// - Physics transform sync
// - Postprocessing config comparison
// - Debug mesh updates

import gleam/dict
import gleam/float
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
  io.println("╔══════════════════════════════════════════════════════════════╗")
  io.println("║       Tiramisu v6.0.0 Performance Benchmark Suite           ║")
  io.println("╚══════════════════════════════════════════════════════════════╝")
  io.println("")

  // NEW: Benchmark with cache (Phase 1 optimization)
  io.println("=== PHASE 1 OPTIMIZATION: Cached Previous Scene ===")
  benchmark_with_cache()

  // Benchmark 1: Scene Diffing (Most Critical)
  benchmark_scene_diffing()

  // Benchmark 2: Scene Diffing - No Changes (Dirty Flag Test)
  benchmark_static_scene()

  // Benchmark 3: Scene Diffing - Partial Changes
  benchmark_partial_scene_changes()

  // Benchmark 4: Deep Hierarchy
  benchmark_deep_hierarchy()
}

// ============================================================================
// Benchmark 1: Scene Diffing Performance
// ============================================================================

pub fn benchmark_scene_diffing() {
  bench.run(
    [
      bench.Input("10 nodes (Small)", create_scene_pair(10, 0.0)),
      bench.Input("50 nodes (Medium)", create_scene_pair(50, 0.0)),
      bench.Input("100 nodes (Large)", create_scene_pair(100, 0.0)),
      bench.Input("500 nodes (Stress)", create_scene_pair(500, 0.0)),
    ],
    [
      bench.Function("diff", fn(pair: ScenePair(String)) {
        scene.diff(pair.previous, pair.current, option.None).0
      }),
    ],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean, bench.Max])
  |> io.println

  io.println("\n📊 Target: >10,000 ops/sec for 100 nodes")
  io.println("   Current bottleneck: flatten_scene() traverses tree twice")
}

// ============================================================================
// Benchmark 2: Static Scene (Dirty Flag Optimization)
// ============================================================================

pub fn benchmark_static_scene() {
  io.println("\n=== 2. Static Scene (Referential Equality) ===")
  io.println("Measures: Best-case scenario when scene hasn't changed")
  io.println("Expected: Should be near-instant (referential equality check)")
  io.println("")

  // Create identical scene references
  let scene_10 = create_scene(10)
  let scene_50 = create_scene(50)
  let scene_100 = create_scene(100)
  let scene_500 = create_scene(500)

  bench.run(
    [
      bench.Input("10 nodes (Unchanged)", ScenePair(scene_10, scene_10)),
      bench.Input("50 nodes (Unchanged)", ScenePair(scene_50, scene_50)),
      bench.Input("100 nodes (Unchanged)", ScenePair(scene_100, scene_100)),
      bench.Input("500 nodes (Unchanged)", ScenePair(scene_500, scene_500)),
    ],
    [
      bench.Function("diff", fn(pair: ScenePair(String)) {
        scene.diff(pair.previous, pair.current, option.None).0
      }),
    ],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean, bench.Max])
  |> io.println

  io.println("\n📊 Target: >1,000,000 ops/sec (just a pointer comparison)")
  io.println("   This validates the referential equality optimization works")
}

// ============================================================================
// Benchmark 3: Partial Scene Changes
// ============================================================================

pub fn benchmark_partial_scene_changes() {
  io.println("\n=== 3. Partial Scene Changes ===")
  io.println("Measures: Realistic scenario - only some nodes changed")
  io.println("")

  bench.run(
    [
      bench.Input(
        "100 nodes, 10% changed",
        create_scene_with_partial_changes(100, 10),
      ),
      bench.Input(
        "100 nodes, 50% changed",
        create_scene_with_partial_changes(100, 50),
      ),
      bench.Input(
        "100 nodes, 100% changed",
        create_scene_with_partial_changes(100, 100),
      ),
    ],
    [
      bench.Function("diff", fn(pair: ScenePair(String)) {
        scene.diff(pair.previous, pair.current, option.None).0
      }),
    ],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean, bench.Max])
  |> io.println

  io.println("\n📊 Optimization Opportunity:")
  io.println(
    "   Memoize flattened scene from previous frame (Phase 3 optimization)",
  )
}

// ============================================================================
// Benchmark 4: Deep Hierarchy
// ============================================================================

pub fn benchmark_deep_hierarchy() {
  io.println("\n=== 4. Deep Hierarchy Performance ===")
  io.println("Measures: Scene with deep nesting (tests hierarchy depth calc)")
  io.println("")

  bench.run(
    [
      bench.Input("5 levels deep", create_nested_scene_pair(5)),
      bench.Input("10 levels deep", create_nested_scene_pair(10)),
      bench.Input("20 levels deep", create_nested_scene_pair(20)),
    ],
    [
      bench.Function("diff", fn(pair: ScenePair(String)) {
        scene.diff(pair.previous, pair.current, option.None).0
      }),
    ],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean, bench.Max])
  |> io.println

  io.println("\n📊 Optimization Opportunity:")
  io.println("   Cache depth during flatten_scene() (Phase 3 optimization)")
  io.println(
    "   Avoid recursive depth calculation in sort_patches_by_hierarchy",
  )
}

// ============================================================================
// NEW: Benchmark with Cache (Phase 1 Optimization)
// ============================================================================

pub fn benchmark_with_cache() {
  io.println(
    "Measures: Realistic consecutive frames with cached previous scene",
  )
  io.println("Expected: ~50% faster than without cache")
  io.println("")

  // Create scenes for comparison
  let inputs = [
    create_cached_benchmark_input(10, 0.1),
    create_cached_benchmark_input(50, 0.1),
    create_cached_benchmark_input(100, 0.1),
    create_cached_benchmark_input(500, 0.1),
  ]

  // Benchmark WITHOUT cache (baseline)
  io.println("▶ Without cache (baseline):")
  bench.run(
    inputs
      |> list.map(fn(input) {
        let label = case input.count {
          10 -> "10 nodes"
          50 -> "50 nodes"
          100 -> "100 nodes"
          500 -> "500 nodes"
          _ -> "unknown"
        }
        bench.Input(label, input)
      }),
    [
      bench.Function("diff (no cache)", fn(input: CachedBenchmarkInput) {
        scene.diff(input.prev_scene, input.curr_scene, option.None).0
      }),
    ],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean, bench.Max])
  |> io.println

  // Benchmark WITH cache (optimized)
  io.println("\n▶ With cache (Phase 1 optimization):")
  bench.run(
    inputs
      |> list.map(fn(input) {
        let label = case input.count {
          10 -> "10 nodes"
          50 -> "50 nodes"
          100 -> "100 nodes"
          500 -> "500 nodes"
          _ -> "unknown"
        }
        bench.Input(label, input)
      }),
    [
      bench.Function("diff (cached)", fn(input: CachedBenchmarkInput) {
        scene.diff(input.prev_scene, input.curr_scene, option.Some(input.cache)).0
      }),
    ],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.Mean, bench.Max])
  |> io.println

  io.println("\n📊 Expected: WITH CACHE should be ~50% faster!")
  io.println(
    "   This simulates real game frames where previous scene dict is available\n",
  )
}

// ============================================================================
// Helper Types & Functions
// ============================================================================

pub type CachedBenchmarkInput {
  CachedBenchmarkInput(
    count: Int,
    prev_scene: option.Option(scene.Node),
    curr_scene: option.Option(scene.Node),
    cache: dict.Dict(String, scene.NodeWithParent),
  )
}

pub type ScenePair(id) {
  ScenePair(
    previous: option.Option(scene.Node),
    current: option.Option(scene.Node),
  )
}

/// Create a scene with N meshes in a flat hierarchy
fn create_scene(count: Int) -> option.Option(scene.Node) {
  let assert Ok(box_geo) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
    )

  let children =
    list.range(0, count - 1)
    |> list.map(fn(i) {
      let id = "mesh_" <> int.to_string(i)
      let x = int.to_float(i) *. 2.0

      scene.mesh(
        id: id,
        geometry: box_geo,
        material: material,
        transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
        physics: option.None,
      )
    })

  option.Some(scene.empty(
    id: "root",
    transform: transform.identity,
    children: children,
  ))
}

/// Create a pair of scenes (previous and current) with offset positions
fn create_scene_pair(count: Int, offset: Float) -> ScenePair(String) {
  let assert Ok(box_geo) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
    )

  let prev_children =
    list.range(0, count - 1)
    |> list.map(fn(i) {
      let id = "mesh_" <> int.to_string(i)
      let x = int.to_float(i) *. 2.0

      scene.mesh(
        id: id,
        geometry: box_geo,
        material: material,
        transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
        physics: option.None,
      )
    })

  let curr_children =
    list.range(0, count - 1)
    |> list.map(fn(i) {
      let id = "mesh_" <> int.to_string(i)
      let x = int.to_float(i) *. 2.0 +. offset

      scene.mesh(
        id: id,
        geometry: box_geo,
        material: material,
        transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
        physics: option.None,
      )
    })

  ScenePair(
    previous: option.Some(scene.empty(
      id: "root",
      transform: transform.identity,
      children: prev_children,
    )),
    current: option.Some(scene.empty(
      id: "root",
      transform: transform.identity,
      children: curr_children,
    )),
  )
}

/// Create scene where only N% of nodes changed
fn create_scene_with_partial_changes(
  total_count: Int,
  percent_changed: Int,
) -> ScenePair(String) {
  let assert Ok(box_geo) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
    )

  let changed_count = { total_count * percent_changed } / 100

  let prev_children =
    list.range(0, total_count - 1)
    |> list.map(fn(i) {
      let id = "mesh_" <> int.to_string(i)
      let x = int.to_float(i) *. 2.0

      scene.mesh(
        id: id,
        geometry: box_geo,
        material: material,
        transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
        physics: option.None,
      )
    })

  let curr_children =
    list.range(0, total_count - 1)
    |> list.map(fn(i) {
      let id = "mesh_" <> int.to_string(i)

      // Only change position for first N% of nodes
      let x = case i < changed_count {
        True -> int.to_float(i) *. 2.0 +. 5.0
        False -> int.to_float(i) *. 2.0
      }

      scene.mesh(
        id: id,
        geometry: box_geo,
        material: material,
        transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
        physics: option.None,
      )
    })

  ScenePair(
    previous: option.Some(scene.empty(
      id: "root",
      transform: transform.identity,
      children: prev_children,
    )),
    current: option.Some(scene.empty(
      id: "root",
      transform: transform.identity,
      children: curr_children,
    )),
  )
}

/// Create a deeply nested scene (tests hierarchy depth calculation)
fn create_nested_scene_pair(depth: Int) -> ScenePair(String) {
  ScenePair(
    previous: option.Some(create_nested_group(depth, 0, 0.0)),
    current: option.Some(create_nested_group(depth, 0, 1.0)),
  )
}

fn create_nested_group(depth: Int, current: Int, offset: Float) -> scene.Node {
  let assert Ok(box_geo) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
    )

  case current >= depth {
    True ->
      scene.mesh(
        id: "leaf_" <> int.to_string(current),
        geometry: box_geo,
        material: material,
        transform: transform.at(position: vec3.Vec3(offset, 0.0, 0.0)),
        physics: option.None,
      )

    False ->
      scene.empty(
        id: "group_" <> int.to_string(current),
        transform: transform.at(position: vec3.Vec3(offset, 0.0, 0.0)),
        children: [create_nested_group(depth, current + 1, offset)],
      )
  }
}

/// Create benchmark input with cached previous scene dictionary
/// This simulates real game frames where the previous frame's cache exists
fn create_cached_benchmark_input(
  count: Int,
  change_percent: Float,
) -> CachedBenchmarkInput {
  let assert Ok(box_geo) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(material) =
    material.basic(
      color: 0xff0000,
      transparent: False,
      opacity: 1.0,
      map: option.None,
    )

  // Create frame 1 (previous frame)
  let prev_children =
    list.range(0, count - 1)
    |> list.map(fn(i) {
      let id = "mesh_" <> int.to_string(i)
      let x = int.to_float(i) *. 2.0

      scene.mesh(
        id: id,
        geometry: box_geo,
        material: material,
        transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
        physics: option.None,
      )
    })

  let prev_scene =
    option.Some(scene.empty(
      id: "root",
      transform: transform.identity,
      children: prev_children,
    ))

  // Create frame 2 (current frame) - only change a few nodes
  let changed_count_int =
    int.to_float(count) *. change_percent
    |> float.round
  let curr_children =
    list.range(0, count - 1)
    |> list.map(fn(i) {
      let id = "mesh_" <> int.to_string(i)
      // Only change position for first N% of nodes
      let x = case i < changed_count_int {
        True -> int.to_float(i) *. 2.0 +. 5.0
        False -> int.to_float(i) *. 2.0
      }

      scene.mesh(
        id: id,
        geometry: box_geo,
        material: material,
        transform: transform.at(position: vec3.Vec3(x, 0.0, 0.0)),
        physics: option.None,
      )
    })

  let curr_scene =
    option.Some(scene.empty(
      id: "root",
      transform: transform.identity,
      children: curr_children,
    ))

  // Generate the cache by diffing frame 1 with empty scene
  // This simulates what would happen in real game after frame 1
  let #(_, cache) = scene.diff(option.None, prev_scene, option.None)

  CachedBenchmarkInput(
    count: count,
    prev_scene: prev_scene,
    curr_scene: curr_scene,
    cache: cache,
  )
}
