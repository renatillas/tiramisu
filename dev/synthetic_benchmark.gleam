// Synthetic Benchmark for Performance Testing
// Run with: gleam run -m dev/synthetic_benchmark
//
// This benchmark measures the performance of key systems before/after optimizations:
// - Scene diffing (flatten, compare, patch generation)
// - Input state capture
// - Physics transform sync
// - Postprocessing config comparison
// - Debug mesh updates

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
  io.println("║                Phase 1 Optimizations                         ║")
  io.println("╚══════════════════════════════════════════════════════════════╝")
  io.println("")

  // Benchmark 1: Scene Diffing (Most Critical)
  benchmark_scene_diffing()

  // Benchmark 2: Scene Diffing - No Changes (Dirty Flag Test)
  benchmark_static_scene()

  // Benchmark 3: Scene Diffing - Partial Changes
  benchmark_partial_scene_changes()

  // Benchmark 4: Deep Hierarchy
  benchmark_deep_hierarchy()

  io.println("\n✅ Benchmark Complete!")
  io.println("\nNext Steps:")
  io.println("1. Run Phase 1 optimizations")
  io.println("2. Re-run this benchmark")
  io.println("3. Compare results and verify improvements")
}

// ============================================================================
// Benchmark 1: Scene Diffing Performance
// ============================================================================

pub fn benchmark_scene_diffing() {
  io.println("\n=== 1. Scene Diffing Performance ===")
  io.println("Measures: flatten_scene() + diff() + patch generation")
  io.println("")

  bench.run(
    [
      bench.Input("10 nodes (Small)", create_scene_pair(10, 0.0)),
      bench.Input("50 nodes (Medium)", create_scene_pair(50, 0.0)),
      bench.Input("100 nodes (Large)", create_scene_pair(100, 0.0)),
      bench.Input("500 nodes (Stress)", create_scene_pair(500, 0.0)),
    ],
    [
      bench.Function("diff", fn(pair: ScenePair(String)) {
        scene.diff(pair.previous, pair.current)
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
        scene.diff(pair.previous, pair.current)
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
        scene.diff(pair.previous, pair.current)
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
        scene.diff(pair.previous, pair.current)
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
// Helper Types & Functions
// ============================================================================

pub type ScenePair(id) {
  ScenePair(
    previous: option.Option(scene.Node(id)),
    current: option.Option(scene.Node(id)),
  )
}

/// Create a scene with N meshes in a flat hierarchy
fn create_scene(count: Int) -> option.Option(scene.Node(String)) {
  let assert Ok(box_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
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
  let assert Ok(box_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
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
  let assert Ok(box_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
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

fn create_nested_group(
  depth: Int,
  current: Int,
  offset: Float,
) -> scene.Node(String) {
  let assert Ok(box_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
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
