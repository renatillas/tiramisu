import gleam/int
import gleam/list
import gleeunit
import gleeunit/should
import tiramisu/spatial
import vec/vec3

pub fn main() {
  gleeunit.main()
}

// --- AABB Tests ---

pub fn aabb_contains_point_inside_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let point = vec3.Vec3(0.0, 0.0, 0.0)

  spatial.aabb_contains_point(bounds, point)
  |> should.be_true
}

pub fn aabb_contains_point_outside_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let point = vec3.Vec3(15.0, 0.0, 0.0)

  spatial.aabb_contains_point(bounds, point)
  |> should.be_false
}

pub fn aabb_contains_point_on_edge_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let point = vec3.Vec3(10.0, 10.0, 10.0)

  spatial.aabb_contains_point(bounds, point)
  |> should.be_true
}

pub fn aabb_intersects_overlapping_test() {
  let a =
    spatial.aabb(
      min: vec3.Vec3(0.0, 0.0, 0.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let b =
    spatial.aabb(
      min: vec3.Vec3(5.0, 5.0, 5.0),
      max: vec3.Vec3(15.0, 15.0, 15.0),
    )

  spatial.aabb_intersects(a, b)
  |> should.be_true
}

pub fn aabb_intersects_separate_test() {
  let a =
    spatial.aabb(
      min: vec3.Vec3(0.0, 0.0, 0.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let b =
    spatial.aabb(
      min: vec3.Vec3(20.0, 20.0, 20.0),
      max: vec3.Vec3(30.0, 30.0, 30.0),
    )

  spatial.aabb_intersects(a, b)
  |> should.be_false
}

pub fn aabb_center_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-10.0, -20.0, -30.0),
      max: vec3.Vec3(10.0, 20.0, 30.0),
    )

  spatial.aabb_center(bounds)
  |> should.equal(vec3.Vec3(0.0, 0.0, 0.0))
}

pub fn aabb_size_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-10.0, -20.0, -30.0),
      max: vec3.Vec3(10.0, 20.0, 30.0),
    )

  spatial.aabb_size(bounds)
  |> should.equal(vec3.Vec3(20.0, 40.0, 60.0))
}

pub fn aabb_from_center_test() {
  let center = vec3.Vec3(5.0, 5.0, 5.0)
  let half_extents = vec3.Vec3(2.0, 3.0, 4.0)

  let bounds = spatial.aabb_from_center(center, half_extents)

  spatial.aabb_center(bounds)
  |> should.equal(center)

  spatial.aabb_size(bounds)
  |> should.equal(vec3.Vec3(4.0, 6.0, 8.0))
}

// --- Octree Tests ---

pub fn octree_new_empty_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 8)

  spatial.octree_count(tree)
  |> should.equal(0)
}

pub fn octree_insert_single_item_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 8)

  let tree = spatial.octree_insert(tree, vec3.Vec3(0.0, 0.0, 0.0), "item1")

  spatial.octree_count(tree)
  |> should.equal(1)
}

pub fn octree_insert_multiple_items_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 8)

  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "item1")
    |> spatial.octree_insert(vec3.Vec3(10.0, 10.0, 10.0), "item2")
    |> spatial.octree_insert(vec3.Vec3(-10.0, -10.0, -10.0), "item3")

  spatial.octree_count(tree)
  |> should.equal(3)
}

pub fn octree_insert_outside_bounds_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let tree = spatial.octree_new(bounds, 8)

  // Insert outside bounds - should be ignored
  let tree =
    spatial.octree_insert(tree, vec3.Vec3(100.0, 100.0, 100.0), "item1")

  spatial.octree_count(tree)
  |> should.equal(0)
}

pub fn octree_subdivides_when_capacity_exceeded_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 2)

  // Insert 3 items - should trigger subdivision
  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "item1")
    |> spatial.octree_insert(vec3.Vec3(1.0, 1.0, 1.0), "item2")
    |> spatial.octree_insert(vec3.Vec3(2.0, 2.0, 2.0), "item3")

  // All items should still be retrievable
  spatial.octree_count(tree)
  |> should.equal(3)
}

pub fn octree_query_all_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 8)

  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "item1")
    |> spatial.octree_insert(vec3.Vec3(10.0, 10.0, 10.0), "item2")
    |> spatial.octree_insert(vec3.Vec3(-10.0, -10.0, -10.0), "item3")

  let results = spatial.octree_query_all(tree)

  results
  |> list.length
  |> should.equal(3)
}

pub fn octree_query_region_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 8)

  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "center")
    |> spatial.octree_insert(vec3.Vec3(50.0, 50.0, 50.0), "far_positive")
    |> spatial.octree_insert(vec3.Vec3(-50.0, -50.0, -50.0), "far_negative")

  // Query small region around center
  let query_bounds =
    spatial.aabb(
      min: vec3.Vec3(-5.0, -5.0, -5.0),
      max: vec3.Vec3(5.0, 5.0, 5.0),
    )
  let results = spatial.octree_query(tree, query_bounds)

  // Should only find center item
  results
  |> list.length
  |> should.equal(1)

  let assert [#(_, item)] = results
  item
  |> should.equal("center")
}

pub fn octree_query_radius_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 8)

  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "center")
    |> spatial.octree_insert(vec3.Vec3(5.0, 0.0, 0.0), "nearby")
    |> spatial.octree_insert(vec3.Vec3(50.0, 0.0, 0.0), "far")

  // Query radius of 10 from origin
  let results =
    spatial.octree_query_radius(tree, vec3.Vec3(0.0, 0.0, 0.0), 10.0)

  // Should find center and nearby, but not far
  results
  |> list.length
  |> should.equal(2)
}

pub fn octree_query_empty_region_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 8)

  let tree = spatial.octree_insert(tree, vec3.Vec3(0.0, 0.0, 0.0), "item1")

  // Query region far away
  let query_bounds =
    spatial.aabb(
      min: vec3.Vec3(50.0, 50.0, 50.0),
      max: vec3.Vec3(60.0, 60.0, 60.0),
    )
  let results = spatial.octree_query(tree, query_bounds)

  results
  |> should.equal([])
}

pub fn octree_many_items_test() {
  let bounds =
    spatial.aabb(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  // Insert 100 items
  let tree =
    list.range(0, 99)
    |> list.fold(tree, fn(acc, i) {
      let fi = int.to_float(i)
      spatial.octree_insert(acc, vec3.Vec3(fi, fi, fi), i)
    })

  spatial.octree_count(tree)
  |> should.equal(100)
}
