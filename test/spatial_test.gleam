import gleam/list
import gleeunit
import tiramisu/spatial
import vec/vec3

pub fn main() {
  gleeunit.main()
}

pub fn aabb_contains_point_inside_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-1.0, -1.0, -1.0),
      max: vec3.Vec3(1.0, 1.0, 1.0),
    )
  let point = vec3.Vec3(0.0, 0.0, 0.0)

  let assert True = spatial.collider_contains_point(bounds, point)
}

pub fn aabb_contains_point_outside_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-1.0, -1.0, -1.0),
      max: vec3.Vec3(1.0, 1.0, 1.0),
    )
  let point = vec3.Vec3(2.0, 0.0, 0.0)

  let assert False = spatial.collider_contains_point(bounds, point)
}

pub fn aabb_contains_point_on_boundary_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-1.0, -1.0, -1.0),
      max: vec3.Vec3(1.0, 1.0, 1.0),
    )
  let point = vec3.Vec3(1.0, 1.0, 1.0)

  let assert True = spatial.collider_contains_point(bounds, point)
}

pub fn aabb_intersects_overlapping_test() {
  let a =
    spatial.collider_box(
      min: vec3.Vec3(-1.0, -1.0, -1.0),
      max: vec3.Vec3(1.0, 1.0, 1.0),
    )
  let b =
    spatial.collider_box(
      min: vec3.Vec3(0.0, 0.0, 0.0),
      max: vec3.Vec3(2.0, 2.0, 2.0),
    )

  let assert True = spatial.collider_intersects(a, b)
}

pub fn aabb_intersects_non_overlapping_test() {
  let a =
    spatial.collider_box(
      min: vec3.Vec3(-1.0, -1.0, -1.0),
      max: vec3.Vec3(1.0, 1.0, 1.0),
    )
  let b =
    spatial.collider_box(
      min: vec3.Vec3(2.0, 2.0, 2.0),
      max: vec3.Vec3(4.0, 4.0, 4.0),
    )

  let assert False = spatial.collider_intersects(a, b)
}

pub fn aabb_intersects_touching_test() {
  let a =
    spatial.collider_box(
      min: vec3.Vec3(-1.0, -1.0, -1.0),
      max: vec3.Vec3(1.0, 1.0, 1.0),
    )
  let b =
    spatial.collider_box(
      min: vec3.Vec3(1.0, 1.0, 1.0),
      max: vec3.Vec3(3.0, 3.0, 3.0),
    )

  let assert True = spatial.collider_intersects(a, b)
}

pub fn aabb_center_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-2.0, -4.0, -6.0),
      max: vec3.Vec3(2.0, 4.0, 6.0),
    )
  let center = spatial.collider_center(bounds)

  assert center == vec3.Vec3(0.0, 0.0, 0.0)
}

pub fn aabb_center_offset_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(0.0, 0.0, 0.0),
      max: vec3.Vec3(10.0, 20.0, 30.0),
    )
  let center = spatial.collider_center(bounds)

  assert center == vec3.Vec3(5.0, 10.0, 15.0)
}

pub fn aabb_size_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-2.0, -4.0, -6.0),
      max: vec3.Vec3(2.0, 4.0, 6.0),
    )
  let size = spatial.collider_size(bounds)

  assert size == vec3.Vec3(4.0, 8.0, 12.0)
}

// Octree Tests

pub fn octree_new_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  let retrieved_bounds = spatial.octree_bounds(tree)
  assert retrieved_bounds == bounds
}

pub fn octree_insert_single_item_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  let position = vec3.Vec3(0.0, 0.0, 0.0)
  let tree = spatial.octree_insert(tree, position, "item1")

  let count = spatial.octree_count(tree)
  let assert 1 = count
}

pub fn octree_insert_multiple_items_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "item1")
    |> spatial.octree_insert(vec3.Vec3(1.0, 1.0, 1.0), "item2")
    |> spatial.octree_insert(vec3.Vec3(-1.0, -1.0, -1.0), "item3")

  let count = spatial.octree_count(tree)
  let assert 3 = count
}

pub fn octree_insert_out_of_bounds_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  // Insert outside bounds
  let tree = spatial.octree_insert(tree, vec3.Vec3(20.0, 20.0, 20.0), "item1")

  // Should not insert
  let count = spatial.octree_count(tree)
  let assert 0 = count
}

pub fn octree_subdivides_at_capacity_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let tree = spatial.octree_new(bounds, 2)

  // Insert 3 items - should trigger subdivision
  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "item1")
    |> spatial.octree_insert(vec3.Vec3(1.0, 1.0, 1.0), "item2")
    |> spatial.octree_insert(vec3.Vec3(2.0, 2.0, 2.0), "item3")

  let count = spatial.octree_count(tree)
  let assert 3 = count
}

pub fn octree_query_all_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "item1")
    |> spatial.octree_insert(vec3.Vec3(1.0, 1.0, 1.0), "item2")

  let items = spatial.octree_query_all(tree)
  let assert 2 = items |> list.length
}

pub fn octree_query_region_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  // Insert items in different regions
  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "center")
    |> spatial.octree_insert(vec3.Vec3(50.0, 50.0, 50.0), "far")
    |> spatial.octree_insert(vec3.Vec3(5.0, 5.0, 5.0), "near")

  // Query small region around origin
  let query_bounds =
    spatial.collider_box(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let items = spatial.octree_query(tree, query_bounds)

  // Should only find center and near items
  let assert 2 = items |> list.length
}

pub fn octree_query_radius_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  // Insert items at various distances
  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "at_center")
    |> spatial.octree_insert(vec3.Vec3(2.0, 0.0, 0.0), "near")
    |> spatial.octree_insert(vec3.Vec3(20.0, 0.0, 0.0), "far")

  // Query radius 5 units around origin
  let center = vec3.Vec3(0.0, 0.0, 0.0)
  let items = spatial.octree_query_radius(tree, center, 5.0)

  // Should only find at_center and near
  let assert 2 = items |> list.length
}

pub fn octree_query_radius_exact_boundary_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-100.0, -100.0, -100.0),
      max: vec3.Vec3(100.0, 100.0, 100.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  let tree =
    tree
    |> spatial.octree_insert(vec3.Vec3(0.0, 0.0, 0.0), "at_center")
    |> spatial.octree_insert(vec3.Vec3(3.0, 4.0, 0.0), "at_radius")

  // Distance to (3,4,0) is exactly 5
  let center = vec3.Vec3(0.0, 0.0, 0.0)
  let items = spatial.octree_query_radius(tree, center, 5.0)

  // Should include item at radius
  let assert 2 = items |> list.length
}

pub fn octree_empty_query_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  let query_bounds =
    spatial.collider_box(
      min: vec3.Vec3(-5.0, -5.0, -5.0),
      max: vec3.Vec3(5.0, 5.0, 5.0),
    )
  let items = spatial.octree_query(tree, query_bounds)

  let assert 0 = items |> list.length
}

pub fn octree_count_empty_test() {
  let bounds =
    spatial.collider_box(
      min: vec3.Vec3(-10.0, -10.0, -10.0),
      max: vec3.Vec3(10.0, 10.0, 10.0),
    )
  let tree = spatial.octree_new(bounds, 4)

  let count = spatial.octree_count(tree)
  let assert 0 = count
}
