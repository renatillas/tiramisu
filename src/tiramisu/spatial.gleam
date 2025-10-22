//// Spatial partitioning data structures for efficient spatial queries.
////
//// Provides octree and Collider (Box and Sphere) for:
//// - Finding objects within a region (10-100x faster than linear search)
//// - Finding nearby objects
//// - Frustum culling optimization
//// - Broad-phase collision detection
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/spatial
////
//// // Create octree for world bounds
//// let world_bounds = spatial.box(
////   min: vec3.Vec3(-100.0, -100.0, -100.0),
////   max: vec3.Vec3(100.0, 100.0, 100.0),
//// )
//// let tree = spatial.octree_new(bounds: world_bounds, capacity: 8)
////
//// // Insert enemies
//// let tree = spatial.octree_insert(tree, enemy_pos, enemy_id)
////
//// // Find enemies near player
//// let nearby = spatial.octree_query_radius(tree, player_pos, radius: 10.0)
//// ```

import gleam/float
import gleam/list
import gleam/option.{type Option, None, Some}
import tiramisu/transform.{type Quaternion, type Transform}
import vec/vec3.{type Vec3}
import vec/vec3f

// --- Types ---

/// Collision volume for spatial queries and collision detection.
pub opaque type Collider {
  Box(min: Vec3(Float), max: Vec3(Float))
  Sphere(center: Vec3(Float), radius: Float)
}

/// Octree node for spatial partitioning
/// Divides 3D space into 8 octants recursively
/// Note: Octrees always use Box bounds (not Sphere)
pub opaque type Octree(a) {
  OctreeNode(
    bounds: Collider,
    capacity: Int,
    items: List(#(Vec3(Float), a)),
    children: Option(OctreeChildren(a)),
  )
}

type OctreeChildren(a) {
  OctreeChildren(
    // Bottom 4 octants (y-)
    bottom_nw: Octree(a),
    bottom_ne: Octree(a),
    bottom_sw: Octree(a),
    bottom_se: Octree(a),
    // Top 4 octants (y+)
    top_nw: Octree(a),
    top_ne: Octree(a),
    top_sw: Octree(a),
    top_se: Octree(a),
  )
}

// --- Collider Constructor Functions ---

/// Create a box collider from min and max points.
///
/// ## Example
/// ```gleam
/// let bounds = spatial.collider_box(
///   min: vec3.Vec3(-1.0, -1.0, -1.0),
///   max: vec3.Vec3(1.0, 1.0, 1.0),
/// )
/// ```
pub fn collider_box(min min: Vec3(Float), max max: Vec3(Float)) -> Collider {
  Box(min: min, max: max)
}

/// Create a box collider from center and half-extents.
///
/// ## Example
/// ```gleam
/// let bounds = spatial.box_from_center(
///   center: vec3.Vec3(0.0, 5.0, 0.0),
///   half_extents: vec3.Vec3(2.0, 1.0, 2.0),
/// )
/// ```
pub fn collider_box_from_center(
  center: Vec3(Float),
  half_extents: Vec3(Float),
) -> Collider {
  Box(
    min: vec3f.subtract(center, half_extents),
    max: vec3f.add(center, half_extents),
  )
}

/// Create a sphere collider from center and radius.
///
/// ## Example
/// ```gleam
/// let bounds = spatial.sphere(
///   center: vec3.Vec3(0.0, 0.0, 0.0),
///   radius: 2.5,
/// )
/// ```
pub fn collider_sphere(
  center center: Vec3(Float),
  radius radius: Float,
) -> Collider {
  Sphere(center: center, radius: radius)
}

// --- Collider Query Functions ---

/// Check if a point is inside a collider.
///
/// Works for both Box and Sphere colliders.
pub fn collider_contains_point(collider: Collider, point: Vec3(Float)) -> Bool {
  case collider {
    Box(min, max) ->
      point.x >=. min.x
      && point.x <=. max.x
      && point.y >=. min.y
      && point.y <=. max.y
      && point.z >=. min.z
      && point.z <=. max.z
    Sphere(center, radius) -> vec3f.distance(center, point) <=. radius
  }
}

/// Check if two colliders intersect.
///
/// Handles Box-Box, Sphere-Sphere, and Box-Sphere collisions.
pub fn collider_intersects(a: Collider, b: Collider) -> Bool {
  case a, b {
    // Box-Box intersection
    Box(min_a, max_a), Box(min_b, max_b) ->
      min_a.x <=. max_b.x
      && max_a.x >=. min_b.x
      && min_a.y <=. max_b.y
      && max_a.y >=. min_b.y
      && min_a.z <=. max_b.z
      && max_a.z >=. min_b.z

    // Sphere-Sphere intersection
    Sphere(center_a, radius_a), Sphere(center_b, radius_b) -> {
      let distance = vec3f.distance(center_a, center_b)
      distance <=. { radius_a +. radius_b }
    }

    // Box-Sphere intersection (order doesn't matter)
    Box(min, max), Sphere(center, radius)
    | Sphere(center, radius), Box(min, max)
    -> {
      // Find closest point on box to sphere center
      let closest_x = float.clamp(center.x, min.x, max.x)
      let closest_y = float.clamp(center.y, min.y, max.y)
      let closest_z = float.clamp(center.z, min.z, max.z)
      let closest = vec3.Vec3(closest_x, closest_y, closest_z)

      // Check if closest point is within sphere radius
      vec3f.distance(center, closest) <=. radius
    }
  }
}

/// Get the center of a collider.
pub fn collider_center(collider: Collider) -> Vec3(Float) {
  case collider {
    Box(min, max) ->
      vec3.Vec3(
        { min.x +. max.x } /. 2.0,
        { min.y +. max.y } /. 2.0,
        { min.z +. max.z } /. 2.0,
      )
    Sphere(center, _) -> center
  }
}

/// Get the size (dimensions) of a box collider.
///
/// For spheres, returns the diameter as a uniform Vec3.
pub fn collider_size(collider: Collider) -> Vec3(Float) {
  case collider {
    Box(min, max) -> vec3f.subtract(max, min)
    Sphere(_, radius) -> {
      let diameter = radius *. 2.0
      vec3.Vec3(diameter, diameter, diameter)
    }
  }
}

// --- Collider Transformation ---

/// Create a new collider from a local-space collider and a transform.
///
/// For Box: Computes a new axis-aligned bounding box that encompasses
/// all 8 corners after rotation, translation, and scaling.
///
/// For Sphere: Transforms the center and scales the radius by the maximum
/// scale component (since spheres remain spherical under uniform scaling).
///
/// ## Example
/// ```gleam
/// // Local space box
/// let local_box = spatial.box(
///   min: vec3.Vec3(-1.0, -1.0, -1.0),
///   max: vec3.Vec3(1.0, 1.0, 1.0),
/// )
///
/// // Rotated 45 degrees around Y axis
/// let transform = transform.identity
///   |> transform.with_position(vec3.Vec3(5.0, 0.0, 0.0))
///   |> transform.with_euler_rotation(vec3.Vec3(0.0, 0.785, 0.0))
///
/// // Get world-space collider
/// let world_box = spatial.collider_from_transform(local_box, transform)
/// ```
pub fn collider_from_transform(
  collider: Collider,
  transform: Transform,
) -> Collider {
  case collider {
    Box(min, max) -> {
      // Get 8 corners of the local box
      let corners = [
        vec3.Vec3(min.x, min.y, min.z),
        vec3.Vec3(max.x, min.y, min.z),
        vec3.Vec3(min.x, max.y, min.z),
        vec3.Vec3(max.x, max.y, min.z),
        vec3.Vec3(min.x, min.y, max.z),
        vec3.Vec3(max.x, min.y, max.z),
        vec3.Vec3(min.x, max.y, max.z),
        vec3.Vec3(max.x, max.y, max.z),
      ]

      // Transform all 8 corners to world space
      let transformed_corners =
        list.map(corners, fn(corner) { transform_point(corner, transform) })

      // Find min/max of all transformed corners
      let init_min = vec3.Vec3(1.0e10, 1.0e10, 1.0e10)
      let init_max = vec3.Vec3(-1.0e10, -1.0e10, -1.0e10)

      let #(new_min, new_max) =
        list.fold(transformed_corners, #(init_min, init_max), fn(acc, point) {
          let #(current_min, current_max) = acc
          #(
            vec3.Vec3(
              float.min(current_min.x, point.x),
              float.min(current_min.y, point.y),
              float.min(current_min.z, point.z),
            ),
            vec3.Vec3(
              float.max(current_max.x, point.x),
              float.max(current_max.y, point.y),
              float.max(current_max.z, point.z),
            ),
          )
        })

      Box(min: new_min, max: new_max)
    }

    Sphere(center, radius) -> {
      // Transform center
      let new_center = transform_point(center, transform)

      // Scale radius by maximum scale component (spheres should use uniform scale)
      let scale = transform.scale(transform)
      let max_scale = float.max(scale.x, float.max(scale.y, scale.z))
      let new_radius = radius *. max_scale

      Sphere(center: new_center, radius: new_radius)
    }
  }
}

/// Transform a point by position, rotation, and scale.
fn transform_point(point: Vec3(Float), transform: Transform) -> Vec3(Float) {
  let pos = transform.position(transform)
  let quat = transform.rotation_quaternion(transform)
  let scale = transform.scale(transform)

  // Apply scale
  let scaled =
    vec3.Vec3(point.x *. scale.x, point.y *. scale.y, point.z *. scale.z)

  // Apply rotation (quaternion)
  let rotated = rotate_by_quaternion(scaled, quat)

  // Apply translation
  vec3f.add(rotated, pos)
}

/// Rotate a vector by a quaternion using the formula: v' = q * v * q^-1
fn rotate_by_quaternion(v: Vec3(Float), q: Quaternion) -> Vec3(Float) {
  // Optimized quaternion rotation
  let qx = q.x
  let qy = q.y
  let qz = q.z
  let qw = q.w

  // Calculate quat * vector
  let ix = qw *. v.x +. qy *. v.z -. qz *. v.y
  let iy = qw *. v.y +. qz *. v.x -. qx *. v.z
  let iz = qw *. v.z +. qx *. v.y -. qy *. v.x
  let iw = 0.0 -. qx *. v.x -. qy *. v.y -. qz *. v.z

  // Calculate result * inverse quat
  vec3.Vec3(
    ix
      *. qw
      +. iw
      *. { 0.0 -. qx }
      +. iy
      *. { 0.0 -. qz }
      -. iz
      *. { 0.0 -. qy },
    iy
      *. qw
      +. iw
      *. { 0.0 -. qy }
      +. iz
      *. { 0.0 -. qx }
      -. ix
      *. { 0.0 -. qz },
    iz
      *. qw
      +. iw
      *. { 0.0 -. qz }
      +. ix
      *. { 0.0 -. qy }
      -. iy
      *. { 0.0 -. qx },
  )
}

// --- Octree Functions ---

/// Create a new empty octree.
///
/// Note: Octrees require Box colliders (not Sphere).
///
/// ## Parameters
/// - `bounds`: The spatial region this octree covers (must be a Box)
/// - `capacity`: Maximum items per node before subdividing (typically 8-16)
///
/// ## Example
/// ```gleam
/// let bounds = spatial.box(
///   min: vec3.Vec3(-100.0, -100.0, -100.0),
///   max: vec3.Vec3(100.0, 100.0, 100.0),
/// )
/// let tree = spatial.octree_new(bounds, capacity: 8)
/// ```
pub fn octree_new(bounds: Collider, capacity: Int) -> Octree(a) {
  OctreeNode(bounds: bounds, capacity: capacity, items: [], children: None)
}

/// Insert an item at a position into the octree
pub fn octree_insert(
  tree: Octree(a),
  position: Vec3(Float),
  item: a,
) -> Octree(a) {
  case tree {
    OctreeNode(bounds, capacity, items, children) -> {
      // Check if position is within bounds
      case collider_contains_point(bounds, position) {
        False -> tree
        // Not in bounds, don't insert
        True ->
          case children {
            // No children yet - add to this node
            None -> {
              let new_items = [#(position, item), ..items]
              // Check if we need to subdivide
              case list.length(new_items) > capacity {
                False -> OctreeNode(..tree, items: new_items)
                True -> {
                  // Subdivide and redistribute items
                  let subdivided = subdivide(tree)
                  // Insert all items (including new one) into subdivided tree
                  list.fold(new_items, subdivided, fn(acc, item_pair) {
                    let #(pos, it) = item_pair
                    octree_insert(acc, pos, it)
                  })
                }
              }
            }
            // Has children - insert into appropriate child
            Some(octants) -> {
              let new_children =
                insert_into_child(bounds, octants, position, item)
              OctreeNode(..tree, children: Some(new_children))
            }
          }
      }
    }
  }
}

/// Subdivide an octree node into 8 octants
fn subdivide(tree: Octree(a)) -> Octree(a) {
  case tree {
    OctreeNode(bounds, capacity, _, _) -> {
      // Extract min/max from Box collider
      let assert Box(min, max) = bounds
      let center = collider_center(bounds)
      let _half_size = vec3f.scale(collider_size(bounds), 0.5)

      // Create 8 child octants
      let bottom_nw =
        octree_new(
          Box(min: min, max: vec3.Vec3(center.x, center.y, center.z)),
          capacity,
        )
      let bottom_ne =
        octree_new(
          Box(
            min: vec3.Vec3(center.x, min.y, min.z),
            max: vec3.Vec3(max.x, center.y, center.z),
          ),
          capacity,
        )
      let bottom_sw =
        octree_new(
          Box(
            min: vec3.Vec3(min.x, min.y, center.z),
            max: vec3.Vec3(center.x, center.y, max.z),
          ),
          capacity,
        )
      let bottom_se =
        octree_new(
          Box(
            min: vec3.Vec3(center.x, min.y, center.z),
            max: vec3.Vec3(max.x, center.y, max.z),
          ),
          capacity,
        )
      let top_nw =
        octree_new(
          Box(
            min: vec3.Vec3(min.x, center.y, min.z),
            max: vec3.Vec3(center.x, max.y, center.z),
          ),
          capacity,
        )
      let top_ne =
        octree_new(
          Box(
            min: vec3.Vec3(center.x, center.y, min.z),
            max: vec3.Vec3(max.x, max.y, center.z),
          ),
          capacity,
        )
      let top_sw =
        octree_new(
          Box(
            min: vec3.Vec3(min.x, center.y, center.z),
            max: vec3.Vec3(center.x, max.y, max.z),
          ),
          capacity,
        )
      let top_se = octree_new(Box(min: center, max: max), capacity)

      OctreeNode(
        bounds: bounds,
        capacity: capacity,
        items: [],
        children: Some(OctreeChildren(
          bottom_nw: bottom_nw,
          bottom_ne: bottom_ne,
          bottom_sw: bottom_sw,
          bottom_se: bottom_se,
          top_nw: top_nw,
          top_ne: top_ne,
          top_sw: top_sw,
          top_se: top_se,
        )),
      )
    }
  }
}

/// Insert item into the appropriate child octant
fn insert_into_child(
  parent_bounds: Collider,
  children: OctreeChildren(a),
  position: Vec3(Float),
  item: a,
) -> OctreeChildren(a) {
  // Determine which octant based on position relative to parent center
  case children {
    OctreeChildren(
      bottom_nw,
      bottom_ne,
      bottom_sw,
      bottom_se,
      top_nw,
      top_ne,
      top_sw,
      top_se,
    ) -> {
      let center = collider_center(parent_bounds)

      // Check x, y, z relative to center
      case
        position.x <. center.x,
        position.y <. center.y,
        position.z <. center.z
      {
        True, True, True ->
          OctreeChildren(
            ..children,
            bottom_nw: octree_insert(bottom_nw, position, item),
          )
        False, True, True ->
          OctreeChildren(
            ..children,
            bottom_ne: octree_insert(bottom_ne, position, item),
          )
        True, True, False ->
          OctreeChildren(
            ..children,
            bottom_sw: octree_insert(bottom_sw, position, item),
          )
        False, True, False ->
          OctreeChildren(
            ..children,
            bottom_se: octree_insert(bottom_se, position, item),
          )
        True, False, True ->
          OctreeChildren(
            ..children,
            top_nw: octree_insert(top_nw, position, item),
          )
        False, False, True ->
          OctreeChildren(
            ..children,
            top_ne: octree_insert(top_ne, position, item),
          )
        True, False, False ->
          OctreeChildren(
            ..children,
            top_sw: octree_insert(top_sw, position, item),
          )
        False, False, False ->
          OctreeChildren(
            ..children,
            top_se: octree_insert(top_se, position, item),
          )
      }
    }
  }
}

/// Query all items within a collider region
pub fn octree_query(
  tree: Octree(a),
  query_bounds: Collider,
) -> List(#(Vec3(Float), a)) {
  case tree {
    OctreeNode(bounds, _, items, children) -> {
      // If query bounds don't intersect this node, return empty
      case collider_intersects(bounds, query_bounds) {
        False -> []
        True -> {
          // Collect items in this node that are within query bounds
          let local_matches =
            list.filter(items, fn(item_pair) {
              let #(pos, _) = item_pair
              collider_contains_point(query_bounds, pos)
            })

          // Recursively query children
          case children {
            None -> local_matches
            Some(OctreeChildren(
              bottom_nw,
              bottom_ne,
              bottom_sw,
              bottom_se,
              top_nw,
              top_ne,
              top_sw,
              top_se,
            )) ->
              list.flatten([
                local_matches,
                octree_query(bottom_nw, query_bounds),
                octree_query(bottom_ne, query_bounds),
                octree_query(bottom_sw, query_bounds),
                octree_query(bottom_se, query_bounds),
                octree_query(top_nw, query_bounds),
                octree_query(top_ne, query_bounds),
                octree_query(top_sw, query_bounds),
                octree_query(top_se, query_bounds),
              ])
          }
        }
      }
    }
  }
}

/// Query all items within a radius of a point
pub fn octree_query_radius(
  tree: Octree(a),
  center: Vec3(Float),
  radius: Float,
) -> List(#(Vec3(Float), a)) {
  // Create a bounding box that encompasses the sphere
  let half_extents = vec3.Vec3(radius, radius, radius)
  let query_bounds = collider_box_from_center(center, half_extents)

  // Query the box, then filter by actual distance
  octree_query(tree, query_bounds)
  |> list.filter(fn(item_pair) {
    let #(pos, _) = item_pair
    vec3f.distance(center, pos) <=. radius
  })
}

/// Query all items in the octree (useful for iteration)
pub fn octree_query_all(tree: Octree(a)) -> List(#(Vec3(Float), a)) {
  case tree {
    OctreeNode(_, _, items, children) ->
      case children {
        None -> items
        Some(OctreeChildren(
          bottom_nw,
          bottom_ne,
          bottom_sw,
          bottom_se,
          top_nw,
          top_ne,
          top_sw,
          top_se,
        )) ->
          list.flatten([
            items,
            octree_query_all(bottom_nw),
            octree_query_all(bottom_ne),
            octree_query_all(bottom_sw),
            octree_query_all(bottom_se),
            octree_query_all(top_nw),
            octree_query_all(top_ne),
            octree_query_all(top_sw),
            octree_query_all(top_se),
          ])
      }
  }
}

/// Count total items in the octree
pub fn octree_count(tree: Octree(a)) -> Int {
  octree_query_all(tree)
  |> list.length
}

/// Get the bounds of the octree
pub fn octree_bounds(tree: Octree(a)) -> Collider {
  case tree {
    OctreeNode(bounds, ..) -> bounds
  }
}
