/// Spatial partitioning data structures for efficient spatial queries
///
/// Provides octree and AABB (Axis-Aligned Bounding Box) for:
/// - Finding objects within a region
/// - Finding nearby objects
/// - Frustum culling optimization
/// - Broad-phase collision detection
import gleam/list
import gleam/option.{type Option, None, Some}
import vec/vec3.{type Vec3}
import vec/vec3f

// --- Types ---

/// Axis-Aligned Bounding Box
pub type AABB {
  AABB(min: Vec3(Float), max: Vec3(Float))
}

/// Octree node for spatial partitioning
/// Divides 3D space into 8 octants recursively
pub opaque type Octree(a) {
  OctreeNode(
    bounds: AABB,
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

// --- AABB Functions ---

/// Create an AABB from min and max points
pub fn aabb(min min: Vec3(Float), max max: Vec3(Float)) -> AABB {
  AABB(min: min, max: max)
}

/// Create an AABB from center and half-extents
pub fn aabb_from_center(center: Vec3(Float), half_extents: Vec3(Float)) -> AABB {
  AABB(
    min: vec3f.subtract(center, half_extents),
    max: vec3f.add(center, half_extents),
  )
}

/// Check if a point is inside an AABB
pub fn aabb_contains_point(bounds: AABB, point: Vec3(Float)) -> Bool {
  point.x >=. bounds.min.x
  && point.x <=. bounds.max.x
  && point.y >=. bounds.min.y
  && point.y <=. bounds.max.y
  && point.z >=. bounds.min.z
  && point.z <=. bounds.max.z
}

/// Check if two AABBs intersect
pub fn aabb_intersects(a: AABB, b: AABB) -> Bool {
  a.min.x <=. b.max.x
  && a.max.x >=. b.min.x
  && a.min.y <=. b.max.y
  && a.max.y >=. b.min.y
  && a.min.z <=. b.max.z
  && a.max.z >=. b.min.z
}

/// Get the center of an AABB
pub fn aabb_center(bounds: AABB) -> Vec3(Float) {
  vec3.Vec3(
    { bounds.min.x +. bounds.max.x } /. 2.0,
    { bounds.min.y +. bounds.max.y } /. 2.0,
    { bounds.min.z +. bounds.max.z } /. 2.0,
  )
}

/// Get the size (dimensions) of an AABB
pub fn aabb_size(bounds: AABB) -> Vec3(Float) {
  vec3f.subtract(bounds.max, bounds.min)
}

// --- Octree Functions ---

/// Create a new empty octree
///
/// ## Parameters
/// - `bounds`: The spatial region this octree covers
/// - `capacity`: Maximum items per node before subdividing (typically 8-16)
///
/// ## Example
/// ```gleam
/// let bounds = aabb(
///   min: vec3.Vec3(-100.0, -100.0, -100.0),
///   max: vec3.Vec3(100.0, 100.0, 100.0)
/// )
/// let tree = octree_new(bounds, capacity: 8)
/// ```
pub fn octree_new(bounds: AABB, capacity: Int) -> Octree(a) {
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
      case aabb_contains_point(bounds, position) {
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
      let center = aabb_center(bounds)
      let _half_size = vec3f.scale(aabb_size(bounds), 0.5)

      // Create 8 child octants
      let bottom_nw =
        octree_new(
          AABB(min: bounds.min, max: vec3.Vec3(center.x, center.y, center.z)),
          capacity,
        )
      let bottom_ne =
        octree_new(
          AABB(
            min: vec3.Vec3(center.x, bounds.min.y, bounds.min.z),
            max: vec3.Vec3(bounds.max.x, center.y, center.z),
          ),
          capacity,
        )
      let bottom_sw =
        octree_new(
          AABB(
            min: vec3.Vec3(bounds.min.x, bounds.min.y, center.z),
            max: vec3.Vec3(center.x, center.y, bounds.max.z),
          ),
          capacity,
        )
      let bottom_se =
        octree_new(
          AABB(
            min: vec3.Vec3(center.x, bounds.min.y, center.z),
            max: vec3.Vec3(bounds.max.x, center.y, bounds.max.z),
          ),
          capacity,
        )
      let top_nw =
        octree_new(
          AABB(
            min: vec3.Vec3(bounds.min.x, center.y, bounds.min.z),
            max: vec3.Vec3(center.x, bounds.max.y, center.z),
          ),
          capacity,
        )
      let top_ne =
        octree_new(
          AABB(
            min: vec3.Vec3(center.x, center.y, bounds.min.z),
            max: vec3.Vec3(bounds.max.x, bounds.max.y, center.z),
          ),
          capacity,
        )
      let top_sw =
        octree_new(
          AABB(
            min: vec3.Vec3(bounds.min.x, center.y, center.z),
            max: vec3.Vec3(center.x, bounds.max.y, bounds.max.z),
          ),
          capacity,
        )
      let top_se = octree_new(AABB(min: center, max: bounds.max), capacity)

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
  parent_bounds: AABB,
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
      let center = aabb_center(parent_bounds)

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

/// Query all items within a bounding box region
pub fn octree_query(
  tree: Octree(a),
  query_bounds: AABB,
) -> List(#(Vec3(Float), a)) {
  case tree {
    OctreeNode(bounds, _, items, children) -> {
      // If query bounds don't intersect this node, return empty
      case aabb_intersects(bounds, query_bounds) {
        False -> []
        True -> {
          // Collect items in this node that are within query bounds
          let local_matches =
            list.filter(items, fn(item_pair) {
              let #(pos, _) = item_pair
              aabb_contains_point(query_bounds, pos)
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
  let query_bounds = aabb_from_center(center, half_extents)

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
pub fn octree_bounds(tree: Octree(a)) -> AABB {
  case tree {
    OctreeNode(bounds, ..) -> bounds
  }
}
