import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $vec3 from "../../vec/vec/vec3.mjs";
import * as $vec3f from "../../vec/vec/vec3f.mjs";
import { toList, prepend as listPrepend, CustomType as $CustomType } from "../gleam.mjs";

export class AABB extends $CustomType {
  constructor(min, max) {
    super();
    this.min = min;
    this.max = max;
  }
}

class OctreeNode extends $CustomType {
  constructor(bounds, capacity, items, children) {
    super();
    this.bounds = bounds;
    this.capacity = capacity;
    this.items = items;
    this.children = children;
  }
}

class OctreeChildren extends $CustomType {
  constructor(bottom_nw, bottom_ne, bottom_sw, bottom_se, top_nw, top_ne, top_sw, top_se) {
    super();
    this.bottom_nw = bottom_nw;
    this.bottom_ne = bottom_ne;
    this.bottom_sw = bottom_sw;
    this.bottom_se = bottom_se;
    this.top_nw = top_nw;
    this.top_ne = top_ne;
    this.top_sw = top_sw;
    this.top_se = top_se;
  }
}

/**
 * Create an AABB from min and max points
 */
export function aabb(min, max) {
  return new AABB(min, max);
}

/**
 * Create an AABB from center and half-extents
 */
export function aabb_from_center(center, half_extents) {
  return new AABB(
    $vec3f.subtract(center, half_extents),
    $vec3f.add(center, half_extents),
  );
}

/**
 * Check if a point is inside an AABB
 */
export function aabb_contains_point(bounds, point) {
  return (((((point.x >= bounds.min.x) && (point.x <= bounds.max.x)) && (point.y >= bounds.min.y)) && (point.y <= bounds.max.y)) && (point.z >= bounds.min.z)) && (point.z <= bounds.max.z);
}

/**
 * Check if two AABBs intersect
 */
export function aabb_intersects(a, b) {
  return (((((a.min.x <= b.max.x) && (a.max.x >= b.min.x)) && (a.min.y <= b.max.y)) && (a.max.y >= b.min.y)) && (a.min.z <= b.max.z)) && (a.max.z >= b.min.z);
}

/**
 * Get the center of an AABB
 */
export function aabb_center(bounds) {
  return new $vec3.Vec3(
    (bounds.min.x + bounds.max.x) / 2.0,
    (bounds.min.y + bounds.max.y) / 2.0,
    (bounds.min.z + bounds.max.z) / 2.0,
  );
}

/**
 * Get the size (dimensions) of an AABB
 */
export function aabb_size(bounds) {
  return $vec3f.subtract(bounds.max, bounds.min);
}

/**
 * Create a new empty octree
 *
 * ## Parameters
 * - `bounds`: The spatial region this octree covers
 * - `capacity`: Maximum items per node before subdividing (typically 8-16)
 *
 * ## Example
 * ```gleam
 * let bounds = aabb(
 *   min: vec3.Vec3(-100.0, -100.0, -100.0),
 *   max: vec3.Vec3(100.0, 100.0, 100.0)
 * )
 * let tree = octree_new(bounds, capacity: 8)
 * ```
 */
export function octree_new(bounds, capacity) {
  return new OctreeNode(bounds, capacity, toList([]), new None());
}

/**
 * Subdivide an octree node into 8 octants
 * 
 * @ignore
 */
function subdivide(tree) {
  let bounds = tree.bounds;
  let capacity = tree.capacity;
  let center = aabb_center(bounds);
  let $ = $vec3f.scale(aabb_size(bounds), 0.5);
  
  let bottom_nw = octree_new(
    new AABB(bounds.min, new $vec3.Vec3(center.x, center.y, center.z)),
    capacity,
  );
  let bottom_ne = octree_new(
    new AABB(
      new $vec3.Vec3(center.x, bounds.min.y, bounds.min.z),
      new $vec3.Vec3(bounds.max.x, center.y, center.z),
    ),
    capacity,
  );
  let bottom_sw = octree_new(
    new AABB(
      new $vec3.Vec3(bounds.min.x, bounds.min.y, center.z),
      new $vec3.Vec3(center.x, center.y, bounds.max.z),
    ),
    capacity,
  );
  let bottom_se = octree_new(
    new AABB(
      new $vec3.Vec3(center.x, bounds.min.y, center.z),
      new $vec3.Vec3(bounds.max.x, center.y, bounds.max.z),
    ),
    capacity,
  );
  let top_nw = octree_new(
    new AABB(
      new $vec3.Vec3(bounds.min.x, center.y, bounds.min.z),
      new $vec3.Vec3(center.x, bounds.max.y, center.z),
    ),
    capacity,
  );
  let top_ne = octree_new(
    new AABB(
      new $vec3.Vec3(center.x, center.y, bounds.min.z),
      new $vec3.Vec3(bounds.max.x, bounds.max.y, center.z),
    ),
    capacity,
  );
  let top_sw = octree_new(
    new AABB(
      new $vec3.Vec3(bounds.min.x, center.y, center.z),
      new $vec3.Vec3(center.x, bounds.max.y, bounds.max.z),
    ),
    capacity,
  );
  let top_se = octree_new(new AABB(center, bounds.max), capacity);
  return new OctreeNode(
    bounds,
    capacity,
    toList([]),
    new Some(
      new OctreeChildren(
        bottom_nw,
        bottom_ne,
        bottom_sw,
        bottom_se,
        top_nw,
        top_ne,
        top_sw,
        top_se,
      ),
    ),
  );
}

/**
 * Query all items within a bounding box region
 */
export function octree_query(tree, query_bounds) {
  let bounds = tree.bounds;
  let items = tree.items;
  let children = tree.children;
  let $ = aabb_intersects(bounds, query_bounds);
  if ($) {
    let local_matches = $list.filter(
      items,
      (item_pair) => {
        let pos;
        pos = item_pair[0];
        return aabb_contains_point(query_bounds, pos);
      },
    );
    if (children instanceof Some) {
      let bottom_nw = children[0].bottom_nw;
      let bottom_ne = children[0].bottom_ne;
      let bottom_sw = children[0].bottom_sw;
      let bottom_se = children[0].bottom_se;
      let top_nw = children[0].top_nw;
      let top_ne = children[0].top_ne;
      let top_sw = children[0].top_sw;
      let top_se = children[0].top_se;
      return $list.flatten(
        toList([
          local_matches,
          octree_query(bottom_nw, query_bounds),
          octree_query(bottom_ne, query_bounds),
          octree_query(bottom_sw, query_bounds),
          octree_query(bottom_se, query_bounds),
          octree_query(top_nw, query_bounds),
          octree_query(top_ne, query_bounds),
          octree_query(top_sw, query_bounds),
          octree_query(top_se, query_bounds),
        ]),
      );
    } else {
      return local_matches;
    }
  } else {
    return toList([]);
  }
}

/**
 * Query all items within a radius of a point
 */
export function octree_query_radius(tree, center, radius) {
  let half_extents = new $vec3.Vec3(radius, radius, radius);
  let query_bounds = aabb_from_center(center, half_extents);
  let _pipe = octree_query(tree, query_bounds);
  return $list.filter(
    _pipe,
    (item_pair) => {
      let pos;
      pos = item_pair[0];
      return $vec3f.distance(center, pos) <= radius;
    },
  );
}

/**
 * Query all items in the octree (useful for iteration)
 */
export function octree_query_all(tree) {
  let items = tree.items;
  let children = tree.children;
  if (children instanceof Some) {
    let bottom_nw = children[0].bottom_nw;
    let bottom_ne = children[0].bottom_ne;
    let bottom_sw = children[0].bottom_sw;
    let bottom_se = children[0].bottom_se;
    let top_nw = children[0].top_nw;
    let top_ne = children[0].top_ne;
    let top_sw = children[0].top_sw;
    let top_se = children[0].top_se;
    return $list.flatten(
      toList([
        items,
        octree_query_all(bottom_nw),
        octree_query_all(bottom_ne),
        octree_query_all(bottom_sw),
        octree_query_all(bottom_se),
        octree_query_all(top_nw),
        octree_query_all(top_ne),
        octree_query_all(top_sw),
        octree_query_all(top_se),
      ]),
    );
  } else {
    return items;
  }
}

/**
 * Count total items in the octree
 */
export function octree_count(tree) {
  let _pipe = octree_query_all(tree);
  return $list.length(_pipe);
}

/**
 * Get the bounds of the octree
 */
export function octree_bounds(tree) {
  let bounds = tree.bounds;
  return bounds;
}

/**
 * Insert item into the appropriate child octant
 * 
 * @ignore
 */
function insert_into_child(parent_bounds, children, position, item) {
  let bottom_nw = children.bottom_nw;
  let bottom_ne = children.bottom_ne;
  let bottom_sw = children.bottom_sw;
  let bottom_se = children.bottom_se;
  let top_nw = children.top_nw;
  let top_ne = children.top_ne;
  let top_sw = children.top_sw;
  let top_se = children.top_se;
  let center = aabb_center(parent_bounds);
  let $ = position.x < center.x;
  let $1 = position.y < center.y;
  let $2 = position.z < center.z;
  if ($2) {
    if ($1) {
      if ($) {
        return new OctreeChildren(
          octree_insert(bottom_nw, position, item),
          children.bottom_ne,
          children.bottom_sw,
          children.bottom_se,
          children.top_nw,
          children.top_ne,
          children.top_sw,
          children.top_se,
        );
      } else {
        return new OctreeChildren(
          children.bottom_nw,
          octree_insert(bottom_ne, position, item),
          children.bottom_sw,
          children.bottom_se,
          children.top_nw,
          children.top_ne,
          children.top_sw,
          children.top_se,
        );
      }
    } else if ($) {
      return new OctreeChildren(
        children.bottom_nw,
        children.bottom_ne,
        children.bottom_sw,
        children.bottom_se,
        octree_insert(top_nw, position, item),
        children.top_ne,
        children.top_sw,
        children.top_se,
      );
    } else {
      return new OctreeChildren(
        children.bottom_nw,
        children.bottom_ne,
        children.bottom_sw,
        children.bottom_se,
        children.top_nw,
        octree_insert(top_ne, position, item),
        children.top_sw,
        children.top_se,
      );
    }
  } else if ($1) {
    if ($) {
      return new OctreeChildren(
        children.bottom_nw,
        children.bottom_ne,
        octree_insert(bottom_sw, position, item),
        children.bottom_se,
        children.top_nw,
        children.top_ne,
        children.top_sw,
        children.top_se,
      );
    } else {
      return new OctreeChildren(
        children.bottom_nw,
        children.bottom_ne,
        children.bottom_sw,
        octree_insert(bottom_se, position, item),
        children.top_nw,
        children.top_ne,
        children.top_sw,
        children.top_se,
      );
    }
  } else if ($) {
    return new OctreeChildren(
      children.bottom_nw,
      children.bottom_ne,
      children.bottom_sw,
      children.bottom_se,
      children.top_nw,
      children.top_ne,
      octree_insert(top_sw, position, item),
      children.top_se,
    );
  } else {
    return new OctreeChildren(
      children.bottom_nw,
      children.bottom_ne,
      children.bottom_sw,
      children.bottom_se,
      children.top_nw,
      children.top_ne,
      children.top_sw,
      octree_insert(top_se, position, item),
    );
  }
}

/**
 * Insert an item at a position into the octree
 */
export function octree_insert(tree, position, item) {
  let bounds = tree.bounds;
  let capacity = tree.capacity;
  let items = tree.items;
  let children = tree.children;
  let $ = aabb_contains_point(bounds, position);
  if ($) {
    if (children instanceof Some) {
      let octants = children[0];
      let new_children = insert_into_child(bounds, octants, position, item);
      return new OctreeNode(
        tree.bounds,
        tree.capacity,
        tree.items,
        new Some(new_children),
      );
    } else {
      let new_items = listPrepend([position, item], items);
      let $1 = $list.length(new_items) > capacity;
      if ($1) {
        let subdivided = subdivide(tree);
        return $list.fold(
          new_items,
          subdivided,
          (acc, item_pair) => {
            let pos;
            let it;
            pos = item_pair[0];
            it = item_pair[1];
            return octree_insert(acc, pos, it);
          },
        );
      } else {
        return new OctreeNode(
          tree.bounds,
          tree.capacity,
          new_items,
          tree.children,
        );
      }
    }
  } else {
    return tree;
  }
}
