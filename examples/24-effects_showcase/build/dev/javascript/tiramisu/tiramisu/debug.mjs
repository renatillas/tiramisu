import * as $maths from "../../gleam_community_maths/gleam_community/maths.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $vec3 from "../../vec/vec/vec3.mjs";
import * as $vec3f from "../../vec/vec/vec3f.mjs";
import {
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  divideFloat,
} from "../gleam.mjs";
import {
  getPerformanceStats as get_performance_stats,
  showColliders as show_colliders_ffi,
} from "../tiramisu.ffi.mjs";
import * as $physics from "../tiramisu/physics.mjs";
import * as $scene from "../tiramisu/scene.mjs";
import * as $transform from "../tiramisu/transform.mjs";

export { get_performance_stats };

const FILEPATH = "src/tiramisu/debug.gleam";

export class PerformanceStats extends $CustomType {
  constructor(fps, frame_time, draw_calls, triangles, memory_mb) {
    super();
    this.fps = fps;
    this.frame_time = frame_time;
    this.draw_calls = draw_calls;
    this.triangles = triangles;
    this.memory_mb = memory_mb;
  }
}

export function bounding_box(id, min, max, color) {
  return new $scene.DebugBox(id, min, max, color);
}

export function sphere(id, center, radius, color) {
  return new $scene.DebugSphere(id, center, radius, color);
}

export function line(id, from, to, color) {
  return new $scene.DebugLine(id, from, to, color);
}

export function ray(id, origin, direction, length, color) {
  let end = $vec3f.add(origin, $vec3f.scale(direction, length));
  return new $scene.DebugLine(id, origin, end, color);
}

export function axes(id, origin, size) {
  return new $scene.DebugAxes(id, origin, size);
}

export function grid(id, size, divisions, color) {
  return new $scene.DebugGrid(id, size, divisions, color);
}

export function point(id, position, size, color) {
  return new $scene.DebugPoint(id, position, size, color);
}

export function box_from_transform(id, t, color) {
  let half_x = t.scale.x / 2.0;
  let half_y = t.scale.y / 2.0;
  let half_z = t.scale.z / 2.0;
  let min = new $vec3.Vec3(
    t.position.x - half_x,
    t.position.y - half_y,
    t.position.z - half_z,
  );
  let max = new $vec3.Vec3(
    t.position.x + half_x,
    t.position.y + half_y,
    t.position.z + half_z,
  );
  return bounding_box(id, min, max, color);
}

function create_path_lines(
  loop$id,
  loop$points,
  loop$color,
  loop$index,
  loop$acc
) {
  while (true) {
    let id = loop$id;
    let points = loop$points;
    let color = loop$color;
    let index = loop$index;
    let acc = loop$acc;
    if (points instanceof $Empty) {
      return $list.reverse(acc);
    } else {
      let $ = points.tail;
      if ($ instanceof $Empty) {
        return $list.reverse(acc);
      } else {
        let p1 = points.head;
        let p2 = $.head;
        let rest = $.tail;
        let line_node = line(id, p1, p2, color);
        loop$id = id;
        loop$points = listPrepend(p2, rest);
        loop$color = color;
        loop$index = index + 1;
        loop$acc = listPrepend(line_node, acc);
      }
    }
  }
}

/**
 * Create multiple lines forming a path through points
 */
export function path(id_prefix, points, color) {
  return create_path_lines(id_prefix, points, color, 0, toList([]));
}

export function cross(id, position, size, color) {
  let half_size = size / 2.0;
  return toList([
    line(
      id,
      new $vec3.Vec3(position.x - half_size, position.y, position.z),
      new $vec3.Vec3(position.x + half_size, position.y, position.z),
      color,
    ),
    line(
      id,
      new $vec3.Vec3(position.x, position.y - half_size, position.z),
      new $vec3.Vec3(position.x, position.y + half_size, position.z),
      color,
    ),
    line(
      id,
      new $vec3.Vec3(position.x, position.y, position.z - half_size),
      new $vec3.Vec3(position.x, position.y, position.z + half_size),
      color,
    ),
  ]);
}

/**
 * Enable or disable debug wireframe visualization for all physics colliders in the scene.
 *
 * This function uses Rapier's built-in collider visualization which renders wireframes
 * for all physics bodies in the physics world. The wireframes update automatically each frame
 * as objects move and rotate.
 *
 * **Note:** This function requires access to the physics world, which is available
 * in the `view` function via the `Context` parameter.
 *
 * ## Example
 *
 * ```gleam
 * import tiramisu/debug
 * import gleam/option
 *
 * pub fn view(model: Model, ctx: tiramisu.Context) {
 *   // Enable/disable debug visualization based on model state
 *   case ctx.physics_world, model.debug_mode {
 *     option.Some(physics_world), True -> {
 *       debug.show_collider_wireframes(physics_world, True)
 *     }
 *     _, _ -> Nil
 *   }
 *
 *   // Return scene as normal
 *   [
 *     scene.Mesh(
 *       id: "cube",
 *       geometry: geometry,
 *       material: material,
 *       transform: transform,
 *       physics: option.Some(physics_body),  // Will show wireframe when enabled
 *     ),
 *     // ... more scene nodes
 *   ]
 * }
 * ```
 */
export function show_collider_wireframes(physics_world, enabled) {
  return show_colliders_ffi(physics_world, enabled);
}

/**
 * @deprecated Use `show_collider_wireframes` with the physics world from context instead.
 *
 * This function is kept for backwards compatibility but will be removed in a future version.
 */
export function with_collider_wireframes(nodes, _) {
  return nodes;
}

/**
 * Helper to get element at index from list (panics if out of bounds)
 * 
 * @ignore
 */
function list_at(list, index) {
  let $ = $list.drop(list, index);
  if ($ instanceof $Empty) {
    throw makeError(
      "panic",
      FILEPATH,
      "tiramisu/debug",
      344,
      "list_at",
      "Index out of bounds",
      {}
    )
  } else {
    let first = $.head;
    return first;
  }
}

/**
 * Visualize a sphere collider
 * 
 * @ignore
 */
function collider_sphere(id, radius, transform, color) {
  let center = transform.position;
  return sphere(id, center, radius, color);
}

/**
 * Generate wireframe lines for a capsule in local space
 * 
 * @ignore
 */
function generate_capsule_wireframe(half_height, radius, segments) {
  let pi = 3.14159265359;
  let neg_half_height = 0.0 - half_height;
  let _block;
  let _pipe = $list.range(0, segments - 1);
  _block = $list.map(
    _pipe,
    (i) => {
      let angle = divideFloat(
        ((2.0 * pi) * $int.to_float(i)),
        $int.to_float(segments)
      );
      let cos = $maths.cos(angle);
      let x = radius * cos;
      let sin = $maths.sin(angle);
      let z = radius * sin;
      let bottom = new $vec3.Vec3(x, neg_half_height, z);
      let top = new $vec3.Vec3(x, half_height, z);
      return [bottom, top];
    },
  );
  let vertical_lines = _block;
  let _block$1;
  let _pipe$1 = $list.range(0, segments - 1);
  _block$1 = $list.flat_map(
    _pipe$1,
    (i) => {
      let angle1 = divideFloat(
        ((2.0 * pi) * $int.to_float(i)),
        $int.to_float(segments)
      );
      let angle2 = divideFloat(
        ((2.0 * pi) * $int.to_float(i + 1)),
        $int.to_float(segments)
      );
      let cos1 = $maths.cos(angle1);
      let x1 = radius * cos1;
      let sin1 = $maths.sin(angle1);
      let z1 = radius * sin1;
      let cos2 = $maths.cos(angle2);
      let x2 = radius * cos2;
      let sin2 = $maths.sin(angle2);
      let z2 = radius * sin2;
      return toList([
        [
          new $vec3.Vec3(x1, neg_half_height, z1),
          new $vec3.Vec3(x2, neg_half_height, z2),
        ],
        [
          new $vec3.Vec3(x1, half_height, z1),
          new $vec3.Vec3(x2, half_height, z2),
        ],
      ]);
    },
  );
  let ring_lines = _block$1;
  let _block$2;
  let _pipe$2 = $list.range(0, 3);
  _block$2 = $list.flat_map(
    _pipe$2,
    (i) => {
      let angle = (((2.0 * pi) * $int.to_float(i))) / 4.0;
      let cos_a = $maths.cos(angle);
      let x = radius * cos_a;
      let sin_a = $maths.sin(angle);
      let z = radius * sin_a;
      let _pipe$3 = $list.range(0, globalThis.Math.trunc(segments / 4));
      return $list.map(
        _pipe$3,
        (j) => {
          let t1 = divideFloat(
            $int.to_float(j),
            $int.to_float(globalThis.Math.trunc(segments / 4))
          );
          let t2 = divideFloat(
            $int.to_float(j + 1),
            $int.to_float(globalThis.Math.trunc(segments / 4))
          );
          let angle1 = pi * ((0.0 - 0.5) + (t1 / 2.0));
          let angle2 = pi * ((0.0 - 0.5) + (t2 / 2.0));
          let sin1 = $maths.sin(angle1);
          let y1 = neg_half_height + (radius * sin1);
          let cos1 = $maths.cos(angle1);
          let r1 = radius * cos1;
          let sin2 = $maths.sin(angle2);
          let y2 = neg_half_height + (radius * sin2);
          let cos2 = $maths.cos(angle2);
          let r2 = radius * cos2;
          let bottom_start = new $vec3.Vec3(
            divideFloat((x * r1), radius),
            y1,
            divideFloat((z * r1), radius),
          );
          let bottom_end = new $vec3.Vec3(
            divideFloat((x * r2), radius),
            y2,
            divideFloat((z * r2), radius),
          );
          return [bottom_start, bottom_end];
        },
      );
    },
  );
  let cap_arcs = _block$2;
  return $list.flatten(toList([vertical_lines, ring_lines, cap_arcs]));
}

/**
 * Generate wireframe lines for a cylinder in local space
 * 
 * @ignore
 */
function generate_cylinder_wireframe(half_height, radius, segments) {
  let pi = 3.14159265359;
  let neg_half_height = 0.0 - half_height;
  let _block;
  let _pipe = $list.range(0, segments - 1);
  _block = $list.map(
    _pipe,
    (i) => {
      let angle = divideFloat(
        ((2.0 * pi) * $int.to_float(i)),
        $int.to_float(segments)
      );
      let cos_val = $maths.cos(angle);
      let x = radius * cos_val;
      let sin_val = $maths.sin(angle);
      let z = radius * sin_val;
      let bottom = new $vec3.Vec3(x, neg_half_height, z);
      let top = new $vec3.Vec3(x, half_height, z);
      return [bottom, top];
    },
  );
  let vertical_lines = _block;
  let _block$1;
  let _pipe$1 = $list.range(0, segments - 1);
  _block$1 = $list.flat_map(
    _pipe$1,
    (i) => {
      let angle1 = divideFloat(
        ((2.0 * pi) * $int.to_float(i)),
        $int.to_float(segments)
      );
      let angle2 = divideFloat(
        ((2.0 * pi) * $int.to_float(i + 1)),
        $int.to_float(segments)
      );
      let cos1 = $maths.cos(angle1);
      let x1 = radius * cos1;
      let sin1 = $maths.sin(angle1);
      let z1 = radius * sin1;
      let cos2 = $maths.cos(angle2);
      let x2 = radius * cos2;
      let sin2 = $maths.sin(angle2);
      let z2 = radius * sin2;
      return toList([
        [
          new $vec3.Vec3(x1, neg_half_height, z1),
          new $vec3.Vec3(x2, neg_half_height, z2),
        ],
        [
          new $vec3.Vec3(x1, half_height, z1),
          new $vec3.Vec3(x2, half_height, z2),
        ],
      ]);
    },
  );
  let ring_lines = _block$1;
  return $list.append(vertical_lines, ring_lines);
}

/**
 * Rotate a point around the Y axis
 * 
 * @ignore
 */
function rotate_y(point, angle) {
  let cos_a = $maths.cos(angle);
  let sin_a = $maths.sin(angle);
  return new $vec3.Vec3(
    (point.x * cos_a) + (point.z * sin_a),
    point.y,
    (0.0 - (point.x * sin_a)) + (point.z * cos_a),
  );
}

/**
 * Transform a point by a transform (position + rotation + scale)
 * 
 * @ignore
 */
function transform_point(point, transform) {
  let scaled = new $vec3.Vec3(
    point.x * transform.scale.x,
    point.y * transform.scale.y,
    point.z * transform.scale.z,
  );
  let rotated = rotate_y(scaled, transform.rotation.y);
  return $vec3f.add(rotated, transform.position);
}

/**
 * Visualize a box collider
 * 
 * @ignore
 */
function collider_box(id, width, height, depth, transform, color) {
  let half_w = width / 2.0;
  let half_h = height / 2.0;
  let half_d = depth / 2.0;
  let neg_half_w = 0.0 - half_w;
  let neg_half_h = 0.0 - half_h;
  let neg_half_d = 0.0 - half_d;
  let corners = toList([
    new $vec3.Vec3(neg_half_w, neg_half_h, neg_half_d),
    new $vec3.Vec3(half_w, neg_half_h, neg_half_d),
    new $vec3.Vec3(half_w, half_h, neg_half_d),
    new $vec3.Vec3(neg_half_w, half_h, neg_half_d),
    new $vec3.Vec3(neg_half_w, neg_half_h, half_d),
    new $vec3.Vec3(half_w, neg_half_h, half_d),
    new $vec3.Vec3(half_w, half_h, half_d),
    new $vec3.Vec3(neg_half_w, half_h, half_d),
  ]);
  let world_corners = $list.map(
    corners,
    (_capture) => { return transform_point(_capture, transform); },
  );
  return new $scene.Group(
    id,
    $transform.identity,
    toList([
      line(id, list_at(world_corners, 0), list_at(world_corners, 1), color),
      line(id, list_at(world_corners, 1), list_at(world_corners, 2), color),
      line(id, list_at(world_corners, 2), list_at(world_corners, 3), color),
      line(id, list_at(world_corners, 3), list_at(world_corners, 0), color),
      line(id, list_at(world_corners, 4), list_at(world_corners, 5), color),
      line(id, list_at(world_corners, 5), list_at(world_corners, 6), color),
      line(id, list_at(world_corners, 6), list_at(world_corners, 7), color),
      line(id, list_at(world_corners, 7), list_at(world_corners, 4), color),
      line(id, list_at(world_corners, 0), list_at(world_corners, 4), color),
      line(id, list_at(world_corners, 1), list_at(world_corners, 5), color),
      line(id, list_at(world_corners, 2), list_at(world_corners, 6), color),
      line(id, list_at(world_corners, 3), list_at(world_corners, 7), color),
    ]),
  );
}

/**
 * Visualize a capsule collider (cylinder with hemispherical caps)
 * 
 * @ignore
 */
function collider_capsule(id, half_height, radius, transform, color) {
  let segments = 16;
  let lines = generate_capsule_wireframe(half_height, radius, segments);
  let world_lines = $list.map(
    lines,
    (line_pair) => {
      let start;
      let end;
      start = line_pair[0];
      end = line_pair[1];
      return [
        transform_point(start, transform),
        transform_point(end, transform),
      ];
    },
  );
  let children = $list.index_map(
    world_lines,
    (line_pair, _) => {
      let start;
      let end;
      start = line_pair[0];
      end = line_pair[1];
      return line(id, start, end, color);
    },
  );
  return new $scene.Group(id, $transform.identity, children);
}

/**
 * Visualize a cylinder collider
 * 
 * @ignore
 */
function collider_cylinder(id, half_height, radius, transform, color) {
  let segments = 16;
  let lines = generate_cylinder_wireframe(half_height, radius, segments);
  let world_lines = $list.map(
    lines,
    (line_pair) => {
      let start;
      let end;
      start = line_pair[0];
      end = line_pair[1];
      return [
        transform_point(start, transform),
        transform_point(end, transform),
      ];
    },
  );
  let children = $list.index_map(
    world_lines,
    (line_pair, _) => {
      let start;
      let end;
      start = line_pair[0];
      end = line_pair[1];
      return line(id, start, end, color);
    },
  );
  return new $scene.Group(id, $transform.identity, children);
}

/**
 * Visualize a physics collider shape at a given transform.
 *
 * This function converts a physics collider into debug visualization nodes
 * that can be added to your scene for debugging physics shapes.
 *
 * ## Example
 *
 * ```gleam
 * import tiramisu/debug
 * import tiramisu/physics
 * import tiramisu/transform
 * import vec/vec3
 *
 * pub fn view(model: Model) {
 *   let body_transform = transform.at(position: vec3.Vec3(0.0, 5.0, 0.0))
 *   let collider = physics.Box(width: 2.0, height: 2.0, depth: 2.0)
 *
 *   [
 *     // Your normal scene nodes...
 *     // Debug visualization for the collider
 *     debug.collider(
 *       id: "player-collider-debug",
 *       shape: collider,
 *       transform: body_transform,
 *       color: debug.color_green,
 *     ),
 *   ]
 * }
 * ```
 */
export function collider(id, shape, transform, color) {
  if (shape instanceof $physics.Box) {
    let width = shape.width;
    let height = shape.height;
    let depth = shape.depth;
    return collider_box(id, width, height, depth, transform, color);
  } else if (shape instanceof $physics.Sphere) {
    let radius = shape.radius;
    return collider_sphere(id, radius, transform, color);
  } else if (shape instanceof $physics.Capsule) {
    let half_height = shape.half_height;
    let radius = shape.radius;
    return collider_capsule(id, half_height, radius, transform, color);
  } else {
    let half_height = shape.half_height;
    let radius = shape.radius;
    return collider_cylinder(id, half_height, radius, transform, color);
  }
}

export const color_red = 0xff0000;

export const color_green = 0xff00;

export const color_blue = 0xff;

export const color_yellow = 0xffff00;

export const color_cyan = 0xffff;

export const color_magenta = 0xff00ff;

export const color_white = 0xffffff;

export const color_black = 0x0;

export const color_orange = 0xffa500;

export const color_purple = 0x800080;
