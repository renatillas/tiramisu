import gleam/int
import gleam/list
import gleam_community/maths
import tiramisu/physics
import tiramisu/scene
import tiramisu/transform.{type Transform}
import vec/vec3.{type Vec3}
import vec/vec3f

pub fn bounding_box(
  id: id,
  min: Vec3(Float),
  max: Vec3(Float),
  color: Int,
) -> scene.Node(id) {
  scene.DebugBox(id, min, max, color)
}

pub fn sphere(
  id: id,
  center: Vec3(Float),
  radius: Float,
  color: Int,
) -> scene.Node(id) {
  scene.DebugSphere(id, center, radius, color)
}

pub fn line(
  id: id,
  from: Vec3(Float),
  to: Vec3(Float),
  color: Int,
) -> scene.Node(id) {
  scene.DebugLine(id, from, to, color)
}

pub fn ray(
  id: id,
  origin: Vec3(Float),
  direction: Vec3(Float),
  length: Float,
  color: Int,
) -> scene.Node(id) {
  let end = vec3f.add(origin, vec3f.scale(direction, length))
  scene.DebugLine(id, origin, end, color)
}

pub fn axes(id: id, origin: Vec3(Float), size: Float) -> scene.Node(id) {
  scene.DebugAxes(id, origin, size)
}

pub fn grid(id: id, size: Float, divisions: Int, color: Int) -> scene.Node(id) {
  scene.DebugGrid(id, size, divisions, color)
}

pub fn point(
  id: id,
  position: Vec3(Float),
  size: Float,
  color: Int,
) -> scene.Node(id) {
  scene.DebugPoint(id, position, size, color)
}

pub fn box_from_transform(
  id: id,
  t: transform.Transform,
  color: Int,
) -> scene.Node(id) {
  let scale = transform.scale(t)
  let pos = transform.position(t)

  let half_x = scale.x /. 2.0
  let half_y = scale.y /. 2.0
  let half_z = scale.z /. 2.0

  let min = vec3.Vec3(pos.x -. half_x, pos.y -. half_y, pos.z -. half_z)

  let max = vec3.Vec3(pos.x +. half_x, pos.y +. half_y, pos.z +. half_z)

  bounding_box(id, min, max, color)
}

/// Create multiple lines forming a path through points
pub fn path(
  id: fn(Int) -> id,
  points: List(Vec3(Float)),
  color: Int,
) -> List(scene.Node(id)) {
  create_path_lines(id, points, color, 0, [])
}

fn create_path_lines(
  id: fn(Int) -> id,
  points: List(Vec3(Float)),
  color: Int,
  index: Int,
  acc: List(scene.Node(id)),
) -> List(scene.Node(id)) {
  case points {
    [] | [_] -> list.reverse(acc)
    [p1, p2, ..rest] -> {
      let line_node = line(id(index), p1, p2, color)
      create_path_lines(id, [p2, ..rest], color, index + 1, [line_node, ..acc])
    }
  }
}

pub fn cross(
  id: id,
  position: Vec3(Float),
  size: Float,
  color: Int,
) -> List(scene.Node(id)) {
  let half_size = size /. 2.0
  [
    line(
      id,
      vec3.Vec3(position.x -. half_size, position.y, position.z),
      vec3.Vec3(position.x +. half_size, position.y, position.z),
      color,
    ),
    line(
      id,
      vec3.Vec3(position.x, position.y -. half_size, position.z),
      vec3.Vec3(position.x, position.y +. half_size, position.z),
      color,
    ),
    line(
      id,
      vec3.Vec3(position.x, position.y, position.z -. half_size),
      vec3.Vec3(position.x, position.y, position.z +. half_size),
      color,
    ),
  ]
}

pub type PerformanceStats {
  PerformanceStats(
    fps: Float,
    frame_time: Float,
    draw_calls: Int,
    triangles: Int,
    memory_mb: Float,
  )
}

@external(javascript, "../tiramisu.ffi.mjs", "getPerformanceStats")
pub fn get_performance_stats() -> PerformanceStats

/// Enable/disable collision shape visualization for a specific physics world
@external(javascript, "../tiramisu.ffi.mjs", "showColliders")
fn show_colliders_ffi(
  physics_world: physics.PhysicsWorld(id),
  enabled: Bool,
) -> Nil

pub const color_red = 0xff0000

pub const color_green = 0x00ff00

pub const color_blue = 0x0000ff

pub const color_yellow = 0xffff00

pub const color_cyan = 0x00ffff

pub const color_magenta = 0xff00ff

pub const color_white = 0xffffff

pub const color_black = 0x000000

pub const color_orange = 0xffa500

pub const color_purple = 0x800080

// ============================================================================
// COLLIDER VISUALIZATION
// ============================================================================

/// Enable or disable debug wireframe visualization for all physics colliders in the scene.
///
/// This function uses Rapier's built-in collider visualization which renders wireframes
/// for all physics bodies in the physics world. The wireframes update automatically each frame
/// as objects move and rotate.
///
/// **Note:** This function requires access to the physics world, which is available
/// in the `view` function via the `Context` parameter.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import gleam/option
///
/// pub fn view(model: Model, ctx: tiramisu.Context) {
///   // Enable/disable debug visualization based on model state
///   case ctx.physics_world, model.debug_mode {
///     option.Some(physics_world), True -> {
///       debug.show_collider_wireframes(physics_world, True)
///     }
///     _, _ -> Nil
///   }
///
///   // Return scene as normal
///   [
///     scene.Mesh(
///       id: "cube",
///       geometry: geometry,
///       material: material,
///       transform: transform,
///       physics: option.Some(physics_body),  // Will show wireframe when enabled
///     ),
///     // ... more scene nodes
///   ]
/// }
/// ```
pub fn show_collider_wireframes(
  physics_world: physics.PhysicsWorld(id),
  enabled: Bool,
) -> Nil {
  show_colliders_ffi(physics_world, enabled)
}

/// @deprecated Use `show_collider_wireframes` with the physics world from context instead.
///
/// This function is kept for backwards compatibility but will be removed in a future version.
pub fn with_collider_wireframes(
  nodes: List(scene.Node(id)),
  _color: Int,
) -> List(scene.Node(id)) {
  // This function can no longer work without the physics world parameter
  // Users should migrate to show_collider_wireframes()
  nodes
}

/// Visualize a physics collider shape at a given transform.
///
/// This function converts a physics collider into debug visualization nodes
/// that can be added to your scene for debugging physics shapes.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import tiramisu/physics
/// import tiramisu/transform
/// import vec/vec3
///
/// pub fn view(model: Model) {
///   let body_transform = transform.at(position: vec3.Vec3(0.0, 5.0, 0.0))
///   let collider = physics.Box(width: 2.0, height: 2.0, depth: 2.0)
///
///   [
///     // Your normal scene nodes...
///     // Debug visualization for the collider
///     debug.collider(
///       id: "player-collider-debug",
///       shape: collider,
///       transform: body_transform,
///       color: debug.color_green,
///     ),
///   ]
/// }
/// ```
pub fn collider(
  id: id,
  shape: physics.ColliderShape,
  transform: Transform,
  color: Int,
) -> scene.Node(id) {
  case shape {
    physics.Box(_offset, width, height, depth) ->
      collider_box(id, width, height, depth, transform, color)
    physics.Sphere(_offset, radius) ->
      collider_sphere(id, radius, transform, color)
    physics.Capsule(_offset, half_height, radius) ->
      collider_capsule(id, half_height, radius, transform, color)
    physics.Cylinder(_offset, half_height, radius) ->
      collider_cylinder(id, half_height, radius, transform, color)
  }
}

/// Visualize a box collider
fn collider_box(
  id: id,
  width: Float,
  height: Float,
  depth: Float,
  transform: Transform,
  color: Int,
) -> scene.Node(id) {
  // Box collider uses half-extents, so full dimensions are already provided
  let half_w = width /. 2.0
  let half_h = height /. 2.0
  let half_d = depth /. 2.0

  // Calculate 8 corners in local space
  let neg_half_w = 0.0 -. half_w
  let neg_half_h = 0.0 -. half_h
  let neg_half_d = 0.0 -. half_d
  let corners = [
    vec3.Vec3(neg_half_w, neg_half_h, neg_half_d),
    vec3.Vec3(half_w, neg_half_h, neg_half_d),
    vec3.Vec3(half_w, half_h, neg_half_d),
    vec3.Vec3(neg_half_w, half_h, neg_half_d),
    vec3.Vec3(neg_half_w, neg_half_h, half_d),
    vec3.Vec3(half_w, neg_half_h, half_d),
    vec3.Vec3(half_w, half_h, half_d),
    vec3.Vec3(neg_half_w, half_h, half_d),
  ]

  // Transform corners to world space
  let world_corners = list.map(corners, transform_point(_, transform))

  // Create wireframe lines for the box
  scene.Group(id:, transform: transform.identity, children: [
    // Bottom face
    line(id, list_at(world_corners, 0), list_at(world_corners, 1), color),
    line(id, list_at(world_corners, 1), list_at(world_corners, 2), color),
    line(id, list_at(world_corners, 2), list_at(world_corners, 3), color),
    line(id, list_at(world_corners, 3), list_at(world_corners, 0), color),
    // Top face
    line(id, list_at(world_corners, 4), list_at(world_corners, 5), color),
    line(id, list_at(world_corners, 5), list_at(world_corners, 6), color),
    line(id, list_at(world_corners, 6), list_at(world_corners, 7), color),
    line(id, list_at(world_corners, 7), list_at(world_corners, 4), color),
    // Vertical edges
    line(id, list_at(world_corners, 0), list_at(world_corners, 4), color),
    line(id, list_at(world_corners, 1), list_at(world_corners, 5), color),
    line(id, list_at(world_corners, 2), list_at(world_corners, 6), color),
    line(id, list_at(world_corners, 3), list_at(world_corners, 7), color),
  ])
}

/// Helper to get element at index from list (panics if out of bounds)
fn list_at(list: List(a), index: Int) -> a {
  case list.drop(list, index) {
    [first, ..] -> first
    [] -> panic as "Index out of bounds"
  }
}

/// Visualize a sphere collider
fn collider_sphere(
  id: id,
  radius: Float,
  transform: Transform,
  color: Int,
) -> scene.Node(id) {
  let center = transform.position(transform)
  sphere(id, center, radius, color)
}

/// Visualize a capsule collider (cylinder with hemispherical caps)
fn collider_capsule(
  id: id,
  half_height: Float,
  radius: Float,
  transform: Transform,
  color: Int,
) -> scene.Node(id) {
  let segments = 16

  // Generate wireframe lines for capsule
  let lines = generate_capsule_wireframe(half_height, radius, segments)

  // Transform all line endpoints to world space
  let world_lines =
    list.map(lines, fn(line_pair) {
      let #(start, end) = line_pair
      #(transform_point(start, transform), transform_point(end, transform))
    })

  // Create debug lines
  let children =
    list.index_map(world_lines, fn(line_pair, _idx) {
      let #(start, end) = line_pair
      line(id, start, end, color)
    })

  scene.Group(id:, transform: transform.identity, children:)
}

/// Generate wireframe lines for a capsule in local space
fn generate_capsule_wireframe(
  half_height: Float,
  radius: Float,
  segments: Int,
) -> List(#(Vec3(Float), Vec3(Float))) {
  let pi = 3.14159265359

  // Generate vertical lines along the capsule
  let neg_half_height = 0.0 -. half_height
  let vertical_lines =
    list.range(0, segments - 1)
    |> list.map(fn(i) {
      let angle = 2.0 *. pi *. int.to_float(i) /. int.to_float(segments)
      let cos = maths.cos(angle)
      let x = radius *. cos
      let sin = maths.sin(angle)
      let z = radius *. sin

      let bottom = vec3.Vec3(x, neg_half_height, z)
      let top = vec3.Vec3(x, half_height, z)
      #(bottom, top)
    })

  // Generate horizontal rings (one at bottom, one at top)
  let ring_lines =
    list.range(0, segments - 1)
    |> list.flat_map(fn(i) {
      let angle1 = 2.0 *. pi *. int.to_float(i) /. int.to_float(segments)
      let angle2 = 2.0 *. pi *. int.to_float(i + 1) /. int.to_float(segments)

      let cos1 = maths.cos(angle1)
      let x1 = radius *. cos1
      let sin1 = maths.sin(angle1)
      let z1 = radius *. sin1
      let cos2 = maths.cos(angle2)
      let x2 = radius *. cos2
      let sin2 = maths.sin(angle2)
      let z2 = radius *. sin2

      [
        // Bottom ring
        #(
          vec3.Vec3(x1, neg_half_height, z1),
          vec3.Vec3(x2, neg_half_height, z2),
        ),
        // Top ring
        #(vec3.Vec3(x1, half_height, z1), vec3.Vec3(x2, half_height, z2)),
      ]
    })

  // Generate hemispherical caps (simplified - just a few arcs)
  let cap_arcs =
    list.range(0, 3)
    |> list.flat_map(fn(i) {
      let angle = 2.0 *. pi *. int.to_float(i) /. 4.0
      let cos_a = maths.cos(angle)
      let x = radius *. cos_a
      let sin_a = maths.sin(angle)
      let z = radius *. sin_a

      // Arc from -half_height to top of sphere
      list.range(0, segments / 4)
      |> list.map(fn(j) {
        let t1 = int.to_float(j) /. int.to_float(segments / 4)
        let t2 = int.to_float(j + 1) /. int.to_float(segments / 4)

        let angle1 = pi *. { 0.0 -. 0.5 +. t1 /. 2.0 }
        let angle2 = pi *. { 0.0 -. 0.5 +. t2 /. 2.0 }

        let sin1 = maths.sin(angle1)
        let y1 = neg_half_height +. radius *. sin1
        let cos1 = maths.cos(angle1)
        let r1 = radius *. cos1
        let sin2 = maths.sin(angle2)
        let y2 = neg_half_height +. radius *. sin2
        let cos2 = maths.cos(angle2)
        let r2 = radius *. cos2

        // Bottom cap
        let bottom_start = vec3.Vec3(x *. r1 /. radius, y1, z *. r1 /. radius)
        let bottom_end = vec3.Vec3(x *. r2 /. radius, y2, z *. r2 /. radius)

        #(bottom_start, bottom_end)
      })
    })

  list.flatten([vertical_lines, ring_lines, cap_arcs])
}

/// Visualize a cylinder collider
fn collider_cylinder(
  id: id,
  half_height: Float,
  radius: Float,
  transform: Transform,
  color: Int,
) -> scene.Node(id) {
  let segments = 16

  // Generate wireframe lines for cylinder
  let lines = generate_cylinder_wireframe(half_height, radius, segments)

  // Transform all line endpoints to world space
  let world_lines =
    list.map(lines, fn(line_pair) {
      let #(start, end) = line_pair
      #(transform_point(start, transform), transform_point(end, transform))
    })

  // Create debug lines
  let children =
    list.index_map(world_lines, fn(line_pair, _idx) {
      let #(start, end) = line_pair
      line(id, start, end, color)
    })

  scene.Group(id:, transform: transform.identity, children:)
}

/// Generate wireframe lines for a cylinder in local space
fn generate_cylinder_wireframe(
  half_height: Float,
  radius: Float,
  segments: Int,
) -> List(#(Vec3(Float), Vec3(Float))) {
  let pi = 3.14159265359
  let neg_half_height = 0.0 -. half_height

  // Generate vertical lines along the cylinder
  let vertical_lines =
    list.range(0, segments - 1)
    |> list.map(fn(i) {
      let angle = 2.0 *. pi *. int.to_float(i) /. int.to_float(segments)
      let cos_val = maths.cos(angle)
      let x = radius *. cos_val
      let sin_val = maths.sin(angle)
      let z = radius *. sin_val

      let bottom = vec3.Vec3(x, neg_half_height, z)
      let top = vec3.Vec3(x, half_height, z)
      #(bottom, top)
    })

  // Generate horizontal rings (bottom and top circles)
  let ring_lines =
    list.range(0, segments - 1)
    |> list.flat_map(fn(i) {
      let angle1 = 2.0 *. pi *. int.to_float(i) /. int.to_float(segments)
      let angle2 = 2.0 *. pi *. int.to_float(i + 1) /. int.to_float(segments)

      let cos1 = maths.cos(angle1)
      let x1 = radius *. cos1
      let sin1 = maths.sin(angle1)
      let z1 = radius *. sin1
      let cos2 = maths.cos(angle2)
      let x2 = radius *. cos2
      let sin2 = maths.sin(angle2)
      let z2 = radius *. sin2

      [
        // Bottom ring
        #(
          vec3.Vec3(x1, neg_half_height, z1),
          vec3.Vec3(x2, neg_half_height, z2),
        ),
        // Top ring
        #(vec3.Vec3(x1, half_height, z1), vec3.Vec3(x2, half_height, z2)),
      ]
    })

  list.append(vertical_lines, ring_lines)
}

/// Transform a point by a transform (position + rotation + scale)
fn transform_point(point: Vec3(Float), transform: Transform) -> Vec3(Float) {
  // Get transform properties via accessor functions
  let scale = transform.scale(transform)
  let rotation_euler = transform.rotation(transform)
  let position = transform.position(transform)

  // First apply scale
  let scaled =
    vec3.Vec3(point.x *. scale.x, point.y *. scale.y, point.z *. scale.z)

  // Then apply rotation (simplified - only Y rotation for now)
  // For full rotation support, we'd need to convert Euler to matrix
  let rotated = rotate_y(scaled, rotation_euler.y)

  // Finally apply translation
  vec3f.add(rotated, position)
}

/// Rotate a point around the Y axis
fn rotate_y(point: Vec3(Float), angle: Float) -> Vec3(Float) {
  let cos_a = maths.cos(angle)
  let sin_a = maths.sin(angle)

  vec3.Vec3(
    point.x *. cos_a +. point.z *. sin_a,
    point.y,
    0.0 -. point.x *. sin_a +. point.z *. cos_a,
  )
}
