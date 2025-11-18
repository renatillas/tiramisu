//// <script>
//// const docs = [
////   {
////     header: "Debug shapes",
////     functions: [
////       "bounding_box",
////       "sphere",
////       "line",
////       "ray",
////       "axes",
////       "grid",
////       "point",
////       "box_from_transform",
////       "path",
////       "cross"
////     ]
////   },
////   {
////     header: "Physics debug",
////     functions: [
////       "show_collider_wireframes",
////       "with_collider_wireframes",
////       "collider"
////     ]
////   },
////   {
////     header: "Performance",
////     functions: [
////       "get_performance_stats"
////     ]
////   }
//// ]
////
//// const callback = () => {
////   const list = document.querySelector(".sidebar > ul:last-of-type")
////   const sortedLists = document.createDocumentFragment()
////   const sortedMembers = document.createDocumentFragment()
////
////   for (const section of docs) {
////     sortedLists.append((() => {
////       const node = document.createElement("h3")
////       node.append(section.header)
////       return node
////     })())
////     sortedMembers.append((() => {
////       const node = document.createElement("h2")
////       node.append(section.header)
////       return node
////     })())
////
////     const sortedList = document.createElement("ul")
////     sortedLists.append(sortedList)
////
////
////     for (const funcName of section.functions) {
////       const href = `#${funcName}`
////       const member = document.querySelector(
////         `.member:has(h2 > a[href="${href}"])`
////       )
////       const sidebar = list.querySelector(`li:has(a[href="${href}"])`)
////       sortedList.append(sidebar)
////       sortedMembers.append(member)
////     }
////   }
////
////   document.querySelector(".sidebar").insertBefore(sortedLists, list)
////   document
////     .querySelector(".module-members:has(#module-values)")
////     .insertBefore(
////       sortedMembers,
////       document.querySelector("#module-values").nextSibling
////     )
//// }
////
//// document.readyState !== "loading"
////   ? callback()
////   : document.addEventListener(
////     "DOMContentLoaded",
////     callback,
////     { once: true }
////   )
//// </script>
//// Debug visualization utilities for game development.
////
//// This module provides tools for visualizing game elements during development:
//// - **Debug shapes**: Boxes, spheres, lines, rays, grids, axes, points
//// - **Physics visualization**: Collider wireframes and shapes
//// - **Performance monitoring**: FPS, frame time, draw calls, memory usage
//// - **Color constants**: Common debug colors for quick visualization
////
//// ## Usage
////
//// Debug visualizations are added to your scene just like regular nodes:
////
//// ```gleam
//// import tiramisu/debug
//// import vec/vec3
////
//// pub fn view(model: Model) {
////   [
////     // Your game objects
////     scene.Mesh(...),
////
////     // Debug visualizations
////     debug.axes("axes", vec3.zero(), 5.0),
////     debug.grid("grid", 20.0, 20, debug.color_white),
////     debug.sphere("target", target_position, 0.5, debug.color_red),
////   ]
//// }
//// ```
////
//// ## Performance Stats
////
//// Monitor your game's performance in the update loop:
////
//// ```gleam
//// pub fn update(model: Model, msg: Msg, ctx: Context) {
////   let stats = debug.get_performance_stats()
////   io.println("FPS: " <> float.to_string(stats.fps))
////   // ...
//// }
//// ```
////
//// ## Physics Debugging
////
//// Visualize physics colliders in your view function:
////
//// ```gleam
//// pub fn view(model: Model, ctx: Context) {
////   // Enable collider wireframes
////   case ctx.physics_world, model.debug_mode {
////     option.Some(physics_world), True ->
////       debug.show_collider_wireframes(physics_world, True)
////     _, _ -> Nil
////   }
////   // ... return scene nodes
//// }
//// ```

import gleam/int
import gleam/list
import gleam_community/maths
import tiramisu/physics
import tiramisu/scene
import tiramisu/transform.{type Transform}
import vec/vec3.{type Vec3}
import vec/vec3f

// ============================================================================
// BASIC DEBUG SHAPES
// ============================================================================

/// Create a debug wireframe box defined by minimum and maximum corners.
///
/// Useful for visualizing bounding boxes, collision volumes, or spatial regions.
///
/// ## Parameters
/// - `id`: Unique identifier for this debug node
/// - `min`: Minimum corner position (x, y, z)
/// - `max`: Maximum corner position (x, y, z)
/// - `color`: Hex color code (e.g., `0xff0000` for red)
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import vec/vec3
///
/// // Visualize a bounding box from (-1, -1, -1) to (1, 1, 1)
/// debug.bounding_box(
///   "bounds",
///   vec3.Vec3(-1.0, -1.0, -1.0),
///   vec3.Vec3(1.0, 1.0, 1.0),
///   debug.color_cyan,
/// )
/// ```
pub fn bounding_box(
  id: id,
  min: Vec3(Float),
  max: Vec3(Float),
  color: Int,
) -> scene.Node(id) {
  scene.debug_box(id, min, max, color)
}

/// Create a debug wireframe sphere.
///
/// Useful for visualizing positions, ranges, trigger zones, or spherical collision shapes.
///
/// ## Parameters
/// - `id`: Unique identifier for this debug node
/// - `center`: Center position of the sphere
/// - `radius`: Radius of the sphere
/// - `color`: Hex color code (e.g., `0x00ff00` for green)
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import vec/vec3
///
/// // Visualize a pickup range around a player
/// debug.sphere(
///   "pickup-range",
///   player_position,
///   2.5,
///   debug.color_yellow,
/// )
/// ```
pub fn sphere(
  id: id,
  center: Vec3(Float),
  radius: Float,
  color: Int,
) -> scene.Node(id) {
  scene.debug_sphere(id, center, radius, color)
}

/// Create a debug line between two points.
///
/// Useful for visualizing connections, trajectories, or directions.
///
/// ## Parameters
/// - `id`: Unique identifier for this debug node
/// - `from`: Starting point of the line
/// - `to`: Ending point of the line
/// - `color`: Hex color code (e.g., `0xff00ff` for magenta)
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import vec/vec3
///
/// // Draw a line from origin to target
/// debug.line(
///   "path-line",
///   vec3.Vec3(0.0, 0.0, 0.0),
///   target_position,
///   debug.color_green,
/// )
/// ```
pub fn line(
  id: id,
  from: Vec3(Float),
  to: Vec3(Float),
  color: Int,
) -> scene.Node(id) {
  scene.debug_line(id, from, to, color)
}

/// Create a debug ray from an origin point in a direction.
///
/// Useful for visualizing raycasts, shooting directions, or look-at vectors.
///
/// ## Parameters
/// - `id`: Unique identifier for this debug node
/// - `origin`: Starting point of the ray
/// - `direction`: Direction vector (should be normalized for predictable length)
/// - `length`: Length of the ray
/// - `color`: Hex color code (e.g., `0xff0000` for red)
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import vec/vec3
///
/// // Visualize a forward-facing ray
/// debug.ray(
///   "look-ray",
///   camera_position,
///   camera_forward,
///   10.0,
///   debug.color_blue,
/// )
/// ```
pub fn ray(
  id: id,
  origin: Vec3(Float),
  direction: Vec3(Float),
  length: Float,
  color: Int,
) -> scene.Node(id) {
  let end = vec3f.add(origin, vec3f.scale(direction, length))
  scene.debug_line(id, origin, end, color)
}

/// Create coordinate axes visualization at a given position.
///
/// Displays X (red), Y (green), and Z (blue) axes to help visualize object orientation
/// and world coordinates. This is particularly useful for understanding transformations
/// and rotations.
///
/// ## Parameters
/// - `id`: Unique identifier for this debug node
/// - `origin`: Position where the axes should be centered
/// - `size`: Length of each axis line
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import vec/vec3
///
/// // Show world axes at origin
/// debug.axes("world-axes", vec3.zero(), 5.0)
///
/// // Show object-local axes
/// debug.axes("player-axes", player_position, 2.0)
/// ```
pub fn axes(id: id, origin: Vec3(Float), size: Float) -> scene.Node(id) {
  scene.debug_axes(id, origin, size)
}

/// Create a grid on the ground plane (XZ plane).
///
/// Useful for visualizing the ground, spatial reference, or movement areas.
///
/// ## Parameters
/// - `id`: Unique identifier for this debug node
/// - `size`: Total size of the grid (e.g., 20.0 creates a 20×20 grid)
/// - `divisions`: Number of grid divisions (higher = more lines)
/// - `color`: Hex color code for all grid lines
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
///
/// // Create a 20×20 grid with 20 divisions
/// debug.grid("ground-grid", 20.0, 20, debug.color_white)
/// ```
pub fn grid(id: id, size: Float, divisions: Int, color: Int) -> scene.Node(id) {
  scene.debug_grid(id, size, divisions, color)
}

/// Create a debug point marker at a position.
///
/// Useful for marking specific coordinates, waypoints, or spawn points.
///
/// ## Parameters
/// - `id`: Unique identifier for this debug node
/// - `position`: World position of the point
/// - `size`: Size of the point marker
/// - `color`: Hex color code (e.g., `0xffa500` for orange)
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
///
/// // Mark spawn points
/// debug.point("spawn-1", spawn_pos_1, 0.3, debug.color_green)
/// debug.point("spawn-2", spawn_pos_2, 0.3, debug.color_green)
/// ```
pub fn point(
  id: id,
  position: Vec3(Float),
  size: Float,
  color: Int,
) -> scene.Node(id) {
  scene.debug_point(id, position, size, color)
}

/// Create a debug bounding box from a transform.
///
/// Convenient helper that extracts position and scale from a transform to create
/// a bounding box visualization. Useful for visualizing object bounds in world space.
///
/// ## Parameters
/// - `id`: Unique identifier for this debug node
/// - `t`: Transform containing position and scale information
/// - `color`: Hex color code for the box wireframe
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import tiramisu/transform
/// import vec/vec3
///
/// let cube_transform = transform.new()
///   |> transform.with_position(vec3.Vec3(0.0, 2.0, 0.0))
///   |> transform.with_scale(vec3.Vec3(2.0, 2.0, 2.0))
///
/// // Visualize the cube's bounds
/// debug.box_from_transform("cube-bounds", cube_transform, debug.color_orange)
/// ```
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

/// Create multiple lines forming a path through points.
///
/// Useful for visualizing paths, trajectories, AI navigation routes, or animation curves.
/// Connects consecutive points with lines.
///
/// ## Parameters
/// - `id`: Function that generates unique IDs for each line segment (receives index)
/// - `points`: List of positions to connect
/// - `color`: Hex color code for all path lines
///
/// ## Returns
/// A list of line nodes, one for each segment between consecutive points.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import vec/vec3
///
/// let waypoints = [
///   vec3.Vec3(0.0, 0.0, 0.0),
///   vec3.Vec3(5.0, 2.0, 0.0),
///   vec3.Vec3(10.0, 0.0, 5.0),
///   vec3.Vec3(15.0, 3.0, 10.0),
/// ]
///
/// // Visualize a patrol path
/// debug.path(
///   fn(i) { "patrol-segment-" <> int.to_string(i) },
///   waypoints,
///   debug.color_yellow,
/// )
/// ```
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

/// Create a 3D cross (three perpendicular lines) at a position.
///
/// Useful for marking points in 3D space with a distinctive visual marker that's
/// visible from all angles, unlike a simple point.
///
/// ## Parameters
/// - `id`: Unique identifier for all three lines (they share the same ID)
/// - `position`: Center position of the cross
/// - `size`: Total length of each line (extends size/2 in each direction)
/// - `color`: Hex color code for all three lines
///
/// ## Returns
/// A list of three line nodes (X, Y, and Z aligned).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import vec/vec3
///
/// // Mark a target position with a visible cross
/// debug.cross(
///   "target-marker",
///   target_position,
///   1.0,
///   debug.color_red,
/// )
/// ```
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

// ============================================================================
// PERFORMANCE MONITORING
// ============================================================================

/// Performance statistics collected from the game engine.
///
/// Contains real-time performance metrics useful for optimization and debugging.
///
/// ## Fields
/// - `fps`: Current frames per second (smoothed average)
/// - `frame_time`: Time in milliseconds to render the last frame
/// - `draw_calls`: Number of draw calls in the last frame (lower is better)
/// - `triangles`: Total number of triangles rendered in the last frame
/// - `memory_mb`: Estimated GPU memory usage in megabytes
pub type PerformanceStats {
  PerformanceStats(
    fps: Float,
    frame_time: Float,
    draw_calls: Int,
    triangles: Int,
    memory_mb: Float,
  )
}

/// Get current performance statistics from the renderer.
///
/// Returns real-time performance metrics that can be used for optimization,
/// profiling, or displaying debug information to the user.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import gleam/io
/// import gleam/float
///
/// pub fn update(model: Model, msg: Msg, ctx: Context) {
///   let stats = debug.get_performance_stats()
///
///   // Log performance issues
///   case stats.fps <. 30.0 {
///     True -> io.println("Warning: Low FPS!")
///     False -> Nil
///   }
///
///   // Track draw calls for optimization
///   case stats.draw_calls > 100 {
///     True -> io.println("Too many draw calls: " <> int.to_string(stats.draw_calls))
///     False -> Nil
///   }
///
///   // ... rest of update logic
/// }
/// ```
@external(javascript, "../tiramisu.ffi.mjs", "getPerformanceStats")
pub fn get_performance_stats() -> PerformanceStats

/// Enable/disable collision shape visualization for a specific physics world
@external(javascript, "../tiramisu.ffi.mjs", "showColliders")
fn show_colliders_ffi(
  physics_world: physics.PhysicsWorld(id),
  enabled: Bool,
) -> Nil

// ============================================================================
// COLOR CONSTANTS
// ============================================================================

/// Red color (0xff0000) - Commonly used for errors, warnings, or X-axis visualization.
pub const color_red = 0xff0000

/// Green color (0x00ff00) - Commonly used for success, active states, or Y-axis visualization.
pub const color_green = 0x00ff00

/// Blue color (0x0000ff) - Commonly used for information, water, or Z-axis visualization.
pub const color_blue = 0x0000ff

/// Yellow color (0xffff00) - Commonly used for caution, highlights, or important markers.
pub const color_yellow = 0xffff00

/// Cyan color (0x00ffff) - Commonly used for secondary information or water/ice elements.
pub const color_cyan = 0x00ffff

/// Magenta color (0xff00ff) - Commonly used for special markers or UI highlights.
pub const color_magenta = 0xff00ff

/// White color (0xffffff) - Commonly used for grids, general outlines, or default markers.
pub const color_white = 0xffffff

/// Black color (0x000000) - Rarely used for debug visualization (not visible on dark backgrounds).
pub const color_black = 0x000000

/// Orange color (0xffa500) - Commonly used for alerts, spawn points, or intermediate states.
pub const color_orange = 0xffa500

/// Purple color (0x800080) - Commonly used for special objects, power-ups, or tertiary markers.
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
  scene.empty(id:, transform: transform.identity, children: [
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

  scene.empty(id:, transform: transform.identity, children:)
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

  scene.empty(id:, transform: transform.identity, children:)
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
