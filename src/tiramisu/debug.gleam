import gleam/list
import tiramisu/scene
import tiramisu/transform
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
  let half_x = t.scale.x /. 2.0
  let half_y = t.scale.y /. 2.0
  let half_z = t.scale.z /. 2.0

  let min =
    vec3.Vec3(
      t.position.x -. half_x,
      t.position.y -. half_y,
      t.position.z -. half_z,
    )

  let max =
    vec3.Vec3(
      t.position.x +. half_x,
      t.position.y +. half_y,
      t.position.z +. half_z,
    )

  bounding_box(id, min, max, color)
}

/// Create multiple lines forming a path through points
pub fn path(
  id_prefix: id,
  points: List(Vec3(Float)),
  color: Int,
) -> List(scene.Node(id)) {
  create_path_lines(id_prefix, points, color, 0, [])
}

fn create_path_lines(
  id: id,
  points: List(Vec3(Float)),
  color: Int,
  index: Int,
  acc: List(scene.Node(id)),
) -> List(scene.Node(id)) {
  case points {
    [] | [_] -> list.reverse(acc)
    [p1, p2, ..rest] -> {
      let line_node = line(id, p1, p2, color)
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

@external(javascript, "./ffi/debug.mjs", "getPerformanceStats")
pub fn get_performance_stats() -> PerformanceStats

/// Enable/disable collision shape visualization for a specific physics world
@external(javascript, "./ffi/debug.mjs", "showColliders")
pub fn show_colliders(enabled: Bool) -> Nil

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
