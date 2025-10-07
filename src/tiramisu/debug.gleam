import gleam/int
import gleam/list
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3.{type Vec3}

pub fn bounding_box(
  id: String,
  min: Vec3,
  max: Vec3,
  color: Int,
) -> scene.SceneNode {
  scene.DebugBox(id, min, max, color)
}

pub fn sphere(
  id: String,
  center: Vec3,
  radius: Float,
  color: Int,
) -> scene.SceneNode {
  scene.DebugSphere(id, center, radius, color)
}

pub fn line(id: String, from: Vec3, to: Vec3, color: Int) -> scene.SceneNode {
  scene.DebugLine(id, from, to, color)
}

pub fn ray(
  id: String,
  origin: Vec3,
  direction: Vec3,
  length: Float,
  color: Int,
) -> scene.SceneNode {
  let end = vec3.add(origin, vec3.scale(direction, length))
  scene.DebugLine(id, origin, end, color)
}

pub fn axes(id: String, origin: Vec3, size: Float) -> scene.SceneNode {
  scene.DebugAxes(id, origin, size)
}

pub fn grid(
  id: String,
  size: Float,
  divisions: Int,
  color: Int,
) -> scene.SceneNode {
  scene.DebugGrid(id, size, divisions, color)
}

pub fn point(
  id: String,
  position: Vec3,
  size: Float,
  color: Int,
) -> scene.SceneNode {
  scene.DebugPoint(id, position, size, color)
}

pub fn box_from_transform(
  id: String,
  t: transform.Transform,
  color: Int,
) -> scene.SceneNode {
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
  id_prefix: String,
  points: List(Vec3),
  color: Int,
) -> List(scene.SceneNode) {
  create_path_lines(id_prefix, points, color, 0, [])
}

fn create_path_lines(
  prefix: String,
  points: List(Vec3),
  color: Int,
  index: Int,
  acc: List(scene.SceneNode),
) -> List(scene.SceneNode) {
  case points {
    [] | [_] -> list.reverse(acc)
    [p1, p2, ..rest] -> {
      let id = prefix <> "_" <> int.to_string(index)
      let line_node = line(id, p1, p2, color)
      create_path_lines(prefix, [p2, ..rest], color, index + 1, [
        line_node,
        ..acc
      ])
    }
  }
}

pub fn cross(
  id: String,
  position: Vec3,
  size: Float,
  color: Int,
) -> List(scene.SceneNode) {
  let half_size = size /. 2.0
  [
    line(
      id <> "_x",
      vec3.Vec3(position.x -. half_size, position.y, position.z),
      vec3.Vec3(position.x +. half_size, position.y, position.z),
      color,
    ),
    line(
      id <> "_y",
      vec3.Vec3(position.x, position.y -. half_size, position.z),
      vec3.Vec3(position.x, position.y +. half_size, position.z),
      color,
    ),
    line(
      id <> "_z",
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
