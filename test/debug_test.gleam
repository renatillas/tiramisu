import tiramisu/debug
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub fn bounding_box_creates_debug_box_test() {
  let min = vec3.Vec3(0.0, 0.0, 0.0)
  let max = vec3.Vec3(1.0, 1.0, 1.0)

  let node = debug.bounding_box("test_box", min, max, debug.red)

  let assert scene.DebugBox(id, node_min, node_max, color) = node
  let assert "test_box" = id
  let assert vec3.Vec3(0.0, 0.0, 0.0) = node_min
  let assert vec3.Vec3(1.0, 1.0, 1.0) = node_max
  let assert 0xff0000 = color
}

pub fn sphere_creates_debug_sphere_test() {
  let center = vec3.Vec3(5.0, 5.0, 5.0)
  let radius = 2.5

  let node = debug.sphere("test_sphere", center, radius, debug.green)

  let assert scene.DebugSphere(id, node_center, node_radius, color) = node
  let assert "test_sphere" = id
  let assert vec3.Vec3(5.0, 5.0, 5.0) = node_center
  let assert 2.5 = node_radius
  let assert 0x00ff00 = color
}

pub fn line_creates_debug_line_test() {
  let from = vec3.Vec3(0.0, 0.0, 0.0)
  let to = vec3.Vec3(10.0, 10.0, 10.0)

  let node = debug.line("test_line", from, to, debug.blue)

  let assert scene.DebugLine(id, node_from, node_to, color) = node
  let assert "test_line" = id
  let assert vec3.Vec3(0.0, 0.0, 0.0) = node_from
  let assert vec3.Vec3(10.0, 10.0, 10.0) = node_to
  let assert 0x0000ff = color
}

pub fn axes_creates_debug_axes_test() {
  let origin = vec3.Vec3(1.0, 2.0, 3.0)
  let size = 5.0

  let node = debug.axes("test_axes", origin, size)

  let assert scene.DebugAxes(id, node_origin, node_size) = node
  let assert "test_axes" = id
  let assert vec3.Vec3(1.0, 2.0, 3.0) = node_origin
  let assert 5.0 = node_size
}

pub fn grid_creates_debug_grid_test() {
  let size = 20.0
  let divisions = 10

  let node = debug.grid("test_grid", size, divisions, debug.white)

  let assert scene.DebugGrid(id, node_size, node_divisions, color) = node
  let assert "test_grid" = id
  let assert 20.0 = node_size
  let assert 10 = node_divisions
  let assert 0xffffff = color
}

pub fn point_creates_debug_point_test() {
  let position = vec3.Vec3(3.0, 4.0, 5.0)
  let size = 0.5

  let node = debug.point("test_point", position, size, debug.yellow)

  let assert scene.DebugPoint(id, node_position, node_size, color) = node
  let assert "test_point" = id
  let assert vec3.Vec3(3.0, 4.0, 5.0) = node_position
  let assert 0.5 = node_size
  let assert 0xffff00 = color
}

pub fn box_from_transform_test() {
  let t =
    transform.Transform(
      position: vec3.Vec3(5.0, 5.0, 5.0),
      rotation: vec3.zero(),
      scale: vec3.Vec3(2.0, 4.0, 6.0),
    )

  let node = debug.box_from_transform("test_transform_box", t, debug.cyan)

  let assert scene.DebugBox(_, min, max, _) = node

  // Min should be position - scale/2
  let assert vec3.Vec3(4.0, 3.0, 2.0) = min
  // Max should be position + scale/2
  let assert vec3.Vec3(6.0, 7.0, 8.0) = max
}

pub fn path_creates_multiple_lines_test() {
  let points = [
    vec3.Vec3(0.0, 0.0, 0.0),
    vec3.Vec3(1.0, 1.0, 1.0),
    vec3.Vec3(2.0, 2.0, 2.0),
    vec3.Vec3(3.0, 3.0, 3.0),
  ]

  let nodes = debug.path("path", points, debug.magenta)

  // Should create 3 lines (N-1 lines for N points)
  let assert [_, _, _] = nodes
}

pub fn path_with_two_points_creates_one_line_test() {
  let points = [vec3.Vec3(0.0, 0.0, 0.0), vec3.Vec3(1.0, 1.0, 1.0)]

  let nodes = debug.path("path", points, debug.orange)

  let assert [line] = nodes
  let assert scene.DebugLine(_, from, to, _) = line
  let assert vec3.Vec3(0.0, 0.0, 0.0) = from
  let assert vec3.Vec3(1.0, 1.0, 1.0) = to
}

pub fn path_with_one_point_creates_no_lines_test() {
  let points = [vec3.Vec3(0.0, 0.0, 0.0)]

  let nodes = debug.path("path", points, debug.purple)

  let assert [] = nodes
}

pub fn cross_creates_three_lines_test() {
  let position = vec3.Vec3(1.0, 2.0, 3.0)
  let size = 2.0

  let nodes = debug.cross("cross", position, size, debug.white)

  // Should create 3 lines (X, Y, Z axes)
  let assert [x_line, y_line, z_line] = nodes

  // Check X line
  let assert scene.DebugLine(_, x_from, x_to, _) = x_line
  let assert vec3.Vec3(0.0, 2.0, 3.0) = x_from
  let assert vec3.Vec3(2.0, 2.0, 3.0) = x_to

  // Check Y line
  let assert scene.DebugLine(_, y_from, y_to, _) = y_line
  let assert vec3.Vec3(1.0, 1.0, 3.0) = y_from
  let assert vec3.Vec3(1.0, 3.0, 3.0) = y_to

  // Check Z line
  let assert scene.DebugLine(_, z_from, z_to, _) = z_line
  let assert vec3.Vec3(1.0, 2.0, 2.0) = z_from
  let assert vec3.Vec3(1.0, 2.0, 4.0) = z_to
}

pub fn ray_creates_line_from_origin_and_direction_test() {
  let origin = vec3.Vec3(0.0, 0.0, 0.0)
  let direction = vec3.Vec3(1.0, 0.0, 0.0)
  let length = 5.0

  let node = debug.ray("test_ray", origin, direction, length, debug.red)

  let assert scene.DebugLine(_, from, to, _) = node
  let assert vec3.Vec3(0.0, 0.0, 0.0) = from
  let assert vec3.Vec3(5.0, 0.0, 0.0) = to
}

pub fn color_constants_test() {
  // Test that color constants are correct hex values
  let assert 0xff0000 = debug.red
  let assert 0x00ff00 = debug.green
  let assert 0x0000ff = debug.blue
  let assert 0xffff00 = debug.yellow
  let assert 0x00ffff = debug.cyan
  let assert 0xff00ff = debug.magenta
  let assert 0xffffff = debug.white
  let assert 0x000000 = debug.black
  let assert 0xffa500 = debug.orange
  let assert 0x800080 = debug.purple
}
