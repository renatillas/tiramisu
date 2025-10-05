import gleam/int
import tiramisu/three/camera

/// Create a 2D orthographic camera centered at origin
/// Width and height are in world units
pub fn create(width: Int, height: Int) -> camera.Camera {
  let w = int.to_float(width)
  let h = int.to_float(height)
  let half_w = w /. 2.0
  let half_h = h /. 2.0

  camera.orthographic(
    left: 0.0 -. half_w,
    right: half_w,
    top: half_h,
    bottom: 0.0 -. half_h,
    near: 0.1,
    far: 1000.0,
  )
  |> camera.set_position(0.0, 0.0, 10.0)
}

/// Create a 2D camera that matches screen pixels (0,0 at top-left)
pub fn create_screen_space(width: Int, height: Int) -> camera.Camera {
  let w = int.to_float(width)
  let h = int.to_float(height)

  camera.orthographic(
    left: 0.0,
    right: w,
    top: 0.0,
    bottom: 0.0 -. h,
    near: 0.1,
    far: 1000.0,
  )
  |> camera.set_position(w /. 2.0, 0.0 -. h /. 2.0, 10.0)
}

/// Create a centered 2D camera with custom bounds
pub fn create_with_bounds(
  left: Float,
  right: Float,
  top: Float,
  bottom: Float,
) -> camera.Camera {
  camera.orthographic(
    left: left,
    right: right,
    top: top,
    bottom: bottom,
    near: 0.1,
    far: 1000.0,
  )
  |> camera.set_position(0.0, 0.0, 10.0)
}
