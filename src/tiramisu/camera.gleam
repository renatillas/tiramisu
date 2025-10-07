//// Camera module - define viewpoints and projections for rendering.
////
//// Cameras determine how your 3D scene is viewed. Use perspective cameras for 3D games
//// and orthographic cameras for 2D games or UI elements.
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/camera
////
//// // 3D perspective camera
//// let assert Ok(cam_3d) = camera.perspective(fov: 75.0, aspect: 16.0 /. 9.0, near: 0.1, far: 1000.0)
////
//// // 2D orthographic camera
//// let cam_2d = camera.camera_2d(width: 800, height: 600, distance: 5.0)
//// ```

import gleam/bool
import gleam/int
import vec/vec3
import vec/vec3f

/// Camera configuration (perspective or orthographic projection).
///
/// Use with `scene.Camera` nodes to define viewpoints in your scene.
pub opaque type Camera {
  Camera(
    position: vec3.Vec3(Float),
    look_at_target: vec3.Vec3(Float),
    projection: CameraProjection,
  )
}

@internal
pub type CameraProjection {
  Perspective(fov: Float, aspect: Float, near: Float, far: Float)
  Orthographic(
    left: Float,
    right: Float,
    top: Float,
    bottom: Float,
    near: Float,
    far: Float,
  )
}

/// Validation errors for camera creation.
pub type CameraError {
  /// Field of view must be between 0 and 180 degrees
  InvalidFieldOfView(Float)
  /// Aspect ratio must be positive
  InvalidAspectRatio(Float)
  /// Near plane must be positive
  InvalidNearPlane(Float)
  /// Far plane must be positive
  InvalidFarPlane(Float)
  /// Near plane must be less than far plane
  NearFarConflict(near: Float, far: Float)
}

/// Create a perspective camera (for 3D games).
///
/// Objects further away appear smaller, like in real life.
///
/// ## Parameters
/// - `field_of_view`: Vertical FOV in degrees (typically 60-90)
/// - `aspect`: Width / height ratio (e.g., 16/9 = 1.777)
/// - `near`: Near clipping plane (objects closer are not rendered)
/// - `far`: Far clipping plane (objects further are not rendered)
///
/// ## Example
///
/// ```gleam
/// let assert Ok(cam) = camera.perspective(
///   fov: 75.0,
///   aspect: 800.0 /. 600.0,
///   near: 0.1,
///   far: 1000.0,
/// )
/// ```
pub fn perspective(
  field_of_view fov: Float,
  aspect aspect: Float,
  near near: Float,
  far far: Float,
) -> Result(Camera, CameraError) {
  use <- bool.guard(
    fov <=. 0.0 || fov >=. 180.0,
    Error(InvalidFieldOfView(fov)),
  )
  use <- bool.guard(aspect <=. 0.0, Error(InvalidAspectRatio(aspect)))
  use <- bool.guard(near <=. 0.0, Error(InvalidNearPlane(near)))
  use <- bool.guard(far <=. 0.0, Error(InvalidFarPlane(far)))
  use <- bool.guard(near >=. far, Error(NearFarConflict(near, far)))

  Ok(Camera(
    position: vec3f.zero,
    look_at_target: vec3f.zero,
    projection: Perspective(fov: fov, aspect: aspect, near: near, far: far),
  ))
}

/// Create an orthographic camera (for 2D games or isometric views).
///
/// No perspective distortion - objects are the same size regardless of distance.
///
/// ## Example
///
/// ```gleam
/// let cam = camera.orthographic(
///   left: -400.0, right: 400.0,
///   top: 300.0, bottom: -300.0,
///   near: 0.1, far: 1000.0,
/// )
/// ```
pub fn orthographic(
  left left: Float,
  right right: Float,
  top top: Float,
  bottom bottom: Float,
  near near: Float,
  far far: Float,
) -> Camera {
  Camera(
    position: vec3f.zero,
    look_at_target: vec3f.zero,
    projection: Orthographic(
      left: left,
      right: right,
      top: top,
      bottom: bottom,
      near: near,
      far: far,
    ),
  )
}

/// Create a 2D camera centered at origin with world coordinates.
///
/// Useful for 2D games where (0,0) is the center of the screen.
///
/// ## Example
///
/// ```gleam
/// let cam = camera.camera_2d(width: 800, height: 600, distance: 5.0)
/// // (0, 0) is screen center, positive Y is up
/// ```
pub fn camera_2d(
  width width: Int,
  height height: Int,
  distance distance: Float,
) -> Camera {
  let w = int.to_float(width)
  let h = int.to_float(height)
  let half_w = w /. 2.0
  let half_h = h /. 2.0

  orthographic(
    left: 0.0 -. half_w,
    right: half_w,
    top: half_h,
    bottom: 0.0 -. half_h,
    near: 0.1,
    far: 1000.0,
  )
  |> set_position(vec3.Vec3(0.0, 0.0, distance))
}

/// Create a 2D camera with screen-space coordinates (top-left origin).
///
/// Useful for UI or pixel-perfect 2D games where (0,0) is top-left corner.
///
/// ## Example
///
/// ```gleam
/// let cam = camera.camera_2d_screen_space(width: 800, height: 600, distance: 5.0)
/// // (0, 0) is top-left, positive Y is down (like CSS)
/// ```
pub fn camera_2d_screen_space(
  width: Int,
  height: Int,
  distance: Float,
) -> Camera {
  let w = int.to_float(width)
  let h = int.to_float(height)

  orthographic(
    left: 0.0,
    right: w,
    top: 0.0,
    bottom: 0.0 -. h,
    near: 0.1,
    far: 1000.0,
  )
  |> set_position(vec3.Vec3(w /. 2.0, 0.0 -. h /. 2.0, distance))
}

pub fn camera_2d_with_bounds(
  left: Float,
  right: Float,
  top: Float,
  bottom: Float,
  distance: Float,
) -> Camera {
  orthographic(
    left: left,
    right: right,
    top: top,
    bottom: bottom,
    near: 0.1,
    far: 1000.0,
  )
  |> set_position(vec3.Vec3(0.0, 0.0, distance))
}

pub fn set_position(
  camera: Camera,
  position position: vec3.Vec3(Float),
) -> Camera {
  Camera(..camera, position:)
}

pub fn look(camera: Camera, at look_at_target: vec3.Vec3(Float)) -> Camera {
  Camera(..camera, look_at_target:)
}
