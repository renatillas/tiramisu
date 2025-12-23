//// <script>
//// const docs = [
////   {
////     header: "3D cameras",
////     functions: [
////       "perspective",
////       "orthographic"
////     ]
////   },
////   {
////     header: "2D cameras",
////     functions: [
////       "camera_2d",
////       "camera_2d_screen_space",
////       "camera_2d_with_bounds"
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
//// Camera module - define viewpoints and projections for rendering.
////
//// Cameras determine how your 3D scene is viewed. Use perspective cameras for 3D games
//// and orthographic cameras for 2D games or UI elements.
////
//// ## Quick Example
////
//// ```gleam
//// import gleam/option
//// import tiramisu/camera
//// import tiramisu/scene
//// import tiramisu/transform
//// import vec/vec3
////
//// // 3D perspective camera (aspect ratio calculated automatically)
//// let assert Ok(cam_3d) = camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
////
//// scene.Camera(
////   id: "main",
////   camera: cam_3d,
////   transform: transform.at(position: vec3.Vec3(0.0, 5.0, 10.0)),
////   look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
////   active: True,
////   viewport: option.None,
//// )
////
//// // 2D orthographic camera
//// let cam_2d = camera.camera_2d(width: 800, height: 600)
////
//// scene.Camera(
////   id: "ui",
////   camera: cam_2d,
////   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
////   look_at: option.None,
////   active: False,
////   viewport: option.None,
//// )
//// ```

import gleam/bool
import gleam/int

/// Camera configuration (perspective or orthographic projection).
///
/// Use with `scene.Camera` nodes to define viewpoints in your scene.
/// Position and orientation are set via the scene.Camera node's transform and look_at fields.
pub opaque type Camera {
  Camera(projection: CameraProjection)
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
/// The aspect ratio is automatically calculated from the viewport or renderer dimensions at render time.
///
/// ## Parameters
/// - `field_of_view`: Vertical FOV in degrees (typically 60-90)
/// - `near`: Near clipping plane (objects closer are not rendered)
/// - `far`: Far clipping plane (objects further are not rendered)
///
/// ## Example
///
/// ```gleam
/// let assert Ok(cam) = camera.perspective(
///   field_of_view: 75.0,
///   near: 0.1,
///   far: 1000.0,
/// )
/// ```
pub fn perspective(
  field_of_view fov: Float,
  near near: Float,
  far far: Float,
) -> Result(Camera, CameraError) {
  use <- bool.guard(
    fov <=. 0.0 || fov >=. 180.0,
    Error(InvalidFieldOfView(fov)),
  )
  use <- bool.guard(near <=. 0.0, Error(InvalidNearPlane(near)))
  use <- bool.guard(far <=. 0.0, Error(InvalidFarPlane(far)))
  use <- bool.guard(near >=. far, Error(NearFarConflict(near, far)))

  // Aspect ratio will be calculated at render time based on viewport/renderer dimensions
  // Use 1.0 as placeholder - the FFI layer will calculate the correct aspect
  Ok(
    Camera(projection: Perspective(fov: fov, aspect: 1.0, near: near, far: far)),
  )
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
  Camera(projection: Orthographic(
    left: left,
    right: right,
    top: top,
    bottom: bottom,
    near: near,
    far: far,
  ))
}

/// Create a 2D camera centered at origin with world coordinates.
///
/// Useful for 2D games where (0,0) is the center of the screen.
///
/// ## Example
///
/// ```gleam
/// let cam = camera.camera_2d(width: 800, height: 600)
/// scene.Camera(
///   id: "main_camera",
///   camera: cam,
///   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
///   look_at: option.None,
///   active: True,
///   viewport: option.None,
/// )
/// // (0, 0) is screen center, positive Y is up
/// ```
pub fn camera_2d(width width: Int, height height: Int) -> Camera {
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
}

/// Create a 2D camera with screen-space coordinates (top-left origin).
///
/// Useful for UI or pixel-perfect 2D games where (0,0) is top-left corner.
///
/// ## Example
///
/// ```gleam
/// let cam = camera.camera_2d_screen_space(width: 800, height: 600)
/// scene.Camera(
///   id: "ui_camera",
///   camera: cam,
///   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
///   look_at: option.None,
///   active: True,
///   viewport: option.None,
/// )
/// // (0, 0) is top-left, positive Y is down (like CSS)
/// ```
pub fn camera_2d_screen_space(width: Int, height: Int) -> Camera {
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
}

/// Create a 2D camera with custom bounds.
///
/// ## Example
///
/// ```gleam
/// let cam = camera.camera_2d_with_bounds(
///   left: -100.0, right: 100.0,
///   top: 75.0, bottom: -75.0,
/// )
/// scene.Camera(
///   id: "game_camera",
///   camera: cam,
///   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
///   look_at: option.None,
///   active: True,
///   viewport: option.None,
/// )
/// ```
pub fn camera_2d_with_bounds(
  left: Float,
  right: Float,
  top: Float,
  bottom: Float,
) -> Camera {
  orthographic(
    left: left,
    right: right,
    top: top,
    bottom: bottom,
    near: 0.1,
    far: 1000.0,
  )
}

/// Viewport configuration for split-screen or picture-in-picture rendering.
///
/// Coordinates are in pixels from the top-left of the canvas.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/camera
/// import tiramisu/scene
///
/// // Create a viewport in the top-right corner
/// let minimap_viewport = camera.ViewPort(
///   x: 650,
///   y: 10,
///   width: 150,
///   height: 100,
/// )
///
/// scene.camera(
///   id: "minimap",
///   camera: minimap_cam,
///   transform: transform.identity,
///   look_at: option.None,
///   active: True,
///   viewport: option.Some(minimap_viewport),
///   postprocessing: option.None,
/// )
/// ```
pub type ViewPort {
  ViewPort(
    /// X position from left edge in pixels
    x: Int,
    /// Y position from top edge in pixels
    y: Int,
    /// Width in pixels
    width: Int,
    /// Height in pixels
    height: Int,
  )
}

/// Internal function to get the camera projection
///
/// Used by the internal renderer to create Three.js cameras
@internal
pub fn get_projection(camera: Camera) -> CameraProjection {
  camera.projection
}
