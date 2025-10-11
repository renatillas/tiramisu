/// Internal camera management
///
/// This module manages the active camera and provides utilities for creating
/// Three.js cameras with proper aspect ratio calculation.
///
/// Camera state is maintained in the renderer's internal state, not as global
/// mutable state.
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}
import tiramisu/camera

/// Opaque type wrapping a Three.js camera object
pub opaque type ThreeCamera {
  ThreeCamera(camera_object: Dynamic)
}

/// Camera manager state
pub type CameraState {
  CameraState(active_camera: Option(ThreeCamera))
}

/// Create initial camera state with no active camera
pub fn init() -> CameraState {
  CameraState(active_camera: option.None)
}

/// Set the active camera
pub fn set_active(_state: CameraState, camera: ThreeCamera) -> CameraState {
  CameraState(active_camera: option.Some(camera))
}

/// Get the active camera
pub fn get_active(state: CameraState) -> Option(ThreeCamera) {
  state.active_camera
}

/// Unwrap the Three.js camera object for FFI use
@internal
pub fn unwrap_camera(camera: ThreeCamera) -> Dynamic {
  camera.camera_object
}

/// Create a Three.js camera from Gleam camera configuration
///
/// This function calculates the proper aspect ratio based on viewport or window
/// dimensions, then creates the appropriate Three.js camera.
///
/// ## Arguments
/// - gleam_camera: Camera from the camera module
/// - viewport: Optional viewport [x, y, width, height]
/// - canvas_width: Canvas width in pixels
/// - canvas_height: Canvas height in pixels
pub fn create_three_camera(
  gleam_camera: camera.Camera,
  viewport: Option(#(Float, Float, Float, Float)),
  canvas_width: Float,
  canvas_height: Float,
) -> ThreeCamera {
  // Calculate aspect ratio
  let aspect = case viewport {
    option.Some(#(_x, _y, w, h)) -> w /. h
    option.None -> canvas_width /. canvas_height
  }

  // Get the projection from the camera
  let projection = camera.get_projection(gleam_camera)

  // Create camera based on projection type
  case projection {
    camera.Perspective(fov: fov, aspect: _aspect, near: near, far: far) -> {
      let cam_obj = create_perspective_camera_ffi(fov, aspect, near, far)
      ThreeCamera(camera_object: cam_obj)
    }

    camera.Orthographic(
      left: left,
      right: right,
      top: top,
      bottom: bottom,
      near: near,
      far: far,
    ) -> {
      let cam_obj =
        create_orthographic_camera_ffi(left, right, top, bottom, near, far)
      ThreeCamera(camera_object: cam_obj)
    }
  }
}

/// Update perspective camera aspect ratio
///
/// This should be called when the window/viewport is resized
pub fn update_aspect(
  cam: ThreeCamera,
  canvas_width: Float,
  canvas_height: Float,
) -> ThreeCamera {
  let aspect = canvas_width /. canvas_height
  update_camera_aspect_ffi(cam.camera_object, aspect)
  update_projection_matrix_ffi(cam.camera_object)
  cam
}

// FFI functions for creating cameras
@external(javascript, "../../threejs.ffi.mjs", "createPerspectiveCamera")
fn create_perspective_camera_ffi(
  fov: Float,
  aspect: Float,
  near: Float,
  far: Float,
) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "createOrthographicCamera")
fn create_orthographic_camera_ffi(
  left: Float,
  right: Float,
  top: Float,
  bottom: Float,
  near: Float,
  far: Float,
) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "setCameraAspect")
fn update_camera_aspect_ffi(camera: Dynamic, aspect: Float) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "updateProjectionMatrix")
fn update_projection_matrix_ffi(camera: Dynamic) -> Nil
