import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType, divideFloat } from "../../gleam.mjs";
import {
  createPerspectiveCamera as create_perspective_camera_ffi,
  createOrthographicCamera as create_orthographic_camera_ffi,
  setCameraAspect as update_camera_aspect_ffi,
  updateProjectionMatrix as update_projection_matrix_ffi,
} from "../../threejs.ffi.mjs";
import * as $camera from "../../tiramisu/camera.mjs";

class ThreeCamera extends $CustomType {
  constructor(camera_object) {
    super();
    this.camera_object = camera_object;
  }
}

export class CameraState extends $CustomType {
  constructor(active_camera) {
    super();
    this.active_camera = active_camera;
  }
}

/**
 * Create initial camera state with no active camera
 */
export function init() {
  return new CameraState(new $option.None());
}

/**
 * Set the active camera
 */
export function set_active(_, camera) {
  return new CameraState(new $option.Some(camera));
}

/**
 * Get the active camera
 */
export function get_active(state) {
  return state.active_camera;
}

/**
 * Unwrap the Three.js camera object for FFI use
 * 
 * @ignore
 */
export function unwrap_camera(camera) {
  return camera.camera_object;
}

/**
 * Create a Three.js camera from Gleam camera configuration
 *
 * This function calculates the proper aspect ratio based on viewport or window
 * dimensions, then creates the appropriate Three.js camera.
 *
 * ## Arguments
 * - gleam_camera: Camera from the camera module
 * - viewport: Optional viewport [x, y, width, height]
 * - canvas_width: Canvas width in pixels
 * - canvas_height: Canvas height in pixels
 */
export function create_three_camera(
  gleam_camera,
  viewport,
  canvas_width,
  canvas_height
) {
  let _block;
  if (viewport instanceof $option.Some) {
    let w = viewport[0][2];
    let h = viewport[0][3];
    _block = divideFloat(w, h);
  } else {
    _block = divideFloat(canvas_width, canvas_height);
  }
  let aspect = _block;
  let projection = $camera.get_projection(gleam_camera);
  if (projection instanceof $camera.Perspective) {
    let fov = projection.fov;
    let near = projection.near;
    let far = projection.far;
    let cam_obj = create_perspective_camera_ffi(fov, aspect, near, far);
    return new ThreeCamera(cam_obj);
  } else {
    let left = projection.left;
    let right = projection.right;
    let top = projection.top;
    let bottom = projection.bottom;
    let near = projection.near;
    let far = projection.far;
    let cam_obj = create_orthographic_camera_ffi(
      left,
      right,
      top,
      bottom,
      near,
      far,
    );
    return new ThreeCamera(cam_obj);
  }
}

/**
 * Update perspective camera aspect ratio
 *
 * This should be called when the window/viewport is resized
 */
export function update_aspect(cam, canvas_width, canvas_height) {
  let aspect = divideFloat(canvas_width, canvas_height);
  update_camera_aspect_ffi(cam.camera_object, aspect);
  update_projection_matrix_ffi(cam.camera_object);
  return cam;
}
