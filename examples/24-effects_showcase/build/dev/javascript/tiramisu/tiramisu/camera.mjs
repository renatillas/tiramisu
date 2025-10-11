import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import { Ok, Error, CustomType as $CustomType } from "../gleam.mjs";

class Camera extends $CustomType {
  constructor(projection) {
    super();
    this.projection = projection;
  }
}

export class Perspective extends $CustomType {
  constructor(fov, aspect, near, far) {
    super();
    this.fov = fov;
    this.aspect = aspect;
    this.near = near;
    this.far = far;
  }
}

export class Orthographic extends $CustomType {
  constructor(left, right, top, bottom, near, far) {
    super();
    this.left = left;
    this.right = right;
    this.top = top;
    this.bottom = bottom;
    this.near = near;
    this.far = far;
  }
}

/**
 * Field of view must be between 0 and 180 degrees
 */
export class InvalidFieldOfView extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Aspect ratio must be positive
 */
export class InvalidAspectRatio extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Near plane must be positive
 */
export class InvalidNearPlane extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Far plane must be positive
 */
export class InvalidFarPlane extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Near plane must be less than far plane
 */
export class NearFarConflict extends $CustomType {
  constructor(near, far) {
    super();
    this.near = near;
    this.far = far;
  }
}

/**
 * Create a perspective camera (for 3D games).
 *
 * Objects further away appear smaller, like in real life.
 * The aspect ratio is automatically calculated from the viewport or renderer dimensions at render time.
 *
 * ## Parameters
 * - `field_of_view`: Vertical FOV in degrees (typically 60-90)
 * - `near`: Near clipping plane (objects closer are not rendered)
 * - `far`: Far clipping plane (objects further are not rendered)
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(cam) = camera.perspective(
 *   field_of_view: 75.0,
 *   near: 0.1,
 *   far: 1000.0,
 * )
 * ```
 */
export function perspective(fov, near, far) {
  return $bool.guard(
    (fov <= 0.0) || (fov >= 180.0),
    new Error(new InvalidFieldOfView(fov)),
    () => {
      return $bool.guard(
        near <= 0.0,
        new Error(new InvalidNearPlane(near)),
        () => {
          return $bool.guard(
            far <= 0.0,
            new Error(new InvalidFarPlane(far)),
            () => {
              return $bool.guard(
                near >= far,
                new Error(new NearFarConflict(near, far)),
                () => {
                  return new Ok(
                    new Camera(new Perspective(fov, 1.0, near, far)),
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

/**
 * Create an orthographic camera (for 2D games or isometric views).
 *
 * No perspective distortion - objects are the same size regardless of distance.
 *
 * ## Example
 *
 * ```gleam
 * let cam = camera.orthographic(
 *   left: -400.0, right: 400.0,
 *   top: 300.0, bottom: -300.0,
 *   near: 0.1, far: 1000.0,
 * )
 * ```
 */
export function orthographic(left, right, top, bottom, near, far) {
  return new Camera(new Orthographic(left, right, top, bottom, near, far));
}

/**
 * Create a 2D camera centered at origin with world coordinates.
 *
 * Useful for 2D games where (0,0) is the center of the screen.
 *
 * ## Example
 *
 * ```gleam
 * let cam = camera.camera_2d(width: 800, height: 600)
 * scene.Camera(
 *   id: "main_camera",
 *   camera: cam,
 *   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
 *   look_at: option.None,
 *   active: True,
 *   viewport: option.None,
 * )
 * // (0, 0) is screen center, positive Y is up
 * ```
 */
export function camera_2d(width, height) {
  let w = $int.to_float(width);
  let h = $int.to_float(height);
  let half_w = w / 2.0;
  let half_h = h / 2.0;
  return orthographic(0.0 - half_w, half_w, half_h, 0.0 - half_h, 0.1, 1000.0);
}

/**
 * Create a 2D camera with screen-space coordinates (top-left origin).
 *
 * Useful for UI or pixel-perfect 2D games where (0,0) is top-left corner.
 *
 * ## Example
 *
 * ```gleam
 * let cam = camera.camera_2d_screen_space(width: 800, height: 600)
 * scene.Camera(
 *   id: "ui_camera",
 *   camera: cam,
 *   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
 *   look_at: option.None,
 *   active: True,
 *   viewport: option.None,
 * )
 * // (0, 0) is top-left, positive Y is down (like CSS)
 * ```
 */
export function camera_2d_screen_space(width, height) {
  let w = $int.to_float(width);
  let h = $int.to_float(height);
  return orthographic(0.0, w, 0.0, 0.0 - h, 0.1, 1000.0);
}

/**
 * Create a 2D camera with custom bounds.
 *
 * ## Example
 *
 * ```gleam
 * let cam = camera.camera_2d_with_bounds(
 *   left: -100.0, right: 100.0,
 *   top: 75.0, bottom: -75.0,
 * )
 * scene.Camera(
 *   id: "game_camera",
 *   camera: cam,
 *   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
 *   look_at: option.None,
 *   active: True,
 *   viewport: option.None,
 * )
 * ```
 */
export function camera_2d_with_bounds(left, right, top, bottom) {
  return orthographic(left, right, top, bottom, 0.1, 1000.0);
}

/**
 * Internal function to get the camera projection
 *
 * Used by the internal renderer to create Three.js cameras
 * 
 * @ignore
 */
export function get_projection(camera) {
  return camera.projection;
}
