import * as $maths from "../../gleam_community_maths/gleam_community/maths.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $vec3 from "../../vec/vec/vec3.mjs";
import * as $vec3f from "../../vec/vec/vec3f.mjs";
import { Ok, CustomType as $CustomType } from "../gleam.mjs";

export class Transform extends $CustomType {
  constructor(position, rotation, scale) {
    super();
    this.position = position;
    this.rotation = rotation;
    this.scale = scale;
  }
}

/**
 * Create a transform at a specific position with default rotation and scale.
 *
 * ## Example
 *
 * ```gleam
 * let t = transform.at(position: vec3.Vec3(5.0, 0.0, -3.0))
 * // Object positioned at (5, 0, -3)
 * ```
 */
export function at(position) {
  return new Transform(position, $vec3f.zero, $vec3f.one);
}

/**
 * Update the position of a transform.
 *
 * ## Example
 *
 * ```gleam
 * let moved = transform.identity
 *   |> transform.set_position(vec3.Vec3(1.0, 2.0, 3.0))
 * ```
 */
export function with_position(transform, position) {
  return new Transform(position, transform.rotation, transform.scale);
}

/**
 * Update the rotation of a transform (Euler angles in radians).
 *
 * ## Example
 *
 * ```gleam
 * let rotated = transform.identity
 *   |> transform.set_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // 90° turn around Y axis
 * ```
 */
export function with_rotation(transform, rotation) {
  return new Transform(transform.position, rotation, transform.scale);
}

/**
 * Update the scale of a transform.
 *
 * ## Example
 *
 * ```gleam
 * let scaled = transform.identity
 *   |> transform.set_scale(vec3.Vec3(2.0, 1.0, 2.0))  // Wide and deep, normal height
 * ```
 */
export function with_scale(transform, scale) {
  return new Transform(transform.position, transform.rotation, scale);
}

function lerp_vec(a, b, t) {
  return new $vec3.Vec3(
    a.x + ((b.x - a.x) * t),
    a.y + ((b.y - a.y) * t),
    a.z + ((b.z - a.z) * t),
  );
}

/**
 * Linearly interpolate between two transforms.
 *
 * Useful for smooth animations and transitions. Parameter `t` should be between 0.0 and 1.0:
 * - `t = 0.0` returns `from`
 * - `t = 1.0` returns `to`
 * - `t = 0.5` returns halfway between
 *
 * ## Example
 *
 * ```gleam
 * let start = transform.at(vec3.Vec3(0.0, 0.0, 0.0))
 * let end = transform.at(vec3.Vec3(10.0, 0.0, 0.0))
 * let halfway = transform.lerp(start, to: end, with: 0.5)
 * // position: (5.0, 0.0, 0.0)
 * ```
 */
export function lerp(from, to, t) {
  return new Transform(
    lerp_vec(from.position, to.position, t),
    lerp_vec(from.rotation, to.rotation, t),
    lerp_vec(from.scale, to.scale, t),
  );
}

/**
 * Compose two transforms (apply second transform after first).
 *
 * Useful for relative transformations. Note: This is a simplified composition
 * that adds positions/rotations and multiplies scales. For proper hierarchical
 * transforms, use scene `Group` nodes instead.
 *
 * ## Example
 *
 * ```gleam
 * let base = transform.at(vec3.Vec3(5.0, 0.0, 0.0))
 * let offset = transform.at(vec3.Vec3(0.0, 2.0, 0.0))
 * let combined = transform.compose(base, offset)
 * // position: (5.0, 2.0, 0.0)
 * ```
 */
export function compose(first, second) {
  return new Transform(
    $vec3f.add(first.position, second.position),
    $vec3f.add(first.rotation, second.rotation),
    new $vec3.Vec3(
      first.scale.x * second.scale.x,
      first.scale.y * second.scale.y,
      first.scale.z * second.scale.z,
    ),
  );
}

/**
 * Create a transform that looks at a target position from a source position.
 *
 * Calculates the rotation needed to point from `from` towards `to`.
 * Uses proper Euler angle conversion with atan2 for stable results.
 *
 * Returns rotation in radians (pitch, yaw, roll) where:
 * - **Pitch (X)**: rotation around X axis (looking up/down)
 * - **Yaw (Y)**: rotation around Y axis (turning left/right)
 * - **Roll (Z)**: rotation around Z axis (typically 0 for look-at)
 *
 * ## Example
 *
 * ```gleam
 * let camera_pos = vec3.Vec3(0.0, 5.0, 10.0)
 * let target_pos = vec3.Vec3(0.0, 0.0, 0.0)
 * let look_transform = transform.look_at(from: camera_pos, to: target_pos)
 * // Camera now faces the origin
 * ```
 */
export function look_at(from, to) {
  let direction = $vec3f.subtract(to, from);
  let _block;
  let $ = $vec3f.length(direction) < 0.0001;
  if ($) {
    _block = new $vec3.Vec3(0.0, 0.0, 1.0);
  } else {
    _block = $vec3f.normalize(direction);
  }
  let direction$1 = _block;
  let _block$1;
  let _pipe = $float.square_root(
    (direction$1.x * direction$1.x) + (direction$1.z * direction$1.z),
  );
  _block$1 = ((result) => {
    if (result instanceof Ok) {
      let val = result[0];
      return val;
    } else {
      return 0.0;
    }
  })(_pipe);
  let horizontal_distance = _block$1;
  let pitch = $maths.atan2(direction$1.y, horizontal_distance);
  let yaw = $maths.atan2(direction$1.x, direction$1.z);
  let roll = 0.0;
  return new Transform(from, new $vec3.Vec3(pitch, yaw, roll), $vec3f.one);
}

/**
 * Move a transform by adding to its current position (relative movement).
 *
 * ## Example
 *
 * ```gleam
 * let t = transform.at(vec3.Vec3(5.0, 0.0, 0.0))
 *   |> transform.translate_by(vec3.Vec3(2.0, 1.0, 0.0))
 * // position: (7.0, 1.0, 0.0)
 * ```
 */
export function translate(transform, offset) {
  return new Transform(
    $vec3f.add(transform.position, offset),
    transform.rotation,
    transform.scale,
  );
}

/**
 * Rotate a transform by adding to its current rotation (relative rotation).
 *
 * ## Example
 *
 * ```gleam
 * let t = transform.identity
 *   |> transform.rotate_by(vec3.Vec3(0.0, 1.57, 0.0))  // Turn 90° right
 *   |> transform.rotate_by(vec3.Vec3(0.0, 1.57, 0.0))  // Turn another 90° right
 * // rotation: (0.0, 3.14, 0.0) - now facing backward
 * ```
 */
export function rotate_by(transform, rotation) {
  return new Transform(
    transform.position,
    $vec3f.add(transform.rotation, rotation),
    transform.scale,
  );
}

/**
 * Scale a transform by multiplying its current scale (relative scaling).
 *
 * ## Example
 *
 * ```gleam
 * let t = transform.identity
 *   |> transform.scale_by(vec3.Vec3(2.0, 1.0, 2.0))
 *   |> transform.scale_by(vec3.Vec3(2.0, 1.0, 1.0))
 * // scale: (4.0, 1.0, 2.0)
 * ```
 */
export function scale_by(transform, scale_factor) {
  return new Transform(
    transform.position,
    transform.rotation,
    new $vec3.Vec3(
      transform.scale.x * scale_factor.x,
      transform.scale.y * scale_factor.y,
      transform.scale.z * scale_factor.z,
    ),
  );
}

/**
 * Set uniform scale on all axes (width = height = depth).
 *
 * ## Example
 *
 * ```gleam
 * let t = transform.identity
 *   |> transform.scale_uniform(2.0)
 * // scale: (2.0, 2.0, 2.0) - twice as big in all dimensions
 * ```
 */
export function scale_uniform(transform, scale) {
  return new Transform(
    transform.position,
    transform.rotation,
    new $vec3.Vec3(scale, scale, scale),
  );
}

/**
 * Rotate around the Y axis (yaw/turn left-right).
 *
 * ## Example
 *
 * ```gleam
 * let t = transform.identity
 *   |> transform.rotate_y(1.57)  // Turn 90° right
 * ```
 */
export function rotate_y(transform, angle) {
  return new Transform(
    transform.position,
    new $vec3.Vec3(
      transform.rotation.x,
      transform.rotation.y + angle,
      transform.rotation.z,
    ),
    transform.scale,
  );
}

/**
 * Rotate around the X axis (pitch/look up-down).
 *
 * ## Example
 *
 * ```gleam
 * let t = transform.identity
 *   |> transform.rotate_x(0.5)  // Look up slightly
 * ```
 */
export function rotate_x(transform, angle) {
  return new Transform(
    transform.position,
    new $vec3.Vec3(
      transform.rotation.x + angle,
      transform.rotation.y,
      transform.rotation.z,
    ),
    transform.scale,
  );
}

/**
 * Rotate around the Z axis (roll/tilt left-right).
 *
 * ## Example
 *
 * ```gleam
 * let t = transform.identity
 *   |> transform.rotate_z(0.3)  // Tilt right
 * ```
 */
export function rotate_z(transform, angle) {
  return new Transform(
    transform.position,
    new $vec3.Vec3(
      transform.rotation.x,
      transform.rotation.y,
      transform.rotation.z + angle,
    ),
    transform.scale,
  );
}

/**
 * Create an identity transform (position at origin, no rotation, scale 1).
 *
 * ## Example
 *
 * ```gleam
 * let t = transform.identity
 * // position: (0, 0, 0), rotation: (0, 0, 0), scale: (1, 1, 1)
 * ```
 */
export const identity = /* @__PURE__ */ new Transform(
  /* @__PURE__ */ new $vec3.Vec3(0.0, 0.0, 0.0),
  /* @__PURE__ */ new $vec3.Vec3(0.0, 0.0, 0.0),
  /* @__PURE__ */ new $vec3.Vec3(1.0, 1.0, 1.0),
);
