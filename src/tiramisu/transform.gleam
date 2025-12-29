//// 3D transform utilities for position, rotation, and scale.
////
//// Transforms define the spatial properties of scene objects: where they are,
//// how they're rotated, and their size. All scene nodes use transforms to
//// position themselves in world space.
////
//// ## Creating Transforms
////
//// ```gleam
//// // Identity transform (origin, no rotation, unit scale)
//// transform.identity
////
//// // At a specific position
//// transform.at(position: vec3.Vec3(5.0, 0.0, -3.0))
////
//// // Builder pattern
//// transform.identity
////   |> transform.with_position(vec3.Vec3(1.0, 2.0, 3.0))
////   |> transform.with_euler_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // 90° Y
////   |> transform.with_scale(vec3.Vec3(2.0, 1.0, 2.0))
//// ```
////
//// ## Rotation
////
//// Rotations are stored internally as quaternions but can be set using Euler angles
//// (in radians). Quaternions avoid gimbal lock and interpolate smoothly.
////
//// ```gleam
//// // Using Euler angles (radians)
//// transform.with_euler_rotation(t, vec3.Vec3(0.0, 3.14, 0.0))
////
//// // Using quaternion directly
//// transform.with_quaternion_rotation(t, my_quaternion)
////
//// // Convenience rotations
//// transform.rotate_y(t, 1.57)  // Turn 90° right
//// transform.rotate_x(t, 0.5)   // Look up
//// ```
////
//// ## Interpolation
////
//// Use `lerp` for smooth transitions between transforms. Great for animations
//// and camera movement:
////
//// ```gleam
//// let halfway = transform.lerp(start, to: end, with: 0.5)
//// ```
////

import gleam/option
import quaternion
import vec/vec3
import vec/vec3f

/// Transform represents the position, rotation, and scale of an object in 3D space.
///
/// - **position**: World-space coordinates (x, y, z)
/// - **rotation**: Quaternion rotation (stored internally, accessible as Euler angles)
/// - **scale**: Size multiplier per axis (1.0 = original size)
///
/// This type is opaque to ensure rotation consistency and proper quaternion handling.
pub opaque type Transform {
  Transform(
    position: vec3.Vec3(Float),
    rotation: quaternion.Quaternion,
    scale: vec3.Vec3(Float),
  )
}

/// Create an identity transform (position at origin, no rotation, scale 1).
///
/// ## Example
///
/// ```gleam
/// let t = transform.identity
/// // position: (0, 0, 0), rotation: quaternion identity, scale: (1, 1, 1)
/// ```
pub const identity = Transform(
  position: vec3.Vec3(0.0, 0.0, 0.0),
  rotation: quaternion.identity,
  scale: vec3.Vec3(1.0, 1.0, 1.0),
)

/// Create a transform at a specific position with default rotation and scale.
///
/// ## Example
///
/// ```gleam
/// let t = transform.at(position: vec3.Vec3(5.0, 0.0, -3.0))
/// // Object positioned at (5, 0, -3)
/// ```
pub fn at(position position: vec3.Vec3(Float)) -> Transform {
  Transform(position:, rotation: quaternion.identity, scale: vec3f.one)
}

// --- Accessor Functions ---

/// Get the position of a transform.
///
/// ## Example
///
/// ```gleam
/// let pos = transform.position(my_transform)
/// // Returns vec3.Vec3(x, y, z)
/// ```
pub fn position(transform: Transform) -> vec3.Vec3(Float) {
  transform.position
}

/// Get the rotation of a transform as Euler angles (in radians).
///
/// Returns rotation as Vec3(x_rotation, y_rotation, z_rotation) in radians.
///
/// ## Example
///
/// ```gleam
/// let euler = transform.rotation(my_transform)
/// // Returns vec3.Vec3(x_rotation, y_rotation, z_rotation)
/// ```
pub fn rotation(transform: Transform) -> vec3.Vec3(Float) {
  quaternion.to_euler(transform.rotation)
}

/// Get the rotation of a transform as a quaternion.
///
/// ## Example
///
/// ```gleam
/// let quat = transform.rotation_quaternion(my_transform)
/// // Returns quaternion.Quaternion(x, y, z, w)
/// ```
pub fn rotation_quaternion(transform: Transform) -> quaternion.Quaternion {
  transform.rotation
}

/// Get the scale of a transform.
///
/// ## Example
///
/// ```gleam
/// let scale = transform.scale(my_transform)
/// // Returns vec3.Vec3(x, y, z)
/// ```
pub fn scale(transform: Transform) -> vec3.Vec3(Float) {
  transform.scale
}

/// Update the position of a transform.
///
/// ## Example
///
/// ```gleam
/// let moved = transform.identity
///   |> transform.with_position(vec3.Vec3(1.0, 2.0, 3.0))
/// ```
pub fn with_position(
  transform: Transform,
  position: vec3.Vec3(Float),
) -> Transform {
  Transform(..transform, position:)
}

/// Update the rotation of a transform using Euler angles (in radians).
///
/// Converts Euler angles to quaternion internally.
///
/// ## Example
///
/// ```gleam
/// let rotated = transform.identity
///   |> transform.with_euler_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // 90° turn around Y axis
/// ```
pub fn with_euler_rotation(
  transform: Transform,
  euler: vec3.Vec3(Float),
) -> Transform {
  let quat = quaternion.from_euler(euler)
  Transform(..transform, rotation: quat)
}

/// Update the rotation of a transform using a quaternion directly.
///
/// Use this when you already have a quaternion or want to avoid Euler angle conversion.
///
/// ## Example
///
/// ```gleam
/// let quat = transform.Quaternion(0.0, 0.707, 0.0, 0.707)
/// let rotated = transform.identity
///   |> transform.with_quaternion_rotation(quat)
/// ```
pub fn with_quaternion_rotation(
  transform: Transform,
  quaternion: quaternion.Quaternion,
) -> Transform {
  Transform(..transform, rotation: quaternion)
}

/// Update the scale of a transform.
///
/// ## Example
///
/// ```gleam
/// let scaled = transform.identity
///   |> transform.with_scale(vec3.Vec3(2.0, 1.0, 2.0))  // Wide and deep, normal height
/// ```
pub fn with_scale(transform: Transform, scale: vec3.Vec3(Float)) -> Transform {
  Transform(..transform, scale:)
}

/// Linearly interpolate between two transforms.
///
/// Uses linear interpolation for position and scale, and spherical linear
/// interpolation (slerp) for rotation to ensure smooth rotation transitions.
///
/// Parameter `t` should be between 0.0 and 1.0:
/// - `t = 0.0` returns `from`
/// - `t = 1.0` returns `to`
/// - `t = 0.5` returns halfway between
///
/// ## Example
///
/// ```gleam
/// let start = transform.at(vec3.Vec3(0.0, 0.0, 0.0))
/// let end = transform.at(vec3.Vec3(10.0, 0.0, 0.0))
/// let halfway = transform.lerp(start, to: end, with: 0.5)
/// // position: (5.0, 0.0, 0.0)
/// ```
pub fn lerp(from: Transform, to to: Transform, with t: Float) -> Transform {
  Transform(
    position: lerp_vec(from.position, to.position, t),
    rotation: quaternion.linear_interpolation(
      from: from.rotation,
      to: to.rotation,
      t: t,
    ),
    scale: lerp_vec(from.scale, to.scale, t),
  )
}

fn lerp_vec(
  a: vec3.Vec3(Float),
  b: vec3.Vec3(Float),
  t: Float,
) -> vec3.Vec3(Float) {
  vec3.Vec3(
    a.x +. { b.x -. a.x } *. t,
    a.y +. { b.y -. a.y } *. t,
    a.z +. { b.z -. a.z } *. t,
  )
}

/// Compose two transforms (apply second transform after first).
///
/// Useful for relative transformations. Combines positions, multiplies quaternions
/// for rotation, and multiplies scales. For proper hierarchical transforms,
/// use scene `Group` nodes instead.
///
/// ## Example
///
/// ```gleam
/// let base = transform.at(vec3.Vec3(5.0, 0.0, 0.0))
/// let offset = transform.at(vec3.Vec3(0.0, 2.0, 0.0))
/// let combined = transform.compose(base, offset)
/// // position: (5.0, 2.0, 0.0)
/// ```
pub fn compose(first: Transform, second: Transform) -> Transform {
  Transform(
    position: vec3f.add(first.position, second.position),
    rotation: quaternion.multiply(first.rotation, second.rotation),
    scale: vec3.Vec3(
      first.scale.x *. second.scale.x,
      first.scale.y *. second.scale.y,
      first.scale.z *. second.scale.z,
    ),
  )
}

/// Create a transform that looks at a target position from a source position.
///
/// Calculates the rotation needed to point from `from` towards `to`.
///
/// ## Example
///
/// ```gleam
/// let camera_pos = transform.at(vec3.Vec3(0.0, 5.0, 10.0))
/// let target_pos = transform.at(vec3.Vec3(0.0, 0.0, 0.0))
/// let look_transform = transform.look_at(from: camera_pos, to: target_pos, up: option.None)
/// // Camera now faces the origin
/// ```
pub fn look_at(
  from from: Transform,
  to to: Transform,
  up up: option.Option(vec3.Vec3(Float)),
) -> Transform {
  // Convert Euler angles to quaternion
  let up = option.unwrap(up, vec3.Vec3(0.0, 1.0, 0.0))
  let quat = quaternion.look_at(from.position, to.position, up)

  // Preserve position and scale from the 'from' transform
  Transform(position: from.position, rotation: quat, scale: from.scale)
}

// --- Convenience Methods for Relative Transforms ---

/// Move a transform by adding to its current position (relative movement).
///
/// ## Example
///
/// ```gleam
/// let t = transform.at(vec3.Vec3(5.0, 0.0, 0.0))
///   |> transform.translate_by(vec3.Vec3(2.0, 1.0, 0.0))
/// // position: (7.0, 1.0, 0.0)
/// ```
pub fn translate(transform: Transform, by offset: vec3.Vec3(Float)) -> Transform {
  Transform(..transform, position: vec3f.add(transform.position, offset))
}

/// Rotate a transform by applying an additional rotation (relative rotation).
///
/// Converts the Euler angle rotation to a quaternion and multiplies it with
/// the current rotation.
///
/// ## Example
///
/// ```gleam
/// let t = transform.identity
///   |> transform.rotate_by(vec3.Vec3(0.0, 1.57, 0.0))  // Turn 90° right
///   |> transform.rotate_by(vec3.Vec3(0.0, 1.57, 0.0))  // Turn another 90° right
/// // Now facing backward
/// ```
pub fn rotate_by(transform: Transform, euler: vec3.Vec3(Float)) -> Transform {
  let additional_rotation = quaternion.from_euler(euler)
  Transform(
    ..transform,
    rotation: quaternion.multiply(transform.rotation, additional_rotation),
  )
}

/// Scale a transform by multiplying its current scale (relative scaling).
///
/// ## Example
///
/// ```gleam
/// let t = transform.identity
///   |> transform.scale_by(vec3.Vec3(2.0, 1.0, 2.0))
///   |> transform.scale_by(vec3.Vec3(2.0, 1.0, 1.0))
/// // scale: (4.0, 1.0, 2.0)
/// ```
pub fn scale_by(
  transform: Transform,
  scale_factor: vec3.Vec3(Float),
) -> Transform {
  Transform(
    ..transform,
    scale: vec3.Vec3(
      transform.scale.x *. scale_factor.x,
      transform.scale.y *. scale_factor.y,
      transform.scale.z *. scale_factor.z,
    ),
  )
}

/// Set uniform scale on all axes (width = height = depth).
///
/// ## Example
///
/// ```gleam
/// let t = transform.identity
///   |> transform.scale_uniform(2.0)
/// // scale: (2.0, 2.0, 2.0) - twice as big in all dimensions
/// ```
pub fn scale_uniform(transform: Transform, scale: Float) -> Transform {
  Transform(..transform, scale: vec3.Vec3(scale, scale, scale))
}

/// Rotate around the Y axis (yaw/turn left-right).
///
/// ## Example
///
/// ```gleam
/// let t = transform.identity
///   |> transform.rotate_y(1.57)  // Turn 90° right
/// ```
pub fn rotate_y(transform: Transform, angle: Float) -> Transform {
  rotate_by(transform, vec3.Vec3(0.0, angle, 0.0))
}

/// Rotate around the X axis (pitch/look up-down).
///
/// ## Example
///
/// ```gleam
/// let t = transform.identity
///   |> transform.rotate_x(0.5)  // Look up slightly
/// ```
pub fn rotate_x(transform: Transform, angle: Float) -> Transform {
  rotate_by(transform, vec3.Vec3(angle, 0.0, 0.0))
}

/// Rotate around the Z axis (roll/tilt left-right).
///
/// ## Example
///
/// ```gleam
/// let t = transform.identity
///   |> transform.rotate_z(0.3)  // Tilt right
/// ```
pub fn rotate_z(transform: Transform, angle: Float) -> Transform {
  rotate_by(transform, vec3.Vec3(0.0, 0.0, angle))
}
