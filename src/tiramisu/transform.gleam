//// Transform module - position, rotation, and scale for 3D objects.
////
//// Transforms define where objects are in 3D space, how they're rotated, and their size.
//// All transforms are immutable - functions return new transforms instead of modifying existing ones.
////
//// Internally, rotations are stored as quaternions for better interpolation and to avoid gimbal lock.
//// You can create transforms with either Euler angles or quaternions.
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/transform
//// import vec/vec3
////
//// // Create with Euler angles (most common)
//// let player_transform = transform.identity
////   |> transform.with_position(vec3.Vec3(0.0, 1.0, 0.0))
////   |> transform.with_euler_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // 90 degrees in radians
////   |> transform.with_scale(vec3.Vec3(1.0, 2.0, 1.0))  // Tall player
////
//// // Or create with quaternion rotation
//// let quat = transform.Quaternion(0.0, 0.707, 0.0, 0.707)
//// let rotated = transform.identity |> transform.with_quaternion_rotation(quat)
//// ```

import gleam/float
import gleam_community/maths
import vec/vec3
import vec/vec3f

/// Quaternion represents a rotation in 3D space.
///
/// Quaternions are a mathematical representation that avoids gimbal lock
/// and provides smooth interpolation between rotations.
pub type Quaternion {
  Quaternion(x: Float, y: Float, z: Float, w: Float)
}

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
    rotation: Quaternion,
    scale: vec3.Vec3(Float),
  )
}

// --- Quaternion <-> Euler Conversion Functions ---

/// Convert Euler angles to a quaternion using Three.js's conversion.
///
/// Uses XYZ rotation order matching Three.js default.
/// - euler.x = rotation around X axis (radians)
/// - euler.y = rotation around Y axis (radians)
/// - euler.z = rotation around Z axis (radians)
@external(javascript, "../threejs.ffi.mjs", "eulerToQuaternion")
pub fn euler_to_quaternion(euler: vec3.Vec3(Float)) -> Quaternion

/// Convert a quaternion to Euler angles using Three.js's conversion.
///
/// Returns angles in radians using XYZ rotation order matching Three.js default.
/// - result.x = rotation around X axis (radians)
/// - result.y = rotation around Y axis (radians)
/// - result.z = rotation around Z axis (radians)
@external(javascript, "../threejs.ffi.mjs", "quaternionToEuler")
fn quaternion_to_euler_ffi(q: Quaternion) -> vec3.Vec3(Float)

/// Convert a quaternion to Euler angles.
///
/// Returns angles in radians using XYZ rotation order matching Three.js default.
pub fn quaternion_to_euler(q: Quaternion) -> vec3.Vec3(Float) {
  quaternion_to_euler_ffi(q)
}

// --- Constants ---

/// Identity quaternion
pub const identity_quaternion = Quaternion(0.0, 0.0, 0.0, 1.0)

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
  rotation: identity_quaternion,
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
  Transform(position:, rotation: identity_quaternion, scale: vec3f.one)
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
  quaternion_to_euler(transform.rotation)
}

/// Get the rotation of a transform as a quaternion.
///
/// ## Example
///
/// ```gleam
/// let quat = transform.rotation_quaternion(my_transform)
/// // Returns Quaternion(x, y, z, w)
/// ```
pub fn rotation_quaternion(transform: Transform) -> Quaternion {
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
  let quat = euler_to_quaternion(euler)
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
  quaternion: Quaternion,
) -> Transform {
  Transform(..transform, rotation: quaternion)
}

/// Update the rotation of a transform (Euler angles in radians).
///
/// Alias for `with_euler_rotation` for backwards compatibility.
///
/// ## Example
///
/// ```gleam
/// let rotated = transform.identity
///   |> transform.with_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // 90° turn around Y axis
/// ```
pub fn with_rotation(transform: Transform, euler: vec3.Vec3(Float)) -> Transform {
  with_euler_rotation(transform, euler)
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
    rotation: slerp_quaternion(from.rotation, to.rotation, t),
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

/// Spherical linear interpolation (slerp) between two quaternions.
///
/// Provides smooth rotation interpolation without gimbal lock issues.
fn slerp_quaternion(q1: Quaternion, q2: Quaternion, t: Float) -> Quaternion {
  // Compute dot product
  let dot = q1.x *. q2.x +. q1.y *. q2.y +. q1.z *. q2.z +. q1.w *. q2.w

  // If dot product is negative, negate q2 to take shorter path
  let #(q2, dot) = case dot <. 0.0 {
    True -> #(
      Quaternion(-1.0 *. q2.x, -1.0 *. q2.y, -1.0 *. q2.z, -1.0 *. q2.w),
      -1.0 *. dot,
    )
    False -> #(q2, dot)
  }

  // If quaternions are very close, use linear interpolation
  case dot >. 0.9995 {
    True -> {
      Quaternion(
        x: q1.x +. { q2.x -. q1.x } *. t,
        y: q1.y +. { q2.y -. q1.y } *. t,
        z: q1.z +. { q2.z -. q1.z } *. t,
        w: q1.w +. { q2.w -. q1.w } *. t,
      )
      |> normalize_quaternion
    }
    False -> {
      // Clamp dot to avoid numerical issues with acos
      let dot_clamped = float.clamp(dot, -1.0, 1.0)

      case maths.acos(dot_clamped) {
        Ok(theta_0) -> {
          let theta = theta_0 *. t
          let sin_theta = maths.sin(theta)
          let sin_theta_0 = maths.sin(theta_0)

          let s1 = maths.cos(theta) -. dot_clamped *. sin_theta /. sin_theta_0
          let s2 = sin_theta /. sin_theta_0

          Quaternion(
            x: q1.x *. s1 +. q2.x *. s2,
            y: q1.y *. s1 +. q2.y *. s2,
            z: q1.z *. s1 +. q2.z *. s2,
            w: q1.w *. s1 +. q2.w *. s2,
          )
        }
        Error(_) -> q1
        // Fallback to first quaternion on error
      }
    }
  }
}

/// Normalize a quaternion to unit length.
fn normalize_quaternion(q: Quaternion) -> Quaternion {
  let mag =
    float.square_root(q.x *. q.x +. q.y *. q.y +. q.z *. q.z +. q.w *. q.w)

  case mag {
    Ok(m) if m >. 0.0001 -> {
      Quaternion(x: q.x /. m, y: q.y /. m, z: q.z /. m, w: q.w /. m)
    }
    _ -> identity_quaternion
  }
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
    rotation: multiply_quaternions(first.rotation, second.rotation),
    scale: vec3.Vec3(
      first.scale.x *. second.scale.x,
      first.scale.y *. second.scale.y,
      first.scale.z *. second.scale.z,
    ),
  )
}

/// Multiply two quaternions (q1 * q2) using Three.js.
///
/// Represents the combined rotation of applying q1 then q2.
@external(javascript, "../threejs.ffi.mjs", "multiplyQuaternions")
fn multiply_quaternions(q1: Quaternion, q2: Quaternion) -> Quaternion

/// Create a transform that looks at a target position from a source position.
///
/// Calculates the rotation needed to point from `from` towards `to`.
/// Uses proper Euler angle conversion with atan2 for stable results, then
/// converts to quaternion internally.
///
/// Returns rotation where:
/// - **X rotation**: rotation around X axis (looking up/down)
/// - **Y rotation**: rotation around Y axis (turning left/right)
/// - **Z rotation**: rotation around Z axis (typically 0 for look-at)
///
/// ## Example
///
/// ```gleam
/// let camera_pos = vec3.Vec3(0.0, 5.0, 10.0)
/// let target_pos = vec3.Vec3(0.0, 0.0, 0.0)
/// let look_transform = transform.look_at(from: camera_pos, to: target_pos)
/// // Camera now faces the origin
/// ```
pub fn look_at(
  from from: vec3.Vec3(Float),
  to to: vec3.Vec3(Float),
) -> Transform {
  // Calculate direction vector from source to target
  let direction = vec3f.subtract(to, from)

  // Handle degenerate case where from == to
  let direction = case vec3f.length(direction) <. 0.0001 {
    True -> vec3.Vec3(0.0, 0.0, 1.0)
    // Default forward
    False -> vec3f.normalize(direction)
  }

  // Calculate horizontal distance (projection on XZ plane)
  let horizontal_distance =
    float.square_root(direction.x *. direction.x +. direction.z *. direction.z)
    |> fn(result) {
      case result {
        Ok(val) -> val
        Error(_) -> 0.0
      }
    }

  // Calculate rotation around X axis (looking up/down)
  // atan2(y, horizontal_distance) gives angle from horizontal plane
  let x_rotation = maths.atan2(direction.y, horizontal_distance)

  // Calculate rotation around Y axis (looking left/right)
  // atan2(x, z) gives angle from forward (Z) axis
  let y_rotation = maths.atan2(direction.x, direction.z)

  // Z rotation is typically 0 for standard look-at (no barrel roll)
  let z_rotation = 0.0

  // Convert Euler angles to quaternion
  let euler = vec3.Vec3(x_rotation, y_rotation, z_rotation)
  let quat = euler_to_quaternion(euler)

  Transform(position: from, rotation: quat, scale: vec3f.one)
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
  let additional_rotation = euler_to_quaternion(euler)
  Transform(
    ..transform,
    rotation: multiply_quaternions(transform.rotation, additional_rotation),
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
