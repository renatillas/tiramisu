//// Transform module - position, rotation, and scale for 3D objects.
////
//// Transforms define where objects are in 3D space, how they're rotated, and their size.
//// All transforms are immutable - functions return new transforms instead of modifying existing ones.
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/transform
//// import vec/vec3
////
//// let player_transform = transform.identity()
////   |> transform.set_position(vec3.Vec3(0.0, 1.0, 0.0))
////   |> transform.set_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // 90 degrees in radians
////   |> transform.set_scale(vec3.Vec3(1.0, 2.0, 1.0))  // Tall player
//// ```

import gleam/float
import gleam_community/maths
import vec/vec3
import vec/vec3f

/// Transform represents the position, rotation, and scale of an object in 3D space.
///
/// - **position**: World-space coordinates (x, y, z)
/// - **rotation**: Euler angles in radians (pitch, yaw, roll / x, y, z)
/// - **scale**: Size multiplier per axis (1.0 = original size)
pub type Transform {
  Transform(
    position: vec3.Vec3(Float),
    rotation: vec3.Vec3(Float),
    scale: vec3.Vec3(Float),
  )
}

/// Create an identity transform (position at origin, no rotation, scale 1).
///
/// ## Example
///
/// ```gleam
/// let t = transform.identity()
/// // position: (0, 0, 0), rotation: (0, 0, 0), scale: (1, 1, 1)
/// ```
pub fn identity() -> Transform {
  Transform(position: vec3f.zero, rotation: vec3f.zero, scale: vec3f.one)
}

/// Create a transform at a specific position with default rotation and scale.
///
/// ## Example
///
/// ```gleam
/// let t = transform.at(vec3.Vec3(5.0, 0.0, -3.0))
/// // Object positioned at (5, 0, -3)
/// ```
pub fn at(position position: vec3.Vec3(Float)) -> Transform {
  Transform(position:, rotation: vec3f.zero, scale: vec3f.one)
}

/// Update the position of a transform.
///
/// ## Example
///
/// ```gleam
/// let moved = transform.identity()
///   |> transform.set_position(vec3.Vec3(1.0, 2.0, 3.0))
/// ```
pub fn set_position(
  transform: Transform,
  position: vec3.Vec3(Float),
) -> Transform {
  Transform(..transform, position:)
}

/// Update the rotation of a transform (Euler angles in radians).
///
/// ## Example
///
/// ```gleam
/// let rotated = transform.identity()
///   |> transform.set_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // 90° turn around Y axis
/// ```
pub fn set_rotation(
  transform: Transform,
  rotation: vec3.Vec3(Float),
) -> Transform {
  Transform(..transform, rotation:)
}

/// Update the scale of a transform.
///
/// ## Example
///
/// ```gleam
/// let scaled = transform.identity()
///   |> transform.set_scale(vec3.Vec3(2.0, 1.0, 2.0))  // Wide and deep, normal height
/// ```
pub fn set_scale(transform: Transform, scale: vec3.Vec3(Float)) -> Transform {
  Transform(..transform, scale:)
}

/// Linearly interpolate between two transforms.
///
/// Useful for smooth animations and transitions. Parameter `t` should be between 0.0 and 1.0:
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
    rotation: lerp_vec(from.rotation, to.rotation, t),
    scale: lerp_vec(from.scale, to.scale, t),
  )
}

fn lerp_vec(a: vec3.Vec3(Float), b: vec3.Vec3(Float), t) {
  vec3.Vec3(
    a.x +. { b.x -. a.x } *. t,
    a.y +. { b.y -. a.y } *. t,
    a.z +. { b.z -. a.z } *. t,
  )
}

/// Compose two transforms (apply second transform after first).
///
/// Useful for relative transformations. Note: This is a simplified composition
/// that adds positions/rotations and multiplies scales. For proper hierarchical
/// transforms, use scene `Group` nodes instead.
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
    rotation: vec3f.add(first.rotation, second.rotation),
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
/// Uses proper Euler angle conversion with atan2 for stable results.
///
/// Returns rotation in radians (pitch, yaw, roll) where:
/// - **Pitch (X)**: rotation around X axis (looking up/down)
/// - **Yaw (Y)**: rotation around Y axis (turning left/right)
/// - **Roll (Z)**: rotation around Z axis (typically 0 for look-at)
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

  // Calculate pitch (rotation around X axis - looking up/down)
  // atan2(y, horizontal_distance) gives angle from horizontal plane
  let pitch = maths.atan2(direction.y, horizontal_distance)

  // Calculate yaw (rotation around Y axis - looking left/right)
  // atan2(x, z) gives angle from forward (Z) axis
  // Note: In Three.js, positive Z is backward, so we negate
  let yaw = maths.atan2(direction.x, direction.z)

  // Roll is typically 0 for standard look-at (no barrel roll)
  let roll = 0.0

  Transform(
    position: from,
    rotation: vec3.Vec3(pitch, yaw, roll),
    scale: vec3f.one,
  )
}
