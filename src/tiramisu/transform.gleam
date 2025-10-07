import gleam/float
import gleam_community/maths
import tiramisu/vec3

/// Transform represents position, rotation, and scale
pub type Transform {
  Transform(position: vec3.Vec3, rotation: vec3.Vec3, scale: vec3.Vec3)
}

/// Helper to create a default transform
pub fn identity() -> Transform {
  Transform(position: vec3.zero(), rotation: vec3.zero(), scale: vec3.one())
}

/// Helper to create a transform with just position
pub fn at(position position: vec3.Vec3) -> Transform {
  Transform(position:, rotation: vec3.zero(), scale: vec3.one())
}

/// Helper to update position in a transform
pub fn set_position(transform: Transform, position: vec3.Vec3) -> Transform {
  Transform(..transform, position:)
}

/// Helper to update rotation in a transform
pub fn set_rotation(transform: Transform, rotation: vec3.Vec3) -> Transform {
  Transform(..transform, rotation:)
}

/// Helper to update scale in a transform
pub fn set_scale(transform: Transform, scale: vec3.Vec3) -> Transform {
  Transform(..transform, scale:)
}

/// Linearly interpolate between two transforms
pub fn lerp(from: Transform, to to: Transform, with t: Float) -> Transform {
  Transform(
    position: vec3.lerp(from.position, to.position, t),
    rotation: vec3.lerp(from.rotation, to.rotation, t),
    scale: vec3.lerp(from.scale, to.scale, t),
  )
}

/// Compose two transforms (apply second transform after first)
pub fn compose(first: Transform, second: Transform) -> Transform {
  Transform(
    position: vec3.add(first.position, second.position),
    rotation: vec3.add(first.rotation, second.rotation),
    scale: vec3.Vec3(
      first.scale.x *. second.scale.x,
      first.scale.y *. second.scale.y,
      first.scale.z *. second.scale.z,
    ),
  )
}

/// Create a transform that looks at a target position from a source position
/// Uses proper Euler angle conversion with atan2 for stable results
/// Returns rotation in radians (pitch, yaw, roll) where:
/// - Pitch (X): rotation around X axis (up/down)
/// - Yaw (Y): rotation around Y axis (left/right)
/// - Roll (Z): rotation around Z axis (typically 0 for look-at)
pub fn look_at(from from: vec3.Vec3, to to: vec3.Vec3) -> Transform {
  // Calculate direction vector from source to target
  let direction = vec3.subtract(to, from)

  // Handle degenerate case where from == to
  let direction = case vec3.magnitude(direction) <. 0.0001 {
    True -> vec3.Vec3(0.0, 0.0, 1.0)
    // Default forward
    False -> vec3.normalize(direction)
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
    scale: vec3.one(),
  )
}
