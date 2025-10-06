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
pub fn look_at(
  from from: vec3.Vec3,
  to to: vec3.Vec3,
  up up: vec3.Vec3,
) -> Transform {
  // Calculate forward direction
  let forward = vec3.normalize(vec3.subtract(to, from))

  // Calculate right direction
  let right = vec3.normalize(vec3.cross(up, forward))

  // Calculate actual up direction
  let _actual_up = vec3.cross(forward, right)

  // Convert to Euler angles (simplified - works for most cases)
  // This is a simplified version; for production you'd want a full matrix-to-euler conversion
  let yaw = case forward.x == 0.0 && forward.z == 0.0 {
    True -> 0.0
    False -> {
      let assert Ok(result) =
        vec3.angle(
          vec3.Vec3(0.0, 0.0, 1.0),
          vec3.Vec3(forward.x, 0.0, forward.z),
        )
      case forward.x <. 0.0 {
        True -> 0.0 -. result
        False -> result
      }
    }
  }

  let pitch = {
    let assert Ok(result) =
      vec3.angle(vec3.Vec3(forward.x, 0.0, forward.z), forward)
    case forward.y <. 0.0 {
      True -> 0.0 -. result
      False -> result
    }
  }

  // Roll is typically 0 for look-at transforms
  let roll = 0.0

  Transform(
    position: from,
    rotation: vec3.Vec3(pitch, yaw, roll),
    scale: vec3.one(),
  )
}
