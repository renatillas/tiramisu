//// Transform type for representing position, rotation, and scale.
////
//// This module provides the Transform type used internally by the physics system
//// for representing transforms on rigid bodies and colliders.
////
//// Note: For scene graph transforms, use the attribute module instead.

import quaternion.{type Quaternion}
import vec/vec3.{type Vec3}

// TYPES -----------------------------------------------------------------------

/// A transform represents position, rotation, and scale in 3D space.
pub type Transform {
  Transform(position: Vec3(Float), rotation: Quaternion, scale: Vec3(Float))
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Create an identity transform (position at origin, no rotation, scale of 1).
pub fn identity() -> Transform {
  Transform(
    position: vec3.Vec3(0.0, 0.0, 0.0),
    rotation: quaternion.identity,
    scale: vec3.Vec3(1.0, 1.0, 1.0),
  )
}

/// Create a transform at a specific position.
pub fn at(position pos: Vec3(Float)) -> Transform {
  Transform(
    position: pos,
    rotation: quaternion.identity,
    scale: vec3.Vec3(1.0, 1.0, 1.0),
  )
}

// ACCESSORS -------------------------------------------------------------------

/// Get the position component of a transform.
pub fn position(transform: Transform) -> Vec3(Float) {
  transform.position
}

/// Get the rotation component as a quaternion.
pub fn rotation_quaternion(transform: Transform) -> Quaternion {
  transform.rotation
}

/// Get the scale component of a transform.
pub fn scale(transform: Transform) -> Vec3(Float) {
  transform.scale
}

// BUILDERS --------------------------------------------------------------------

/// Set the position of a transform.
pub fn with_position(transform: Transform, position: Vec3(Float)) -> Transform {
  Transform(..transform, position: position)
}

/// Set the rotation of a transform using a quaternion.
pub fn with_quaternion_rotation(
  transform: Transform,
  rotation: Quaternion,
) -> Transform {
  Transform(..transform, rotation: rotation)
}

/// Set the rotation of a transform using Euler angles (in radians).
pub fn with_euler_rotation(
  transform: Transform,
  x: Float,
  y: Float,
  z: Float,
) -> Transform {
  let quat = euler_to_quaternion(x, y, z)
  Transform(..transform, rotation: quat)
}

/// Set the scale of a transform.
pub fn with_scale(transform: Transform, scale: Vec3(Float)) -> Transform {
  Transform(..transform, scale: scale)
}

/// Set a uniform scale on all axes.
pub fn with_uniform_scale(transform: Transform, scale: Float) -> Transform {
  Transform(..transform, scale: vec3.Vec3(scale, scale, scale))
}

// HELPERS ---------------------------------------------------------------------

/// Convert Euler angles (in radians) to a quaternion.
/// Order: YXZ (yaw, pitch, roll)
fn euler_to_quaternion(x: Float, y: Float, z: Float) -> Quaternion {
  let cx = cos(x /. 2.0)
  let cy = cos(y /. 2.0)
  let cz = cos(z /. 2.0)
  let sx = sin(x /. 2.0)
  let sy = sin(y /. 2.0)
  let sz = sin(z /. 2.0)

  quaternion.Quaternion(
    x: sx *. cy *. cz +. cx *. sy *. sz,
    y: cx *. sy *. cz -. sx *. cy *. sz,
    z: cx *. cy *. sz +. sx *. sy *. cz,
    w: cx *. cy *. cz -. sx *. sy *. sz,
  )
}

// FFI -------------------------------------------------------------------------

@external(javascript, "./transform.ffi.mjs", "cos")
fn cos(x: Float) -> Float

@external(javascript, "./transform.ffi.mjs", "sin")
fn sin(x: Float) -> Float
