//// Transform type for representing position, rotation, and scale.
////
//// This module provides the Transform type and parsing for web components.
////
//// ## Attribute Format
////
//// ```
//// transform="pos:0,1,0 quat:0,0,0,1 scale:2,2,2"
//// ```
////
//// Parts can be omitted to use defaults:
//// - `pos:x,y,z` - Position (default: 0,0,0)
//// - `quat:x,y,z,w` - Rotation as quaternion (default: 0,0,0,1 identity)
//// - `scale:x,y,z` - Scale (default: 1,1,1)

import gleam/float
import gleam/int
import gleam/list
import gleam/string
import quaternion.{type Quaternion}
import vec/vec3.{type Vec3}
import vec/vec3f

// TYPES -----------------------------------------------------------------------

/// A transform represents position, rotation, and scale in 3D space.
pub type Transform {
  Transform(position: Vec3(Float), rotation: Quaternion, scale: Vec3(Float))
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Create an identity transform (position at origin, no rotation, scale of 1).
pub const identity = Transform(
  position: vec3.Vec3(0.0, 0.0, 0.0),
  rotation: quaternion.identity,
  scale: vec3.Vec3(1.0, 1.0, 1.0),
)

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
pub fn with_rotation(transform: Transform, rotation: Quaternion) -> Transform {
  Transform(..transform, rotation: rotation)
}

/// Set the scale of a transform.
pub fn with_scale(transform: Transform, scale: Vec3(Float)) -> Transform {
  Transform(..transform, scale: scale)
}

/// Set a uniform scale on all axes.
pub fn with_uniform_scale(transform: Transform, scale: Float) -> Transform {
  Transform(..transform, scale: vec3.Vec3(scale, scale, scale))
}

/// Set the rotation to look at a target point from the current position.
pub fn with_look_at(transform: Transform, target: Vec3(Float)) -> Transform {
  let direction = vec3f.subtract(target, transform.position) |> vec3f.normalize
  let forward = vec3.Vec3(0.0, 0.0, -1.0)
  let up = vec3.Vec3(0.0, 1.0, 0.0)
  let rotation = quaternion.look_at(forward:, target: direction, up:)
  Transform(..transform, rotation:)
}

// PARSING ---------------------------------------------------------------------

/// Parse a transform string in the format "pos:x,y,z quat:x,y,z,w scale:x,y,z".
/// Parts can be omitted to use defaults.
pub fn parse(input: String) -> Transform {
  let parts = string.split(input, " ") |> list.map(string.trim)

  let pos = find_and_parse_vec3(parts, "pos:", vec3.Vec3(0.0, 0.0, 0.0))
  let rot = find_and_parse_quaternion(parts, "quat:", quaternion.identity)
  let scl = find_and_parse_vec3(parts, "scale:", vec3.Vec3(1.0, 1.0, 1.0))

  Transform(position: pos, rotation: rot, scale: scl)
}

fn find_and_parse_vec3(
  parts: List(String),
  prefix: String,
  default: Vec3(Float),
) -> Vec3(Float) {
  case list.find(parts, fn(p) { string.starts_with(p, prefix) }) {
    Ok(part) -> {
      let value_str = string.drop_start(part, string.length(prefix))
      parse_vec3(value_str, default)
    }
    Error(_) -> default
  }
}

fn find_and_parse_quaternion(
  parts: List(String),
  prefix: String,
  default: Quaternion,
) -> Quaternion {
  case list.find(parts, fn(p) { string.starts_with(p, prefix) }) {
    Ok(part) -> {
      let value_str = string.drop_start(part, string.length(prefix))
      parse_quaternion(value_str, default)
    }
    Error(_) -> default
  }
}

fn parse_vec3(input: String, default: Vec3(Float)) -> Vec3(Float) {
  case string.split(input, ",") |> list.map(string.trim) {
    [x_str, y_str, z_str] -> {
      case parse_number(x_str), parse_number(y_str), parse_number(z_str) {
        Ok(x), Ok(y), Ok(z) -> vec3.Vec3(x, y, z)
        _, _, _ -> default
      }
    }
    _ -> default
  }
}

fn parse_quaternion(input: String, default: Quaternion) -> Quaternion {
  case string.split(input, ",") |> list.map(string.trim) {
    [x_str, y_str, z_str, w_str] -> {
      case
        parse_number(x_str),
        parse_number(y_str),
        parse_number(z_str),
        parse_number(w_str)
      {
        Ok(x), Ok(y), Ok(z), Ok(w) -> quaternion.Quaternion(x:, y:, z:, w:)
        _, _, _, _ -> default
      }
    }
    _ -> default
  }
}

fn parse_number(input: String) -> Result(Float, Nil) {
  case float.parse(input) {
    Ok(f) -> Ok(f)
    Error(_) -> {
      case int.parse(input) {
        Ok(i) -> Ok(int.to_float(i))
        Error(_) -> Error(Nil)
      }
    }
  }
}

// QUATERNION EXTRACTION -------------------------------------------------------

/// Extract quaternion components (x, y, z, w) from a transform's rotation.
/// Use this for passing directly to Three.js quaternion.set().
pub fn to_quaternion_xyzw(transform: Transform) -> #(Float, Float, Float, Float) {
  let quaternion.Quaternion(x:, y:, z:, w:) = transform.rotation
  #(x, y, z, w)
}
