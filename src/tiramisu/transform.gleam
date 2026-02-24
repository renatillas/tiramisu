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
import gleam/result
import gleam/string
import lustre/attribute
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
  Transform(..transform, scale: vec3.splat(scale))
}

/// Set the rotation to look at a target point from the current position.
pub fn with_look_at(transform: Transform, target: Vec3(Float)) -> Transform {
  let target = vec3f.subtract(target, transform.position) |> vec3f.normalize
  let forward = vec3.Vec3(0.0, 0.0, -1.0)
  let up = vec3.Vec3(0.0, 1.0, 0.0)
  let rotation = quaternion.look_at(forward:, target:, up:)
  Transform(..transform, rotation:)
}

// PARSING ---------------------------------------------------------------------

/// Parse a transform string in the format "pos:x,y,z quat:x,y,z,w scale:x,y,z".
/// Parts can be omitted to use defaults.
@internal
pub fn parse(input: String) -> Transform {
  let parts = string.split(input, ";") |> list.map(string.trim)

  let pos = find_and_parse_vec3(parts, "position:", vec3.Vec3(0.0, 0.0, 0.0))
  let rot = find_and_parse_rotation(parts, "rotation:", quaternion.identity)
  let scl = find_and_parse_vec3(parts, "scale:", vec3.Vec3(1.0, 1.0, 1.0))

  Transform(position: pos, rotation: rot, scale: scl)
}

fn find_and_parse_vec3(
  parts: List(String),
  prefix: String,
  default: Vec3(Float),
) -> Vec3(Float) {
  case list.find(parts, string.starts_with(_, prefix)) {
    Ok(part) ->
      prefix
      |> string.length
      |> string.drop_start(part, _)
      |> parse_vec3(default)

    Error(_) -> default
  }
}

fn find_and_parse_rotation(
  parts: List(String),
  prefix: String,
  default: Quaternion,
) -> Quaternion {
  case list.find(parts, string.starts_with(_, prefix)) {
    Ok(part) ->
      prefix
      |> string.length
      |> string.drop_start(part, _)
      |> parse_rotation(default)

    Error(_) -> default
  }
}

fn parse_vec3(input: String, default: Vec3(Float)) -> Vec3(Float) {
  case string.split(input, ",") |> list.map(string.trim) {
    [x_str, y_str, z_str] -> {
      use x <- result.try(parse_number(x_str))
      use y <- result.try(parse_number(y_str))
      use z <- result.map(parse_number(z_str))
      vec3.Vec3(x, y, z)
    }
    _ -> Error(Nil)
  }
  |> result.unwrap(default)
}

fn parse_rotation(input: String, default: Quaternion) -> Quaternion {
  case string.split(input, ",") |> list.map(string.trim) {
    [x_str, y_str, z_str, w_str] -> {
      use x <- result.try(parse_number(x_str))
      use y <- result.try(parse_number(y_str))
      use z <- result.try(parse_number(z_str))
      use w <- result.map(parse_number(w_str))
      quaternion.Quaternion(x:, y:, z:, w:)
    }
    [x_str, y_str, z_str] -> {
      use x <- result.try(parse_number(x_str))
      use y <- result.try(parse_number(y_str))
      use z <- result.map(parse_number(z_str))
      vec3.Vec3(x:, y:, z:)
      |> quaternion.from_euler
    }
    _ -> Error(Nil)
  }
  |> result.unwrap(default)
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

/// Set the full transform of an element.
///
/// Works with cameras, lights, meshes, empties, LODs, instanced meshes,
/// positional audio, and debug helpers.
///
pub fn transform(transform: Transform) -> attribute.Attribute(msg) {
  attribute.attribute("transform", to_string(transform))
}

@internal
pub fn to_string(transform: Transform) -> String {
  "position:"
  <> float.to_string(transform.position.x)
  <> ", "
  <> float.to_string(transform.position.y)
  <> ", "
  <> float.to_string(transform.position.z)
  <> "; rotation:"
  <> float.to_string(transform.rotation.x)
  <> ", "
  <> float.to_string(transform.rotation.y)
  <> ", "
  <> float.to_string(transform.rotation.z)
  <> ", "
  <> float.to_string(transform.rotation.w)
  <> "; scale:"
  <> float.to_string(transform.scale.x)
  <> ", "
  <> float.to_string(transform.scale.y)
  <> ", "
  <> float.to_string(transform.scale.z)
}
