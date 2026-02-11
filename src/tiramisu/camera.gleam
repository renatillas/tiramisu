//// The tiramisu-camera element.
////
//// Cameras define viewpoints into the scene. The renderer parses these
//// elements from the light DOM and manages the corresponding Three.js
//// cameras automatically.
////
//// ## Usage
////
//// ```html
//// <tiramisu-camera
////   id="main"
////   type="perspective"
////   fov="75"
////   near="0.1"
////   far="1000"
////   transform="pos:0,5,10"
////   active="true"
//// ></tiramisu-camera>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier for the camera (required)
//// - `type`: Camera type - "perspective" or "orthographic" (default: "perspective")
//// - `fov`: Field of view in degrees (perspective only, default: 75)
//// - `near`: Near clipping plane (default: 0.1)
//// - `far`: Far clipping plane (default: 1000)
//// - `transform`: Transform as "pos:x,y,z quat:x,y,z,w scale:x,y,z" (default: pos:0,0,5)
//// - `active`: Whether this camera is used for rendering (default: "false")

// IMPORTS ---------------------------------------------------------------------

import gleam/float

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

import tiramisu/transform.{type Transform}

import vec/vec3

// CONSTANTS -------------------------------------------------------------------

/// The tag name for camera elements.
pub const tag_name = "tiramisu-camera"

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-camera element.
///
/// Cameras define viewpoints into the scene. Mark one camera as `active`
/// to use it for rendering.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/camera
///
/// camera.camera("main", [
///   camera.fov(75.0),
///   camera.active(True),
/// ], [])
/// ```
///
pub fn camera(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the camera type ("perspective" or "orthographic").
///
pub fn camera_type(type_: String) -> Attribute(msg) {
  attribute.attribute("type", type_)
}

/// Set the field of view for a perspective camera (in degrees).
///
pub fn fov(degrees: Float) -> Attribute(msg) {
  attribute.attribute("fov", float.to_string(degrees))
}

/// Set the near clipping plane distance.
///
pub fn near(distance: Float) -> Attribute(msg) {
  attribute.attribute("near", float.to_string(distance))
}

/// Set the far clipping plane distance.
///
pub fn far(distance: Float) -> Attribute(msg) {
  attribute.attribute("far", float.to_string(distance))
}

/// Mark a camera as active (only active cameras render).
///
pub fn active(is_active: Bool) -> Attribute(msg) {
  attribute.attribute("active", case is_active {
    True -> "true"
    False -> "false"
  })
}

/// Set the full transform of the camera.
///
pub fn transform(t: Transform) -> Attribute(msg) {
  let vec3.Vec3(px, py, pz) = transform.position(t)
  let #(qx, qy, qz, qw) = transform.to_quaternion_xyzw(t)
  let vec3.Vec3(sx, sy, sz) = transform.scale(t)

  attribute.attribute(
    "transform",
    "pos:"
      <> float.to_string(px)
      <> ","
      <> float.to_string(py)
      <> ","
      <> float.to_string(pz)
      <> " quat:"
      <> float.to_string(qx)
      <> ","
      <> float.to_string(qy)
      <> ","
      <> float.to_string(qz)
      <> ","
      <> float.to_string(qw)
      <> " scale:"
      <> float.to_string(sx)
      <> ","
      <> float.to_string(sy)
      <> ","
      <> float.to_string(sz),
  )
}
