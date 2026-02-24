import gleam/float
import gleam/json
import lustre/attribute.{type Attribute}

@internal
pub const tag = "tiramisu-camera"

pub type Camera {
  Perspective
  Orthographic
}

// CAMERA ATTRIBUTES -----------------------------------------------------------

/// Set the camera type ("perspective" or "orthographic").
///
pub fn kind(kind: Camera) -> Attribute(msg) {
  attribute.attribute("camera-type", case kind {
    Perspective -> "perspective"
    Orthographic -> "orthographic"
  })
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
  case is_active {
    True -> attribute.attribute("active", "")
    False -> attribute.property("active", json.bool(False))
  }
}
