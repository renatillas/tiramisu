import gleam/float
import gleam/int
import gleam/json

import lustre/attribute.{type Attribute}

@internal
pub const tag = "tiramisu-light"

pub type Light {
  Ambient
  Directional
  Point
  Spot
}

// LIGHT ATTRIBUTES ------------------------------------------------------------

/// Set the light type ("ambient", "directional", "point", or "spot").
///
pub fn kind(kind: Light) -> Attribute(msg) {
  attribute.attribute("light-type", case kind {
    Ambient -> "ambient"
    Directional -> "directional"
    Point -> "point"
    Spot -> "spot"
  })
}

/// Set the intensity of the light.
///
pub fn intensity(value: Float) -> Attribute(msg) {
  attribute.attribute("intensity", float.to_string(value))
}

/// Set the base color as a hex integer.
///
/// ## Example
///
/// ```gleam
/// tiramisu.color(0xff0000)  // Red
/// ```
///
pub fn color(hex hex: Int) -> Attribute(msg) {
  attribute.attribute("color", "#" <> int.to_base16(hex))
}

/// Enable shadow casting on the element.
///
/// For meshes and instanced meshes: the element will cast shadows.
/// For lights: the light will produce shadows.
/// Requires a light with shadow casting enabled.
///
pub fn cast_shadow(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("cast-shadow", "")
    False -> attribute.property("cast-shadow", json.bool(False))
  }
}
