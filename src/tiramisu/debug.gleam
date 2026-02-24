import gleam/float
import gleam/int

import lustre/attribute.{type Attribute}

@internal
pub const tag = "tiramisu-debug"

pub type Debug {
  Axes
  Grid
}

// DEBUG ATTRIBUTES ------------------------------------------------------------

/// Set the debug helper type ("axes" or "grid").
///
pub fn debug_type(debug: Debug) -> Attribute(msg) {
  attribute.attribute("type", case debug {
    Axes -> "axes"
    Grid -> "grid"
  })
}

/// Set the size of the debug helper.
///
pub fn size(s: Float) -> Attribute(msg) {
  attribute.attribute("size", float.to_string(s))
}

/// Set the number of grid divisions (only for "grid" type).
///
pub fn divisions(d: Int) -> Attribute(msg) {
  attribute.attribute("divisions", int.to_string(d))
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
