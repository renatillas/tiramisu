import gleam/int
import gleam/json
import gleam/list
import gleam/string
import tiramisu/transform

import lustre/attribute.{type Attribute}

@internal
pub const tag = "tiramisu-instanced-mesh"

// INSTANCED MESH ATTRIBUTES ---------------------------------------------------

/// Set the geometry specification string for an instanced mesh.
///
pub fn geometry(spec: String) -> Attribute(msg) {
  attribute.attribute("geometry", spec)
}

/// Set the instance transforms from a list of (position, rotation, scale) tuples.
///
/// Each tuple contains three Vec3 values:
/// - Position (x, y, z)
/// - Rotation in radians (rx, ry, rz)
/// - Scale (sx, sy, sz)
///
pub fn instances(transforms: List(transform.Transform)) -> Attribute(msg) {
  transforms
  |> list.map(transform.to_string)
  |> string.join("|")
  |> attribute.attribute("instances", _)
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

/// Set the element's visibility.
///
/// Works with meshes, empties, and instanced meshes.
///
pub fn visible(is_visible: Bool) -> Attribute(msg) {
  case is_visible {
    True -> attribute.attribute("visible", "")
    False -> attribute.property("visible", json.bool(False))
  }
}
