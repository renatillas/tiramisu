//// The tiramisu-debug element.
////
//// Debug helpers for visualizing axes, grids, and bounding boxes.
//// These are immutable visual aids — any attribute change causes a full
//// remove + recreate.
////
//// ## Usage
////
//// ```html
//// <tiramisu-debug id="axes" type="axes" size="5"></tiramisu-debug>
//// <tiramisu-debug id="grid" type="grid" size="20" divisions="20" color="#444444"></tiramisu-debug>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier (required)
//// - `type`: Helper type — "axes" or "grid" (default: "axes")
//// - `size`: Size of the helper (default: 5.0)
//// - `divisions`: Grid divisions, only for "grid" type (default: 10)
//// - `color`: Color for grid lines as hex (default: "#888888")
//// - `transform`: Transform as "pos:x,y,z quat:x,y,z,w scale:x,y,z"

import gleam/float
import gleam/int

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

import tiramisu/transform.{type Transform}

import vec/vec3

// CONSTANTS -------------------------------------------------------------------

/// The tag name for debug elements.
pub const tag_name = "tiramisu-debug"

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-debug element.
///
/// Debug helpers are immutable visual aids for development.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
///
/// debug.debug("axes-helper", [debug.debug_type("axes"), debug.size(5.0)])
/// ```
///
pub fn debug(id: String, attributes: List(Attribute(msg))) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], [])
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the debug helper type ("axes" or "grid").
///
pub fn debug_type(t: String) -> Attribute(msg) {
  attribute.attribute("type", t)
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

/// Set the color as a hex string (e.g., "#888888").
///
pub fn color_string(c: String) -> Attribute(msg) {
  attribute.attribute("color", c)
}

/// Set the color as a hex integer (e.g., 0x888888).
///
pub fn color(c: Int) -> Attribute(msg) {
  attribute.attribute("color", "#" <> int.to_base16(c))
}

/// Set the full transform of the debug helper.
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
