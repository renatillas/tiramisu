//// The tiramisu-light element.
////
//// Lights illuminate the scene. The renderer parses these elements from
//// the light DOM and manages the corresponding Three.js lights automatically.
////
//// ## Usage
////
//// ```html
//// <tiramisu-light
////   id="sun"
////   type="directional"
////   color="#ffffff"
////   intensity="1"
////   transform="pos:10,10,10"
////   cast-shadow="true"
//// ></tiramisu-light>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier for the light (required)
//// - `type`: Light type - "ambient", "directional", "point", "spot" (default: "ambient")
//// - `color`: Light color as hex string (default: "#ffffff")
//// - `intensity`: Light intensity (default: 1.0)
//// - `transform`: Transform as "pos:x,y,z quat:x,y,z,w scale:x,y,z" (default: origin)
//// - `cast-shadow`: Whether light casts shadows (default: "false")

// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/int

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

import tiramisu/transform.{type Transform}

import vec/vec3

// CONSTANTS -------------------------------------------------------------------

/// The tag name for light elements.
pub const tag_name = "tiramisu-light"

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-light element.
///
/// Lights illuminate the scene. Different types create different
/// lighting effects.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/light
///
/// light.light("sun", [
///   light.light_type("directional"),
///   light.intensity(1.5),
///   light.cast_shadow(True),
/// ], [])
/// ```
///
pub fn light(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the light type (ambient, directional, point, spot).
///
pub fn light_type(type_: String) -> Attribute(msg) {
  attribute.attribute("type", type_)
}

/// Set the light color (as hex integer).
///
pub fn color(hex: Int) -> Attribute(msg) {
  attribute.attribute("color", "#" <> int.to_base16(hex))
}

/// Set the light color (as hex string).
///
pub fn color_string(hex: String) -> Attribute(msg) {
  attribute.attribute("color", hex)
}

/// Set the intensity of the light.
///
pub fn intensity(value: Float) -> Attribute(msg) {
  attribute.attribute("intensity", float.to_string(value))
}

/// Set whether the light casts shadows.
///
pub fn cast_shadow(enabled: Bool) -> Attribute(msg) {
  attribute.attribute("cast-shadow", case enabled {
    True -> ""
    False -> "false"
  })
}

/// Set the full transform of the light.
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
