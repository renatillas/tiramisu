//// The tiramisu-empty element.
////
//// Empty nodes are invisible groups used for hierarchical organization.
//// Children of an empty are transformed relative to it in the Three.js
//// scene graph.
////
//// ## Usage
////
//// ```html
//// <tiramisu-empty id="enemies" transform="pos:0,0,-10">
////   <tiramisu-mesh id="enemy1" geometry="box:1,1,1" transform="pos:-2,0,0"></tiramisu-mesh>
////   <tiramisu-mesh id="enemy2" geometry="box:1,1,1" transform="pos:2,0,0"></tiramisu-mesh>
//// </tiramisu-empty>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier for the group (required)
//// - `transform`: Transform as "pos:x,y,z quat:x,y,z,w scale:x,y,z" (default: origin)
//// - `visible`: Visibility (default: "true")

// IMPORTS ---------------------------------------------------------------------

import gleam/float

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

import tiramisu/transform.{type Transform}

import vec/vec3

// CONSTANTS -------------------------------------------------------------------

/// The tag name for empty (group) elements.
pub const tag_name = "tiramisu-empty"

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-empty element (group).
///
/// Empty nodes are invisible groups used for hierarchical organization.
/// Children of an empty are transformed relative to it.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/empty
/// import tiramisu/mesh
///
/// empty.empty("enemies", [], [
///   mesh.mesh("enemy1", [...], []),
///   mesh.mesh("enemy2", [...], []),
/// ])
/// ```
///
pub fn empty(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the full transform of the group.
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

/// Set the group's visibility.
///
pub fn visible(is_visible: Bool) -> Attribute(msg) {
  attribute.attribute("visible", case is_visible {
    True -> "true"
    False -> "false"
  })
}
