//// The tiramisu-instanced-mesh element.
////
//// InstancedMesh renders many copies of the same geometry+material in a
//// single draw call. Instance transforms are provided as a serialized string.
////
//// ## Usage
////
//// ```html
//// <tiramisu-instanced-mesh
////   id="trees"
////   geometry="sphere:1,16,8"
////   color="#00ff00"
////   instances="0,0,0,0,0,0,1,1,1|5,0,3,0,0,0,1,1,1|10,0,-2,0,0,0,1,1,1"
//// ></tiramisu-instanced-mesh>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier (required)
//// - `geometry`: Geometry specification (e.g., "box:1,1,1", "sphere:0.5,16,8")
//// - `material-type`: Material type (default: "standard")
//// - `color`: Hex color (default: "#ffffff")
//// - `metalness`: Material metalness 0.0-1.0 (default: 0.5)
//// - `roughness`: Material roughness 0.0-1.0 (default: 0.5)
//// - `opacity`: Material opacity 0.0-1.0 (default: 1.0)
//// - `wireframe`: Enable wireframe mode (default: "false")
//// - `transparent`: Force transparency (default: "false")
//// - `instances`: Pipe-delimited instance transforms — "x,y,z,rx,ry,rz,sx,sy,sz|..."
//// - `transform`: Transform for the entire instanced mesh group
//// - `visible`: Visibility (default: "true")
//// - `cast-shadow`: Enable shadow casting (default: "false")
//// - `receive-shadow`: Enable shadow receiving (default: "false")

import gleam/float
import gleam/int
import gleam/list
import gleam/string

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

import tiramisu/transform.{type Transform}

import vec/vec3

// CONSTANTS -------------------------------------------------------------------

/// The tag name for instanced mesh elements.
pub const tag_name = "tiramisu-instanced-mesh"

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-instanced-mesh element.
///
/// InstancedMesh renders many copies of the same geometry+material efficiently.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/instanced_mesh
///
/// instanced_mesh.instanced_mesh("forest", [
///   instanced_mesh.geometry("sphere:1,16,8"),
///   instanced_mesh.color(0x00ff00),
///   instanced_mesh.instances([
///     #(vec3.Vec3(0.0, 0.0, 0.0), vec3.Vec3(0.0, 0.0, 0.0), vec3.Vec3(1.0, 1.0, 1.0)),
///     #(vec3.Vec3(5.0, 0.0, 3.0), vec3.Vec3(0.0, 0.0, 0.0), vec3.Vec3(1.0, 1.0, 1.0)),
///   ]),
/// ], [])
/// ```
///
pub fn instanced_mesh(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the geometry specification string.
///
pub fn geometry(spec: String) -> Attribute(msg) {
  attribute.attribute("geometry", spec)
}

/// Set the material type ("standard", "basic", "phong", "lambert", "toon").
///
pub fn material_type(t: String) -> Attribute(msg) {
  attribute.attribute("material-type", t)
}

/// Set the base color as a hex integer.
///
pub fn color(hex: Int) -> Attribute(msg) {
  attribute.attribute("color", "#" <> int.to_base16(hex))
}

/// Set the base color as a hex string.
///
pub fn color_string(hex: String) -> Attribute(msg) {
  attribute.attribute("color", hex)
}

/// Set the metalness of the material (0.0-1.0).
///
pub fn metalness(m: Float) -> Attribute(msg) {
  attribute.attribute("metalness", float.to_string(m))
}

/// Set the roughness of the material (0.0-1.0).
///
pub fn roughness(r: Float) -> Attribute(msg) {
  attribute.attribute("roughness", float.to_string(r))
}

/// Set the opacity of the material (0.0-1.0).
///
pub fn opacity(o: Float) -> Attribute(msg) {
  attribute.attribute("opacity", float.to_string(o))
}

/// Enable wireframe rendering.
///
pub fn wireframe() -> Attribute(msg) {
  attribute.attribute("wireframe", "")
}

/// Force transparency on the material.
///
pub fn transparent() -> Attribute(msg) {
  attribute.attribute("transparent", "")
}

/// Set the instance transforms from a list of (position, rotation, scale) tuples.
///
/// Each tuple contains three Vec3 values:
/// - Position (x, y, z)
/// - Rotation in radians (rx, ry, rz)
/// - Scale (sx, sy, sz)
///
pub fn instances(
  transforms: List(#(vec3.Vec3(Float), vec3.Vec3(Float), vec3.Vec3(Float))),
) -> Attribute(msg) {
  let value =
    transforms
    |> list.map(fn(t) {
      let #(vec3.Vec3(px, py, pz), vec3.Vec3(rx, ry, rz), vec3.Vec3(sx, sy, sz)) =
        t
      [px, py, pz, rx, ry, rz, sx, sy, sz]
      |> list.map(float.to_string)
      |> string.join(",")
    })
    |> string.join("|")
  attribute.attribute("instances", value)
}

/// Set the instance transforms from a raw serialized string.
///
/// Format: "x,y,z,rx,ry,rz,sx,sy,sz|x,y,z,rx,ry,rz,sx,sy,sz|..."
///
pub fn instances_raw(raw: String) -> Attribute(msg) {
  attribute.attribute("instances", raw)
}

/// Set the full transform of the instanced mesh group.
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

/// Set the visibility of the instanced mesh.
///
pub fn visible(v: Bool) -> Attribute(msg) {
  attribute.attribute("visible", case v {
    True -> ""
    False -> "false"
  })
}

/// Enable shadow casting.
///
pub fn cast_shadow() -> Attribute(msg) {
  attribute.attribute("cast-shadow", "")
}

/// Enable shadow receiving.
///
pub fn receive_shadow() -> Attribute(msg) {
  attribute.attribute("receive-shadow", "")
}
