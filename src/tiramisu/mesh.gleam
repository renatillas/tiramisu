//// The tiramisu-mesh element.
////
//// Meshes are 3D objects with geometry and material. The renderer parses
//// these elements from the light DOM and manages the corresponding Three.js
//// objects automatically.
////
//// ## Usage
////
//// ```html
//// <tiramisu-mesh
////   id="cube"
////   geometry="box:2,2,2"
////   color="#ff6b6b"
////   metalness="0.5"
////   roughness="0.5"
////   transform="pos:0,1,0"
//// ></tiramisu-mesh>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier for the mesh (required)
//// - `geometry`: Geometry specification (e.g., "box:1,1,1", "sphere:0.5,32,16")
//// - `src`: URL for an external 3D model (GLTF/GLB/FBX)
//// - `color`: Hex color (e.g., "#ff0000")
//// - `metalness`: Material metalness 0.0-1.0 (default: 0.0)
//// - `roughness`: Material roughness 0.0-1.0 (default: 0.5)
//// - `opacity`: Material opacity 0.0-1.0 (default: 1.0)
//// - `wireframe`: Enable wireframe mode (default: "false")
//// - `transform`: Transform as "pos:x,y,z quat:x,y,z,w scale:x,y,z" (parts optional)
//// - `visible`: Visibility (default: "true")
//// - `physics-controlled`: Whether transform is driven by physics (default: "false")

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode
import gleam/float
import gleam/int

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}
import lustre/event

import tiramisu/transform.{type Transform}

import vec/vec2
import vec/vec3

// CONSTANTS -------------------------------------------------------------------

/// The tag name for mesh elements.
pub const tag_name = "tiramisu-mesh"

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-mesh element.
///
/// Meshes are 3D objects with geometry and material. Use attributes to
/// configure their shape, appearance, and transform.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/mesh
///
/// mesh.mesh("player", [
///   mesh.geometry_box(1.0, 2.0, 1.0),
///   mesh.color(0x00ff00),
/// ], [])
/// ```
///
pub fn mesh(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the source URL for an external 3D model (GLTF/GLB/FBX).
///
/// When src is set, the mesh will load the external model instead of
/// creating a primitive geometry. Supports GLTF, GLB, and FBX formats.
///
pub fn src(url: String) -> Attribute(msg) {
  attribute.attribute("src", url)
}

/// Set a box geometry with width, height, and depth.
///
pub fn geometry_box(shape: vec3.Vec3(Float)) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "box:"
      <> float.to_string(shape.x)
      <> ","
      <> float.to_string(shape.y)
      <> ","
      <> float.to_string(shape.z),
  )
}

/// Set a sphere geometry with radius and segments.
///
pub fn geometry_sphere(
  radius: Float,
  segments: vec2.Vec2(Int),
) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "sphere:"
      <> float.to_string(radius)
      <> ","
      <> int.to_string(segments.x)
      <> ","
      <> int.to_string(segments.y),
  )
}

/// Set a sphere geometry with just radius (default segments).
///
pub fn geometry_sphere_simple(radius: Float) -> Attribute(msg) {
  attribute.attribute("geometry", "sphere:" <> float.to_string(radius))
}

/// Set a plane geometry with width and height.
///
pub fn geometry_plane(size: vec2.Vec2(Float)) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "plane:" <> float.to_string(size.x) <> "," <> float.to_string(size.y),
  )
}

/// Set a cylinder geometry.
///
pub fn geometry_cylinder(
  radius_top radius_top: Float,
  radius_bottom radius_bottom: Float,
  height height: Float,
  segments segments: Int,
) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "cylinder:"
      <> float.to_string(radius_top)
      <> ","
      <> float.to_string(radius_bottom)
      <> ","
      <> float.to_string(height)
      <> ","
      <> int.to_string(segments),
  )
}

/// Set a cylinder geometry with uniform radius.
///
pub fn geometry_cylinder_simple(radius: Float, height: Float) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "cylinder:" <> float.to_string(radius) <> "," <> float.to_string(height),
  )
}

/// Set a cone geometry.
///
pub fn geometry_cone(
  radius radius: Float,
  height height: Float,
  segments segments: Int,
) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "cone:"
      <> float.to_string(radius)
      <> ","
      <> float.to_string(height)
      <> ","
      <> int.to_string(segments),
  )
}

/// Set a cone geometry with default segments.
///
pub fn geometry_cone_simple(radius: Float, height: Float) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "cone:" <> float.to_string(radius) <> "," <> float.to_string(height),
  )
}

/// Set a torus geometry.
///
pub fn geometry_torus(
  radius radius: Float,
  tube tube: Float,
  radial_segments radial_segments: Int,
  tubular_segments tubular_segments: Int,
) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "torus:"
      <> float.to_string(radius)
      <> ","
      <> float.to_string(tube)
      <> ","
      <> int.to_string(radial_segments)
      <> ","
      <> int.to_string(tubular_segments),
  )
}

/// Set a torus geometry with default segments.
///
pub fn geometry_torus_simple(radius: Float, tube: Float) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "torus:" <> float.to_string(radius) <> "," <> float.to_string(tube),
  )
}

/// Set the base color of the material (as a hex integer).
///
/// ## Example
///
/// ```gleam
/// mesh.color(0xff0000)  // Red
/// ```
///
pub fn color(hex hex: Int) -> Attribute(msg) {
  attribute.attribute("color", "#" <> int.to_base16(hex))
}

/// Set the base color of the material (as a hex string).
///
pub fn color_string(hex hex: String) -> Attribute(msg) {
  attribute.attribute("color", hex)
}

/// Set the metalness of the material (0.0 = dielectric, 1.0 = metal).
///
pub fn metalness(m: Float) -> Attribute(msg) {
  attribute.attribute("metalness", float.to_string(m))
}

/// Set the roughness of the material (0.0 = smooth, 1.0 = rough).
///
pub fn roughness(r: Float) -> Attribute(msg) {
  attribute.attribute("roughness", float.to_string(r))
}

/// Set the opacity of the material (0.0 = transparent, 1.0 = opaque).
///
pub fn opacity(o: Float) -> Attribute(msg) {
  attribute.attribute("opacity", float.to_string(o))
}

/// Enable or disable wireframe rendering.
///
pub fn wireframe() -> Attribute(msg) {
  attribute.attribute("wireframe", "")
}

/// Set the full transform of the mesh.
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

/// Set the mesh's visibility.
///
pub fn visible() -> Attribute(msg) {
  attribute.attribute("visible", "")
}

// EVENTS ----------------------------------------------------------------------

/// Listen for the model-loaded event.
///
/// This event fires when an external 3D model (set via `src`) has finished
/// loading. The handler receives the mesh ID.
///
/// ## Example
///
/// ```gleam
/// mesh.mesh("character", [
///   mesh.src("/models/character.glb"),
///   mesh.on_model_loaded(fn(id) { ModelLoaded(id) }),
/// ], [])
/// ```
///
pub fn on_model_loaded(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("tiramisu:model-loaded", {
    use id <- decode.subfield(["detail", "id"], decode.string)
    decode.success(handler(id))
  })
}

/// Listen for the model-error event.
///
/// This event fires when an external 3D model fails to load.
/// The handler receives the mesh ID.
///
pub fn on_model_error(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("tiramisu:model-error", {
    use id <- decode.subfield(["detail", "id"], decode.string)
    decode.success(handler(id))
  })
}
