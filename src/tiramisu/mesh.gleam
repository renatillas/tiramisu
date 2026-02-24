import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json

import lustre/attribute.{type Attribute}
import lustre/event

import vec/vec2
import vec/vec3

@internal
pub const tag = "tiramisu-mesh"

// GEOMETRY ATTRIBUTES ---------------------------------------------------------

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
pub fn sphere(
  radius radius: Float,
  segments segments: vec2.Vec2(Int),
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

/// Set a plane geometry with width and height.
///
pub fn plane(size: vec2.Vec2(Float)) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "plane:" <> float.to_string(size.x) <> "," <> float.to_string(size.y),
  )
}

/// Set a cylinder geometry.
///
pub fn cylinder(
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

/// Set a cone geometry.
///
pub fn cone(
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

/// Set a torus geometry.
///
pub fn torus(
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

// OTHER MESH ATTRIBUTES -------------------------------------------------------

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

// EVENTS ----------------------------------------------------------------------

/// Listen for the model-loaded event.
///
/// This event fires when an external 3D model (set via `src`) has finished
/// loading. The handler receives the mesh ID.
///
/// ## Example
///
/// ```gleam
/// tiramisu.mesh("character", [
///   attribute.src("/models/character.glb"),
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
