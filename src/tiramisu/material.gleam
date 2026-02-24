import gleam/float
import gleam/int
import gleam/json

import lustre/attribute.{type Attribute}

// TYPES -----------------------------------------------------------------------

/// Which sides of geometry faces to render.
pub type MaterialSide {
  /// Render front faces only (default).
  FrontSide
  /// Render back faces only.
  BackSide
  /// Render both front and back faces.
  DoubleSide
}

// MATERIAL TYPE ATTRIBUTES ----------------------------------------------------

/// Use the standard (PBR) material with metalness/roughness workflow.
/// This is the default material type.
pub fn standard() -> Attribute(msg) {
  attribute.attribute("material-type", "standard")
}

/// Use the basic (unlit) material. No lighting calculations.
/// Fastest material, useful for UI elements, flat colors, and debugging.
pub fn basic() -> Attribute(msg) {
  attribute.attribute("material-type", "basic")
}

/// Use the phong material with specular highlights.
/// Good for shiny surfaces like plastic or ceramic.
pub fn phong() -> Attribute(msg) {
  attribute.attribute("material-type", "phong")
}

/// Use the lambert material for matte surfaces.
/// Diffuse-only lighting, good for cloth, wood, or concrete.
pub fn lambert() -> Attribute(msg) {
  attribute.attribute("material-type", "lambert")
}

/// Use the toon (cel-shaded) material for cartoon-style rendering.
pub fn toon() -> Attribute(msg) {
  attribute.attribute("material-type", "toon")
}

// ADDITIONAL MATERIAL PROPERTIES ----------------------------------------------

/// Set the shininess of a phong material.
///
/// Controls the size and sharpness of specular highlights.
/// Higher values create smaller, sharper highlights. Default is 30.0.
/// Only applies to phong materials.
pub fn shininess(value: Float) -> Attribute(msg) {
  attribute.attribute("shininess", float.to_string(value))
}

/// Set the alpha test threshold.
///
/// Pixels with alpha below this value are discarded (not rendered).
/// Useful for materials with alpha-tested textures like foliage or fences
/// where you want hard-edged transparency without sorting artifacts.
/// Default is 0.0 (disabled).
///
/// ## Example
///
/// ```gleam
/// tiramisu.mesh("tree", [
///   material.color_map("/textures/leaves.png"),
///   material.alpha_test(0.5),
/// ], [])
/// ```
pub fn alpha_test(value: Float) -> Attribute(msg) {
  attribute.attribute("alpha-test", float.to_string(value))
}

/// Enable transparency on the material.
///
/// Normally transparency is derived from opacity being less than 1.0.
/// Use this attribute when you need transparency with opacity=1.0,
/// such as for alpha-tested textures or materials with transparent regions
/// in their color map.
pub fn transparent(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("transparent", "")
    False -> attribute.property("transparent", json.bool(False))
  }
}

// MATERIAL PROPERTIES ---------------------------------------------------------

/// Set the emissive (glow) color as a hex integer.
///
/// Objects with emissive colors appear to emit light. Works best with
/// `emissive_intensity` and bloom post-processing.
///
/// ## Example
///
/// ```gleam
/// material.emissive(0xff0000)  // Red glow
/// ```
pub fn emissive(hex: Int) -> Attribute(msg) {
  attribute.attribute("emissive", "#" <> int.to_base16(hex))
}

/// Set the emissive intensity (glow brightness).
///
/// 0.0 = no emission, higher values = brighter glow.
pub fn emissive_intensity(intensity: Float) -> Attribute(msg) {
  attribute.attribute("emissive-intensity", float.to_string(intensity))
}

/// Set which sides of geometry faces to render.
///
/// ## Example
///
/// ```gleam
/// material.side(material.DoubleSide)  // Render both sides
/// ```
pub fn side(s: MaterialSide) -> Attribute(msg) {
  attribute.attribute("side", case s {
    FrontSide -> "front"
    BackSide -> "back"
    DoubleSide -> "double"
  })
}

// TEXTURE MAP ATTRIBUTES ------------------------------------------------------

/// Set the color/albedo texture map URL.
///
/// The texture modulates the base color. Set base color to white (0xffffff)
/// when using a color map to let the texture define the color entirely.
///
/// ## Example
///
/// ```gleam
/// tiramisu.mesh("textured", [
///   tiramisu.color(0xffffff),
///   material.color_map("/textures/brick.jpg"),
/// ], [])
/// ```
pub fn color_map(url: String) -> Attribute(msg) {
  attribute.attribute("color-map", url)
}

/// Set the normal map URL for surface detail.
///
/// Normal maps add surface details like bumps and grooves without
/// adding geometry.
pub fn normal_map(url: String) -> Attribute(msg) {
  attribute.attribute("normal-map", url)
}

/// Set the ambient occlusion map URL.
///
/// AO maps darken areas where ambient light would be occluded,
/// adding depth to crevices and corners.
pub fn ambient_occlusion_map(url: String) -> Attribute(msg) {
  attribute.attribute("ambient-occlusion-map", url)
}

/// Set the roughness map URL for per-pixel roughness variation.
///
/// Allows different parts of the surface to have different roughness,
/// like scratches on polished metal.
pub fn roughness_map(url: String) -> Attribute(msg) {
  attribute.attribute("roughness-map", url)
}

/// Set the metalness map URL for per-pixel metalness variation.
///
/// Useful for surfaces that are partially metallic, like painted metal
/// with scratches revealing bare metal.
pub fn metalness_map(url: String) -> Attribute(msg) {
  attribute.attribute("metalness-map", url)
}

/// Set the displacement map URL for vertex displacement.
///
/// Unlike normal maps which only affect shading, displacement maps
/// actually move the geometry vertices, creating real bumps and dents.
/// Requires geometry with enough subdivisions (segments) to displace.
///
/// ## Example
///
/// ```gleam
/// tiramisu.mesh("terrain", [
///   mesh.geometry_sphere_simple(2.0),
///   material.displacement_map("/textures/heightmap.jpg"),
///   material.displacement_scale(0.5),
/// ], [])
/// ```
pub fn displacement_map(url: String) -> Attribute(msg) {
  attribute.attribute("displacement-map", url)
}

/// Set the displacement scale (strength of vertex displacement).
///
/// Higher values create more pronounced displacement. Default is 1.0.
pub fn displacement_scale(scale: Float) -> Attribute(msg) {
  attribute.attribute("displacement-scale", float.to_string(scale))
}

/// Set the displacement bias (offset for vertex displacement).
///
/// Shifts the displacement up or down. Default is 0.0.
pub fn displacement_bias(bias: Float) -> Attribute(msg) {
  attribute.attribute("displacement-bias", float.to_string(bias))
}

/// Set the metalness of the material (0.0 = dielectric, 1.0 = metal).
///
/// Works with meshes and instanced meshes.
///
pub fn metalness(m: Float) -> Attribute(msg) {
  attribute.attribute("metalness", float.to_string(m))
}

/// Set the roughness of the material (0.0 = smooth, 1.0 = rough).
///
/// Works with meshes and instanced meshes.
///
pub fn roughness(r: Float) -> Attribute(msg) {
  attribute.attribute("roughness", float.to_string(r))
}

/// Set the opacity of the material (0.0 = transparent, 1.0 = opaque).
///
/// Works with meshes and instanced meshes.
///
pub fn opacity(o: Float) -> Attribute(msg) {
  attribute.attribute("opacity", float.to_string(o))
}

pub fn wireframe(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("wireframe", "")
    False -> attribute.property("wireframe", json.bool(False))
  }
}

/// Enable shadow casting on the element.
///
/// For meshes and instanced meshes: the element will cast shadows.
/// Requires a light with shadow casting enabled.
///
pub fn cast_shadow(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("cast-shadow", "")
    False -> attribute.property("cast-shadow", json.bool(False))
  }
}

/// Enable shadow receiving on the mesh.
///
/// The mesh will show shadows cast by other meshes. Requires a light
/// with shadow casting enabled.
///
pub fn receive_shadow(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("receive-shadow", "")
    False -> attribute.property("receive-shadow", json.bool(False))
  }
}
