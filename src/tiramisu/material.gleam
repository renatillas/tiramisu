//// Material attributes for tiramisu-mesh elements.
////
//// Materials define how surfaces are rendered and interact with light.
//// These attribute functions set material properties on mesh elements
//// via DOM attributes.
////
//// ## Material Types
////
//// - **standard** (default): Physically-based (PBR) with metalness/roughness
//// - **basic**: Unlit, no lighting calculations (fastest)
//// - **phong**: Shiny surfaces with specular highlights
//// - **lambert**: Matte surfaces, diffuse-only (fast)
//// - **toon**: Cel-shaded cartoon style
////
//// ## Example
////
//// ```gleam
//// import tiramisu/mesh
//// import tiramisu/material
////
//// mesh.mesh("shiny-cube", [
////   mesh.geometry_box(vec3.Vec3(2.0, 2.0, 2.0)),
////   mesh.color(0xff6b6b),
////   material.standard(),
////   material.emissive(0xff0000),
////   material.emissive_intensity(0.5),
////   material.side(material.DoubleSide),
//// ], [])
//// ```
////
//// ## Texture Maps
////
//// ```gleam
//// mesh.mesh("textured", [
////   mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
////   material.color_map("/textures/brick_color.jpg"),
////   material.normal_map("/textures/brick_normal.jpg"),
//// ], [])
//// ```

import gleam/float
import gleam/int

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
/// mesh.mesh("tree", [
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
pub fn transparent() -> Attribute(msg) {
  attribute.attribute("transparent", "")
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

/// Set the emissive (glow) color as a hex string.
pub fn emissive_string(hex: String) -> Attribute(msg) {
  attribute.attribute("emissive", hex)
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
/// mesh.mesh("textured", [
///   mesh.color(0xffffff),
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
pub fn ao_map(url: String) -> Attribute(msg) {
  attribute.attribute("ao-map", url)
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
/// mesh.mesh("terrain", [
///   mesh.geometry_sphere(2.0),
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
