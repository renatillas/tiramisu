//// Material system for defining how 3D surfaces appear when rendered.
////
//// Materials control the visual appearance of meshes by defining how they interact with lights
//// and what textures they display. Tiramisu provides several material types with different
//// performance and visual characteristics.
////
//// ## Material Types
////
//// - **StandardMaterial**: Physically-based rendering (PBR) with metalness/roughness workflow - most realistic
//// - **PhongMaterial**: Specular highlights for shiny plastic or ceramic
//// - **LambertMaterial**: Matte diffuse surfaces like cloth or wood
//// - **ToonMaterial**: Cartoon-style cel-shaded rendering
//// - **BasicMaterial**: Unlit surfaces (no lighting calculations) - fastest
//// - **SpriteMaterial**: 2D billboards that face the camera
//// - **LineMaterial**: For rendering lines
////
//// ## Builder Pattern
////
//// For StandardMaterial (the most common choice), use the builder pattern for cleaner code:
////
//// ```gleam
//// import tiramisu/material
//// import gleam/option
////
//// // Builder pattern (recommended)
//// let assert Ok(metal) = material.new()
////   |> material.with_color(0xcccccc)
////   |> material.with_metalness(1.0)
////   |> material.with_roughness(0.3)
////   |> material.build()
////
//// // Direct constructor (more explicit)
//// let assert Ok(plastic) = material.standard(
////   color: 0xff0000,
////   metalness: 0.0,
////   roughness: 0.5,
////   transparent: False,
////   opacity: 1.0,
////   map: option.None,
////   normal_map: option.None,
////   ambient_oclusion_map: option.None,
////   roughness_map: option.None,
////   metalness_map: option.None,
//// )
//// ```
////
//// ## Texture Mapping
////
//// All materials (except LineMaterial) support textures:
////
//// ```gleam
//// import tiramisu/asset
////
//// // Load textures
//// let assert Ok(color_tex) = asset.get_texture(cache, "brick_color.jpg")
//// let assert Ok(normal_tex) = asset.get_texture(cache, "brick_normal.jpg")
////
//// // Apply to material
//// let assert Ok(brick_material) = material.new()
////   |> material.with_color_map(color_tex)
////   |> material.with_normal_map(normal_tex)
////   |> material.with_roughness(0.8)
////   |> material.build()
//// ```
////
//// ## Material Examples
////
//// ### Metallic Surfaces
//// ```gleam
//// // Gold
//// let assert Ok(gold) = material.new()
////   |> material.with_color(0xffd700)
////   |> material.with_metalness(1.0)
////   |> material.with_roughness(0.3)
////   |> material.build()
////
//// // Brushed steel
//// let assert Ok(steel) = material.new()
////   |> material.with_color(0xaaaaaa)
////   |> material.with_metalness(1.0)
////   |> material.with_roughness(0.5)
////   |> material.build()
//// ```
////
//// ### Non-Metallic Surfaces
//// ```gleam
//// // Plastic
//// let assert Ok(plastic) = material.new()
////   |> material.with_color(0xff0000)
////   |> material.with_metalness(0.0)
////   |> material.with_roughness(0.4)
////   |> material.build()
////
//// // Matte wood (Lambert)
//// let assert Ok(wood) = material.lambert(
////   color: 0x8b4513,
////   map: option.None,
////   normal_map: option.None,
////   ambient_oclusion_map: option.None,
//// )
//// ```
////
//// ### Special Effects
//// ```gleam
//// // Glass (transparent)
//// let assert Ok(glass) = material.new()
////   |> material.with_color(0x88ccff)
////   |> material.with_metalness(0.0)
////   |> material.with_roughness(0.0)
////   |> material.with_transparent(True)
////   |> material.with_opacity(0.3)
////   |> material.build()
////
//// // Cartoon cel-shading
//// let assert Ok(toon) = material.toon(
////   color: 0xff0000,
////   map: option.None,
////   normal_map: option.None,
////   ambient_oclusion_map: option.None,
//// )
//// ```

import gleam/bool
import gleam/option.{type Option}
import tiramisu/asset

/// Material types for rendering objects.
///
/// Materials define how surfaces appear when rendered. Different materials
/// have different performance characteristics and visual properties.
///
/// ## Performance
///
/// - `BasicMaterial`: Fastest, no lighting calculations
/// - `LambertMaterial`, `ToonMaterial`: Fast, simple lighting
/// - `PhongMaterial`: Medium, specular highlights
/// - `StandardMaterial`: Physically-based, most realistic but slower
pub opaque type Material {
  /// Unlit material (no lighting calculations). Fast and useful for flat-shaded objects.
  BasicMaterial(
    color: Int,
    map: Option(asset.Texture),
    transparent: Bool,
    opacity: Float,
  )
  /// Physically-based material with metalness/roughness workflow. Most realistic.
  StandardMaterial(
    color: Int,
    map: Option(asset.Texture),
    normal_map: Option(asset.Texture),
    ambient_oclusion_map: Option(asset.Texture),
    roughness_map: Option(asset.Texture),
    metalness_map: Option(asset.Texture),
    metalness: Float,
    roughness: Float,
    transparent: Bool,
    opacity: Float,
    emissive: Int,
    emissive_intensity: Float,
  )
  /// Shiny material with specular highlights (like plastic or ceramic).
  PhongMaterial(
    color: Int,
    map: Option(asset.Texture),
    normal_map: Option(asset.Texture),
    ambient_oclusion_map: Option(asset.Texture),
    shininess: Float,
  )
  /// Matte material (like cloth or wood). Non-shiny diffuse lighting.
  LambertMaterial(
    color: Int,
    map: Option(asset.Texture),
    normal_map: Option(asset.Texture),
    ambient_oclusion_map: Option(asset.Texture),
    transparent: Bool,
    opacity: Float,
    alpha_test: Float,
  )
  /// Cartoon-style material with banded shading.
  ToonMaterial(
    color: Int,
    map: Option(asset.Texture),
    normal_map: Option(asset.Texture),
    ambient_oclusion_map: Option(asset.Texture),
  )
  /// Material for rendering lines.
  LineMaterial(color: Int, linewidth: Float)
  /// Material for 2D sprites that always face the camera.
  SpriteMaterial(
    color: Int,
    map: Option(asset.Texture),
    transparent: Bool,
    opacity: Float,
  )
}

/// Which sides of geometry to render.
pub type MaterialSide {
  FrontSide
  BackSide
  DoubleSide
}

pub type MaterialError {
  OutOfBoundsColor(Int)
  OutOfBoundsOpacity(Float)
  OutOfBoundsRoughness(Float)
  OutOfBoundsMetalness(Float)
  NonPositiveLinewidth(Float)
  NonPositiveShininess(Float)
  OutOfBoundsEmissive(Int)
  OutOfBoundsEmissiveIntensity(Float)
}

/// Create a validated basic (unlit) material.
///
/// Basic materials don't react to lights, making them very fast to render.
/// Opacity must be between 0.0 (fully transparent) and 1.0 (fully opaque).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(red) = material.basic(color: 0xff0000, transparent: False, opacity: 1.0, map: option.None)
/// let assert Ok(glass) = material.basic(color: 0x88ccff, transparent: True, opacity: 0.5, map: option.None)
/// ```
pub fn basic(
  color color: Int,
  transparent transparent: Bool,
  opacity opacity: Float,
  map map: option.Option(asset.Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(
    opacity <. 0.0 || opacity >. 1.0,
    Error(OutOfBoundsOpacity(opacity)),
  )

  Ok(BasicMaterial(color:, transparent:, opacity:, map:))
}

/// Create a validated physically-based (PBR) standard material.
///
/// Standard materials use metalness/roughness workflow for realistic rendering.
/// - Metalness: 0.0 = dielectric (plastic, wood), 1.0 = metal
/// - Roughness: 0.0 = mirror-smooth, 1.0 = completely rough
///
/// ## Example
///
/// ```gleam
/// let assert Ok(gold) = scene.standard_material(color: 0xffd700, metalness: 1.0, roughness: 0.3, transparent: False, opacity: 1.0, map: option.None, normal_map: option.None, ao_map: option.None, roughness_map: option.None, metalness_map: option.None)
/// let assert Ok(plastic) = scene.standard_material(color: 0xff0000, metalness: 0.0, roughness: 0.5, transparent: False, opacity: 1.0, map: option.None, normal_map: option.None, ao_map: option.None, roughness_map: option.None, metalness_map: option.None)
/// ```
pub fn standard(
  color color: Int,
  metalness metalness: Float,
  roughness roughness: Float,
  transparent transparent: Bool,
  opacity opacity: Float,
  map map: option.Option(asset.Texture),
  normal_map normal_map: option.Option(asset.Texture),
  ambient_oclusion_map ambient_oclusion_map: option.Option(asset.Texture),
  roughness_map roughness_map: option.Option(asset.Texture),
  metalness_map metalness_map: option.Option(asset.Texture),
  emissive emissive: Int,
  emissive_intensity emissive_intensity: Float,
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(
    metalness <. 0.0 || metalness >. 1.0,
    Error(OutOfBoundsMetalness(metalness)),
  )
  use <- bool.guard(
    roughness <. 0.0 || roughness >. 1.0,
    Error(OutOfBoundsRoughness(roughness)),
  )
  use <- bool.guard(
    opacity <. 0.0 || opacity >. 1.0,
    Error(OutOfBoundsOpacity(opacity)),
  )
  use <- bool.guard(
    emissive < 0x000000 || emissive > 0xffffff,
    Error(OutOfBoundsEmissive(emissive)),
  )
  use <- bool.guard(
    emissive_intensity <. 0.0,
    Error(OutOfBoundsEmissiveIntensity(emissive_intensity)),
  )

  Ok(StandardMaterial(
    color:,
    metalness:,
    roughness:,
    transparent:,
    opacity:,
    map:,
    normal_map:,
    roughness_map:,
    metalness_map:,
    ambient_oclusion_map:,
    emissive:,
    emissive_intensity:,
  ))
}

/// Create a validated line material for rendering lines.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(line_mat) = scene.line_material(color: 0xff0000, linewidth: 2.0)
/// ```
pub fn line(
  color color: Int,
  linewidth linewidth: Float,
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(linewidth <=. 0.0, Error(NonPositiveLinewidth(linewidth)))

  Ok(LineMaterial(color, linewidth))
}

/// Create a validated sprite material for 2D billboards.
///
/// Sprites always face the camera and are useful for particles, UI elements, etc.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(sprite_mat) = material.sprite(color: 0xffffff, transparent: True, opacity: 0.8, map: option.None)
/// ```
pub fn sprite(
  color color: Int,
  transparent transparent: Bool,
  opacity opacity: Float,
  map map: option.Option(asset.Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(
    opacity <. 0.0 || opacity >. 1.0,
    Error(OutOfBoundsOpacity(opacity)),
  )

  Ok(SpriteMaterial(color:, transparent:, opacity:, map:))
}

/// Create a Lambert material for matte, non-shiny surfaces.
///
/// Lambert materials use diffuse-only lighting with no specular highlights, making them
/// ideal for cloth, wood, concrete, or other matte surfaces. Cheaper than Phong or Standard.
///
/// **Color**: Base color without lighting (0x000000 to 0xFFFFFF).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/material
/// import gleam/option
///
/// // Matte red cloth
/// let assert Ok(cloth) = material.lambert(
///   color: 0xcc0000,
///   map: option.None,
///   normal_map: option.None,
///   ambient_oclusion_map: option.None,
/// )
///
/// // Wood with texture
/// let assert Ok(color_tex) = asset.get_texture(cache, "wood.jpg")
/// let assert Ok(wood) = material.lambert(
///   color: 0xffffff,  // White base color (texture provides color)
///   map: option.Some(color_tex),
///   normal_map: option.None,
///   ambient_oclusion_map: option.None,
/// )
/// ```
pub fn lambert(
  color color: Int,
  map map: Option(asset.Texture),
  normal_map normal_map: Option(asset.Texture),
  ambient_oclusion_map ambient_oclusion_map: Option(asset.Texture),
  transparent transparent: Bool,
  opacity opacity: Float,
  alpha_test alpha_test: Float,
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(
    opacity <. 0.0 || opacity >. 1.0,
    Error(OutOfBoundsOpacity(opacity)),
  )
  use <- bool.guard(
    alpha_test <. 0.0 || alpha_test >. 1.0,
    Error(OutOfBoundsOpacity(alpha_test)),
  )
  Ok(LambertMaterial(
    color:,
    map:,
    normal_map:,
    ambient_oclusion_map:,
    transparent:,
    opacity:,
    alpha_test:,
  ))
}

/// Create a Phong material for shiny surfaces with specular highlights.
///
/// Phong materials add specular highlights to simulate shiny surfaces like plastic,
/// ceramic, or polished surfaces. More expensive than Lambert but cheaper than Standard.
///
/// **Color**: Base diffuse color (0x000000 to 0xFFFFFF).
/// **Shininess**: Specular highlight size. Higher = smaller, sharper highlight (typical: 30-100).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/material
/// import gleam/option
///
/// // Shiny red plastic
/// let assert Ok(plastic) = material.phong(
///   color: 0xff0000,
///   shininess: 80.0,
///   map: option.None,
///   normal_map: option.None,
///   ambient_oclusion_map: option.None,
/// )
///
/// // Ceramic with low shininess (larger highlight)
/// let assert Ok(ceramic) = material.phong(
///   color: 0xf5f5dc,
///   shininess: 30.0,
///   map: option.None,
///   normal_map: option.None,
///   ambient_oclusion_map: option.None,
/// )
/// ```
pub fn phong(
  color color: Int,
  shininess shininess: Float,
  map map: Option(asset.Texture),
  normal_map normal_map: Option(asset.Texture),
  ambient_oclusion_map ambient_oclusion_map: Option(asset.Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(shininess <. 0.0, Error(NonPositiveShininess(shininess)))
  Ok(PhongMaterial(color:, shininess:, map:, normal_map:, ambient_oclusion_map:))
}

/// Create a Toon material for cartoon-style cel-shaded rendering.
///
/// Toon materials create a cartoon/anime aesthetic by using banded shading instead of
/// smooth gradients. Colors are quantized into distinct bands, creating a hand-drawn look.
///
/// **Color**: Base color (0x000000 to 0xFFFFFF).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/material
/// import gleam/option
///
/// // Cartoon character
/// let assert Ok(toon_mat) = material.toon(
///   color: 0xff6b35,
///   map: option.None,
///   normal_map: option.None,
///   ambient_oclusion_map: option.None,
/// )
///
/// // Cel-shaded with texture
/// let assert Ok(color_tex) = asset.get_texture(cache, "character.png")
/// let assert Ok(toon_textured) = material.toon(
///   color: 0xffffff,  // White base (texture provides color)
///   map: option.Some(color_tex),
///   normal_map: option.None,
///   ambient_oclusion_map: option.None,
/// )
/// ```
pub fn toon(
  color color: Int,
  map map: Option(asset.Texture),
  normal_map normal_map: Option(asset.Texture),
  ambient_oclusion_map ambient_oclusion_map: Option(asset.Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  Ok(ToonMaterial(color:, map:, normal_map:, ambient_oclusion_map:))
}

// --- Material Builder Pattern ---

/// Builder for standard (PBR) materials with sensible defaults.
///
/// Use this opaque type with the builder functions to construct StandardMaterials
/// using a fluent interface.
pub opaque type StandardMaterialBuilder {
  StandardMaterialBuilder(
    color: Int,
    metalness: Float,
    roughness: Float,
    transparent: Bool,
    opacity: Float,
    map: Option(asset.Texture),
    normal_map: Option(asset.Texture),
    ambient_oclusion_map: Option(asset.Texture),
    roughness_map: Option(asset.Texture),
    metalness_map: Option(asset.Texture),
    emissive: Int,
    emissive_intensity: Float,
  )
}

/// Create a new StandardMaterial builder with sensible defaults.
///
/// **Default values:**
/// - Color: 0x808080 (medium gray)
/// - Metalness: 0.5 (semi-metallic)
/// - Roughness: 0.5 (semi-rough)
/// - Transparent: False
/// - Opacity: 1.0 (fully opaque)
/// - All texture maps: None
///
/// ## Example
///
/// ```gleam
/// import tiramisu/material
///
/// // Start building a material
/// let assert Ok(metal) = material.new()
///   |> material.with_color(0xcccccc)
///   |> material.with_metalness(1.0)
///   |> material.with_roughness(0.3)
///   |> material.build()
/// ```
pub fn new() -> StandardMaterialBuilder {
  StandardMaterialBuilder(
    color: 0x808080,
    metalness: 0.5,
    roughness: 0.5,
    transparent: False,
    opacity: 1.0,
    map: option.None,
    normal_map: option.None,
    ambient_oclusion_map: option.None,
    roughness_map: option.None,
    metalness_map: option.None,
    emissive: 0x000000,
    emissive_intensity: 0.0,
  )
}

/// Set the base color.
///
/// **Color**: Hex color from 0x000000 (black) to 0xFFFFFF (white).
///
/// ## Example
///
/// ```gleam
/// material.new()
///   |> material.with_color(0xff0000)  // Red
/// ```
pub fn with_color(
  builder: StandardMaterialBuilder,
  color: Int,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, color: color)
}

/// Set the metalness value.
///
/// **Metalness**: 0.0 = non-metal (plastic, wood, fabric), 1.0 = pure metal (gold, steel).
///
/// ## Example
///
/// ```gleam
/// // Metallic surface
/// material.new()
///   |> material.with_metalness(1.0)
///
/// // Non-metallic surface
/// material.new()
///   |> material.with_metalness(0.0)
/// ```
pub fn with_metalness(
  builder: StandardMaterialBuilder,
  metalness: Float,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, metalness: metalness)
}

/// Set the roughness value.
///
/// **Roughness**: 0.0 = mirror-smooth (polished), 1.0 = completely rough (matte).
///
/// ## Example
///
/// ```gleam
/// // Polished chrome
/// material.new()
///   |> material.with_roughness(0.1)
///
/// // Rough concrete
/// material.new()
///   |> material.with_roughness(0.9)
/// ```
pub fn with_roughness(
  builder: StandardMaterialBuilder,
  roughness: Float,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, roughness: roughness)
}

/// Set the color/albedo texture map.
///
/// The texture modulates the base color. Common practice is to set base color to white (0xFFFFFF)
/// when using a color map.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(texture) = asset.get_texture(cache, "brick_color.jpg")
///
/// material.new()
///   |> material.with_color(0xffffff)  // White base
///   |> material.with_color_map(texture)
/// ```
pub fn with_color_map(
  builder: StandardMaterialBuilder,
  map: asset.Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, map: option.Some(map))
}

/// Set the normal map for surface detail.
///
/// Normal maps add surface details like bumps and grooves without adding geometry.
/// They affect how light interacts with the surface.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(normal) = asset.get_texture(cache, "brick_normal.jpg")
///
/// material.new()
///   |> material.with_normal_map(normal)
/// ```
pub fn with_normal_map(
  builder: StandardMaterialBuilder,
  normal_map: asset.Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, normal_map: option.Some(normal_map))
}

/// Set the ambient occlusion map for contact shadows.
///
/// AO maps darken areas where ambient light would be occluded, adding depth and realism
/// to crevices and corners.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(ao) = asset.get_texture(cache, "brick_ao.jpg")
///
/// material.new()
///   |> material.with_ambient_oclusion_map(ao)
/// ```
pub fn with_ambient_oclusion_map(
  builder: StandardMaterialBuilder,
  ambient_oclusion_map: asset.Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(
    ..builder,
    ambient_oclusion_map: option.Some(ambient_oclusion_map),
  )
}

/// Set the roughness map for per-pixel roughness variation.
///
/// Allows different parts of the surface to have different roughness values,
/// like scratches on polished metal or worn areas on wood.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(roughness) = asset.get_texture(cache, "metal_roughness.jpg")
///
/// material.new()
///   |> material.with_roughness_map(roughness)
/// ```
pub fn with_roughness_map(
  builder: StandardMaterialBuilder,
  roughness_map: asset.Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, roughness_map: option.Some(roughness_map))
}

/// Set the metalness map for per-pixel metalness variation.
///
/// Useful for surfaces that are partially metallic, like painted metal with scratches
/// revealing bare metal underneath.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(metalness) = asset.get_texture(cache, "metal_metalness.jpg")
///
/// material.new()
///   |> material.with_metalness_map(metalness)
/// ```
pub fn with_metalness_map(
  builder: StandardMaterialBuilder,
  metalness_map: asset.Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, metalness_map: option.Some(metalness_map))
}

/// Enable or disable transparency.
///
/// When True, the material's opacity value will be used for alpha blending.
/// Set to True when creating glass, water, or semi-transparent effects.
///
/// ## Example
///
/// ```gleam
/// // Glass material
/// material.new()
///   |> material.with_transparent(True)
///   |> material.with_opacity(0.3)
/// ```
pub fn with_transparent(
  builder: StandardMaterialBuilder,
  transparent: Bool,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, transparent: transparent)
}

/// Set the opacity value.
///
/// **Opacity**: 0.0 = fully transparent, 1.0 = fully opaque.
/// Only takes effect when `with_transparent(True)` is set.
///
/// ## Example
///
/// ```gleam
/// // Semi-transparent glass
/// material.new()
///   |> material.with_color(0x88ccff)
///   |> material.with_transparent(True)
///   |> material.with_opacity(0.3)
/// ```
pub fn with_opacity(
  builder: StandardMaterialBuilder,
  opacity: Float,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, opacity: opacity)
}

/// Set the emissive color (glow).
///
/// **Emissive**: Hex color from 0x000000 (no glow) to 0xFFFFFF (white glow).
/// Objects with emissive colors appear to emit light and are perfect for bloom effects.
///
/// ## Example
///
/// ```gleam
/// // Glowing red cube
/// material.new()
///   |> material.with_color(0xff0000)
///   |> material.with_emissive(0xff0000)
///   |> material.with_emissive_intensity(0.5)
/// ```
pub fn with_emissive(
  builder: StandardMaterialBuilder,
  emissive: Int,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, emissive: emissive)
}

/// Set the emissive intensity.
///
/// **Emissive Intensity**: Multiplier for the emissive color brightness.
/// 0.0 = no emission, higher values = brighter glow. Especially visible with bloom.
///
/// ## Example
///
/// ```gleam
/// // Bright glowing sphere
/// material.new()
///   |> material.with_color(0x00ff00)
///   |> material.with_emissive(0x00ff00)
///   |> material.with_emissive_intensity(1.0)
/// ```
pub fn with_emissive_intensity(
  builder: StandardMaterialBuilder,
  intensity: Float,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, emissive_intensity: intensity)
}

/// Build the final StandardMaterial from the builder.
///
/// Validates all parameters and returns a Result. Will fail if any values
/// are out of valid ranges.
///
/// ## Example
///
/// ```gleam
/// let result = material.new()
///   |> material.with_color(0xff0000)
///   |> material.with_metalness(0.8)
///   |> material.with_roughness(0.3)
///   |> material.build()
///
/// case result {
///   Ok(material) -> // Use the material
///   Error(material.OutOfBoundsColor(_)) -> // Handle error
/// }
/// ```
pub fn build(
  builder: StandardMaterialBuilder,
) -> Result(Material, MaterialError) {
  standard(
    color: builder.color,
    metalness: builder.metalness,
    roughness: builder.roughness,
    transparent: builder.transparent,
    opacity: builder.opacity,
    map: builder.map,
    normal_map: builder.normal_map,
    ambient_oclusion_map: builder.ambient_oclusion_map,
    roughness_map: builder.roughness_map,
    metalness_map: builder.metalness_map,
    emissive: builder.emissive,
    emissive_intensity: builder.emissive_intensity,
  )
}

@internal
pub type ThreeMaterial

@internal
pub fn create_material(material: Material) -> ThreeMaterial {
  case material {
    BasicMaterial(color:, map:, transparent:, opacity:) ->
      create_basic_material(color, transparent, opacity, map)
    StandardMaterial(
      color:,
      map:,
      normal_map:,
      ambient_oclusion_map:,
      roughness_map:,
      metalness_map:,
      metalness:,
      roughness:,
      transparent:,
      opacity:,
      emissive:,
      emissive_intensity:,
    ) ->
      create_standard_material(
        color,
        metalness,
        roughness,
        transparent,
        opacity,
        map,
        normal_map,
        ambient_oclusion_map,
        roughness_map,
        metalness_map,
        emissive,
        emissive_intensity,
      )
    PhongMaterial(color:, map:, normal_map:, ambient_oclusion_map:, shininess:) ->
      create_phong_material(
        color,
        shininess,
        map,
        normal_map,
        ambient_oclusion_map,
      )
    LambertMaterial(
      color:,
      map:,
      normal_map:,
      ambient_oclusion_map:,
      transparent:,
      opacity:,
      alpha_test:,
    ) ->
      create_lambert_material(
        color,
        map,
        normal_map,
        ambient_oclusion_map,
        transparent,
        opacity,
        alpha_test,
      )
    ToonMaterial(color:, map:, normal_map:, ambient_oclusion_map:) ->
      create_toon_material(color, map, normal_map, ambient_oclusion_map)
    LineMaterial(color:, linewidth:) -> create_line_material(color, linewidth)
    SpriteMaterial(color:, map:, transparent:, opacity:) ->
      create_sprite_material(color, transparent, opacity, map)
  }
}

@external(javascript, "../threejs.ffi.mjs", "createBasicMaterial")
fn create_basic_material(
  color: Int,
  transparent: Bool,
  opacity: Float,
  map: Option(asset.Texture),
) -> ThreeMaterial

@external(javascript, "../threejs.ffi.mjs", "createStandardMaterial")
fn create_standard_material(
  color: Int,
  metalness: Float,
  roughness: Float,
  transparent: Bool,
  opacity: Float,
  map: Option(asset.Texture),
  normal_map: Option(asset.Texture),
  ao_map: Option(asset.Texture),
  roughness_map: Option(asset.Texture),
  metalness_map: Option(asset.Texture),
  emissive: Int,
  emissive_intensity: Float,
) -> ThreeMaterial

@external(javascript, "../threejs.ffi.mjs", "createPhongMaterial")
fn create_phong_material(
  color: Int,
  shininess: Float,
  map: Option(asset.Texture),
  normal_map: Option(asset.Texture),
  ao_map: Option(asset.Texture),
) -> ThreeMaterial

@external(javascript, "../threejs.ffi.mjs", "createLambertMaterial")
fn create_lambert_material(
  color: Int,
  map: Option(asset.Texture),
  normal_map: Option(asset.Texture),
  ao_map: Option(asset.Texture),
  transparent: Bool,
  opacity: Float,
  alpha_test: Float,
) -> ThreeMaterial

@external(javascript, "../threejs.ffi.mjs", "createToonMaterial")
fn create_toon_material(
  color: Int,
  map: Option(asset.Texture),
  normal_map: Option(asset.Texture),
  ao_map: Option(asset.Texture),
) -> ThreeMaterial

@external(javascript, "../threejs.ffi.mjs", "createLineMaterial")
fn create_line_material(color: Int, linewidth: Float) -> ThreeMaterial

@external(javascript, "../threejs.ffi.mjs", "createSpriteMaterial")
fn create_sprite_material(
  color: Int,
  transparent: Bool,
  opacity: Float,
  map: Option(asset.Texture),
) -> ThreeMaterial
