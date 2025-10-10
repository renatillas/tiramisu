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
    normal_map: Option(asset.Texture),
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
    normal_map: Option(asset.Texture),
    transparent: Bool,
    opacity: Float,
  )
}

pub type MaterialError {
  OutOfBoundsColor(Int)
  OutOfBoundsOpacity(Float)
  OutOfBoundsRoughness(Float)
  OutOfBoundsMetalness(Float)
  NonPositiveLinewidth(Float)
  NonPositiveShininess(Float)
}

/// Create a validated basic (unlit) material.
///
/// Basic materials don't react to lights, making them very fast to render.
/// Opacity must be between 0.0 (fully transparent) and 1.0 (fully opaque).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(red) = scene.basic_material(color: 0xff0000, transparent: False, opacity: 1.0)
/// let assert Ok(glass) = scene.basic_material(color: 0x88ccff, transparent: True, opacity: 0.5)
/// ```
pub fn basic(
  color color: Int,
  transparent transparent: Bool,
  opacity opacity: Float,
  map map: option.Option(asset.Texture),
  normal_map normal_map: option.Option(asset.Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(
    opacity <. 0.0 || opacity >. 1.0,
    Error(OutOfBoundsOpacity(opacity)),
  )

  Ok(BasicMaterial(color:, transparent:, opacity:, map:, normal_map:))
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
/// let assert Ok(gold) = scene.standard_material(color: 0xffd700, metalness: 1.0, roughness: 0.3, map: option.None, normal_map: option.None, ao_map: option.None, roughness_map: option.None, metalness_map: option.None)
/// let assert Ok(plastic) = scene.standard_material(color: 0xff0000, metalness: 0.0, roughness: 0.5, map: option.None, normal_map: option.None, ao_map: option.None, roughness_map: option.None, metalness_map: option.None)
/// ```
pub fn standard(
  color color: Int,
  metalness metalness: Float,
  roughness roughness: Float,
  map map: option.Option(asset.Texture),
  normal_map normal_map: option.Option(asset.Texture),
  ambient_oclusion_map ambient_oclusion_map: option.Option(asset.Texture),
  roughness_map roughness_map: option.Option(asset.Texture),
  metalness_map metalness_map: option.Option(asset.Texture),
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

  Ok(StandardMaterial(
    color:,
    metalness:,
    roughness:,
    map:,
    normal_map:,
    roughness_map:,
    metalness_map:,
    ambient_oclusion_map:,
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
/// let assert Ok(sprite_mat) = scene.sprite_material(color: 0xffffff, transparent: True, opacity: 0.8)
/// ```
pub fn sprite(
  color color: Int,
  transparent transparent: Bool,
  opacity opacity: Float,
  map map: option.Option(asset.Texture),
  normal_map normal_map: option.Option(asset.Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(
    opacity <. 0.0 || opacity >. 1.0,
    Error(OutOfBoundsOpacity(opacity)),
  )

  Ok(SpriteMaterial(color:, transparent:, opacity:, map:, normal_map:))
}

pub fn lambert(
  color color: Int,
  map map: Option(asset.Texture),
  normal_map normal_map: Option(asset.Texture),
  ambient_oclusion_map ambient_oclusion_map: Option(asset.Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  Ok(LambertMaterial(color:, map:, normal_map:, ambient_oclusion_map:))
}

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
/// Start with `new_standard_material()`, chain setter methods, then call `build()`.
///
/// ## Example
///
/// ```gleam
/// let material = material.new()
///   |> material.with_color(0xff0000)
///   |> material.with_metalness(0.8)
///   |> material.with_roughness(0.3)
///   |> material.build()
/// ```
pub opaque type StandardMaterialBuilder {
  StandardMaterialBuilder(
    color: Int,
    metalness: Float,
    roughness: Float,
    map: Option(asset.Texture),
    normal_map: Option(asset.Texture),
    ambient_oclusion_map: Option(asset.Texture),
    roughness_map: Option(asset.Texture),
    metalness_map: Option(asset.Texture),
  )
}

pub fn new() -> StandardMaterialBuilder {
  StandardMaterialBuilder(
    color: 0x808080,
    metalness: 0.5,
    roughness: 0.5,
    map: option.None,
    normal_map: option.None,
    ambient_oclusion_map: option.None,
    roughness_map: option.None,
    metalness_map: option.None,
  )
}

pub fn with_color(
  builder: StandardMaterialBuilder,
  color: Int,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, color: color)
}

pub fn with_metalness(
  builder: StandardMaterialBuilder,
  metalness: Float,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, metalness: metalness)
}

pub fn with_roughness(
  builder: StandardMaterialBuilder,
  roughness: Float,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, roughness: roughness)
}

pub fn with_color_map(
  builder: StandardMaterialBuilder,
  map: asset.Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, map: option.Some(map))
}

pub fn with_normal_map(
  builder: StandardMaterialBuilder,
  normal_map: asset.Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, normal_map: option.Some(normal_map))
}

pub fn with_ambient_oclusion_map(
  builder: StandardMaterialBuilder,
  ambient_oclusion_map: asset.Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(
    ..builder,
    ambient_oclusion_map: option.Some(ambient_oclusion_map),
  )
}

pub fn with_roughness_map(
  builder: StandardMaterialBuilder,
  roughness_map: asset.Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, roughness_map: option.Some(roughness_map))
}

pub fn with_metalness_map(
  builder: StandardMaterialBuilder,
  metalness_map: asset.Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, metalness_map: option.Some(metalness_map))
}

pub fn build(
  builder: StandardMaterialBuilder,
) -> Result(Material, MaterialError) {
  standard(
    color: builder.color,
    metalness: builder.metalness,
    roughness: builder.roughness,
    map: builder.map,
    normal_map: builder.normal_map,
    ambient_oclusion_map: builder.ambient_oclusion_map,
    roughness_map: builder.roughness_map,
    metalness_map: builder.metalness_map,
  )
}

@internal
pub fn create_material(material: Material) -> Nil {
  case material {
    BasicMaterial(color:, map:, normal_map:, transparent:, opacity:) ->
      create_basic_material(color, transparent, opacity, map, normal_map)
    StandardMaterial(
      color:,
      map:,
      normal_map:,
      ambient_oclusion_map:,
      roughness_map:,
      metalness_map:,
      metalness:,
      roughness:,
    ) ->
      create_standard_material(
        color,
        metalness,
        roughness,
        map,
        normal_map,
        ambient_oclusion_map,
        roughness_map,
        metalness_map,
      )
    PhongMaterial(color:, map:, normal_map:, ambient_oclusion_map:, shininess:) ->
      create_phong_material(
        color,
        shininess,
        map,
        normal_map,
        ambient_oclusion_map,
      )
    LambertMaterial(color:, map:, normal_map:, ambient_oclusion_map:) ->
      create_lambert_material(color, map, normal_map, ambient_oclusion_map)
    ToonMaterial(color:, map:, normal_map:, ambient_oclusion_map:) ->
      create_toon_material(color, map, normal_map, ambient_oclusion_map)
    LineMaterial(color:, linewidth:) -> create_line_material(color, linewidth)
    SpriteMaterial(color:, map:, normal_map:, transparent:, opacity:) ->
      create_sprite_material(color, transparent, opacity, map, normal_map)
  }
}

@external(javascript, "./ffi/renderer.mjs", "createBasicMaterial")
fn create_basic_material(
  color: Int,
  transparent: Bool,
  opacity: Float,
  map: Option(asset.Texture),
  normal_map: Option(asset.Texture),
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createStandardMaterial")
fn create_standard_material(
  color: Int,
  metalness: Float,
  roughness: Float,
  map: Option(asset.Texture),
  normal_map: Option(asset.Texture),
  ao_map: Option(asset.Texture),
  roughness_map: Option(asset.Texture),
  metalness_map: Option(asset.Texture),
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createPhongMaterial")
fn create_phong_material(
  color: Int,
  shininess: Float,
  map: Option(asset.Texture),
  normal_map: Option(asset.Texture),
  ao_map: Option(asset.Texture),
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createLambertMaterial")
fn create_lambert_material(
  color: Int,
  map: Option(asset.Texture),
  normal_map: Option(asset.Texture),
  ao_map: Option(asset.Texture),
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createToonMaterial")
fn create_toon_material(
  color: Int,
  map: Option(asset.Texture),
  normal_map: Option(asset.Texture),
  ao_map: Option(asset.Texture),
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createLineMaterial")
fn create_line_material(color: Int, linewidth: Float) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createSpriteMaterial")
fn create_sprite_material(
  color: Int,
  transparent: Bool,
  opacity: Float,
  map: Option(asset.Texture),
  normal_map: Option(asset.Texture),
) -> Nil
