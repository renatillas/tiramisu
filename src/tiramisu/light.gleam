//// Lighting system for illuminating 3D scenes.
////
//// Provides various light types with different characteristics and performance trade-offs.
//// Lights are validated at creation time to ensure proper values.
////
//// ## Light Types
////
//// - **Ambient**: Global illumination with no direction (cheapest, always use for base lighting)
//// - **Directional**: Parallel rays like the sun (outdoor scenes, can cast shadows)
//// - **Point**: Radiates in all directions like a light bulb (indoor scenes, can cast shadows)
//// - **Spot**: Cone-shaped beam like a flashlight (focused lighting, can cast shadows)
//// - **Hemisphere**: Sky/ground colors for outdoor ambient (more realistic than pure ambient)
////
//// ## Typical Lighting Setups
////
//// ### Outdoor Scene
//// ```gleam
//// import tiramisu/light
//// import tiramisu/scene
//// import tiramisu/transform
//// import vec/vec3
////
//// // Sun as directional light with shadows
//// let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)
////   |> light.with_shadows(True)
////   |> light.with_shadow_resolution(2048)
////
//// scene.Light(
////   id: "sun",
////   light: sun,
////   transform: transform.identity
////     |> transform.with_euler_rotation(vec3.Vec3(-0.5, 0.3, 0.0)),
//// )
////
//// // Hemisphere for sky/ground ambient
//// let assert Ok(sky) = light.hemisphere(
////   intensity: 0.3,
////   sky_color: 0x87ceeb,    // Sky blue
////   ground_color: 0x8b7355,  // Brown earth
//// )
//// ```
////
//// ### Indoor Scene
//// ```gleam
//// // Base ambient
//// let assert Ok(ambient) = light.ambient(intensity: 0.2, color: 0x404040)
////
//// // Ceiling lights
//// let assert Ok(ceiling_light) = light.point(
////   intensity: 1.0,
////   color: 0xfff5e1,
////   distance: 10.0,
//// ) |> light.with_shadows(True)
////
//// scene.Light(
////   id: "ceiling-light-1",
////   light: ceiling_light,
////   transform: transform.at(position: vec3.Vec3(0.0, 5.0, 0.0)),
//// )
//// ```

import gleam/bool
import tiramisu/asset

/// Light types for illuminating the scene.
///
/// Different lights have different performance impacts and visual characteristics.
/// Most games use a combination of ambient + directional for outdoor scenes,
/// or ambient + point/spot for indoor scenes.
pub opaque type Light {
  /// Global ambient light (affects all objects equally, no direction).
  Ambient(intensity: Float, color: Int)
  /// Directional light like the sun (parallel rays, infinite distance).
  Directional(
    intensity: Float,
    color: Int,
    cast_shadow: Bool,
    shadow_resolution: Int,
    shadow_bias: Float,
  )
  /// Point light that radiates in all directions (like a light bulb).
  Point(
    intensity: Float,
    color: Int,
    distance: Float,
    cast_shadow: Bool,
    shadow_resolution: Int,
    shadow_bias: Float,
  )
  /// Cone-shaped spotlight (like a flashlight or stage light).
  Spot(
    intensity: Float,
    color: Int,
    distance: Float,
    angle: Float,
    penumbra: Float,
    cast_shadow: Bool,
    shadow_resolution: Int,
    shadow_bias: Float,
  )
  /// Hemisphere light with different colors for sky and ground (outdoor ambient).
  Hemisphere(intensity: Float, sky_color: Int, ground_color: Int)
}

/// Errors that can occur when creating or configuring lights.
pub type LightError {
  /// Intensity must be non-negative
  NegativeIntensity(Float)
  /// Color must be a valid hex color (0x000000 to 0xFFFFFF)
  OutOfBoundsColor(Int)
  /// Distance must be non-negative
  NegativeDistance(Float)
  /// Shadow resolution must be positive and a power of 2
  InvalidShadowResolution(Int)
  /// Shadow bias must be non-negative
  InvalidShadowBias(Float)
}

/// Create an ambient light for global base illumination.
///
/// Ambient light has no direction and affects all objects equally. It's the cheapest
/// light type and should be used as a base level of illumination in every scene.
///
/// **Intensity**: Typical values are 0.1-0.5 for subtle ambient, 0.5-1.0 for brighter scenes.
/// **Color**: Hex color (e.g., 0xffffff for white, 0x404040 for dim gray).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/light
/// import tiramisu/scene
/// import tiramisu/transform
///
/// // Subtle gray ambient for indoor scene
/// let assert Ok(ambient) = light.ambient(intensity: 0.2, color: 0x404040)
///
/// scene.Light(
///   id: "ambient",
///   light: ambient,
///   transform: transform.identity,
/// )
/// ```
pub fn ambient(
  intensity intensity: Float,
  color color: Int,
) -> Result(Light, LightError) {
  use <- bool.guard(intensity <. 0.0, Error(NegativeIntensity(intensity)))
  use <- bool.guard(
    color < 0 && color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  Ok(Ambient(intensity:, color:))
}

/// Create a directional light with parallel rays like the sun.
///
/// Directional lights simulate sunlight - all rays are parallel, coming from an infinite
/// distance. The light's position doesn't matter, only its rotation (direction).
/// Can cast high-quality shadows across the entire scene.
///
/// **Intensity**: Typical values are 0.5-1.5. Higher for harsh sunlight, lower for overcast.
/// **Color**: Hex color (e.g., 0xffffff for noon sun, 0xffeedd for sunset).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/light
/// import tiramisu/scene
/// import tiramisu/transform
/// import vec/vec3
///
/// // Sun at an angle with shadows
/// let assert Ok(sun) = light.directional(intensity: 1.2, color: 0xffffff)
///   |> light.with_shadows(True)
///   |> light.with_shadow_resolution(2048)
///
/// scene.Light(
///   id: "sun",
///   light: sun,
///   transform: transform.identity
///     |> transform.with_euler_rotation(vec3.Vec3(-0.8, 0.5, 0.0)),  // Angled downward
/// )
/// ```
pub fn directional(
  intensity intensity: Float,
  color color: Int,
) -> Result(Light, LightError) {
  use <- bool.guard(intensity <. 0.0, Error(NegativeIntensity(intensity)))
  use <- bool.guard(
    color < 0 && color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  Ok(Directional(
    intensity:,
    color:,
    cast_shadow: False,
    shadow_resolution: 1024,
    shadow_bias: 0.0001,
  ))
}

/// Create a point light that radiates in all directions.
///
/// Point lights simulate light bulbs, torches, or lamps. They emit light equally in all
/// directions from their position. Light intensity decreases with distance.
///
/// **Intensity**: Typical values are 0.5-2.0 depending on desired brightness.
/// **Color**: Hex color (e.g., 0xfff5e1 for warm bulb, 0xffffff for cool white).
/// **Distance**: Maximum range where light intensity reaches zero. Use 0.0 for infinite range (not recommended for performance).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/light
/// import tiramisu/scene
/// import tiramisu/transform
/// import vec/vec3
///
/// // Warm ceiling light
/// let assert Ok(bulb) = light.point(
///   intensity: 1.0,
///   color: 0xfff5e1,  // Warm yellow-white
///   distance: 15.0,   // Light fades out at 15 units
/// ) |> light.with_shadows(True)
///
/// scene.Light(
///   id: "ceiling-light",
///   light: bulb,
///   transform: transform.at(position: vec3.Vec3(0.0, 5.0, 0.0)),
/// )
/// ```
pub fn point(
  intensity intensity: Float,
  color color: Int,
  distance distance: Float,
) -> Result(Light, LightError) {
  use <- bool.guard(intensity <. 0.0, Error(NegativeIntensity(intensity)))
  use <- bool.guard(
    color < 0 && color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(distance <. 0.0, Error(NegativeDistance(distance)))
  Ok(Point(
    intensity:,
    color:,
    distance:,
    cast_shadow: False,
    shadow_resolution: 1024,
    shadow_bias: 0.0001,
  ))
}

/// Create a spotlight with a focused cone of light.
///
/// Spotlights simulate flashlights, stage lights, or car headlights. They emit light in a
/// cone shape from their position, with the cone pointing in the light's forward direction.
///
/// **Intensity**: Typical values are 0.5-2.0.
/// **Color**: Hex color (e.g., 0xffffff for white, 0xffff00 for yellow headlight).
/// **Distance**: Maximum range where light intensity reaches zero.
/// **Angle**: Cone angle in **radians** (e.g., Math.PI/4 for 45°, Math.PI/6 for 30°).
/// **Penumbra**: Edge softness from 0.0 (hard edge) to 1.0 (very soft edge). Typical: 0.1-0.3.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/light
/// import tiramisu/scene
/// import tiramisu/transform
/// import vec/vec3
/// import gleam_community/maths
///
/// // Flashlight spotlight
/// let assert Ok(flashlight) = light.spot(
///   intensity: 1.5,
///   color: 0xffffff,
///   distance: 20.0,
///   angle: maths.pi /. 6.0,  // 30 degree cone (in radians)
///   penumbra: 0.2,           // Soft edges
/// ) |> light.with_shadows(True)
///
/// scene.Light(
///   id: "flashlight",
///   light: flashlight,
///   transform: transform.at(position: vec3.Vec3(0.0, 2.0, 0.0))
///     |> transform.with_euler_rotation(vec3.Vec3(-1.57, 0.0, 0.0)),  // Point downward
/// )
/// ```
pub fn spot(
  intensity intensity: Float,
  color color: Int,
  distance distance: Float,
  angle angle: Float,
  penumbra penumbra: Float,
) -> Result(Light, LightError) {
  use <- bool.guard(intensity <. 0.0, Error(NegativeIntensity(intensity)))
  use <- bool.guard(
    color < 0 && color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(distance <. 0.0, Error(NegativeDistance(distance)))
  Ok(Spot(
    intensity:,
    color:,
    distance:,
    angle:,
    penumbra:,
    cast_shadow: False,
    shadow_resolution: 1024,
    shadow_bias: 0.0001,
  ))
}

/// Create a hemisphere light for outdoor ambient lighting.
///
/// Hemisphere lights simulate outdoor ambient light by using different colors for the sky
/// (upper hemisphere) and ground (lower hemisphere). Objects facing upward receive the sky
/// color, and objects facing downward receive the ground color. Creates more realistic
/// outdoor ambient than a single flat ambient light.
///
/// **Intensity**: Typical values are 0.2-0.5 for subtle ambient contribution.
/// **Sky Color**: Upper hemisphere color (e.g., 0x87ceeb for sky blue).
/// **Ground Color**: Lower hemisphere color (e.g., 0x8b7355 for brown earth).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/light
/// import tiramisu/scene
/// import tiramisu/transform
///
/// // Outdoor ambient with blue sky and brown ground
/// let assert Ok(outdoor_ambient) = light.hemisphere(
///   intensity: 0.4,
///   sky_color: 0x87ceeb,    // Sky blue
///   ground_color: 0x8b7355,  // Brown earth
/// )
///
/// scene.Light(
///   id: "outdoor-ambient",
///   light: outdoor_ambient,
///   transform: transform.identity,
/// )
/// ```
pub fn hemisphere(
  intensity intensity: Float,
  sky_color sky_color: Int,
  ground_color ground_color: Int,
) -> Result(Light, LightError) {
  use <- bool.guard(intensity <. 0.0, Error(NegativeIntensity(intensity)))
  use <- bool.guard(
    sky_color < 0 && sky_color > 0xffffff,
    Error(OutOfBoundsColor(sky_color)),
  )
  use <- bool.guard(
    ground_color < 0 && ground_color > 0xffffff,
    Error(OutOfBoundsColor(sky_color)),
  )
  Ok(Hemisphere(intensity:, sky_color:, ground_color:))
}

// --- Shadow Configuration Builder Methods ---

/// Enable shadow casting for a light.
///
/// Only directional, point, and spot lights can cast shadows.
/// Ambient and hemisphere lights are ignored.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)
///   |> light.with_shadows(True)
/// ```
pub fn with_shadows(light: Light, cast_shadow: Bool) -> Light {
  case light {
    Directional(intensity:, color:, shadow_resolution:, shadow_bias:, ..) ->
      Directional(
        intensity:,
        color:,
        cast_shadow:,
        shadow_resolution:,
        shadow_bias:,
      )
    Point(intensity:, color:, distance:, shadow_resolution:, shadow_bias:, ..) ->
      Point(
        intensity:,
        color:,
        distance:,
        cast_shadow:,
        shadow_resolution:,
        shadow_bias:,
      )
    Spot(
      intensity:,
      color:,
      distance:,
      angle:,
      penumbra:,
      shadow_resolution:,
      shadow_bias:,
      ..,
    ) ->
      Spot(
        intensity:,
        color:,
        distance:,
        angle:,
        penumbra:,
        cast_shadow:,
        shadow_resolution:,
        shadow_bias:,
      )
    _ -> light
  }
}

/// Set shadow map resolution (in pixels).
///
/// Higher values produce sharper shadows but use more memory.
/// Common values: 512, 1024 (default), 2048, 4096.
/// Must be a power of 2.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)
///   |> light.with_shadows(True)
///   |> light.with_shadow_resolution(2048)
/// ```
pub fn with_shadow_resolution(
  light: Light,
  resolution: Int,
) -> Result(Light, LightError) {
  use <- bool.guard(
    resolution <= 0 || resolution % 2 != 0,
    Error(InvalidShadowResolution(resolution)),
  )

  case light {
    Directional(intensity:, color:, cast_shadow:, shadow_bias:, ..) ->
      Ok(Directional(
        intensity:,
        color:,
        cast_shadow:,
        shadow_resolution: resolution,
        shadow_bias:,
      ))
    Point(intensity:, color:, distance:, cast_shadow:, shadow_bias:, ..) ->
      Ok(Point(
        intensity:,
        color:,
        distance:,
        cast_shadow:,
        shadow_resolution: resolution,
        shadow_bias:,
      ))
    Spot(
      intensity:,
      color:,
      distance:,
      angle:,
      penumbra:,
      cast_shadow:,
      shadow_bias:,
      ..,
    ) ->
      Ok(Spot(
        intensity:,
        color:,
        distance:,
        angle:,
        penumbra:,
        cast_shadow:,
        shadow_resolution: resolution,
        shadow_bias:,
      ))
    _ -> Ok(light)
  }
}

/// Set shadow bias to reduce shadow acne artifacts.
///
/// Typical values: 0.00001 to 0.001 (default: 0.0001).
/// Increase if you see shadow artifacts (shadow acne).
/// Decrease if shadows appear detached from objects.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)
///   |> light.with_shadows(True)
///   |> light.with_shadow_bias(0.0005)
/// ```
pub fn with_shadow_bias(light: Light, bias: Float) -> Result(Light, LightError) {
  use <- bool.guard(bias <. 0.0, Error(InvalidShadowBias(bias)))

  case light {
    Directional(intensity:, color:, cast_shadow:, shadow_resolution:, ..) ->
      Ok(Directional(
        intensity:,
        color:,
        cast_shadow:,
        shadow_resolution:,
        shadow_bias: bias,
      ))
    Point(intensity:, color:, distance:, cast_shadow:, shadow_resolution:, ..) ->
      Ok(Point(
        intensity:,
        color:,
        distance:,
        cast_shadow:,
        shadow_resolution:,
        shadow_bias: bias,
      ))
    Spot(
      intensity:,
      color:,
      distance:,
      angle:,
      penumbra:,
      cast_shadow:,
      shadow_resolution:,
      ..,
    ) ->
      Ok(Spot(
        intensity:,
        color:,
        distance:,
        angle:,
        penumbra:,
        cast_shadow:,
        shadow_resolution:,
        shadow_bias: bias,
      ))
    _ -> Ok(light)
  }
}

@internal
pub fn create_light(light: Light) -> asset.Object3D {
  case light {
    Ambient(intensity:, color:) -> create_ambient_light(color, intensity)
    Directional(
      intensity:,
      color:,
      cast_shadow:,
      shadow_resolution:,
      shadow_bias:,
    ) ->
      create_directional_light(
        color,
        intensity,
        cast_shadow,
        shadow_resolution,
        shadow_bias,
      )
    Point(
      intensity:,
      color:,
      distance:,
      cast_shadow:,
      shadow_resolution:,
      shadow_bias:,
    ) ->
      create_point_light(
        color,
        intensity,
        distance,
        cast_shadow,
        shadow_resolution,
        shadow_bias,
      )
    Spot(
      intensity:,
      color:,
      distance:,
      angle:,
      penumbra:,
      cast_shadow:,
      shadow_resolution:,
      shadow_bias:,
    ) ->
      create_spot_light(
        color,
        intensity,
        distance,
        angle,
        penumbra,
        cast_shadow,
        shadow_resolution,
        shadow_bias,
      )
    Hemisphere(intensity:, sky_color:, ground_color:) ->
      create_hemisphere_light(sky_color, ground_color, intensity)
  }
}

@external(javascript, "../threejs.ffi.mjs", "createAmbientLight")
fn create_ambient_light(color: Int, intensity: Float) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "createDirectionalLight")
fn create_directional_light(
  color: Int,
  intensity: Float,
  cast_shadow: Bool,
  shadow_resolution: Int,
  shadow_bias: Float,
) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "createPointLight")
fn create_point_light(
  color: Int,
  intensity: Float,
  distance: Float,
  cast_shadow: Bool,
  shadow_resolution: Int,
  shadow_bias: Float,
) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "createSpotLight")
fn create_spot_light(
  color: Int,
  intensity: Float,
  distance: Float,
  angle: Float,
  penumbra: Float,
  cast_shadow: Bool,
  shadow_resolution: Int,
  shadow_bias: Float,
) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "createHemisphereLight")
fn create_hemisphere_light(
  sky_color: Int,
  ground_color: Int,
  intensity: Float,
) -> asset.Object3D
