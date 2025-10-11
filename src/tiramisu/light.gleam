import gleam/bool

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

pub type LightError {
  NegativeIntensity(Float)
  OutOfBoundsColor(Int)
  NegativeDistance(Float)
  InvalidShadowResolution(Int)
  InvalidShadowBias(Float)
}

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
      Directional(intensity:, color:, cast_shadow:, shadow_resolution:, shadow_bias:)
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
pub fn with_shadow_bias(
  light: Light,
  bias: Float,
) -> Result(Light, LightError) {
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
pub fn create_light(light: Light) -> Nil {
  case light {
    Ambient(intensity:, color:) -> create_ambient_light(intensity, color)
    Directional(
      intensity:,
      color:,
      cast_shadow:,
      shadow_resolution:,
      shadow_bias:,
    ) ->
      create_directional_light(
        intensity,
        color,
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
        intensity,
        color,
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
        intensity,
        color,
        distance,
        angle,
        penumbra,
        cast_shadow,
        shadow_resolution,
        shadow_bias,
      )
    Hemisphere(intensity:, sky_color:, ground_color:) ->
      create_hemisphere_light(intensity, sky_color, ground_color)
  }
}

@external(javascript, "./ffi/renderer.mjs", "createAmbientLight")
fn create_ambient_light(intensity: Float, color: Int) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createDirectionalLight")
fn create_directional_light(
  intensity: Float,
  color: Int,
  cast_shadow: Bool,
  shadow_resolution: Int,
  shadow_bias: Float,
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createPointLight")
fn create_point_light(
  intensity: Float,
  color: Int,
  distance: Float,
  cast_shadow: Bool,
  shadow_resolution: Int,
  shadow_bias: Float,
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createSpotLight")
fn create_spot_light(
  intensity: Float,
  color: Int,
  distance: Float,
  angle: Float,
  penumbra: Float,
  cast_shadow: Bool,
  shadow_resolution: Int,
  shadow_bias: Float,
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createHemisphereLight")
fn create_hemisphere_light(
  intensity: Float,
  sky_color: Int,
  ground_color: Int,
) -> Nil
