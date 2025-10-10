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
  Directional(intensity: Float, color: Int)
  /// Point light that radiates in all directions (like a light bulb).
  Point(intensity: Float, color: Int, distance: Float)
  /// Cone-shaped spotlight (like a flashlight or stage light).
  Spot(
    intensity: Float,
    color: Int,
    distance: Float,
    angle: Float,
    penumbra: Float,
  )
  /// Hemisphere light with different colors for sky and ground (outdoor ambient).
  Hemisphere(intensity: Float, sky_color: Int, ground_color: Int)
}

pub type LightError {
  NegativeIntensity(Float)
  OutOfBoundsColor(Int)
  NegativeDistance(Float)
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
  Ok(Directional(intensity:, color:))
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
  Ok(Point(intensity:, color:, distance:))
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
  Ok(Spot(intensity:, color:, distance:, angle:, penumbra:))
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

@internal
pub fn create_light(light: Light) -> Nil {
  case light {
    Ambient(intensity:, color:) -> create_ambient_light(intensity, color)
    Directional(intensity:, color:) ->
      create_directional_light(intensity, color)
    Point(intensity:, color:, distance:) ->
      create_point_light(intensity, color, distance)
    Spot(intensity:, color:, distance:, angle:, penumbra:) ->
      create_spot_light(intensity, color, distance, angle, penumbra)
    Hemisphere(intensity:, sky_color:, ground_color:) ->
      create_hemisphere_light(intensity, sky_color, ground_color)
  }
}

@external(javascript, "./ffi/renderer.mjs", "createAmbientLight")
fn create_ambient_light(intensity: Float, color: Int) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createDirectionalLight")
fn create_directional_light(intensity: Float, color: Int) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createPointLight")
fn create_point_light(intensity: Float, color: Int, distance: Float) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createSpotLight")
fn create_spot_light(
  intensity: Float,
  color: Int,
  distance: Float,
  angle: Float,
  penumbra: Float,
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createHemisphereLight")
fn create_hemisphere_light(
  intensity: Float,
  sky_color: Int,
  ground_color: Int,
) -> Nil
