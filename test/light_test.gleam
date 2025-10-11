import tiramisu/light

pub fn ambient_valid_test() {
  let assert Ok(_) = light.ambient(intensity: 0.5, color: 0xffffff)
}

pub fn ambient_invalid_intensity_negative_test() {
  let assert Error(light.NegativeIntensity(-0.1)) =
    light.ambient(intensity: -0.1, color: 0xffffff)
}

pub fn directional_valid_test() {
  let assert Ok(_) = light.directional(intensity: 1.0, color: 0xffffff)
}

pub fn directional_invalid_intensity_test() {
  let assert Error(light.NegativeIntensity(-1.0)) =
    light.directional(intensity: -1.0, color: 0xffffff)
}

pub fn directional_with_shadows_test() {
  let assert Ok(l) = light.directional(intensity: 1.0, color: 0xffffff)
  let _with_shadows = light.with_shadows(l, True)
}

pub fn point_valid_test() {
  let assert Ok(_) =
    light.point(intensity: 2.0, color: 0xff0000, distance: 100.0)
}

pub fn point_invalid_intensity_test() {
  let assert Error(light.NegativeIntensity(-1.0)) =
    light.point(intensity: -1.0, color: 0xff0000, distance: 100.0)
}

pub fn point_invalid_distance_test() {
  let assert Error(light.NegativeDistance(-10.0)) =
    light.point(intensity: 1.0, color: 0xffffff, distance: -10.0)
}

pub fn point_zero_distance_test() {
  let assert Error(light.NegativeDistance(-0.1)) =
    light.point(intensity: 1.0, color: 0xffffff, distance: -0.1)
}

pub fn spot_valid_test() {
  let assert Ok(_) =
    light.spot(
      intensity: 1.5,
      color: 0x00ff00,
      distance: 50.0,
      angle: 1.0,
      penumbra: 0.5,
    )
}

pub fn spot_invalid_intensity_test() {
  let assert Error(light.NegativeIntensity(-0.5)) =
    light.spot(
      intensity: -0.5,
      color: 0x00ff00,
      distance: 50.0,
      angle: 1.0,
      penumbra: 0.5,
    )
}

pub fn spot_invalid_distance_test() {
  let assert Error(light.NegativeDistance(-1.0)) =
    light.spot(
      intensity: 1.5,
      color: 0x00ff00,
      distance: -1.0,
      angle: 1.0,
      penumbra: 0.5,
    )
}

pub fn hemisphere_valid_test() {
  let assert Ok(_) =
    light.hemisphere(
      intensity: 0.8,
      sky_color: 0x87ceeb,
      ground_color: 0x8b4513,
    )
}

pub fn hemisphere_invalid_intensity_test() {
  let assert Error(light.NegativeIntensity(-0.5)) =
    light.hemisphere(
      intensity: -0.5,
      sky_color: 0x87ceeb,
      ground_color: 0x8b4513,
    )
}
