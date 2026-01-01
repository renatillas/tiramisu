import gleam/option
import tiramisu/material

pub fn standard_builder_test() {
  let assert Ok(_) =
    material.new()
    |> material.with_color(0xff0000)
    |> material.with_metalness(0.8)
    |> material.with_roughness(0.3)
    |> material.build()
}

pub fn standard_direct_test() {
  let assert Ok(_) =
    material.standard(
      color: 0x00ff00,
      metalness: 0.5,
      roughness: 0.5,
      transparent: False,
      opacity: 1.0,
      emissive: 0x000000,
      emissive_intensity: 0.0,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
      displacement_map: option.None,
      displacement_scale: 1.0,
      displacement_bias: 0.0,
      roughness_map: option.None,
      metalness_map: option.None,
    )
}

pub fn standard_invalid_metalness_high_test() {
  let assert Error(material.OutOfBoundsMetalness(1.5)) =
    material.new()
    |> material.with_metalness(1.5)
    |> material.build()
}

pub fn standard_invalid_metalness_low_test() {
  let assert Error(material.OutOfBoundsMetalness(-0.1)) =
    material.new()
    |> material.with_metalness(-0.1)
    |> material.build()
}

pub fn standard_invalid_roughness_negative_test() {
  let assert Error(material.OutOfBoundsRoughness(-0.1)) =
    material.new()
    |> material.with_roughness(-0.1)
    |> material.build()
}

pub fn standard_invalid_roughness_high_test() {
  let assert Error(material.OutOfBoundsRoughness(1.5)) =
    material.new()
    |> material.with_roughness(1.5)
    |> material.build()
}

pub fn basic_test() {
  let assert Ok(_) =
    material.basic(
      color: 0x0000ff,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      side: material.FrontSide,
      alpha_test: 0.0,
      depth_write: True,
    )
}

pub fn basic_invalid_opacity_high_test() {
  let assert Error(material.OutOfBoundsOpacity(1.5)) =
    material.basic(
      color: 0x0000ff,
      transparent: True,
      opacity: 1.5,
      map: option.None,
      side: material.FrontSide,
      alpha_test: 0.0,
      depth_write: True,
    )
}

pub fn basic_invalid_opacity_negative_test() {
  let assert Error(material.OutOfBoundsOpacity(-0.1)) =
    material.basic(
      color: 0x0000ff,
      transparent: True,
      opacity: -0.1,
      map: option.None,
      side: material.FrontSide,
      alpha_test: 0.0,
      depth_write: True,
    )
}

pub fn phong_test() {
  let assert Ok(_) =
    material.phong(
      color: 0xff00ff,
      shininess: 50.0,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
      transparent: False,
      opacity: 1.0,
      alpha_test: 0.0,
    )
}

pub fn phong_invalid_shininess_test() {
  let assert Error(material.NonPositiveShininess(-10.0)) =
    material.phong(
      color: 0xff00ff,
      shininess: -10.0,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
      transparent: False,
      opacity: 1.0,
      alpha_test: 0.0,
    )
}

pub fn phong_zero_shininess_test() {
  let assert Error(material.NonPositiveShininess(-1.0)) =
    material.phong(
      color: 0xff00ff,
      shininess: -1.0,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
      transparent: False,
      opacity: 1.0,
      alpha_test: 0.0,
    )
}

pub fn lambert_test() {
  let assert Ok(_) =
    material.lambert(
      color: 0xffff00,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
      transparent: False,
      opacity: 1.0,
      alpha_test: 0.0,
    )
}

pub fn toon_test() {
  let assert Ok(_) =
    material.toon(
      color: 0x00ffff,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
      transparent: False,
      opacity: 1.0,
      alpha_test: 0.0,
    )
}

pub fn standard_with_transparency_test() {
  let assert Ok(_) =
    material.new()
    |> material.with_color(0xff0000)
    |> material.with_transparent(True)
    |> material.with_opacity(0.5)
    |> material.build()
}

pub fn standard_invalid_opacity_high_test() {
  let assert Error(material.OutOfBoundsOpacity(1.5)) =
    material.new()
    |> material.with_opacity(1.5)
    |> material.build()
}

pub fn standard_invalid_opacity_negative_test() {
  let assert Error(material.OutOfBoundsOpacity(-0.1)) =
    material.new()
    |> material.with_opacity(-0.1)
    |> material.build()
}
