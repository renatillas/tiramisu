//// Light extension attributes and runtime integration.
////
//// Lights are regular scene nodes. That means they can be positioned under
//// groups, animated with transforms, and updated through normal Lustre view
//// changes.

import gleam/dict
import gleam/float
import gleam/int
import gleam/json
import gleam/option
import gleam/result
import gleam_community/maths
import lustre/effect
import savoiardi.{type Object3D}

import lustre/attribute.{type Attribute}

import tiramisu/dev/extension
import tiramisu/dev/runtime.{type Runtime}

/// The custom element tag used for light nodes.
pub const tag = "tiramisu-light"

pub fn ambient() -> Attribute(msg) {
  attribute.attribute("type", "ambient")
}

pub fn directional() -> Attribute(msg) {
  attribute.attribute("type", "directional")
}

pub fn hemisphere() -> Attribute(msg) {
  attribute.attribute("type", "hemisphere")
}

pub fn point() -> Attribute(msg) {
  attribute.attribute("type", "point")
}

pub fn spot() -> Attribute(msg) {
  attribute.attribute("type", "spot")
}

/// Set light intensity.
pub fn intensity(value: Float) -> Attribute(msg) {
  attribute.attribute("intensity", float.to_string(value))
}

/// Set the light color as a hex integer.
pub fn color(hex hex: Int) -> Attribute(msg) {
  attribute.attribute("color", int.to_base16(hex))
}

/// Enable or disable shadow casting on the light.
pub fn cast_shadow(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("cast-shadow", "")
    False -> attribute.property("cast-shadow", json.bool(False))
  }
}

/// Set the distance for point and spot lights.
pub fn distance(value: Float) -> Attribute(msg) {
  attribute.attribute("distance", float.to_string(value))
}

/// Set the spot light angle in radians.
pub fn angle(value: Float) -> Attribute(msg) {
  attribute.attribute("angle", float.to_string(value))
}

/// Set the spot light penumbra.
pub fn penumbra(value: Float) -> Attribute(msg) {
  attribute.attribute("penumbra", float.to_string(value))
}

/// Set the sky color for hemisphere lights.
pub fn sky(color: Int) -> Attribute(msg) {
  attribute.attribute("sky", int.to_string(color))
}

/// Set the ground color for hemisphere lights.
pub fn ground(color: Int) -> Attribute(msg) {
  attribute.attribute("ground", int.to_string(color))
}

fn create(
  runtime: Runtime,
  id: String,
  parent_id: String,
  attributes: dict.Dict(String, String),
) -> #(Runtime, effect.Effect(extension.Msg)) {
  let light = create_light(attributes)
  let casts_shadow = extension.get_bool(attributes, "cast-shadow")
  savoiardi.set_light_cast_shadow(light, casts_shadow)

  let object = savoiardi.light_to_object3d(light)
  let runtime = runtime.add_object(runtime, id, object:, parent_id:, tag:)
  #(runtime, effect.none())
}

fn create_directional(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let color =
    extension.get(attributes, "color", 0xffffff, int.base_parse(_, 16))
  let intensity =
    extension.get(attributes, "intensity", 1.0, extension.parse_number)
  savoiardi.create_directional_light(color:, intensity:)
}

fn create_point(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let distance =
    extension.get(attributes, "distance", 0.0, extension.parse_number)
  let color =
    extension.get(attributes, "color", 0xffffff, int.base_parse(_, 16))
  let intensity =
    extension.get(attributes, "intensity", 1.0, extension.parse_number)
  savoiardi.create_point_light(color:, intensity:, distance:)
}

fn create_hemisphere(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let sky_color =
    extension.get(attributes, "sky", 0x000000, int.base_parse(_, 16))
  let ground_color =
    extension.get(attributes, "ground", 0x000000, int.base_parse(_, 16))
  let intensity =
    extension.get(attributes, "intensity", 1.0, extension.parse_number)
  savoiardi.create_hemisphere_light(sky_color:, ground_color:, intensity:)
}

fn create_spot(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let distance =
    extension.get(attributes, "distance", 0.0, extension.parse_number)
  let angle =
    extension.get(
      attributes,
      "angle",
      maths.pi() /. 4.0,
      extension.parse_number,
    )
  let penumbra =
    extension.get(attributes, "penumbra", 0.5, extension.parse_number)
  let color =
    extension.get(attributes, "color", 0xffffff, int.base_parse(_, 16))
  let intensity =
    extension.get(attributes, "intensity", 1.0, extension.parse_number)
  savoiardi.create_spot_light(color:, intensity:, distance:, angle:, penumbra:)
}

fn create_ambient(attributes: dict.Dict(String, String)) {
  let color =
    extension.get(attributes, "color", 0xffffff, int.base_parse(_, 16))
  let intensity =
    extension.get(attributes, "intensity", 1.0, extension.parse_number)
  savoiardi.create_ambient_light(color:, intensity:)
}

fn create_light(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  case dict.get(attributes, "type") |> result.unwrap("point") {
    "directional" -> create_directional(attributes)
    "point" -> create_point(attributes)
    "spot" -> create_spot(attributes)
    "hemisphere" -> create_hemisphere(attributes)
    _ -> create_ambient(attributes)
  }
}

fn update(
  runtime: Runtime,
  id: String,
  _parent_id: String,
  object: option.Option(Object3D),
  attributes: dict.Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> #(Runtime, effect.Effect(extension.Msg)) {
  case object {
    option.Some(object) -> {
      let #(object, runtime) =
        rebuild_light_if_needed(
          runtime,
          id,
          object,
          attributes,
          changed_attributes,
        )
      let light = savoiardi.object3d_to_light(object)
      let _ = update_light_properties(light, attributes, changed_attributes)
      #(runtime, effect.none())
    }

    option.None -> #(runtime, effect.none())
  }
}

fn rebuild_light_if_needed(
  scene_runtime: Runtime,
  id: String,
  object: Object3D,
  attributes: dict.Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> #(Object3D, Runtime) {
  case extension.has_change(changed_attributes, "type") {
    True -> {
      let object = create_light(attributes) |> savoiardi.light_to_object3d
      #(object, runtime.replace_object(scene_runtime, id, object))
    }

    False -> #(object, scene_runtime)
  }
}

fn update_light_properties(
  light: savoiardi.Light,
  attributes: dict.Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Nil {
  case extension.bool_change(changed_attributes, "cast-shadow") {
    Ok(casts_shadow) -> savoiardi.set_light_cast_shadow(light, casts_shadow)
    Error(Nil) -> Nil
  }

  case extension.has_change(changed_attributes, "color") {
    True ->
      savoiardi.update_light_color(
        light,
        extension.get(attributes, "color", 0xffffff, int.base_parse(_, 16)),
      )

    False -> Nil
  }

  case extension.has_change(changed_attributes, "intensity") {
    True ->
      savoiardi.update_light_intensity(
        light,
        extension.get(attributes, "intensity", 1.0, extension.parse_number),
      )

    False -> Nil
  }
}

fn remove(
  runtime: Runtime,
  id: String,
  parent_id: String,
  object: Object3D,
) -> #(Runtime, effect.Effect(extension.Msg)) {
  let runtime = runtime.remove_object(runtime, id, parent_id, object)
  #(runtime, effect.none())
}

pub fn ext() -> extension.Extension {
  let observed_attributes = [
    "type",
    "color",
    "intensity",
    "cast-shadow",
    "distance",
    "angle",
    "penumbra",
    "sky",
    "ground",
  ]
  extension.node_extension(
    tag:,
    observed_attributes:,
    create:,
    update:,
    remove:,
  )
}
