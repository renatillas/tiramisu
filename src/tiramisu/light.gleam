import gleam/dict
import gleam/float
import gleam/int
import gleam/json
import gleam/option
import gleam/result
import gleam/set.{type Set}
import gleam_community/maths
import savoiardi.{type Object3D}

import lustre/attribute.{type Attribute}

import tiramisu/dev/extension
import tiramisu/dev/registry
import tiramisu/internal/node

pub const tag = "tiramisu-light"

pub fn extension() {
  let observed_attributes =
    set.from_list([
      "type",
      "color",
      "intensity",
      "cast-shadow",
      "distance",
      "angle",
      "penumbra",
      "sky",
      "ground",
    ])
  extension.Node(tag:, observed_attributes:, create:, update:, remove:)
  |> extension.NodeExtension
}

pub type Light {
  Ambient
  Directional
  Point
  Spot
  Hemisphere
}

pub fn kind(kind: Light) -> Attribute(msg) {
  attribute.attribute("type", case kind {
    Ambient -> "ambient"
    Directional -> "directional"
    Hemisphere -> "hemisphere"
    Point -> "point"
    Spot -> "spot"
  })
}

pub fn intensity(value: Float) -> Attribute(msg) {
  attribute.attribute("intensity", float.to_string(value))
}

pub fn color(hex hex: Int) -> Attribute(msg) {
  attribute.attribute("color", "#" <> int.to_base16(hex))
}

pub fn cast_shadow(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("cast-shadow", "")
    False -> attribute.property("cast-shadow", json.bool(False))
  }
}

pub fn distance(value: Float) {
  attribute.attribute("distance", float.to_string(value))
}

pub fn angle(value: Float) {
  attribute.attribute("angle", float.to_string(value))
}

pub fn penumbra(value: Float) {
  attribute.attribute("penumbra", float.to_string(value))
}

pub fn sky(color: Int) {
  attribute.attribute("sky", int.to_string(color))
}

pub fn ground(color: Int) {
  attribute.attribute("ground", int.to_string(color))
}

fn create(
  context: extension.Context,
  id: String,
  parent_id: String,
  attributes: dict.Dict(String, String),
) -> extension.Context {
  let kind = dict.get(attributes, "type") |> result.unwrap("point")
  let light = case kind {
    "directional" -> create_directional(attributes)
    "point" -> create_point(attributes)
    "spot" -> create_spot(attributes)
    "hemisphere" -> create_hemisphere(attributes)
    _ -> create_ambient(attributes)
  }
  let casts_shadow = node.get_bool(attributes, "cast-shadow")
  savoiardi.set_light_cast_shadow(light, casts_shadow)

  let object = savoiardi.light_to_object3d(light)
  let registry = registry.add(context.registry, id, object:, parent_id:, tag:)
  extension.Context(..context, registry: registry)
}

fn create_directional(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let color = node.get(attributes, "color", 0xffffff, node.parse_color)
  let intensity = node.get(attributes, "intensity", 1.0, node.parse_number)
  savoiardi.create_directional_light(color:, intensity:)
}

fn create_point(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let distance = node.get(attributes, "distance", 0.0, node.parse_number)
  let color = node.get(attributes, "color", 0xffffff, node.parse_color)
  let intensity = node.get(attributes, "intensity", 1.0, node.parse_number)
  savoiardi.create_point_light(color:, intensity:, distance:)
}

fn create_hemisphere(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let sky_color = node.get(attributes, "sky", 0x000000, node.parse_color)
  let ground_color = node.get(attributes, "ground", 0x000000, node.parse_color)
  let intensity = node.get(attributes, "intensity", 1.0, node.parse_number)
  savoiardi.create_hemisphere_light(sky_color:, ground_color:, intensity:)
}

fn create_spot(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let distance = node.get(attributes, "distance", 0.0, node.parse_number)
  let angle =
    node.get(attributes, "angle", maths.pi() /. 4.0, node.parse_number)
  let penumbra = node.get(attributes, "penumbra", 0.5, node.parse_number)
  let color = node.get(attributes, "color", 0xffffff, node.parse_color)
  let intensity = node.get(attributes, "intensity", 1.0, node.parse_number)
  savoiardi.create_spot_light(color:, intensity:, distance:, angle:, penumbra:)
}

fn create_ambient(attributes: dict.Dict(String, String)) {
  let color = node.get(attributes, "color", 0xffffff, node.parse_color)
  let intensity = node.get(attributes, "intensity", 1.0, node.parse_number)
  savoiardi.create_ambient_light(color:, intensity:)
}

fn update(
  context: extension.Context,
  id: String,
  _parent_id: String,
  object: option.Option(Object3D),
  attributes: dict.Dict(String, String),
  changed_attributes: Set(String),
) -> extension.Context {
  let result = {
    use object <- result.try(object |> option.to_result(Nil))
    let #(light, registry) = case set.contains("kind", in: changed_attributes) {
      True -> {
        let kind = dict.get(attributes, "type") |> result.unwrap("point")
        let light = case kind {
          "directional" -> create_directional(attributes)
          "point" -> create_point(attributes)
          "spot" -> create_spot(attributes)
          "hemisphere" -> create_hemisphere(attributes)
          _ -> create_ambient(attributes)
        }
        let light = savoiardi.light_to_object3d(light)
        #(light, registry.replace(context.registry, id, light))
      }
      False -> #(object, context.registry)
    }

    let light = savoiardi.object3d_to_light(light)

    case set.contains("cast-shadow", in: changed_attributes) {
      True ->
        savoiardi.set_light_cast_shadow(
          light,
          node.get_bool(attributes, "cast-shadow"),
        )
      False -> Nil
    }
    case set.contains("color", in: changed_attributes) {
      True -> {
        savoiardi.update_light_color(
          light,
          node.get(attributes, "color", 0xffffff, node.parse_color),
        )
      }
      False -> Nil
    }
    case set.contains("intensity", in: changed_attributes) {
      True ->
        savoiardi.update_light_intensity(
          light,
          node.get(attributes, "intensity", 1.0, node.parse_number),
        )
      False -> Nil
    }

    Ok(registry)
  }
  case result {
    Ok(registry) -> extension.Context(..context, registry:)
    Error(Nil) -> context
  }
}

fn remove(
  context: extension.Context,
  id: String,
  parent_id: String,
  object: Object3D,
) -> extension.Context {
  let reg = registry.remove(context.registry, id, parent_id, object)
  extension.Context(..context, registry: reg)
}
