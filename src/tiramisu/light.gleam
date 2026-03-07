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
import tiramisu/transform

pub const tag = "tiramisu-light"

pub fn extension() {
  let observed_attributes =
    set.from_list([
      "transform",
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

pub fn transform(transform: transform.Transform) -> attribute.Attribute(msg) {
  attribute.attribute("transform", transform.to_string(transform))
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

  let light = savoiardi.light_to_object3d(light)

  case node.get_transform(attributes) {
    Ok(transform) -> node.set_transform(light, transform)
    Error(Nil) -> node.set_transform(light, transform.identity)
  }

  let registry =
    registry.register_and_add_object(
      context.registry,
      id,
      light,
      parent_id,
      tag,
    )
  extension.Context(..context, registry: registry)
}

fn create_directional(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let color = node.get_color(attributes, "color", 0xffffff)
  let intensity = node.get_float(attributes, "intensity", 1.0)
  savoiardi.create_directional_light(color:, intensity:)
}

fn create_point(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let distance = node.get_float(attributes, "distance", 0.0)
  let color = node.get_color(attributes, "color", 0xffffff)
  let intensity = node.get_float(attributes, "intensity", 1.0)
  savoiardi.create_point_light(color:, intensity:, distance:)
}

fn create_hemisphere(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let sky_color = node.get_int(attributes, "sky", 0x000000)
  let ground_color = node.get_int(attributes, "ground", 0x000000)
  let intensity = node.get_float(attributes, "intensity", 1.0)
  savoiardi.create_hemisphere_light(sky_color:, ground_color:, intensity:)
}

fn create_spot(attributes: dict.Dict(String, String)) -> savoiardi.Light {
  let distance = node.get_float(attributes, "distance", 0.0)
  let angle = node.get_float(attributes, "angle", maths.pi() /. 4.0)
  let penumbra = node.get_float(attributes, "penumbra", 0.5)
  let color = node.get_color(attributes, "color", 0xffffff)
  let intensity = node.get_float(attributes, "intensity", 1.0)
  savoiardi.create_spot_light(color:, intensity:, distance:, angle:, penumbra:)
}

fn create_ambient(attributes: dict.Dict(String, String)) {
  let color = node.get_color(attributes, "color", 0xffffff)
  let intensity = node.get_float(attributes, "intensity", 1.0)
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
    let #(light, registry) = case node.contains(["kind"], changed_attributes) {
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
        #(light, registry.replace_object_model(context.registry, id, light))
      }
      False -> #(object, context.registry)
    }
    case node.get_transform(attributes) {
      Ok(transform) -> node.set_transform(light, transform)
      Error(Nil) -> node.set_transform(light, transform.identity)
    }

    let light = savoiardi.object3d_to_light(object)

    case node.contains(["cast-shadow"], changed_attributes) {
      True ->
        savoiardi.set_light_cast_shadow(
          light,
          node.get_bool(attributes, "cast-shadow"),
        )
      False -> Nil
    }
    case node.contains(["color"], changed_attributes) {
      True -> {
        savoiardi.update_light_color(
          light,
          node.get_color(attributes, "color", 0xffffff),
        )
      }
      False -> Nil
    }
    case node.contains(["intensity"], changed_attributes) {
      True ->
        savoiardi.update_light_intensity(
          light,
          node.get_float(attributes, "intensity", 1.0),
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
  let reg = registry.remove_object(context.registry, id, parent_id, object)
  extension.Context(..context, registry: reg)
}
