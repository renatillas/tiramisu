//// Shared transform attributes for Tiramisu scene nodes.
////
//// These attributes are implemented once and applied across all built-in node
//// types, which keeps transform behaviour consistent throughout the library.

import gleam/dict
import gleam/float
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import lustre/attribute
import lustre/effect
import quaternion
import savoiardi
import tiramisu/dev/extension.{type AttributeChanges}
import tiramisu/dev/runtime
import vec/vec3

/// Set a node position in `x y z` form.
pub fn position(position: vec3.Vec3(Float)) -> attribute.Attribute(a) {
  attribute.attribute("position", vec_to_string(position))
}

/// Set a node scale in `x y z` form.
pub fn scale(scale: vec3.Vec3(Float)) -> attribute.Attribute(a) {
  attribute.attribute("scale", vec_to_string(scale))
}

/// Set Euler rotation in radians.
pub fn rotation(rotation: vec3.Vec3(Float)) -> attribute.Attribute(a) {
  attribute.attribute("rotation", vec_to_string(rotation))
}

/// Set quaternion rotation in `x y z w` form.
pub fn rotation_quaternion(
  rotation: quaternion.Quaternion,
) -> attribute.Attribute(a) {
  attribute.attribute("rotation", quaternion_to_string(rotation))
}

fn vec_to_string(vec: vec3.Vec3(Float)) -> String {
  float.to_string(vec.x)
  <> " "
  <> float.to_string(vec.y)
  <> " "
  <> float.to_string(vec.z)
}

fn quaternion_to_string(rotation: quaternion.Quaternion) -> String {
  float.to_string(rotation.x)
  <> " "
  <> float.to_string(rotation.y)
  <> " "
  <> float.to_string(rotation.z)
  <> " "
  <> float.to_string(rotation.w)
}

fn apply_all(
  object: savoiardi.Object3D,
  attributes: dict.Dict(String, String),
) -> Nil {
  let _ = apply_position(object, attributes)
  let _ = apply_rotation(object, attributes)
  apply_scale(object, attributes)
}

fn apply_changed(
  object: savoiardi.Object3D,
  attributes: dict.Dict(String, String),
  changed_attributes: AttributeChanges,
) -> Nil {
  let _ = case extension.has_change(changed_attributes, "position") {
    True -> apply_position(object, attributes)
    False -> Nil
  }

  let _ = case extension.has_change(changed_attributes, "rotation") {
    True -> apply_rotation(object, attributes)
    False -> Nil
  }

  case extension.has_change(changed_attributes, "scale") {
    True -> apply_scale(object, attributes)
    False -> Nil
  }
}

fn apply_position(
  object: savoiardi.Object3D,
  attributes: dict.Dict(String, String),
) -> Nil {
  let _ =
    dict.get(attributes, "position")
    |> result.try(vector_to_numbers)
    |> result.try(parse_vector)
    |> result.map(savoiardi.set_object_position(object, _))
  Nil
}

fn apply_scale(
  object: savoiardi.Object3D,
  attributes: dict.Dict(String, String),
) -> Nil {
  let _ =
    dict.get(attributes, "scale")
    |> result.try(vector_to_numbers)
    |> result.try(parse_vector)
    |> result.map(savoiardi.set_object_scale(object, _))
  Nil
}

fn apply_rotation(
  object: savoiardi.Object3D,
  attributes: dict.Dict(String, String),
) -> Nil {
  let rotation =
    dict.get(attributes, "rotation")
    |> result.try(vector_to_numbers)
  let _ =
    rotation
    |> result.try(parse_quaternion)
    |> result.lazy_or(fn() {
      result.try(rotation, parse_vector)
      |> result.map(quaternion.from_euler)
    })
    |> result.map(savoiardi.set_object_quaternion(object, _))
  Nil
}

fn parse_vector(values: List(Float)) -> Result(vec3.Vec3(Float), Nil) {
  case values {
    [x, y, z] -> Ok(vec3.Vec3(x, y, z))
    _ -> Error(Nil)
  }
}

fn parse_quaternion(values: List(Float)) -> Result(quaternion.Quaternion, Nil) {
  case values {
    [x, y, z, w] -> Ok(quaternion.Quaternion(x, y, z, w))
    _ -> Error(Nil)
  }
}

fn vector_to_numbers(vector: String) -> Result(List(Float), Nil) {
  vector
  |> string.split(on: " ")
  |> list.map(string.trim)
  |> list.map(extension.parse_number)
  |> result.all
}

fn on_create(
  _runtime: runtime.Runtime,
  _tag: String,
  _id: String,
  object: option.Option(savoiardi.Object3D),
  attributes: dict.Dict(String, String),
) -> effect.Effect(extension.Msg) {
  let _ = case object {
    // On objects that dont register themselves upon creation, we do nothing (yet)
    option.None -> Nil
    option.Some(object) -> apply_all(object, attributes)
  }
  effect.none()
}

fn on_update(
  _runtime: runtime.Runtime,
  _tag: String,
  _id: String,
  object: option.Option(savoiardi.Object3D),
  attributes: dict.Dict(String, String),
  changed_attributes: dict.Dict(String, extension.AttributeChange),
) -> effect.Effect(extension.Msg) {
  let _ = case object {
    option.None -> Nil
    option.Some(object) -> {
      apply_changed(object, attributes, changed_attributes)
    }
  }
  effect.none()
}

fn on_resolved(
  _runtime: runtime.Runtime,
  _tag: String,
  _id: String,
  object: savoiardi.Object3D,
  attributes: dict.Dict(String, String),
) -> effect.Effect(extension.Msg) {
  let _ = apply_all(object, attributes)
  effect.none()
}

fn on_remove(
  _runtime: runtime.Runtime,
  _tag: String,
  _id: String,
  _object: savoiardi.Object3D,
) -> effect.Effect(extension.Msg) {
  effect.none()
}

pub fn ext() -> extension.Extension {
  extension.attribute_extension(
    observed_attributes: ["position", "rotation", "scale"],
    on_create:,
    on_update:,
    on_remove:,
    on_resolved:,
  )
}
