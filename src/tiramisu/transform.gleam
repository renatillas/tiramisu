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
import quaternion
import savoiardi
import tiramisu/dev/extension
import tiramisu/internal/node
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

/// Internal attribute extension that applies transforms to registered objects.
pub fn extension() -> extension.Extension {
  extension.AttributeExtension(extension.Attribute(
    observed_attributes: ["position", "rotation", "scale"],
    on_create: fn(_context, _tag, _id, object, attributes) {
      case object {
        // On objects that dont register themselves upon creation, we do nothing (yet)
        option.None -> Nil
        option.Some(object) -> apply_all(object, attributes)
      }
    },
    on_update: fn(_context, _tag, _id, object, attributes, changed_attributes) {
      case object {
        option.None -> Nil
        option.Some(object) -> {
          apply_changed(object, attributes, changed_attributes)
        }
      }
    },
    on_remove: fn(_context, _id, _parent_id, _object) { Nil },
    // Once the object has been resolved we can set the material
    on_object_resolved: fn(_context, _tag, _id, object, attributes) {
      apply_all(object, attributes)
    },
  ))
}

fn apply_all(
  object: savoiardi.Object3D,
  attributes: dict.Dict(String, String),
) -> Nil {
  apply_position(object, attributes)
  apply_rotation(object, attributes)
  apply_scale(object, attributes)
}

fn apply_changed(
  object: savoiardi.Object3D,
  attributes: dict.Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Nil {
  case extension.has_change(changed_attributes, "position") {
    True -> apply_position(object, attributes)
    False -> Nil
  }

  case extension.has_change(changed_attributes, "rotation") {
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
  |> list.map(node.parse_number)
  |> result.all
}
