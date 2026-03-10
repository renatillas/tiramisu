import gleam/dict
import gleam/float
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string
import lustre/attribute
import quaternion
import savoiardi
import tiramisu/dev/extension
import tiramisu/internal/node
import vec/vec3

pub fn position(position: vec3.Vec3(Float)) -> attribute.Attribute(a) {
  attribute.attribute("position", vec_to_string(position))
}

pub fn scale(scale: vec3.Vec3(Float)) -> attribute.Attribute(a) {
  attribute.attribute("scale", vec_to_string(scale))
}

pub fn rotation(rotation: vec3.Vec3(Float)) -> attribute.Attribute(a) {
  attribute.attribute("rotation", vec_to_string(rotation))
}

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

pub fn extension() -> extension.Extension {
  extension.AttributeExtension(extension.Attribute(
    observed_attributes: ["position", "rotation", "scale"],
    on_create: fn(_context, _tag, _id, object, attributes) {
      case object {
        // On objects that dont register themselves upon creation, we do nothing (yet)
        option.None -> Nil
        option.Some(object) -> {
          let _ =
            dict.get(attributes, "position")
            |> result.try(vector_to_numbers)
            |> result.try(parse_vector)
            |> result.map(savoiardi.set_object_position(object, _))
          let _ =
            dict.get(attributes, "scale")
            |> result.try(vector_to_numbers)
            |> result.try(parse_vector)
            |> result.map(savoiardi.set_object_scale(object, _))
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
      }
    },
    on_update: fn(_context, _tag, _id, object, attributes, changed_attributes) {
      case object {
        option.None -> Nil
        option.Some(object) -> {
          case set.contains(changed_attributes, this: "position") {
            False -> Nil
            True -> {
              let _ =
                dict.get(attributes, "position")
                |> result.try(vector_to_numbers)
                |> result.try(parse_vector)
                |> result.map(savoiardi.set_object_position(object, _))
              Nil
            }
          }
          case set.contains(changed_attributes, this: "rotation") {
            False -> Nil
            True -> {
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
          }
          case set.contains(changed_attributes, this: "scale") {
            False -> Nil
            True -> {
              let _ =
                dict.get(attributes, "scale")
                |> result.try(vector_to_numbers)
                |> result.try(parse_vector)
                |> result.map(savoiardi.set_object_scale(object, _))
              Nil
            }
          }
        }
      }
    },
    on_remove: fn(_context, _id, _parent_id, _object) { Nil },
    // Once the object has been resolved we can set the material
    on_object_resolved: fn(_context, _tag, _id, object, attributes) {
      let _ =
        dict.get(attributes, "position")
        |> result.try(vector_to_numbers)
        |> result.try(parse_vector)
        |> result.map(savoiardi.set_object_position(object, _))
      let _ =
        dict.get(attributes, "scale")
        |> result.try(vector_to_numbers)
        |> result.try(parse_vector)
        |> result.map(savoiardi.set_object_scale(object, _))
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
    },
  ))
}

fn parse_vector(list) -> Result(vec3.Vec3(Float), Nil) {
  case list {
    [x, y, z] -> Ok(vec3.Vec3(x, y, z))
    _ -> Error(Nil)
  }
}

fn parse_quaternion(list) -> Result(quaternion.Quaternion, Nil) {
  case list {
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
