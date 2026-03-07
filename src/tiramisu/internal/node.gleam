import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/result
import gleam/string
import savoiardi
import tiramisu/transform

pub fn parse_color(string: String) -> Result(Int, Nil) {
  string
  |> string.replace("#", "")
  |> int.base_parse(16)
}

pub fn get_bool(attrs: Dict(String, String), key: String) -> Bool {
  case dict.get(attrs, key) {
    Ok("true") | Ok("") -> True
    _ -> False
  }
}

pub fn get(
  attributes attributes: Dict(String, String),
  key key: String,
  default default: a,
  parse_fn parse_fn: fn(String) -> Result(a, Nil),
) -> a {
  dict.get(attributes, key)
  |> result.try(parse_fn)
  |> result.unwrap(default)
}

pub fn parse_number(number) {
  case float.parse(number) {
    Ok(float) -> Ok(float)
    Error(Nil) -> result.map(int.parse(number), int.to_float)
  }
}

pub fn get_transform(
  attributes: Dict(String, String),
) -> Result(transform.Transform, Nil) {
  dict.get(attributes, "transform")
  |> result.try(transform.parse)
}

pub fn set_transform(
  object: savoiardi.Object3D,
  transform: transform.Transform,
) -> Nil {
  savoiardi.set_object_position(object, transform.position)
  savoiardi.set_object_quaternion(object, transform.rotation)
  savoiardi.set_object_scale(object, transform.scale)
}
