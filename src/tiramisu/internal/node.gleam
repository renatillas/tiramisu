import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/result
import gleam/string

pub fn parse_color(string: String) -> Result(Int, Nil) {
  string
  |> string.replace("#", "")
  |> int.base_parse(16)
}

// TODO: Fix: when a boolean attribute goes from existing to not existing the value of the attribute still is "". 
// Marking it as True wrongly
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

pub fn parse_number(number: String) -> Result(Float, Nil) {
  case float.parse(number) {
    Ok(float) -> Ok(float)
    Error(Nil) -> result.map(int.parse(number), int.to_float)
  }
}
