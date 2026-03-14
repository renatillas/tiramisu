import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/result

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
