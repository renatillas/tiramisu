//// A bidirectional map (BiMap)
//// A data structure similar to a Dictionary but stores
//// the association in both directions
////
//// For example, keys have association to values:
//// ```
//// animal → cat
//// color → red
//// ```
//// And values have reverse association to those keys
//// ```
//// cat → animal
//// red → color
//// ```
////

import gleam/dict.{type Dict}

pub opaque type BiMap(k, v) {
  BiMap(direct: Dict(k, v), reverse: Dict(v, k))
}

pub fn new() {
  BiMap(direct: dict.new(), reverse: dict.new())
}

/// Insert a key an associated value
pub fn insert(into into: BiMap(k, v), key key: k, value value: v) {
  let without =
    into
    |> delete(key)
    |> delete_val(value)

  let direct = without.direct |> dict.insert(key, value)

  let reverse = without.reverse |> dict.insert(value, key)

  BiMap(direct:, reverse:)
}

/// Get the associated value for a key
pub fn get(from from: BiMap(k, v), key key: k) -> Result(v, Nil) {
  dict.get(from.direct, key)
}

/// Get the associated key for a value
pub fn get_val(from from: BiMap(k, v), value value: v) -> Result(k, Nil) {
  dict.get(from.reverse, value)
}

/// Delete a key and associated value from the bimap
pub fn delete(from from: BiMap(k, v), key key: k) {
  let value = get(from, key)

  let direct = dict.delete(from.direct, key)

  let reverse = case value {
    Ok(previous) -> dict.delete(from.reverse, previous)
    Error(Nil) -> from.reverse
  }

  BiMap(direct:, reverse:)
}

/// Delete a value and associated key from the bimap
pub fn delete_val(from from: BiMap(k, v), value value: v) {
  let key = get_val(from, value)

  let reverse = dict.delete(from.reverse, value)

  let direct = case key {
    Ok(previous) -> dict.delete(from.direct, previous)
    Error(Nil) -> from.direct
  }

  BiMap(direct:, reverse:)
}
