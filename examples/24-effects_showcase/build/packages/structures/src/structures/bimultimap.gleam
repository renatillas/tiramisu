//// A bidirectional multi map (BiMultiMap)
//// A data structure that stores associations between keys and values
//// in both directions
//// A key can point to multiple values
//// A value can point to multiple keys
////
//// For example, keys have association to values:
//// ```
//// animal → [cat, lion]
//// pet → [cat]
//// ```
//// And values have reverse association to those keys
//// ```
//// cat → [animal, pet]
//// lion → [animal]
//// ```

import gleam/dict.{type Dict}
import gleam/option.{None, Some}
import gleam/result
import gleam/set.{type Set}

pub opaque type BiMultiMap(k, v) {
  BiMultiMap(direct: Dict(k, Set(v)), reverse: Dict(v, Set(k)))
}

pub fn new() {
  BiMultiMap(direct: dict.new(), reverse: dict.new())
}

/// Insert a key an associated value
pub fn insert(into into: BiMultiMap(k, v), key key: k, value value: v) {
  let direct =
    dict.upsert(into.direct, key, fn(option) {
      case option {
        None -> set.from_list([value])
        Some(existing) -> set.insert(existing, value)
      }
    })
  let reverse =
    dict.upsert(into.reverse, value, fn(option) {
      case option {
        None -> set.from_list([key])
        Some(existing) -> set.insert(existing, key)
      }
    })
  BiMultiMap(direct:, reverse:)
}

/// Get the associated values for a key
pub fn get(from from: BiMultiMap(k, v), key key: k) -> Set(v) {
  dict.get(from.direct, key) |> result.unwrap(set.new())
}

/// Get the associated keys for a value
pub fn get_val(from from: BiMultiMap(k, v), value value: v) -> Set(k) {
  dict.get(from.reverse, value)
  |> result.unwrap(set.new())
}

fn delete_in_dict(dict, dict_key, value) {
  dict.upsert(dict, dict_key, fn(option) {
    case option {
      None -> set.new()
      Some(existing) -> set.delete(existing, value)
    }
  })
}

/// Delete a key from the multimap
/// This removes the association from this key to any values
/// and from values to this key
pub fn delete(from from: BiMultiMap(k, v), key key: k) {
  let values = get(from, key)
  let direct = dict.delete(from.direct, key)

  let reverse =
    set.fold(values, from.reverse, fn(acc, value) {
      delete_in_dict(acc, value, key)
    })

  BiMultiMap(direct:, reverse:)
}

/// Delete a value from the multimap
/// This removes association from keys to this value and
/// from this value to keys
pub fn delete_val(from from: BiMultiMap(k, v), value value: v) {
  let keys = get_val(from, value)
  let reverse = dict.delete(from.reverse, value)

  let direct =
    set.fold(keys, from.direct, fn(acc, key) { delete_in_dict(acc, key, value) })

  BiMultiMap(direct:, reverse:)
}

/// Delete a specific combination of key and value
pub fn delete_key_val(from from: BiMultiMap(k, v), key key: k, value value: v) {
  let direct = delete_in_dict(from.direct, key, value)
  let reverse = delete_in_dict(from.reverse, value, key)
  BiMultiMap(direct:, reverse:)
}
