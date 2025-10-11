import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import { toList, CustomType as $CustomType } from "../gleam.mjs";

class BiMultiMap extends $CustomType {
  constructor(direct, reverse) {
    super();
    this.direct = direct;
    this.reverse = reverse;
  }
}

export function new$() {
  return new BiMultiMap($dict.new$(), $dict.new$());
}

/**
 * Insert a key an associated value
 */
export function insert(into, key, value) {
  let direct = $dict.upsert(
    into.direct,
    key,
    (option) => {
      if (option instanceof Some) {
        let existing = option[0];
        return $set.insert(existing, value);
      } else {
        return $set.from_list(toList([value]));
      }
    },
  );
  let reverse = $dict.upsert(
    into.reverse,
    value,
    (option) => {
      if (option instanceof Some) {
        let existing = option[0];
        return $set.insert(existing, key);
      } else {
        return $set.from_list(toList([key]));
      }
    },
  );
  return new BiMultiMap(direct, reverse);
}

/**
 * Get the associated values for a key
 */
export function get(from, key) {
  let _pipe = $dict.get(from.direct, key);
  return $result.unwrap(_pipe, $set.new$());
}

/**
 * Get the associated keys for a value
 */
export function get_val(from, value) {
  let _pipe = $dict.get(from.reverse, value);
  return $result.unwrap(_pipe, $set.new$());
}

function delete_in_dict(dict, dict_key, value) {
  return $dict.upsert(
    dict,
    dict_key,
    (option) => {
      if (option instanceof Some) {
        let existing = option[0];
        return $set.delete$(existing, value);
      } else {
        return $set.new$();
      }
    },
  );
}

/**
 * Delete a key from the multimap
 * This removes the association from this key to any values
 * and from values to this key
 */
export function delete$(from, key) {
  let values = get(from, key);
  let direct = $dict.delete$(from.direct, key);
  let reverse = $set.fold(
    values,
    from.reverse,
    (acc, value) => { return delete_in_dict(acc, value, key); },
  );
  return new BiMultiMap(direct, reverse);
}

/**
 * Delete a value from the multimap
 * This removes association from keys to this value and
 * from this value to keys
 */
export function delete_val(from, value) {
  let keys = get_val(from, value);
  let reverse = $dict.delete$(from.reverse, value);
  let direct = $set.fold(
    keys,
    from.direct,
    (acc, key) => { return delete_in_dict(acc, key, value); },
  );
  return new BiMultiMap(direct, reverse);
}

/**
 * Delete a specific combination of key and value
 */
export function delete_key_val(from, key, value) {
  let direct = delete_in_dict(from.direct, key, value);
  let reverse = delete_in_dict(from.reverse, value, key);
  return new BiMultiMap(direct, reverse);
}
