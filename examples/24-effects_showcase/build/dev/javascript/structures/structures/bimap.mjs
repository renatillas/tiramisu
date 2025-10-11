import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import { Ok, CustomType as $CustomType } from "../gleam.mjs";

class BiMap extends $CustomType {
  constructor(direct, reverse) {
    super();
    this.direct = direct;
    this.reverse = reverse;
  }
}

export function new$() {
  return new BiMap($dict.new$(), $dict.new$());
}

/**
 * Get the associated value for a key
 */
export function get(from, key) {
  return $dict.get(from.direct, key);
}

/**
 * Get the associated key for a value
 */
export function get_val(from, value) {
  return $dict.get(from.reverse, value);
}

/**
 * Delete a key and associated value from the bimap
 */
export function delete$(from, key) {
  let value = get(from, key);
  let direct = $dict.delete$(from.direct, key);
  let _block;
  if (value instanceof Ok) {
    let previous = value[0];
    _block = $dict.delete$(from.reverse, previous);
  } else {
    _block = from.reverse;
  }
  let reverse = _block;
  return new BiMap(direct, reverse);
}

/**
 * Delete a value and associated key from the bimap
 */
export function delete_val(from, value) {
  let key = get_val(from, value);
  let reverse = $dict.delete$(from.reverse, value);
  let _block;
  if (key instanceof Ok) {
    let previous = key[0];
    _block = $dict.delete$(from.direct, previous);
  } else {
    _block = from.direct;
  }
  let direct = _block;
  return new BiMap(direct, reverse);
}

/**
 * Insert a key an associated value
 */
export function insert(into, key, value) {
  let _block;
  let _pipe = into;
  let _pipe$1 = delete$(_pipe, key);
  _block = delete_val(_pipe$1, value);
  let without = _block;
  let _block$1;
  let _pipe$2 = without.direct;
  _block$1 = $dict.insert(_pipe$2, key, value);
  let direct = _block$1;
  let _block$2;
  let _pipe$3 = without.reverse;
  _block$2 = $dict.insert(_pipe$3, value, key);
  let reverse = _block$2;
  return new BiMap(direct, reverse);
}
