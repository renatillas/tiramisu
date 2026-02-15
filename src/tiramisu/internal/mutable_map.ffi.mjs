import { Result$Ok, Result$Error } from "../../../prelude.mjs";

export function empty() {
  return new Map();
}

export function get(map, key) {
  return map?.get(key);
}

export function get_or_compute(map, key, compute) {
  return map?.get(key) ?? compute();
}

export function has_key(map, key) {
  return map && map.has(key);
}

export function insert(map, key, value) {
  map ??= new Map();
  map.set(key, value);

  return map;
}

export function remove(map, key) {
  map?.delete(key);

  return map;
}

export function size(map) {
  return map ? map.size : 0;
}

export function fold(map, initial, callback) {
  if (!map) return initial;
  let acc = initial;
  map.forEach((value, key) => {
    acc = callback(acc, key, value);
  });
  return acc;
}


export function safe_get(map, key) {
  if (!map || !map.has(key)) return Result$Error();
  return Result$Ok(map.get(key));
}
