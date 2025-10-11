import { empty as new$, get, has_key, insert, remove as delete$, size } from "./mutable_map.ffi.mjs";

export { delete$, get, has_key, insert, new$, size };

/**
 *
 */
export function is_empty(map) {
  return size(map) === 0;
}
