import { make as new$, get, update } from "./booklet_ffi.mjs";

export { get, new$, update };

/**
 * Atomically replace the value stored in the booklet.
 */
export function set(booklet, new_value) {
  update(booklet, (_) => { return new_value; });
  return undefined;
}
