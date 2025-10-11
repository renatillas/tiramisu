import { toList, prepend as listPrepend } from "../../gleam.mjs";
import {
  toArray as from_list,
  length as size,
  map,
  reduce as fold,
  reduceRight as fold_right,
  index as get,
} from "../../gleam_javascript_ffi.mjs";

export { fold, fold_right, from_list, get, map, size };

/**
 * Convert a JavaScript array to a Gleam list.
 *
 * Runs in linear time.
 */
export function to_list(items) {
  return fold_right(
    items,
    toList([]),
    (list, item) => { return listPrepend(item, list); },
  );
}
