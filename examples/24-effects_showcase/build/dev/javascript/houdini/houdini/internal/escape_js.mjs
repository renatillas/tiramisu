import { do_escape } from "../../houdini.ffi.mjs";

/**
 * This `escape` function will work on all targets, beware that the version
 * specifically optimised for Erlang will be _way faster_ than this one when
 * running on the BEAM. That's why this fallback implementation is only ever
 * used when running on the JS backend.
 */
export function escape(text) {
  return do_escape(text);
}
