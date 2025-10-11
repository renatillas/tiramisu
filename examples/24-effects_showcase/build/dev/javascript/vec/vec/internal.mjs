import { Ok, Error } from "../gleam.mjs";
import { cos as do_cos, acos as do_acos } from "../maths.mjs";

/**
 * The Cosine function.
 */
export function cos(x) {
  return do_cos(x);
}

/**
 * The inverse Cosine function.
 */
export function acos(x) {
  let $ = (x >= -1.0) && (x <= 1.0);
  if ($) {
    return new Ok(do_acos(x));
  } else {
    return new Error(undefined);
  }
}

/**
 * The mathematical constant Pi.
 */
export const pi = 3.1415926535897932;

/**
 * The Sine function.
 */
export function sin(x) {
  return cos((pi / 2.0) - x);
}
