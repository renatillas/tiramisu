import * as $float from "../../../gleam_stdlib/gleam/float.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import { Ok, makeError, divideFloat } from "../../gleam.mjs";
import * as $colour from "../../gleam_community/colour.mjs";

const FILEPATH = "src/gleam_community/colour/accessibility.gleam";

function intensity(colour_value) {
  let $ = true;
  if (colour_value <= 0.03928) {
    return colour_value / 12.92;
  } else {
    let $1 = $float.power((colour_value + 0.055) / 1.055, 2.4);
    let i;
    if ($1 instanceof Ok) {
      i = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/colour/accessibility",
        62,
        "intensity",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 2399,
          end: 2470,
          pattern_start: 2410,
          pattern_end: 2415
        }
      )
    }
    return i;
  }
}

/**
 * Returns the relative brightness of the given `Colour` as a `Float` between
 * 0.0, and 1.0 with 0.0 being the darkest possible colour and 1.0 being the lightest.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   luminance(colour.white) // 1.0
 * }
 * ```
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/colour/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function luminance(colour) {
  let $ = $colour.to_rgba(colour);
  let r;
  let g;
  let b;
  r = $[0];
  g = $[1];
  b = $[2];
  let r_intensity = intensity(r);
  let g_intensity = intensity(g);
  let b_intensity = intensity(b);
  return ((0.2126 * r_intensity) + (0.7152 * g_intensity)) + (0.0722 * b_intensity);
}

/**
 * Returns the contrast between two `Colour` values as a `Float` between 1.0,
 * and 21.0 with 1.0 being no contrast and, 21.0 being the highest possible contrast.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   contrast_ratio(between: colour.white, and: colour.black) // 21.0
 * }
 * ```
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/colour/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function contrast_ratio(colour_a, colour_b) {
  let luminance_a = luminance(colour_a) + 0.05;
  let luminance_b = luminance(colour_b) + 0.05;
  let $ = luminance_a > luminance_b;
  if ($) {
    return divideFloat(luminance_a, luminance_b);
  } else {
    return divideFloat(luminance_b, luminance_a);
  }
}

/**
 * Returns the `Colour` with the highest contrast between the base `Colour`,
 * and and the other provided `Colour` values.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   maximum_contrast(
 *    colour.yellow,
 *    [colour.white, colour.dark_blue, colour.green],
 *  )
 * }
 * ```
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/colour/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function maximum_contrast(base, colours) {
  let _pipe = colours;
  let _pipe$1 = $list.sort(
    _pipe,
    (colour_a, colour_b) => {
      let contrast_a = contrast_ratio(base, colour_a);
      let contrast_b = contrast_ratio(base, colour_b);
      return $float.compare(contrast_b, contrast_a);
    },
  );
  return $list.first(_pipe$1);
}
