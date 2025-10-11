import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $decode from "../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, CustomType as $CustomType, makeError, divideFloat } from "../gleam.mjs";

const FILEPATH = "src/gleam_community/colour.gleam";

class Rgba extends $CustomType {
  constructor(r, g, b, a) {
    super();
    this.r = r;
    this.g = g;
    this.b = b;
    this.a = a;
  }
}

class Hsla extends $CustomType {
  constructor(h, s, l, a) {
    super();
    this.h = h;
    this.s = s;
    this.l = l;
    this.a = a;
  }
}

function valid_colour_value(c) {
  let $ = (c > 1.0) || (c < 0.0);
  if ($) {
    return new Error(undefined);
  } else {
    return new Ok(c);
  }
}

function hue_to_rgb(hue, m1, m2) {
  let _block;
  if (hue < 0.0) {
    _block = hue + 1.0;
  } else if (hue > 1.0) {
    _block = hue - 1.0;
  } else {
    _block = hue;
  }
  let h = _block;
  let h_t_6 = h * 6.0;
  let h_t_2 = h * 2.0;
  let h_t_3 = h * 3.0;
  if (h_t_6 < 1.0) {
    return m1 + (((m2 - m1) * h) * 6.0);
  } else if (h_t_2 < 1.0) {
    return m2;
  } else if (h_t_3 < 2.0) {
    return m1 + (((m2 - m1) * ((2.0 / 3.0) - h)) * 6.0);
  } else {
    return m1;
  }
}

function hex_string_to_int(hex_string) {
  let _block;
  if (hex_string.startsWith("#")) {
    let hex_number = hex_string.slice(1);
    _block = hex_number;
  } else if (hex_string.startsWith("0x")) {
    let hex_number = hex_string.slice(2);
    _block = hex_number;
  } else {
    _block = hex_string;
  }
  let hex = _block;
  let _pipe = hex;
  let _pipe$1 = $string.lowercase(_pipe);
  let _pipe$2 = $string.to_graphemes(_pipe$1);
  let _pipe$3 = $list.reverse(_pipe$2);
  return $list.index_fold(
    _pipe$3,
    new Ok(0),
    (total, char, index) => {
      if (total instanceof Ok) {
        let v = total[0];
        return $result.try$(
          (() => {
            if (char === "a") {
              return new Ok(10);
            } else if (char === "b") {
              return new Ok(11);
            } else if (char === "c") {
              return new Ok(12);
            } else if (char === "d") {
              return new Ok(13);
            } else if (char === "e") {
              return new Ok(14);
            } else if (char === "f") {
              return new Ok(15);
            } else {
              return $int.parse(char);
            }
          })(),
          (num) => {
            return $result.try$(
              $int.power(16, $int.to_float(index)),
              (base) => {
                return new Ok(v + $float.round($int.to_float(num) * base));
              },
            );
          },
        );
      } else {
        return total;
      }
    },
  );
}

function hsla_to_rgba(h, s, l, a) {
  let _block;
  let $ = l <= 0.5;
  if ($) {
    _block = l * (s + 1.0);
  } else {
    _block = (l + s) - (l * s);
  }
  let m2 = _block;
  let m1 = (l * 2.0) - m2;
  let r = hue_to_rgb(h + (1.0 / 3.0), m1, m2);
  let g = hue_to_rgb(h, m1, m2);
  let b = hue_to_rgb(h - (1.0 / 3.0), m1, m2);
  return [r, g, b, a];
}

function rgba_to_hsla(r, g, b, a) {
  let min_colour = $float.min(r, $float.min(g, b));
  let max_colour = $float.max(r, $float.max(g, b));
  let _block;
  let $ = true;
  if (max_colour === r) {
    _block = $float.divide(g - b, max_colour - min_colour);
  } else if (max_colour === g) {
    let _pipe = $float.divide(b - r, max_colour - min_colour);
    _block = $result.try$(_pipe, (d) => { return new Ok(2.0 + d); });
  } else {
    let _pipe = $float.divide(r - g, max_colour - min_colour);
    _block = $result.try$(_pipe, (d) => { return new Ok(4.0 + d); });
  }
  let h1 = _block;
  let _block$1;
  if (h1 instanceof Ok) {
    let v = h1[0];
    _block$1 = new Ok(v * (1.0 / 6.0));
  } else {
    _block$1 = h1;
  }
  let h2 = _block$1;
  let _block$2;
  if (h2 instanceof Ok) {
    let v = h2[0];
    if (v < 0.0) {
      _block$2 = v + 1.0;
    } else {
      let v$1 = h2[0];
      _block$2 = v$1;
    }
  } else {
    _block$2 = 0.0;
  }
  let h3 = _block$2;
  let l = (min_colour + max_colour) / 2.0;
  let _block$3;
  let $1 = true;
  if (min_colour === max_colour) {
    _block$3 = 0.0;
  } else if (l < 0.5) {
    _block$3 = divideFloat((max_colour - min_colour), (max_colour + min_colour));
  } else {
    _block$3 = divideFloat(
      (max_colour - min_colour),
      ((2.0 - max_colour) - min_colour)
    );
  }
  let s = _block$3;
  return [h3, s, l, a];
}

/**
 * Returns a `Result(Colour)` created from the given 8 bit RGB values.
 *
 * Returns `Error(Nil)` if the supplied RGB values are greater than 255 or less than 0.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgb255(255, 0, 0)
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
export function from_rgb255(red, green, blue) {
  return $result.try$(
    (() => {
      let _pipe = red;
      let _pipe$1 = $int.to_float(_pipe);
      let _pipe$2 = $float.divide(_pipe$1, 255.0);
      return $result.try$(_pipe$2, valid_colour_value);
    })(),
    (r) => {
      return $result.try$(
        (() => {
          let _pipe = green;
          let _pipe$1 = $int.to_float(_pipe);
          let _pipe$2 = $float.divide(_pipe$1, 255.0);
          return $result.try$(_pipe$2, valid_colour_value);
        })(),
        (g) => {
          return $result.try$(
            (() => {
              let _pipe = blue;
              let _pipe$1 = $int.to_float(_pipe);
              let _pipe$2 = $float.divide(_pipe$1, 255.0);
              return $result.try$(_pipe$2, valid_colour_value);
            })(),
            (b) => { return new Ok(new Rgba(r, g, b, 1.0)); },
          );
        },
      );
    },
  );
}

/**
 * Returns `Result(Colour)` created from the given RGB values.
 *
 * If the supplied RGB values are greater than 1.0 or less than 0.0 returns `Error(Nil)`
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgb(1.0, 0.0, 0.0)
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
export function from_rgb(red, green, blue) {
  return $result.try$(
    valid_colour_value(red),
    (r) => {
      return $result.try$(
        valid_colour_value(green),
        (g) => {
          return $result.try$(
            valid_colour_value(blue),
            (b) => { return new Ok(new Rgba(r, g, b, 1.0)); },
          );
        },
      );
    },
  );
}

/**
 * Returns `Result(Colour)` created from the given RGBA values.
 *
 * Returns `Error(Nil)` if the supplied RGBA values are greater than 1.0 or less than 0.0.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red_half_opacity) = from_rbga(1.0, 0.0, 0.0, 0.5)
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
export function from_rgba(red, green, blue, alpha) {
  return $result.try$(
    valid_colour_value(red),
    (r) => {
      return $result.try$(
        valid_colour_value(green),
        (g) => {
          return $result.try$(
            valid_colour_value(blue),
            (b) => {
              return $result.try$(
                valid_colour_value(alpha),
                (a) => { return new Ok(new Rgba(r, g, b, a)); },
              );
            },
          );
        },
      );
    },
  );
}

/**
 * Returns `Result(Colour)` created from the given HSLA values.
 *
 * Returns `Error(Nil)`f the supplied HSLA values are greater than 1.0 or less than 0.0.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red_half_opacity) = from_hsla(0.0, 1.0, 0.5, 0.5)
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
export function from_hsla(hue, saturation, lightness, alpha) {
  return $result.try$(
    valid_colour_value(hue),
    (h) => {
      return $result.try$(
        valid_colour_value(saturation),
        (s) => {
          return $result.try$(
            valid_colour_value(lightness),
            (l) => {
              return $result.try$(
                valid_colour_value(alpha),
                (a) => { return new Ok(new Hsla(h, s, l, a)); },
              );
            },
          );
        },
      );
    },
  );
}

/**
 * Returns `Result(Colour)` created from the given HSL values.
 *
 * Returns `Error(Nil)` if the supplied HSL values are greater than 1.0 or less than 0.0.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_hsla(0.0, 1.0, 0.5)
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
export function from_hsl(hue, saturation, lightness) {
  return from_hsla(hue, saturation, lightness, 1.0);
}

/**
 * Returns a `Result(Colour)` created from the given hex `Int`.
 *
 * Returns `Error(Nil)` if the supplied hex `Int is greater than 0xffffff or less than 0x0.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgb_hex(0xff0000)
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
export function from_rgb_hex(hex) {
  let $ = (hex > 0xffffff) || (hex < 0);
  if ($) {
    return new Error(undefined);
  } else {
    let _block;
    let _pipe = $int.bitwise_shift_right(hex, 16);
    _block = $int.bitwise_and(_pipe, 0xff);
    let r = _block;
    let _block$1;
    let _pipe$1 = $int.bitwise_shift_right(hex, 8);
    _block$1 = $int.bitwise_and(_pipe$1, 0xff);
    let g = _block$1;
    let b = $int.bitwise_and(hex, 0xff);
    return from_rgb255(r, g, b);
  }
}

/**
 * Returns a `Result(Colour)` created from the given RGB hex `String`.
 *
 * Returns `Error(Nil)` if the supplied hex `String` is invalid, or greater than `"#ffffff" or less than `"#0"`
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgb_hex_string("#ff0000")
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
export function from_rgb_hex_string(hex_string) {
  return $result.try$(
    hex_string_to_int(hex_string),
    (hex_int) => { return from_rgb_hex(hex_int); },
  );
}

/**
 * Returns a `Result(Colour)` created from the given hex `Int`.
 *
 * Returns `Error(Nil)` if the supplied hex `Int is greater than 0xffffffff or less than 0x0.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red_half_opacity) = from_rgba_hex(0xff00007f)
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
export function from_rgba_hex(hex) {
  let $ = (hex > 0xffffffff) || (hex < 0);
  if ($) {
    return new Error(undefined);
  } else {
    let _block;
    let _pipe = $int.bitwise_shift_right(hex, 24);
    let _pipe$1 = $int.bitwise_and(_pipe, 0xff);
    let _pipe$2 = $int.to_float(_pipe$1);
    _block = $float.divide(_pipe$2, 255.0);
    let $1 = _block;
    let r;
    if ($1 instanceof Ok) {
      r = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/colour",
        590,
        "from_rgba_hex",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 17111,
          end: 17260,
          pattern_start: 17122,
          pattern_end: 17127
        }
      )
    }
    let _block$1;
    let _pipe$3 = $int.bitwise_shift_right(hex, 16);
    let _pipe$4 = $int.bitwise_and(_pipe$3, 0xff);
    let _pipe$5 = $int.to_float(_pipe$4);
    _block$1 = $float.divide(_pipe$5, 255.0);
    let $2 = _block$1;
    let g;
    if ($2 instanceof Ok) {
      g = $2[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/colour",
        596,
        "from_rgba_hex",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $2,
          start: 17332,
          end: 17481,
          pattern_start: 17343,
          pattern_end: 17348
        }
      )
    }
    let _block$2;
    let _pipe$6 = $int.bitwise_shift_right(hex, 8);
    let _pipe$7 = $int.bitwise_and(_pipe$6, 0xff);
    let _pipe$8 = $int.to_float(_pipe$7);
    _block$2 = $float.divide(_pipe$8, 255.0);
    let $3 = _block$2;
    let b;
    if ($3 instanceof Ok) {
      b = $3[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/colour",
        602,
        "from_rgba_hex",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $3,
          start: 17553,
          end: 17701,
          pattern_start: 17564,
          pattern_end: 17569
        }
      )
    }
    let _block$3;
    let _pipe$9 = $int.bitwise_and(hex, 0xff);
    let _pipe$10 = $int.to_float(_pipe$9);
    _block$3 = $float.divide(_pipe$10, 255.0);
    let $4 = _block$3;
    let a;
    if ($4 instanceof Ok) {
      a = $4[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/colour",
        608,
        "from_rgba_hex",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $4,
          start: 17773,
          end: 17883,
          pattern_start: 17784,
          pattern_end: 17789
        }
      )
    }
    return from_rgba(r, g, b, a);
  }
}

/**
 * Returns a `Result(Colour)` created from the given RGBA hex `String`.
 *
 * Returns `Error(Nil)` if the supplied hex `String` is invalid, or greater than `"#ffffffff" or less than `"#0"`
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red_half_opacity) = from_rgba_hex_string("#ff00007f")
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
export function from_rgba_hex_string(hex_string) {
  return $result.try$(
    hex_string_to_int(hex_string),
    (hex_int) => { return from_rgba_hex(hex_int); },
  );
}

/**
 * Returns `#(Float, Float, Float, Float)` representing the given `Colour`'s
 * R, G, B, and A values respectively.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgb255(255, 0, 0)
 *   let #(r, g, b, a) = to_rgba(red)
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
export function to_rgba(colour) {
  if (colour instanceof Rgba) {
    let r = colour.r;
    let g = colour.g;
    let b = colour.b;
    let a = colour.a;
    return [r, g, b, a];
  } else {
    let h = colour.h;
    let s = colour.s;
    let l = colour.l;
    let a = colour.a;
    return hsla_to_rgba(h, s, l, a);
  }
}

/**
 * Returns `#(Float, Float, Float, Float)` representing the given `Colour`'s
 * H, S, L, and A values respectively.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgb255(255, 0, 0)
 *   let #(h, s, l, a) = to_hsla(red)
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
export function to_hsla(colour) {
  if (colour instanceof Rgba) {
    let r = colour.r;
    let g = colour.g;
    let b = colour.b;
    let a = colour.a;
    return rgba_to_hsla(r, g, b, a);
  } else {
    let h = colour.h;
    let s = colour.s;
    let l = colour.l;
    let a = colour.a;
    return [h, s, l, a];
  }
}

/**
 * Returns an rgba formatted CSS `String` created from the given `Colour`.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgb255(255, 0, 0)
 *   let css_red = to_css_rgba_string(red)
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
export function to_css_rgba_string(colour) {
  let $ = to_rgba(colour);
  let r;
  let g;
  let b;
  let a;
  r = $[0];
  g = $[1];
  b = $[2];
  a = $[3];
  let percent = (x) => {
    let _block;
    let _pipe = x;
    let _pipe$1 = $float.multiply(_pipe, 10_000.0);
    let _pipe$2 = $float.round(_pipe$1);
    let _pipe$3 = $int.to_float(_pipe$2);
    _block = $float.divide(_pipe$3, 100.0);
    let $1 = _block;
    let p;
    if ($1 instanceof Ok) {
      p = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/colour",
        706,
        "to_css_rgba_string",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 20510,
          end: 20646,
          pattern_start: 20521,
          pattern_end: 20526
        }
      )
    }
    return p;
  };
  let round_to = (x) => {
    let _block;
    let _pipe = x;
    let _pipe$1 = $float.multiply(_pipe, 1000.0);
    let _pipe$2 = $float.round(_pipe$1);
    let _pipe$3 = $int.to_float(_pipe$2);
    _block = $float.divide(_pipe$3, 1000.0);
    let $1 = _block;
    let r$1;
    if ($1 instanceof Ok) {
      r$1 = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "gleam_community/colour",
        718,
        "to_css_rgba_string",
        "Pattern match failed, no pattern matched the value.",
        {
          value: $1,
          start: 20768,
          end: 20903,
          pattern_start: 20779,
          pattern_end: 20784
        }
      )
    }
    return r$1;
  };
  return $string.join(
    toList([
      "rgba(",
      $float.to_string(percent(r)) + "%,",
      $float.to_string(percent(g)) + "%,",
      $float.to_string(percent(b)) + "%,",
      $float.to_string(round_to(a)),
      ")",
    ]),
    "",
  );
}

/**
 * Returns an hex `Int` created from the given `Colour`.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgba(1.0, 0.0, 0.0, 1.0)
 *   let red_hex_int = to_rgba_hex(red)
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
export function to_rgba_hex(colour) {
  let $ = to_rgba(colour);
  let r;
  let g;
  let b;
  let a;
  r = $[0];
  g = $[1];
  b = $[2];
  a = $[3];
  let _block;
  let _pipe = r * 255.0;
  let _pipe$1 = $float.round(_pipe);
  _block = $int.bitwise_shift_left(_pipe$1, 24);
  let red$1 = _block;
  let _block$1;
  let _pipe$2 = g * 255.0;
  let _pipe$3 = $float.round(_pipe$2);
  _block$1 = $int.bitwise_shift_left(_pipe$3, 16);
  let green$1 = _block$1;
  let _block$2;
  let _pipe$4 = b * 255.0;
  let _pipe$5 = $float.round(_pipe$4);
  _block$2 = $int.bitwise_shift_left(_pipe$5, 8);
  let blue$1 = _block$2;
  let _block$3;
  let _pipe$6 = a * 255.0;
  _block$3 = $float.round(_pipe$6);
  let alpha = _block$3;
  return ((red$1 + green$1) + blue$1) + alpha;
}

/**
 * Returns an rgba hex formatted `String` created from the given `Colour`.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgba(1.0, 0.0, 0.0, 1.0)
 *   let red_hex = to_rgba_hex_string(red)
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
export function to_rgba_hex_string(colour) {
  let _block;
  let _pipe = to_rgba_hex(colour);
  _block = $int.to_base16(_pipe);
  let hex_string = _block;
  let $ = $string.length(hex_string);
  if ($ === 8) {
    return hex_string;
  } else {
    let l = $;
    return $string.repeat("0", 8 - l) + hex_string;
  }
}

/**
 * Returns a rgb hex `Int` created from the given `Colour`.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgba(255, 0, 0)
 *   let red_hex_int = to_rgb_hex(red)
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
export function to_rgb_hex(colour) {
  let $ = to_rgba(colour);
  let r;
  let g;
  let b;
  r = $[0];
  g = $[1];
  b = $[2];
  let _block;
  let _pipe = r * 255.0;
  let _pipe$1 = $float.round(_pipe);
  _block = $int.bitwise_shift_left(_pipe$1, 16);
  let red$1 = _block;
  let _block$1;
  let _pipe$2 = g * 255.0;
  let _pipe$3 = $float.round(_pipe$2);
  _block$1 = $int.bitwise_shift_left(_pipe$3, 8);
  let green$1 = _block$1;
  let _block$2;
  let _pipe$4 = b * 255.0;
  _block$2 = $float.round(_pipe$4);
  let blue$1 = _block$2;
  return (red$1 + green$1) + blue$1;
}

/**
 * Returns an rgb hex formatted `String` created from the given `Colour`.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   assert Ok(red) = from_rgba(255, 0, 0)
 *   let red_hex = to_rgb_hex_string(red)
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
export function to_rgb_hex_string(colour) {
  let _block;
  let _pipe = to_rgb_hex(colour);
  _block = $int.to_base16(_pipe);
  let hex_string = _block;
  let $ = $string.length(hex_string);
  if ($ === 6) {
    return hex_string;
  } else {
    let l = $;
    return $string.repeat("0", 6 - l) + hex_string;
  }
}

function encode_rgba(r, g, b, a) {
  return $json.object(
    toList([
      ["r", $json.float(r)],
      ["g", $json.float(g)],
      ["b", $json.float(b)],
      ["a", $json.float(a)],
    ]),
  );
}

function encode_hsla(h, s, l, a) {
  return $json.object(
    toList([
      ["h", $json.float(h)],
      ["s", $json.float(s)],
      ["l", $json.float(l)],
      ["a", $json.float(a)],
    ]),
  );
}

/**
 * Encodes a `Colour` value as a Gleam [`Json`](https://hexdocs.pm/gleam_json/gleam/json.html#Json)
 * value. You'll need this if you want to send a `Colour` value over the network
 * in a HTTP request or response, for example.
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
export function encode(colour) {
  if (colour instanceof Rgba) {
    let r = colour.r;
    let g = colour.g;
    let b = colour.b;
    let a = colour.a;
    return encode_rgba(r, g, b, a);
  } else {
    let h = colour.h;
    let s = colour.s;
    let l = colour.l;
    let a = colour.a;
    return encode_hsla(h, s, l, a);
  }
}

function rgba_decoder() {
  return $decode.field(
    "r",
    $decode.float,
    (r) => {
      return $decode.field(
        "g",
        $decode.float,
        (g) => {
          return $decode.field(
            "b",
            $decode.float,
            (b) => {
              return $decode.field(
                "a",
                $decode.float,
                (a) => { return $decode.success(new Rgba(r, g, b, a)); },
              );
            },
          );
        },
      );
    },
  );
}

function hsla_decoder() {
  return $decode.field(
    "h",
    $decode.float,
    (h) => {
      return $decode.field(
        "s",
        $decode.float,
        (s) => {
          return $decode.field(
            "l",
            $decode.float,
            (l) => {
              return $decode.field(
                "a",
                $decode.float,
                (a) => { return $decode.success(new Hsla(h, s, l, a)); },
              );
            },
          );
        },
      );
    },
  );
}

/**
 * Attempt to decode some [`Dynamic`](https://hexdocs.pm/gleam_stdlib/gleam/dynamic.html#Dynamic)
 * value into a `Colour`. Most often you'll use this to decode some JSON.
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
export function decoder() {
  return $decode.one_of(rgba_decoder(), toList([hsla_decoder()]));
}

/**
 * A `Colour` reprsenting the colour RGBA(239, 41, 41, 1.0)
 */
export const light_red = /* @__PURE__ */ new Rgba(
  0.9372549019607843,
  0.1607843137254902,
  0.1607843137254902,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(204, 0, 0, 1.0)
 */
export const red = /* @__PURE__ */ new Rgba(0.8, 0.0, 0.0, 1.0);

/**
 * A `Colour` reprsenting the colour RGBA(164, 0, 0, 1.0)
 */
export const dark_red = /* @__PURE__ */ new Rgba(
  0.6431372549019608,
  0.0,
  0.0,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(252, 175, 62, 1.0)
 */
export const light_orange = /* @__PURE__ */ new Rgba(
  0.9882352941176471,
  0.6862745098039216,
  0.24313725490196078,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(245, 121, 0, 1.0)
 */
export const orange = /* @__PURE__ */ new Rgba(
  0.9607843137254902,
  0.4745098039215686,
  0.0,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(206, 92, 0, 1.0)
 */
export const dark_orange = /* @__PURE__ */ new Rgba(
  0.807843137254902,
  0.3607843137254902,
  0.0,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(255, 233, 79, 1.0)
 */
export const light_yellow = /* @__PURE__ */ new Rgba(
  1.0,
  0.9137254901960784,
  0.30980392156862746,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(237, 212, 0, 1.0)
 */
export const yellow = /* @__PURE__ */ new Rgba(
  0.9294117647058824,
  0.8313725490196079,
  0.0,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(196, 160, 0, 1.0)
 */
export const dark_yellow = /* @__PURE__ */ new Rgba(
  0.7686274509803922,
  0.6274509803921569,
  0.0,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(138, 226, 52, 1.0)
 */
export const light_green = /* @__PURE__ */ new Rgba(
  0.5411764705882353,
  0.8862745098039215,
  0.20392156862745098,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(115, 210, 22, 1.0)
 */
export const green = /* @__PURE__ */ new Rgba(
  0.45098039215686275,
  0.8235294117647058,
  0.08627450980392157,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(78, 154, 6, 1.0)
 */
export const dark_green = /* @__PURE__ */ new Rgba(
  0.3058823529411765,
  0.6039215686274509,
  0.023529411764705882,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(114, 159, 207, 1.0)
 */
export const light_blue = /* @__PURE__ */ new Rgba(
  0.4470588235294118,
  0.6235294117647059,
  0.8117647058823529,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(52, 101, 164, 1.0)
 */
export const blue = /* @__PURE__ */ new Rgba(
  0.20392156862745098,
  0.396078431372549,
  0.6431372549019608,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(32, 74, 135, 1.0)
 */
export const dark_blue = /* @__PURE__ */ new Rgba(
  0.12549019607843137,
  0.2901960784313726,
  0.5294117647058824,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(173, 127, 168, 1.0)
 */
export const light_purple = /* @__PURE__ */ new Rgba(
  0.6784313725490196,
  0.4980392156862745,
  0.6588235294117647,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(117, 80, 123, 1.0)
 */
export const purple = /* @__PURE__ */ new Rgba(
  0.4588235294117647,
  0.3137254901960784,
  0.4823529411764706,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(92, 53, 102, 1.0)
 */
export const dark_purple = /* @__PURE__ */ new Rgba(
  0.3607843137254902,
  0.20784313725490197,
  0.4,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(233, 185, 110, 1.0)
 */
export const light_brown = /* @__PURE__ */ new Rgba(
  0.9137254901960784,
  0.7254901960784313,
  0.43137254901960786,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(193, 125, 17, 1.0)
 */
export const brown = /* @__PURE__ */ new Rgba(
  0.7568627450980392,
  0.49019607843137253,
  0.06666666666666667,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(143, 89, 2, 1.0)
 */
export const dark_brown = /* @__PURE__ */ new Rgba(
  0.5607843137254902,
  0.34901960784313724,
  0.00784313725490196,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(0, 0, 0, 1.0)
 */
export const black = /* @__PURE__ */ new Rgba(0.0, 0.0, 0.0, 1.0);

/**
 * A `Colour` reprsenting the colour RGBA(255, 255, 255, 1.0)
 */
export const white = /* @__PURE__ */ new Rgba(1.0, 1.0, 1.0, 1.0);

/**
 * A `Colour` reprsenting the colour RGBA(238, 238, 236, 1.0)
 */
export const light_grey = /* @__PURE__ */ new Rgba(
  0.9333333333333333,
  0.9333333333333333,
  0.9254901960784314,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(211, 215, 207, 1.0)
 */
export const grey = /* @__PURE__ */ new Rgba(
  0.8274509803921568,
  0.8431372549019608,
  0.8117647058823529,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(186, 189, 182, 1.0)
 */
export const dark_grey = /* @__PURE__ */ new Rgba(
  0.7294117647058823,
  0.7411764705882353,
  0.7137254901960784,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(238, 238, 236, 1.0)
 */
export const light_gray = /* @__PURE__ */ new Rgba(
  0.9333333333333333,
  0.9333333333333333,
  0.9254901960784314,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(211, 215, 207, 1.0)
 */
export const gray = /* @__PURE__ */ new Rgba(
  0.8274509803921568,
  0.8431372549019608,
  0.8117647058823529,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(186, 189, 182, 1.0)
 */
export const dark_gray = /* @__PURE__ */ new Rgba(
  0.7294117647058823,
  0.7411764705882353,
  0.7137254901960784,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(136, 138, 133, 1.0)
 */
export const light_charcoal = /* @__PURE__ */ new Rgba(
  0.5333333333333333,
  0.5411764705882353,
  0.5215686274509804,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(85, 87, 83, 1.0)
 */
export const charcoal = /* @__PURE__ */ new Rgba(
  0.3333333333333333,
  0.3411764705882353,
  0.3254901960784314,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(46, 52, 54, 1.0)
 */
export const dark_charcoal = /* @__PURE__ */ new Rgba(
  0.1803921568627451,
  0.20392156862745098,
  0.21176470588235294,
  1.0,
);

/**
 * A `Colour` reprsenting the colour RGBA(255, 175, 243, 1.0)
 */
export const pink = /* @__PURE__ */ new Rgba(
  1.0,
  0.6862745098039216,
  0.9529411764705882,
  1.0,
);
