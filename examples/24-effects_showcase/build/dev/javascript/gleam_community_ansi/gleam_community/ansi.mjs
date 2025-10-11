import * as $gc_colour from "../../gleam_community_colour/gleam_community/colour.mjs";
import * as $regexp from "../../gleam_regexp/gleam/regexp.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Ok, toList, CustomType as $CustomType, makeError } from "../gleam.mjs";

const FILEPATH = "src/gleam_community/ansi.gleam";

class Code extends $CustomType {
  constructor(open, close, regexp) {
    super();
    this.open = open;
    this.close = close;
    this.regexp = regexp;
  }
}

/**
 * Applies colour and background based on colour code and its associated text
 * 
 * @ignore
 */
function run(text, code) {
  return (code.open + $string.replace(text, code.regexp, code.open)) + code.close;
}

/**
 * Strips the ansi control characters from the text.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   let bold_lucy = ansi.bold("lucy")
 *   // => "\x1B[1mlucy\x1B[22m"
 *   ansi.strip(bold_lucy)
 *   // => "lucy"
 * }
 * ```
 *
 * In this example, the text "lucy" is boldened by `ansi.bold` and then converted back to the original
 * string with `ansi.strip`.
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function strip(text) {
  let regexp_options = new $regexp.Options(false, true);
  let $ = $regexp.compile("(?:\\[(?:\\d+;?)+m)+", regexp_options);
  let r;
  if ($ instanceof Ok) {
    r = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "gleam_community/ansi",
      2346,
      "strip",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 68014,
        end: 68098,
        pattern_start: 68025,
        pattern_end: 68030
      }
    )
  }
  let _pipe = r;
  let _pipe$1 = $regexp.split(_pipe, text);
  return $string.join(_pipe$1, "");
}

const asci_escape_character = "\u{001b}";

/**
 * Builds colour code
 * 
 * @ignore
 */
function code(open, close) {
  let close_str = $int.to_string(close);
  let open_strs = $list.map(open, $int.to_string);
  return new Code(
    ((asci_escape_character + "[") + $string.join(open_strs, ";")) + "m",
    ((asci_escape_character + "[") + close_str) + "m",
    ((asci_escape_character + "[") + close_str) + "m",
  );
}

/**
 * Reset the text modified
 */
export function reset(text) {
  return run(text, code(toList([0]), 0));
}

/**
 * Style the given text bold.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bold("lucy")
 *   // => "\x1B[1mlucy\x1B[22m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[22m"` added to the string. This is the escape code
 * for the "default" bold/dim style of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * style, it will use both the outter style and the inner style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.dim("Isn't " <> ansi.bold("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be dim but the text "fun?" will be
 * both underlined, *and* bold!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bold(text) {
  return run(text, code(toList([1]), 22));
}

/**
 * Style the given text's colour to be dimmer.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.dim("lucy")
 *   // => "\x1B[2mlucy\x1B[22m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[22m"` added to the string. This is the escape code
 * for the "default" bold/dim style of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * style, it will use both the outter style and the inner style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.dim("Isn't " <> ansi.bold("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be dim but the text "fun?" will be
 * both underlined, *and* bold!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function dim(text) {
  return run(text, code(toList([2]), 22));
}

/**
 * Style the given text italic.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.italic("lucy")
 *   // => "\x1B[3mlucy\x1B[23m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[23m"` added to the string. This is the escape code
 * for the "default" italic style of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * style, it will use both the outter style and the inner style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.underline("Isn't " <> ansi.bold("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be underlined but the text "fun?" will be
 * both underlined, *and* bold!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function italic(text) {
  return run(text, code(toList([3]), 23));
}

/**
 * Style the given text's colour to be dimmer.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.underline("lucy")
 *   // => "\x1B[4mlucy\x1B[24m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[24m"` added to the string. This is the escape code
 * for the "default" underline style of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * style, it will use both the outter style and the inner style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.dim("Isn't " <> ansi.bold("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be dim but the text "fun?" will be
 * both underlined, *and* bold!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function underline(text) {
  return run(text, code(toList([4]), 24));
}

/**
 * Inverse the given text's colour, and background colour.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.inverse("lucy")
 *   // => "\x1B[7mlucy\x1B[27m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[27m"` added to the string. This is the escape code
 * for the "default" inverse style of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * style, it will use both the outter style and the inner style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.dim("Isn't " <> ansi.bold("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be dim but the text "fun?" will be
 * both underlined, *and* bold!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function inverse(text) {
  return run(text, code(toList([7]), 27));
}

/**
 * Style the given text to be hidden.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.hidden("lucy")
 *   // => "\x1B[8mlucy\x1B[28m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[28m"` added to the string. This is the escape code
 * for the "default" hidden style of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * style, it will use both the outter style and the inner style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.dim("Isn't " <> ansi.bold("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be dim but the text "fun?" will be
 * both underlined, *and* bold!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function hidden(text) {
  return run(text, code(toList([8]), 28));
}

/**
 * Style the given text to be striked through.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.strikethrough("lucy")
 *   // => "\x1B[9mlucy\x1B[29m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[29m"` added to the string. This is the escape code
 * for the "default" strikethrough style of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * style, it will use both the outter style and the inner style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.dim("Isn't " <> ansi.bold("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be dim but the text "fun?" will be
 * both underlined, *and* bold!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function strikethrough(text) {
  return run(text, code(toList([9]), 29));
}

/**
 * Colour the given text black.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.black("lucy")
 *   // => "\x1B[30mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function black(text) {
  return run(text, code(toList([30]), 39));
}

/**
 * Colour the given text red.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.red("lucy")
 *   // => "\x1B[31mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function red(text) {
  return run(text, code(toList([31]), 39));
}

/**
 * Colour the given text green.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.green("lucy")
 *   // => "\x1B[32mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function green(text) {
  return run(text, code(toList([32]), 39));
}

/**
 * Colour the given text yellow.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("lucy")
 *   // => "\x1B[33mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function yellow(text) {
  return run(text, code(toList([33]), 39));
}

/**
 * Colour the given text blue.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.blue("lucy")
 *   // => "\x1B[34mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function blue(text) {
  return run(text, code(toList([34]), 39));
}

/**
 * Colour the given text magenta.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.magenta("lucy")
 *   // => "\x1B[35mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function magenta(text) {
  return run(text, code(toList([35]), 39));
}

/**
 * Colour the given text cyan.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.cyan("lucy")
 *   // => "\x1B[36mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function cyan(text) {
  return run(text, code(toList([36]), 39));
}

/**
 * Colour the given text white.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.white("lucy")
 *   // => "\x1B[37mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function white(text) {
  return run(text, code(toList([37]), 39));
}

/**
 * Colour the given text bright black. This should increase the luminosity of
 * the base colour, but some terminals will interpret this as bold instead.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bright_black("lucy")
 *   // => "\x1B[90mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bright_black(text) {
  return run(text, code(toList([90]), 39));
}

/**
 * Colour the given text gray.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.gray("lucy")
 *   // => "\x1B[90mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function grey(text) {
  return bright_black(text);
}

/**
 * This is an alias for [`grey`](#grey) for those who prefer the American English
 * spelling.
 */
export function gray(text) {
  return bright_black(text);
}

/**
 * Colour the given text bright red. This should increase the luminosity of
 * the base colour, but some terminals will interpret this as bold instead.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bright_red("lucy")
 *   // => "\x1B[91mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bright_red(text) {
  return run(text, code(toList([91]), 39));
}

/**
 * Colour the given text bright green. This should increase the luminosity of
 * the base colour, but some terminals will interpret this as bold instead.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   ansi.bright_green("lucy")
 *   // => "\x1B[92mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bright_green(text) {
  return run(text, code(toList([92]), 39));
}

/**
 * Colour the given text bright yellow. This should increase the luminosity of
 * the base colour, but some terminals will interpret this as bold instead.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   ansi.bright_yellow("lucy")
 *   // => "\x1B[93mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bright_yellow(text) {
  return run(text, code(toList([93]), 39));
}

/**
 * Colour the given text bright blue. This should increase the luminosity of
 * the base colour, but some terminals will interpret this as bold instead.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   ansi.bright_blue("lucy")
 *   // => "\x1B[94mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bright_blue(text) {
  return run(text, code(toList([94]), 39));
}

/**
 * Colour the given text bright gremagentaen. This should increase the luminosity
 * of the base colour, but some terminals will interpret this as bold instead.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   ansi.bright_magenta("lucy")
 *   // => "\x1B[95mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bright_magenta(text) {
  return run(text, code(toList([95]), 39));
}

/**
 * Colour the given text bright cyan. This should increase the luminosity of
 * the base colour, but some terminals will interpret this as bold instead.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   ansi.bright_cyan("lucy")
 *   // => "\x1B[96mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bright_cyan(text) {
  return run(text, code(toList([96]), 39));
}

/**
 * Colour the given text bright white. This should increase the luminosity of
 * the base colour, but some terminals will interpret this as bold instead.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * fn example() {
 *   ansi.bright_white("lucy")
 *   // => "\x1B[97mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bright_white(text) {
  return run(text, code(toList([97]), 39));
}

/**
 * Colour the given text pink.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.pink("lucy")
 *   // => "\x1B[38;5;219mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[39m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function pink(text) {
  return run(text, code(toList([38, 5, 219]), 39));
}

/**
 * Colour the given text the given colour represented by a hex `Int`.
 *
 * The given hex Int can be any valid [shorthand hexadecimal form](https://en.wikipedia.org/wiki/Web_colors#Shorthand_hexadecimal_form).
 *
 * ❗️ Note that if supplied hex Int is less than 0 or larger than 0xfffff the
 * colour will be set to black and white respectively.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.hex("lucy", 0xffaff3)
 *   // => "\x1B[38;2;255;175;243mlucy\x1B[39m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function hex(text, colour) {
  let colour$1 = $int.clamp(colour, 0x0, 0xffffff);
  return run(
    text,
    code(
      toList([
        38,
        2,
        (() => {
          let _pipe = $int.bitwise_shift_right(colour$1, 16);
          return $int.bitwise_and(_pipe, 0xff);
        })(),
        (() => {
          let _pipe = $int.bitwise_shift_right(colour$1, 8);
          return $int.bitwise_and(_pipe, 0xff);
        })(),
        $int.bitwise_and(colour$1, 0xff),
      ]),
      39,
    ),
  );
}

/**
 * Colour the given text the given colour represented by a `Colour`.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 * import gleam_community/colour.{Colour}
 *
 * fn example() {
 *   let pink = colour.from_hsl(0.8583, 1.0, 0,84)
 *   ansi.colour("lucy", pink)
 *   // => "\x1B[48;2;255;175;243mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function colour(text, colour) {
  let hex_colour = $gc_colour.to_rgb_hex(colour);
  return hex(text, hex_colour);
}

/**
 * This is an alias for [`colour`](#colour) for those who prefer the American English
 * spelling.
 */
export function color(text, color) {
  return colour(text, color);
}

/**
 * Colour the given text's background black.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_black("lucy")
 *   // => "\x1B[40mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_black(text) {
  return run(text, code(toList([40]), 49));
}

/**
 * Colour the given text's background red.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_red("lucy")
 *   // => "\x1B[41mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_red(text) {
  return run(text, code(toList([41]), 49));
}

/**
 * Colour the given text's background green.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_green("lucy")
 *   // => "\x1B[42mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_green(text) {
  return run(text, code(toList([42]), 49));
}

/**
 * Colour the given text's background yellow.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_yellow("lucy")
 *   // => "\x1B[43mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_yellow(text) {
  return run(text, code(toList([43]), 49));
}

/**
 * Colour the given text's background blue.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_blue("lucy")
 *   // => "\x1B[44mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_blue(text) {
  return run(text, code(toList([44]), 49));
}

/**
 * Colour the given text's background magenta.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_magenta("lucy")
 *   // => "\x1B[45mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_magenta(text) {
  return run(text, code(toList([45]), 49));
}

/**
 * Colour the given text's background cyan.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_cyan("lucy")
 *   // => "\x1B[46mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_cyan(text) {
  return run(text, code(toList([46]), 49));
}

/**
 * Colour the given text's background white.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_white("lucy")
 *   // => "\x1B[47mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_white(text) {
  return run(text, code(toList([47]), 49));
}

/**
 * Colour the given text's background bright black.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_bright_black("lucy")
 *   // => "\x1B[100mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_bright_black(text) {
  return run(text, code(toList([100]), 49));
}

/**
 * Colour the given text's background bright red.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_bright_red("lucy")
 *   // => "\x1B[101mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_bright_red(text) {
  return run(text, code(toList([101]), 49));
}

/**
 * Colour the given text's background bright green.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_bright_green("lucy")
 *   // => "\x1B[102mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_bright_green(text) {
  return run(text, code(toList([102]), 49));
}

/**
 * Colour the given text's background bright yellow.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_bright_yellow("lucy")
 *   // => "\x1B[103mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_bright_yellow(text) {
  return run(text, code(toList([103]), 49));
}

/**
 * Colour the given text's background bright blue.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_bright_blue("lucy")
 *   // => "\x1B[104mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_bright_blue(text) {
  return run(text, code(toList([104]), 49));
}

/**
 * Colour the given text's background bright magenta.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_bright_magenta("lucy")
 *   // => "\x1B[105mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_bright_magenta(text) {
  return run(text, code(toList([105]), 49));
}

/**
 * Colour the given text's background bright cyan.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_bright_cyan("lucy")
 *   // => "\x1B[106mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_bright_cyan(text) {
  return run(text, code(toList([106]), 49));
}

/**
 * Colour the given text's background bright white.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_bright_white("lucy")
 *   // => "\x1B[107mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_bright_white(text) {
  return run(text, code(toList([107]), 49));
}

/**
 * Colour the given text's background pink.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.bg_pink("lucy")
 *   // => "\x1B[48;5;219mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_pink(text) {
  return run(text, code(toList([48, 5, 219]), 49));
}

/**
 * Colour the given text's background the given colour represented by a hex `Int`.
 *
 * The given hex Int can be any valid [shorthand hexadecimal form](https://en.wikipedia.org/wiki/Web_colors#Shorthand_hexadecimal_form).
 *
 * ❗️ Note that if supplied hex Int is less than 0 or larger than 0xfffff the
 * colour will be set to black and white respectively.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.hex("lucy", 0xffaff3)
 *   // => "\x1B[48;2;255;175;243mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_hex(text, colour) {
  return run(
    text,
    code(
      toList([
        48,
        2,
        (() => {
          let _pipe = $int.bitwise_shift_right(colour, 16);
          return $int.bitwise_and(_pipe, 0xff);
        })(),
        (() => {
          let _pipe = $int.bitwise_shift_right(colour, 8);
          return $int.bitwise_and(_pipe, 0xff);
        })(),
        $int.bitwise_and(colour, 0xff),
      ]),
      49,
    ),
  );
}

/**
 * Colour the given text's background with the given colour represented by a `Colour`.
 *
 * <details>
 * <summary>Example:</summary>
 *
 * ```gleam
 * import gleam_community/ansi
 * import gleam_community/colour.{Colour}
 *
 * fn example() {
 *   let pink = colour.from_hsl(0.8583, 1.0, 0,84)
 *   ansi.bg_colour("lucy", pink)
 *   // => "\x1B[48;2;255;175;243mlucy\x1B[49m"
 * }
 * ```
 *
 * ❗️ Note the trailing `"\x1B[49m"` added to the string. This is the escape code
 * for the "default" colour of the terminal. This means text you write after
 * this will revert back to default.
 *
 * ✨ `gleam_community/ansi` is smart about nested styles; instead of using the default
 * colour, it will use the colour of the outter style.
 *
 * ```gleam
 * import gleam_community/ansi
 *
 * fn example() {
 *   ansi.yellow("Isn't " <> ansi.pink("Gleam") <> " fun?")
 * }
 * ```
 *
 * In this example, the text "Gleam" will be pink but the text "fun?" will be
 * yellow, *not* the default colour!
 * </details>
 *
 * <div style="position: relative;">
 *     <a style="position: absolute; left: 0;" href="https://github.com/gleam-community/ansi/issues">
 *         <small>Spot a typo? Open an issue!</small>
 *     </a>
 *     <a style="position: absolute; right: 0;" href="#">
 *         <small>Back to top ↑</small>
 *     </a>
 * </div>
 */
export function bg_colour(text, colour) {
  let hex_colour = $gc_colour.to_rgb_hex(colour);
  return bg_hex(text, hex_colour);
}

/**
 * This is an alias for [`bg_colour`](#bg_colour) for those who prefer the American English
 * spelling.
 */
export function bg_color(text, colour) {
  return bg_colour(text, colour);
}
