import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import { toList, Empty as $Empty, prepend as listPrepend } from "./gleam.mjs";

function add(words, word) {
  if (word === "") {
    return words;
  } else {
    return listPrepend(word, words);
  }
}

function is_upper(g) {
  return $string.lowercase(g) !== g;
}

function split(loop$in, loop$up, loop$word, loop$words) {
  while (true) {
    let in$ = loop$in;
    let up = loop$up;
    let word = loop$word;
    let words = loop$words;
    if (in$ instanceof $Empty) {
      if (word === "") {
        return $list.reverse(words);
      } else {
        return $list.reverse(add(words, word));
      }
    } else {
      let $ = in$.head;
      if ($ === "\n") {
        let in$1 = in$.tail;
        loop$in = in$1;
        loop$up = false;
        loop$word = "";
        loop$words = add(words, word);
      } else if ($ === "\t") {
        let in$1 = in$.tail;
        loop$in = in$1;
        loop$up = false;
        loop$word = "";
        loop$words = add(words, word);
      } else if ($ === "!") {
        let in$1 = in$.tail;
        loop$in = in$1;
        loop$up = false;
        loop$word = "";
        loop$words = add(words, word);
      } else if ($ === "?") {
        let in$1 = in$.tail;
        loop$in = in$1;
        loop$up = false;
        loop$word = "";
        loop$words = add(words, word);
      } else if ($ === "#") {
        let in$1 = in$.tail;
        loop$in = in$1;
        loop$up = false;
        loop$word = "";
        loop$words = add(words, word);
      } else if ($ === ".") {
        let in$1 = in$.tail;
        loop$in = in$1;
        loop$up = false;
        loop$word = "";
        loop$words = add(words, word);
      } else if ($ === "-") {
        let in$1 = in$.tail;
        loop$in = in$1;
        loop$up = false;
        loop$word = "";
        loop$words = add(words, word);
      } else if ($ === "_") {
        let in$1 = in$.tail;
        loop$in = in$1;
        loop$up = false;
        loop$word = "";
        loop$words = add(words, word);
      } else if ($ === " ") {
        let in$1 = in$.tail;
        loop$in = in$1;
        loop$up = false;
        loop$word = "";
        loop$words = add(words, word);
      } else {
        let g = $;
        let in$1 = in$.tail;
        let $1 = is_upper(g);
        if ($1) {
          if (up) {
            loop$in = in$1;
            loop$up = up;
            loop$word = word + g;
            loop$words = words;
          } else {
            loop$in = in$1;
            loop$up = true;
            loop$word = g;
            loop$words = add(words, word);
          }
        } else {
          loop$in = in$1;
          loop$up = false;
          loop$word = word + g;
          loop$words = words;
        }
      }
    }
  }
}

function split_words(text) {
  let _pipe = text;
  let _pipe$1 = $string.to_graphemes(_pipe);
  return split(_pipe$1, false, "", toList([]));
}

/**
 * Convert a string to a `snake_case`.
 *
 * # Examples
 *
 * ```gleam
 * snake_case("Hello World")
 * // -> "hello_world"
 * ```
 */
export function snake_case(text) {
  let _pipe = text;
  let _pipe$1 = split_words(_pipe);
  let _pipe$2 = $string.join(_pipe$1, "_");
  return $string.lowercase(_pipe$2);
}

/**
 * Convert a string to a `camelCase`.
 *
 * # Examples
 *
 * ```gleam
 * camel_case("Hello World")
 * // -> "helloWorld"
 * ```
 */
export function camel_case(text) {
  let _pipe = text;
  let _pipe$1 = split_words(_pipe);
  let _pipe$2 = $list.index_map(
    _pipe$1,
    (word, i) => {
      if (i === 0) {
        return $string.lowercase(word);
      } else {
        return $string.capitalise(word);
      }
    },
  );
  return $string.concat(_pipe$2);
}

/**
 * Convert a string to a `PascalCase`.
 *
 * # Examples
 *
 * ```gleam
 * pascal_case("Hello World")
 * // -> "HelloWorld"
 * ```
 */
export function pascal_case(text) {
  let _pipe = text;
  let _pipe$1 = split_words(_pipe);
  let _pipe$2 = $list.map(_pipe$1, $string.capitalise);
  return $string.concat(_pipe$2);
}

/**
 * Convert a string to a `kebab-case`.
 *
 * # Examples
 *
 * ```gleam
 * kabab_case("Hello World")
 * // -> "hello-world
 * ```
 */
export function kebab_case(text) {
  let _pipe = text;
  let _pipe$1 = split_words(_pipe);
  let _pipe$2 = $string.join(_pipe$1, "-");
  return $string.lowercase(_pipe$2);
}

/**
 * Convert a string to a `Sentence case`.
 *
 * # Examples
 *
 * ```gleam
 * sentence_case("hello-world")
 * // -> "Hello world
 * ```
 */
export function sentence_case(text) {
  let _pipe = text;
  let _pipe$1 = split_words(_pipe);
  let _pipe$2 = $string.join(_pipe$1, " ");
  return $string.capitalise(_pipe$2);
}
