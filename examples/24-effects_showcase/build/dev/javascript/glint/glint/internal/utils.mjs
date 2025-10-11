import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { toList, Empty as $Empty, prepend as listPrepend } from "../../gleam.mjs";

/**
 * Returns the length of the longest string in the list.
 */
export function max_string_length(strings) {
  return $list.fold(
    strings,
    0,
    (max, f) => {
      let _pipe = f;
      let _pipe$1 = $string.length(_pipe);
      return $int.max(_pipe$1, max);
    },
  );
}

function do_wordwrap(loop$tokens, loop$max_width, loop$line, loop$lines) {
  while (true) {
    let tokens = loop$tokens;
    let max_width = loop$max_width;
    let line = loop$line;
    let lines = loop$lines;
    if (tokens instanceof $Empty) {
      if (line === "") {
        return $list.reverse(lines);
      } else {
        return $list.reverse(listPrepend(line, lines));
      }
    } else {
      let token = tokens.head;
      let tokens$1 = tokens.tail;
      let token_length = $string.length(token);
      let line_length = $string.length(line);
      let $ = ((line_length + 1) + token_length) <= max_width;
      if (line === "") {
        loop$tokens = tokens$1;
        loop$max_width = max_width;
        loop$line = token;
        loop$lines = lines;
      } else if ($) {
        loop$tokens = tokens$1;
        loop$max_width = max_width;
        loop$line = (line + " ") + token;
        loop$lines = lines;
      } else {
        loop$tokens = tokens$1;
        loop$max_width = max_width;
        loop$line = token;
        loop$lines = listPrepend(line, lines);
      }
    }
  }
}

/**
 * split a string for consecutive newline groups
 * replaces individual newlines with a spacet
 * groups of newlines > 1 are replaced with n-2 newlines followed by a new item
 * 
 * @ignore
 */
function space_split_lines(s) {
  let _block;
  let _pipe = s;
  let _pipe$1 = $string.trim(_pipe);
  let _pipe$2 = $string.to_graphemes(_pipe$1);
  _block = $list.chunk(_pipe$2, (s) => { return s === "\n"; });
  let chunks = _block;
  let lines = $list.fold(
    chunks,
    [toList([]), false],
    (acc, chunk) => {
      let $ = acc[0];
      if ($ instanceof $Empty) {
        return [listPrepend($string.concat(chunk), acc[0]), false];
      } else if (chunk instanceof $Empty) {
        if (!acc[1]) {
          let s$1 = $.head;
          let accs = $.tail;
          return [listPrepend(s$1 + $string.concat(chunk), accs), false];
        } else {
          return [listPrepend($string.concat(chunk), acc[0]), false];
        }
      } else {
        let $1 = chunk.tail;
        if ($1 instanceof $Empty) {
          let $2 = chunk.head;
          if ($2 === "\n") {
            let s$1 = $.head;
            let accs = $.tail;
            return [listPrepend(s$1 + " ", accs), false];
          } else if (!acc[1]) {
            let s$1 = $.head;
            let accs = $.tail;
            return [listPrepend(s$1 + $string.concat(chunk), accs), false];
          } else {
            return [listPrepend($string.concat(chunk), acc[0]), false];
          }
        } else {
          let $2 = $1.head;
          if ($2 === "\n") {
            let $3 = chunk.head;
            if ($3 === "\n") {
              let s$1 = $.head;
              let accs = $.tail;
              let rest = $1.tail;
              return [listPrepend(s$1 + $string.concat(rest), accs), true];
            } else if (!acc[1]) {
              let s$1 = $.head;
              let accs = $.tail;
              return [listPrepend(s$1 + $string.concat(chunk), accs), false];
            } else {
              return [listPrepend($string.concat(chunk), acc[0]), false];
            }
          } else if (!acc[1]) {
            let s$1 = $.head;
            let accs = $.tail;
            return [listPrepend(s$1 + $string.concat(chunk), accs), false];
          } else {
            return [listPrepend($string.concat(chunk), acc[0]), false];
          }
        }
      }
    },
  );
  return $list.reverse(lines[0]);
}

/**
 * Wraps the given string so that no lines exceed the given width. Newlines in
 * the input string are retained.
 */
export function wordwrap(s, max_width) {
  return $bool.guard(
    s === "",
    toList([]),
    () => {
      return $list.flat_map(
        space_split_lines(s),
        (line) => {
          let _pipe = line;
          let _pipe$1 = $string.split(_pipe, " ");
          return do_wordwrap(_pipe$1, max_width, "", toList([]));
        },
      );
    },
  );
}
