import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import {
  compile as do_compile,
  check as do_check,
  split as do_split,
  scan as do_scan,
  replace,
  match_map,
} from "../gleam_regexp_ffi.mjs";

export { match_map, replace };

export class Match extends $CustomType {
  constructor(content, submatches) {
    super();
    this.content = content;
    this.submatches = submatches;
  }
}

export class CompileError extends $CustomType {
  constructor(error, byte_index) {
    super();
    this.error = error;
    this.byte_index = byte_index;
  }
}

export class Options extends $CustomType {
  constructor(case_insensitive, multi_line) {
    super();
    this.case_insensitive = case_insensitive;
    this.multi_line = multi_line;
  }
}

/**
 * Creates a `Regexp` with some additional options.
 *
 * ## Examples
 *
 * ```gleam
 * let options = Options(case_insensitive: False, multi_line: True)
 * let assert Ok(re) = compile("^[0-9]", with: options)
 * check(re, "abc\n123")
 * // -> True
 * ```
 *
 * ```gleam
 * let options = Options(case_insensitive: True, multi_line: False)
 * let assert Ok(re) = compile("[A-Z]", with: options)
 * check(re, "abc123")
 * // -> True
 * ```
 */
export function compile(pattern, options) {
  return do_compile(pattern, options);
}

/**
 * Creates a new `Regexp`.
 *
 * ## Examples
 *
 * ```gleam
 * let assert Ok(re) = from_string("[0-9]")
 * check(re, "abc123")
 * // -> True
 * ```
 *
 * ```gleam
 * check(re, "abcxyz")
 * // -> False
 * ```
 *
 * ```gleam
 * from_string("[0-9")
 * // -> Error(CompileError(
 * //   error: "missing terminating ] for character class",
 * //   byte_index: 4
 * // ))
 * ```
 */
export function from_string(pattern) {
  return compile(pattern, new Options(false, false));
}

/**
 * Returns a boolean indicating whether there was a match or not.
 *
 * ## Examples
 *
 * ```gleam
 * let assert Ok(re) = from_string("^f.o.?")
 * check(with: re, content: "foo")
 * // -> True
 * ```
 *
 * ```gleam
 * check(with: re, content: "boo")
 * // -> False
 * ```
 */
export function check(regexp, string) {
  return do_check(regexp, string);
}

/**
 * Splits a string.
 *
 * ## Examples
 *
 * ```gleam
 * let assert Ok(re) = from_string(" *, *")
 * split(with: re, content: "foo,32, 4, 9  ,0")
 * // -> ["foo", "32", "4", "9", "0"]
 * ```
 */
export function split(regexp, string) {
  return do_split(regexp, string);
}

/**
 * Collects all matches of the regular expression.
 *
 * ## Examples
 *
 * ```gleam
 * let assert Ok(re) = from_string("[oi]n a (\\w+)")
 * scan(with: re, content: "I am on a boat in a lake.")
 * // -> [
 * //   Match(content: "on a boat", submatches: [Some("boat")]),
 * //   Match(content: "in a lake", submatches: [Some("lake")]),
 * // ]
 * ```
 *
 * ```gleam
 * let assert Ok(re) = regexp.from_string("([+|\\-])?(\\d+)(\\w+)?")
 * scan(with: re, content: "-36")
 * // -> [
 * //   Match(content: "-36", submatches: [Some("-"), Some("36")])
 * // ]
 *
 * scan(with: re, content: "36")
 * // -> [
 * //   Match(content: "36", submatches: [None, Some("36")])
 * // ]
 * ```
 *
 * ```gleam
 * let assert Ok(re) =
 *   regexp.from_string("var\\s*(\\w+)\\s*(int|string)?\\s*=\\s*(.*)")
 * scan(with: re, content: "var age = 32")
 * // -> [
 * //   Match(
 * //     content: "var age = 32",
 * //     submatches: [Some("age"), None, Some("32")],
 * //   ),
 * // ]
 * ```
 *
 * ```gleam
 * let assert Ok(re) = regexp.from_string("let (\\w+) = (\\w+)")
 * scan(with: re, content: "let age = 32")
 * // -> [
 * //   Match(
 * //     content: "let age = 32",
 * //     submatches: [Some("age"), Some("32")],
 * //   ),
 * // ]
 *
 * scan(with: re, content: "const age = 32")
 * // -> []
 * ```
 */
export function scan(regexp, string) {
  return do_scan(regexp, string);
}
