//// This package uses the regular expression engine of the underlying platform.
//// Regular expressions in Erlang and JavaScript largely share the same syntax, but
//// there are some differences and have different performance characteristics. Be
//// sure to thoroughly test your code on all platforms that you support when using
//// this library.

import gleam/option.{type Option}

pub type Regexp

/// The details about a particular match:
///
pub type Match {
  Match(
    /// The full string of the match.
    content: String,
    /// A `Regexp` can have subpatterns, sup-parts that are in parentheses.
    submatches: List(Option(String)),
  )
}

/// When a regular expression fails to compile:
///
pub type CompileError {
  CompileError(
    /// The problem encountered that caused the compilation to fail
    error: String,
    /// The byte index into the string to where the problem was found
    /// This value may not be correct in JavaScript environments.
    byte_index: Int,
  )
}

pub type Options {
  Options(case_insensitive: Bool, multi_line: Bool)
}

/// Creates a `Regexp` with some additional options.
///
/// ## Examples
///
/// ```gleam
/// let options = Options(case_insensitive: False, multi_line: True)
/// let assert Ok(re) = compile("^[0-9]", with: options)
/// check(re, "abc\n123")
/// // -> True
/// ```
///
/// ```gleam
/// let options = Options(case_insensitive: True, multi_line: False)
/// let assert Ok(re) = compile("[A-Z]", with: options)
/// check(re, "abc123")
/// // -> True
/// ```
///
pub fn compile(
  pattern: String,
  with options: Options,
) -> Result(Regexp, CompileError) {
  do_compile(pattern, options)
}

@external(erlang, "gleam_regexp_ffi", "compile")
@external(javascript, "../gleam_regexp_ffi.mjs", "compile")
fn do_compile(
  pattern: String,
  with with: Options,
) -> Result(Regexp, CompileError)

/// Creates a new `Regexp`.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = from_string("[0-9]")
/// check(re, "abc123")
/// // -> True
/// ```
///
/// ```gleam
/// check(re, "abcxyz")
/// // -> False
/// ```
///
/// ```gleam
/// from_string("[0-9")
/// // -> Error(CompileError(
/// //   error: "missing terminating ] for character class",
/// //   byte_index: 4
/// // ))
/// ```
///
pub fn from_string(pattern: String) -> Result(Regexp, CompileError) {
  compile(pattern, Options(case_insensitive: False, multi_line: False))
}

/// Returns a boolean indicating whether there was a match or not.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = from_string("^f.o.?")
/// check(with: re, content: "foo")
/// // -> True
/// ```
///
/// ```gleam
/// check(with: re, content: "boo")
/// // -> False
/// ```
///
pub fn check(with regexp: Regexp, content string: String) -> Bool {
  do_check(regexp, string)
}

@external(erlang, "gleam_regexp_ffi", "check")
@external(javascript, "../gleam_regexp_ffi.mjs", "check")
fn do_check(regexp: Regexp, string: String) -> Bool

/// Splits a string.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = from_string(" *, *")
/// split(with: re, content: "foo,32, 4, 9  ,0")
/// // -> ["foo", "32", "4", "9", "0"]
/// ```
///
pub fn split(with regexp: Regexp, content string: String) -> List(String) {
  do_split(regexp, string)
}

@external(erlang, "gleam_regexp_ffi", "split")
@external(javascript, "../gleam_regexp_ffi.mjs", "split")
fn do_split(regexp: Regexp, string: String) -> List(String)

/// Collects all matches of the regular expression.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = from_string("[oi]n a (\\w+)")
/// scan(with: re, content: "I am on a boat in a lake.")
/// // -> [
/// //   Match(content: "on a boat", submatches: [Some("boat")]),
/// //   Match(content: "in a lake", submatches: [Some("lake")]),
/// // ]
/// ```
///
/// ```gleam
/// let assert Ok(re) = regexp.from_string("([+|\\-])?(\\d+)(\\w+)?")
/// scan(with: re, content: "-36")
/// // -> [
/// //   Match(content: "-36", submatches: [Some("-"), Some("36")])
/// // ]
///
/// scan(with: re, content: "36")
/// // -> [
/// //   Match(content: "36", submatches: [None, Some("36")])
/// // ]
/// ```
///
/// ```gleam
/// let assert Ok(re) =
///   regexp.from_string("var\\s*(\\w+)\\s*(int|string)?\\s*=\\s*(.*)")
/// scan(with: re, content: "var age = 32")
/// // -> [
/// //   Match(
/// //     content: "var age = 32",
/// //     submatches: [Some("age"), None, Some("32")],
/// //   ),
/// // ]
/// ```
///
/// ```gleam
/// let assert Ok(re) = regexp.from_string("let (\\w+) = (\\w+)")
/// scan(with: re, content: "let age = 32")
/// // -> [
/// //   Match(
/// //     content: "let age = 32",
/// //     submatches: [Some("age"), Some("32")],
/// //   ),
/// // ]
///
/// scan(with: re, content: "const age = 32")
/// // -> []
/// ```
///
pub fn scan(with regexp: Regexp, content string: String) -> List(Match) {
  do_scan(regexp, string)
}

@external(erlang, "gleam_regexp_ffi", "scan")
@external(javascript, "../gleam_regexp_ffi.mjs", "scan")
fn do_scan(regexp: Regexp, string: String) -> List(Match)

/// Creates a new `String` by replacing all substrings that match the regular
/// expression.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = regexp.from_string("^https://")
/// replace(each: re, in: "https://example.com", with: "www.")
/// // -> "www.example.com"
/// ```
///
/// ```gleam
/// let assert Ok(re) = regexp.from_string("[, +-]")
/// replace(each: re, in: "a,b-c d+e", with: "/")
/// // -> "a/b/c/d/e"
/// ```
@external(erlang, "gleam_regexp_ffi", "replace")
@external(javascript, "../gleam_regexp_ffi.mjs", "replace")
pub fn replace(
  each pattern: Regexp,
  in string: String,
  with substitute: String,
) -> String

/// Creates a new `String` by replacing all substrings that match the regular
/// expression with the result of applying the function to each match.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = regexp.from_string("\\w+")
/// regexp.match_map(re, "hello, joe!", fn(m) { string.capitalise(m.content) })
/// // -> "Hello, Joe!"
/// ```
@external(erlang, "gleam_regexp_ffi", "match_map")
@external(javascript, "../gleam_regexp_ffi.mjs", "match_map")
pub fn match_map(
  each pattern: Regexp,
  in string: String,
  with substitute: fn(Match) -> String,
) -> String
