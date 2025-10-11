//// Work with file paths in Gleam!
////
//// This library expects paths to be valid unicode. If you need to work with
//// non-unicode paths you will need to convert them to unicode before using
//// this library.

// References:
// https://github.com/erlang/otp/blob/master/lib/stdlib/src/filename.erl
// https://github.com/elixir-lang/elixir/blob/main/lib/elixir/lib/path.ex
// https://github.com/elixir-lang/elixir/blob/main/lib/elixir/test/elixir/path_test.exs
// https://cs.opensource.google/go/go/+/refs/tags/go1.21.4:src/path/match.go

import gleam/bool
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

@external(erlang, "filepath_ffi", "is_windows")
@external(javascript, "./filepath_ffi.mjs", "is_windows")
fn is_windows() -> Bool

/// Join two paths together.
///
/// This function does not expand `..` or `.` segments, use the `expand`
/// function to do this.
///
/// ## Examples
///
/// ```gleam
/// join("/usr/local", "bin")
/// // -> "/usr/local/bin"
/// ```
///
pub fn join(left: String, right: String) -> String {
  case left, right {
    _, "/" -> left
    "", _ -> relative(right)
    "/", "/" <> _ -> right
    "/", _ -> left <> right
    _, _ ->
      remove_trailing_slash(left)
      |> string.append("/")
      |> string.append(relative(right))
  }
  |> remove_trailing_slash
}

fn relative(path: String) -> String {
  case path {
    "/" <> path -> relative(path)
    _ -> path
  }
}

fn remove_trailing_slash(path: String) -> String {
  case string.ends_with(path, "/") {
    True -> string.drop_end(path, 1)
    False -> path
  }
}

// TODO: Windows support
/// Split a path into its segments.
///
/// When running on Windows both `/` and `\` are treated as path separators, and 
/// if the path starts with a drive letter then the drive letter then it is
/// lowercased.
///
/// ## Examples
///
/// ```gleam
/// split("/usr/local/bin", "bin")
/// // -> ["/", "usr", "local", "bin"]
/// ```
///
pub fn split(path: String) -> List(String) {
  case is_windows() {
    True -> split_windows(path)
    False -> split_unix(path)
  }
}

/// Split a path into its segments, using `/` as the path separator.
///
/// Typically you would want to use `split` instead of this function, but if you
/// want non-Windows path behaviour on a Windows system then you can use this
/// function.
///
/// ## Examples
///
/// ```gleam
/// split("/usr/local/bin", "bin")
/// // -> ["/", "usr", "local", "bin"]
/// ```
///
pub fn split_unix(path: String) -> List(String) {
  case string.split(path, "/") {
    [""] -> []
    ["", ..rest] -> ["/", ..rest]
    rest -> rest
  }
  |> list.filter(fn(x) { x != "" })
}

/// Split a path into its segments, using `/` and `\` as the path separators. If
/// there is a drive letter at the start of the path then it is lowercased.
///
/// Typically you would want to use `split` instead of this function, but if you
/// want Windows path behaviour on a non-Windows system then you can use this
/// function.
///
/// ## Examples
///
/// ```gleam
/// split("/usr/local/bin", "bin")
/// // -> ["/", "usr", "local", "bin"]
/// ```
///
pub fn split_windows(path: String) -> List(String) {
  let #(drive, path) = pop_windows_drive_specifier(path)

  let segments =
    string.split(path, "/")
    |> list.flat_map(string.split(_, "\\"))

  let segments = case drive {
    Some(drive) -> [drive, ..segments]
    None -> segments
  }

  case segments {
    [""] -> []
    ["", ..rest] -> ["/", ..rest]
    rest -> rest
  }
}

const codepoint_slash = 47

const codepoint_backslash = 92

const codepoint_colon = 58

const codepoint_a = 65

const codepoint_z = 90

const codepoint_a_up = 97

const codepoint_z_up = 122

fn pop_windows_drive_specifier(path: String) -> #(Option(String), String) {
  let start = string.slice(from: path, at_index: 0, length: 3)
  let codepoints = string.to_utf_codepoints(start)
  case list.map(codepoints, string.utf_codepoint_to_int) {
    [drive, colon, slash]
      if { slash == codepoint_slash || slash == codepoint_backslash }
      && colon == codepoint_colon
      && {
        drive >= codepoint_a
        && drive <= codepoint_z
        || drive >= codepoint_a_up
        && drive <= codepoint_z_up
      }
    -> {
      let drive_letter = string.slice(from: path, at_index: 0, length: 1)
      let drive = string.lowercase(drive_letter) <> ":/"
      let path = string.drop_start(path, 3)
      #(Some(drive), path)
    }
    _ -> #(None, path)
  }
}

/// Get the file extension of a path.
///
/// ## Examples
///
/// ```gleam
/// extension("src/main.gleam")
/// // -> Ok("gleam")
/// ```
///
/// ```gleam
/// extension("package.tar.gz")
/// // -> Ok("gz")
/// ```
///
pub fn extension(path: String) -> Result(String, Nil) {
  let file = base_name(path)
  case string.split(file, ".") {
    ["", _] -> Error(Nil)
    [_, extension] -> Ok(extension)
    [_, ..rest] -> list.last(rest)
    _ -> Error(Nil)
  }
}

/// Remove the extension from a file, if it has any.
/// 
/// ## Examples
/// 
/// ```gleam
/// strip_extension("src/main.gleam")
/// // -> "src/main"
/// ```
/// 
/// ```gleam
/// strip_extension("package.tar.gz")
/// // -> "package.tar"
/// ```
/// 
/// ```gleam
/// strip_extension("src/gleam")
/// // -> "src/gleam"
/// ```
/// 
pub fn strip_extension(path: String) -> String {
  case extension(path) {
    Ok(extension) ->
      // Since the extension string doesn't have a leading `.`
      // we drop a grapheme more to remove that as well.
      string.drop_end(path, string.length(extension) + 1)
    Error(Nil) -> path
  }
}

// TODO: windows support
/// Get the base name of a path, that is the name of the file without the
/// containing directory.
///
/// ## Examples
///
/// ```gleam
/// base_name("/usr/local/bin")
/// // -> "bin"
/// ```
///
pub fn base_name(path: String) -> String {
  use <- bool.guard(when: path == "/", return: "")

  path
  |> split
  |> list.last
  |> result.unwrap("")
}

// TODO: windows support
/// Get the directory name of a path, that is the path without the file name.
///
/// ## Examples
///
/// ```gleam
/// directory_name("/usr/local/bin")
/// // -> "/usr/local"
/// ```
///
pub fn directory_name(path: String) -> String {
  let path = remove_trailing_slash(path)
  case path {
    "/" <> rest -> get_directory_name(string.to_graphemes(rest), "/", "")
    _ -> get_directory_name(string.to_graphemes(path), "", "")
  }
}

fn get_directory_name(
  path: List(String),
  acc: String,
  segment: String,
) -> String {
  case path {
    ["/", ..rest] -> get_directory_name(rest, acc <> segment, "/")
    [first, ..rest] -> get_directory_name(rest, acc, segment <> first)
    [] -> acc
  }
}

// TODO: windows support
/// Check if a path is absolute.
///
/// ## Examples
///
/// ```gleam
/// is_absolute("/usr/local/bin")
/// // -> True
/// ```
///
/// ```gleam
/// is_absolute("usr/local/bin")
/// // -> False
/// ```
///
pub fn is_absolute(path: String) -> Bool {
  string.starts_with(path, "/")
}

//TODO: windows support
/// Expand `..` and `.` segments in a path.
///
/// If the path has a `..` segment that would go up past the root of the path
/// then an error is returned. This may be useful to example to ensure that a
/// path specified by a user does not go outside of a directory.
///
/// If the path is absolute then the result will always be absolute.
///
/// ## Examples
///
/// ```gleam
/// expand("/usr/local/../bin")
/// // -> Ok("/usr/bin")
/// ```
///
/// ```gleam
/// expand("/tmp/../..")
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// expand("src/../..")
/// // -> Error("..")
/// ```
///
pub fn expand(path: String) -> Result(String, Nil) {
  let is_absolute = is_absolute(path)
  let result =
    path
    |> split
    |> root_slash_to_empty
    |> expand_segments([])
    |> result.map(remove_trailing_slash)

  case is_absolute && result == Ok("") {
    True -> Ok("/")
    False -> result
  }
}

fn expand_segments(
  path: List(String),
  base: List(String),
) -> Result(String, Nil) {
  case base, path {
    // Going up past the root (empty string in this representation)
    [""], ["..", ..] -> Error(Nil)

    // Going up past the top of a relative path
    [], ["..", ..] -> Error(Nil)

    // Going up successfully
    [_, ..base], ["..", ..path] -> expand_segments(path, base)

    // Discarding `.`
    _, [".", ..path] -> expand_segments(path, base)

    // Adding a segment
    _, [s, ..path] -> expand_segments(path, [s, ..base])

    // Done!
    _, [] -> Ok(string.join(list.reverse(base), "/"))
  }
}

fn root_slash_to_empty(segments: List(String)) -> List(String) {
  case segments {
    ["/", ..rest] -> ["", ..rest]
    _ -> segments
  }
}
