import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import { is_windows } from "./filepath_ffi.mjs";
import { Ok, Error, toList, Empty as $Empty, prepend as listPrepend, isEqual } from "./gleam.mjs";

function relative(loop$path) {
  while (true) {
    let path = loop$path;
    if (path.startsWith("/")) {
      let path$1 = path.slice(1);
      loop$path = path$1;
    } else {
      return path;
    }
  }
}

function remove_trailing_slash(path) {
  let $ = $string.ends_with(path, "/");
  if ($) {
    return $string.drop_end(path, 1);
  } else {
    return path;
  }
}

/**
 * Join two paths together.
 *
 * This function does not expand `..` or `.` segments, use the `expand`
 * function to do this.
 *
 * ## Examples
 *
 * ```gleam
 * join("/usr/local", "bin")
 * // -> "/usr/local/bin"
 * ```
 */
export function join(left, right) {
  let _block;
  if (right === "/") {
    _block = left;
  } else if (right.startsWith("/")) {
    if (left === "") {
      _block = relative(right);
    } else if (left === "/") {
      _block = right;
    } else {
      let _pipe = remove_trailing_slash(left);
      let _pipe$1 = $string.append(_pipe, "/");
      _block = $string.append(_pipe$1, relative(right));
    }
  } else if (left === "") {
    _block = relative(right);
  } else if (left === "/") {
    _block = left + right;
  } else {
    let _pipe = remove_trailing_slash(left);
    let _pipe$1 = $string.append(_pipe, "/");
    _block = $string.append(_pipe$1, relative(right));
  }
  let _pipe = _block;
  return remove_trailing_slash(_pipe);
}

/**
 * Split a path into its segments, using `/` as the path separator.
 *
 * Typically you would want to use `split` instead of this function, but if you
 * want non-Windows path behaviour on a Windows system then you can use this
 * function.
 *
 * ## Examples
 *
 * ```gleam
 * split("/usr/local/bin", "bin")
 * // -> ["/", "usr", "local", "bin"]
 * ```
 */
export function split_unix(path) {
  let _block;
  let $ = $string.split(path, "/");
  if ($ instanceof $Empty) {
    _block = $;
  } else {
    let $1 = $.head;
    if ($1 === "") {
      let $2 = $.tail;
      if ($2 instanceof $Empty) {
        _block = toList([]);
      } else {
        let rest = $2;
        _block = listPrepend("/", rest);
      }
    } else {
      _block = $;
    }
  }
  let _pipe = _block;
  return $list.filter(_pipe, (x) => { return x !== ""; });
}

function get_directory_name(loop$path, loop$acc, loop$segment) {
  while (true) {
    let path = loop$path;
    let acc = loop$acc;
    let segment = loop$segment;
    if (path instanceof $Empty) {
      return acc;
    } else {
      let $ = path.head;
      if ($ === "/") {
        let rest = path.tail;
        loop$path = rest;
        loop$acc = acc + segment;
        loop$segment = "/";
      } else {
        let first = $;
        let rest = path.tail;
        loop$path = rest;
        loop$acc = acc;
        loop$segment = segment + first;
      }
    }
  }
}

/**
 * Get the directory name of a path, that is the path without the file name.
 *
 * ## Examples
 *
 * ```gleam
 * directory_name("/usr/local/bin")
 * // -> "/usr/local"
 * ```
 */
export function directory_name(path) {
  let path$1 = remove_trailing_slash(path);
  if (path$1.startsWith("/")) {
    let rest = path$1.slice(1);
    return get_directory_name($string.to_graphemes(rest), "/", "");
  } else {
    return get_directory_name($string.to_graphemes(path$1), "", "");
  }
}

/**
 * Check if a path is absolute.
 *
 * ## Examples
 *
 * ```gleam
 * is_absolute("/usr/local/bin")
 * // -> True
 * ```
 *
 * ```gleam
 * is_absolute("usr/local/bin")
 * // -> False
 * ```
 */
export function is_absolute(path) {
  return $string.starts_with(path, "/");
}

function expand_segments(loop$path, loop$base) {
  while (true) {
    let path = loop$path;
    let base = loop$base;
    if (path instanceof $Empty) {
      return new Ok($string.join($list.reverse(base), "/"));
    } else {
      let $ = path.head;
      if ($ === "..") {
        if (base instanceof $Empty) {
          return new Error(undefined);
        } else {
          let $1 = base.tail;
          if ($1 instanceof $Empty) {
            let $2 = base.head;
            if ($2 === "") {
              return new Error(undefined);
            } else {
              let path$1 = path.tail;
              let base$1 = $1;
              loop$path = path$1;
              loop$base = base$1;
            }
          } else {
            let path$1 = path.tail;
            let base$1 = $1;
            loop$path = path$1;
            loop$base = base$1;
          }
        }
      } else if ($ === ".") {
        let path$1 = path.tail;
        loop$path = path$1;
        loop$base = base;
      } else {
        let s = $;
        let path$1 = path.tail;
        loop$path = path$1;
        loop$base = listPrepend(s, base);
      }
    }
  }
}

function root_slash_to_empty(segments) {
  if (segments instanceof $Empty) {
    return segments;
  } else {
    let $ = segments.head;
    if ($ === "/") {
      let rest = segments.tail;
      return listPrepend("", rest);
    } else {
      return segments;
    }
  }
}

const codepoint_slash = 47;

const codepoint_backslash = 92;

const codepoint_colon = 58;

const codepoint_a = 65;

const codepoint_z = 90;

const codepoint_a_up = 97;

const codepoint_z_up = 122;

function pop_windows_drive_specifier(path) {
  let start = $string.slice(path, 0, 3);
  let codepoints = $string.to_utf_codepoints(start);
  let $ = $list.map(codepoints, $string.utf_codepoint_to_int);
  if ($ instanceof $Empty) {
    return [new None(), path];
  } else {
    let $1 = $.tail;
    if ($1 instanceof $Empty) {
      return [new None(), path];
    } else {
      let $2 = $1.tail;
      if ($2 instanceof $Empty) {
        return [new None(), path];
      } else {
        let $3 = $2.tail;
        if ($3 instanceof $Empty) {
          let drive = $.head;
          let colon = $1.head;
          let slash = $2.head;
          if (
            (((slash === 47) || (slash === 92)) && (colon === 58)) && (((drive >= 65) && (drive <= 90)) || ((drive >= 97) && (drive <= 122)))
          ) {
            let drive_letter = $string.slice(path, 0, 1);
            let drive$1 = $string.lowercase(drive_letter) + ":/";
            let path$1 = $string.drop_start(path, 3);
            return [new Some(drive$1), path$1];
          } else {
            return [new None(), path];
          }
        } else {
          return [new None(), path];
        }
      }
    }
  }
}

/**
 * Split a path into its segments, using `/` and `\` as the path separators. If
 * there is a drive letter at the start of the path then it is lowercased.
 *
 * Typically you would want to use `split` instead of this function, but if you
 * want Windows path behaviour on a non-Windows system then you can use this
 * function.
 *
 * ## Examples
 *
 * ```gleam
 * split("/usr/local/bin", "bin")
 * // -> ["/", "usr", "local", "bin"]
 * ```
 */
export function split_windows(path) {
  let $ = pop_windows_drive_specifier(path);
  let drive;
  let path$1;
  drive = $[0];
  path$1 = $[1];
  let _block;
  let _pipe = $string.split(path$1, "/");
  _block = $list.flat_map(
    _pipe,
    (_capture) => { return $string.split(_capture, "\\"); },
  );
  let segments = _block;
  let _block$1;
  if (drive instanceof Some) {
    let drive$1 = drive[0];
    _block$1 = listPrepend(drive$1, segments);
  } else {
    _block$1 = segments;
  }
  let segments$1 = _block$1;
  if (segments$1 instanceof $Empty) {
    return segments$1;
  } else {
    let $1 = segments$1.head;
    if ($1 === "") {
      let $2 = segments$1.tail;
      if ($2 instanceof $Empty) {
        return toList([]);
      } else {
        let rest = $2;
        return listPrepend("/", rest);
      }
    } else {
      return segments$1;
    }
  }
}

/**
 * Split a path into its segments.
 *
 * When running on Windows both `/` and `\` are treated as path separators, and 
 * if the path starts with a drive letter then the drive letter then it is
 * lowercased.
 *
 * ## Examples
 *
 * ```gleam
 * split("/usr/local/bin", "bin")
 * // -> ["/", "usr", "local", "bin"]
 * ```
 */
export function split(path) {
  let $ = is_windows();
  if ($) {
    return split_windows(path);
  } else {
    return split_unix(path);
  }
}

/**
 * Get the base name of a path, that is the name of the file without the
 * containing directory.
 *
 * ## Examples
 *
 * ```gleam
 * base_name("/usr/local/bin")
 * // -> "bin"
 * ```
 */
export function base_name(path) {
  return $bool.guard(
    path === "/",
    "",
    () => {
      let _pipe = path;
      let _pipe$1 = split(_pipe);
      let _pipe$2 = $list.last(_pipe$1);
      return $result.unwrap(_pipe$2, "");
    },
  );
}

/**
 * Get the file extension of a path.
 *
 * ## Examples
 *
 * ```gleam
 * extension("src/main.gleam")
 * // -> Ok("gleam")
 * ```
 *
 * ```gleam
 * extension("package.tar.gz")
 * // -> Ok("gz")
 * ```
 */
export function extension(path) {
  let file = base_name(path);
  let $ = $string.split(file, ".");
  if ($ instanceof $Empty) {
    return new Error(undefined);
  } else {
    let $1 = $.tail;
    if ($1 instanceof $Empty) {
      let rest = $1;
      return $list.last(rest);
    } else {
      let $2 = $1.tail;
      if ($2 instanceof $Empty) {
        let $3 = $.head;
        if ($3 === "") {
          return new Error(undefined);
        } else {
          let extension$1 = $1.head;
          return new Ok(extension$1);
        }
      } else {
        let rest = $1;
        return $list.last(rest);
      }
    }
  }
}

/**
 * Remove the extension from a file, if it has any.
 * 
 * ## Examples
 * 
 * ```gleam
 * strip_extension("src/main.gleam")
 * // -> "src/main"
 * ```
 * 
 * ```gleam
 * strip_extension("package.tar.gz")
 * // -> "package.tar"
 * ```
 * 
 * ```gleam
 * strip_extension("src/gleam")
 * // -> "src/gleam"
 * ```
 */
export function strip_extension(path) {
  let $ = extension(path);
  if ($ instanceof Ok) {
    let extension$1 = $[0];
    return $string.drop_end(path, $string.length(extension$1) + 1);
  } else {
    return path;
  }
}

/**
 * Expand `..` and `.` segments in a path.
 *
 * If the path has a `..` segment that would go up past the root of the path
 * then an error is returned. This may be useful to example to ensure that a
 * path specified by a user does not go outside of a directory.
 *
 * If the path is absolute then the result will always be absolute.
 *
 * ## Examples
 *
 * ```gleam
 * expand("/usr/local/../bin")
 * // -> Ok("/usr/bin")
 * ```
 *
 * ```gleam
 * expand("/tmp/../..")
 * // -> Error(Nil)
 * ```
 *
 * ```gleam
 * expand("src/../..")
 * // -> Error("..")
 * ```
 */
export function expand(path) {
  let is_absolute$1 = is_absolute(path);
  let _block;
  let _pipe = path;
  let _pipe$1 = split(_pipe);
  let _pipe$2 = root_slash_to_empty(_pipe$1);
  let _pipe$3 = expand_segments(_pipe$2, toList([]));
  _block = $result.map(_pipe$3, remove_trailing_slash);
  let result = _block;
  let $ = is_absolute$1 && (isEqual(result, new Ok("")));
  if ($) {
    return new Ok("/");
  } else {
    return result;
  }
}
