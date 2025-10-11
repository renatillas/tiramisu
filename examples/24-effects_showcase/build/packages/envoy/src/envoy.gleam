import gleam/dict.{type Dict}

/// Get an environment variable by name.
///
/// ```gleam
/// get("HOME")
/// // -> Ok("/home/lucy")
/// ```
///
/// ```gleam
/// get("WORLD_PEACE")
/// // -> Error(Nil)
/// ```
///
@external(erlang, "envoy_ffi", "get")
@external(javascript, "./envoy_ffi.mjs", "get")
pub fn get(name: String) -> Result(String, Nil)

//
/// Set an environment variable.
///
/// ```gleam
/// get("FAVOURITE_COLOUR")
/// // -> Error(Nil)
/// set("FAVOURITE_COLOUR", "Pink")
///
/// get("FAVOURITE_COLOUR")
/// // -> Ok("Pink")
/// ```
///
@external(erlang, "envoy_ffi", "set")
@external(javascript, "./envoy_ffi.mjs", "set")
pub fn set(name: String, value: String) -> Nil

/// Unset an environment variable.
///
/// ```gleam
/// set("FAVOURITE_COLOUR", "Pink")
///
/// get("FAVOURITE_COLOUR")
/// // -> Ok("Pink")
///
/// unset("FAVOURITE_COLOUR")
///
/// get("FAVOURITE_COLOUR")
/// // -> Error(Nil)
/// ```
///
@external(erlang, "envoy_ffi", "unset")
@external(javascript, "./envoy_ffi.mjs", "unset")
pub fn unset(name: String) -> Nil

/// Get all the environment variables.
///
@external(erlang, "envoy_ffi", "all")
@external(javascript, "./envoy_ffi.mjs", "all")
pub fn all() -> Dict(String, String)
