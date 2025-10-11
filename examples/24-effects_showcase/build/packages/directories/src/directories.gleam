import envoy
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import platform
import simplifile

/// Return the first environment variable from the list
/// that is set and is a valid directory
fn check_dir_from_env(vars: List(String)) -> Result(String, Nil) {
  vars |> list.filter_map(envoy.get) |> check_dirs
}

/// Return the first directory from the list that exists, or Nil
fn check_dirs(paths: List(String)) -> Result(String, Nil) {
  paths
  |> list.filter(fn(a) {
    !string.is_empty(a) && result.unwrap(simplifile.is_directory(a), False)
  })
  |> list.first
}

fn get_env(var: String) -> String {
  result.unwrap(envoy.get(var), "")
}

fn home_dir_path(path: String) -> String {
  get_env("HOME") <> path
}

fn other_os_message(other_os: String) -> Result(String, Nil) {
  io.print_error(
    "[WARN][directories] Operating system '"
    <> other_os
    <> "' is not supported by this library",
  )
  Error(Nil)
}

/// Returns the path to a temporary directory
/// 
/// It'll first check `%TMPDIR%`, `%TEMP%`, `%TMP%`, and return the first one that is a valid directory
/// 
/// If that fails, It'll check `C:\TEMP`, `C:\TMP`, `\TEMP`, `\TMP` on windows.
/// 
/// On MacOS, Linux, and FreeBSD, it'll check `/tmp`, `/var/tmp`, `/usr/tmp`,
pub fn tmp_dir() -> Result(String, Nil) {
  // This behavior is mostly copied from how python's tempfile module works with small changes
  // It'll first check the set of env vars, then the known paths, and if none of them exist, the current working directory
  // TODO: check if this even works on windows
  case check_dir_from_env(["TMPDIR", "TEMP", "TMP"]) {
    Ok(path) -> Ok(path)
    Error(Nil) -> {
      case platform.os() {
        platform.Win32 -> check_dirs(["C:\\TEMP", "C:\\TMP", "\\TEMP", "\\TMP"])
        platform.Darwin
        | platform.Linux
        | platform.FreeBsd
        | platform.OpenBsd
        | platform.SunOs
        | platform.Aix -> check_dirs(["/tmp", "/var/tmp", "/usr/tmp"])
        platform.OtherOs(os) -> other_os_message(os)
      }
    }
  }
}

/// Returns the path to the user's home directory
/// 
/// It'll check `%UserProfile%` and `%Profile%` on windows, returning first one that is a valid directory
/// 
/// On MacOS, Linux, and FreeBSD, it'll return the value of `$HOME` if it exists
pub fn home_dir() -> Result(String, Nil) {
  case platform.os() {
    platform.Win32 -> check_dir_from_env(["UserProfile", "Profile"])
    platform.Darwin
    | platform.Linux
    | platform.FreeBsd
    | platform.OpenBsd
    | platform.SunOs
    | platform.Aix -> check_dir_from_env(["HOME"])
    platform.OtherOs(os) -> other_os_message(os)
  }
}

/// Returns the path to the user-specific cache directory
/// 
/// On Windows, it'll return the value of `%APPDATA%` if it exists
/// 
/// On MacOS, it'll return value of `$HOME/Library/Caches` if it exists
/// 
/// On Linux and FreeBSD, it'll check `$XDG_CACHE_HOME` and `$HOME/.cache`, returning the first one that is a valid directory
pub fn cache_dir() -> Result(String, Nil) {
  case platform.os() {
    platform.Win32 -> check_dir_from_env(["APPDATA"])
    platform.Darwin -> check_dirs([get_env("HOME") <> "/Library/Caches"])
    platform.Linux
    | platform.FreeBsd
    | platform.OpenBsd
    | platform.SunOs
    | platform.Aix ->
      check_dirs([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")])
    platform.OtherOs(os) -> other_os_message(os)
  }
}

/// Returns the path to the user-specific config directory. This directory may be synced across computers
/// 
/// On Windows, it'll return the value of `%APPDATA%` if it exists
/// 
/// On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists
/// 
/// On Linux and FreeBSD, it'll check `$XDG_CONFIG_HOME` and `$HOME/.config`, returning the first one that is a valid directory
pub fn config_dir() -> Result(String, Nil) {
  case platform.os() {
    platform.Win32 -> check_dir_from_env(["APPDATA"])
    platform.Darwin ->
      check_dirs([get_env("HOME") <> "/Library/Application Support"])
    platform.Linux
    | platform.FreeBsd
    | platform.OpenBsd
    | platform.SunOs
    | platform.Aix ->
      check_dirs([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")])
    platform.OtherOs(os) -> other_os_message(os)
  }
}

/// Returns the path to the user-specific local config directory. Similar to `config_dir`, except Windows won't sync it when connected to a domain with a roaming profile
/// 
/// On Windows, it'll return the value of `%LOCALAPPDATA%` if it exists
/// 
/// On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists
/// 
/// On Linux and FreeBSD, it'll check `$XDG_CONFIG_HOME` and `$HOME/.config`, returning the first one that is a valid directory
pub fn config_local_dir() -> Result(String, Nil) {
  case platform.os() {
    platform.Win32 -> check_dir_from_env(["LOCALAPPDATA"])
    _ -> config_dir()
  }
}

/// Returns the path to the user-specific data directory. This directory may be synced across computers
/// 
/// On Windows, it'll return the value of `%APPDATA%` if it exists
/// 
/// On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists
/// 
/// On Linux and FreeBSD, it'll check `$XDG_DATA_HOME``and $HOME/.local/share, returning the first one that is a valid directory
pub fn data_dir() -> Result(String, Nil) {
  case platform.os() {
    platform.Linux | platform.FreeBsd ->
      check_dirs([get_env("XDG_DATA_HOME"), home_dir_path("/.local/share")])
    _ -> config_dir()
  }
}

/// Returns the path to the user-specific data directory. Similar to `data_dir`, except Windows won't sync it when connected to a domain with a roaming profile
/// 
/// On Windows, it'll return the value of `%LOCALAPPDATA%` if it exists
/// 
/// On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists
/// 
/// On Linux and FreeBSD, it'll check DG_DATA_HOME ```$H`````ocal/share, r```g``` the first one that is a valid directory
pub fn data_local_dir() -> Result(String, Nil) {
  case platform.os() {
    platform.Win32 -> check_dir_from_env(["LOCALAPPDATA"])
    _ -> data_dir()
  }
}

/// Returns the path to which user-specific executable files may be written. 
/// 
/// On Linux and FreeBSD, it'll check $XDG_BIN_HOME, $HOME/.local/bin, $XDG_DATA_HOME/../bin and return the first one that is a valid directory
/// 
/// On all other platforms, it'll always return `Error(Nil)`
pub fn executable_dir() -> Result(String, Nil) {
  case platform.os() {
    platform.Win32 | platform.Darwin -> Error(Nil)
    platform.Linux
    | platform.FreeBsd
    | platform.OpenBsd
    | platform.SunOs
    | platform.Aix ->
      check_dirs([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") <> "../bin",
      ])
    platform.OtherOs(os) -> other_os_message(os)
  }
}

/// Returns the path to the user-specific preferences directory. This directory may be synced across computers
/// 
/// On Windows, it'll return the value of `%APPDATA%` if it exists
/// 
/// On MacOS, it'll return the value of `$HOME/Library/Preferences` if it exists
/// 
/// On Linux and FreeBSD, it'll check $XDG_CONFIG_HOME and $HOME/.config, returning the first one that is a valid directory
pub fn preference_dir() -> Result(String, Nil) {
  case platform.os() {
    platform.Darwin -> check_dirs([home_dir_path("/Library/Preferences")])
    _ -> config_dir()
  }
}

/// Returns the path to which user-specific runtime files and other file objects may be placed. 
/// 
/// On Linux and FreeBSD, it'll check $XDG_RUNTIME_DIR if it is a valid directory
/// 
/// On all other platforms, it'll always return `Error(Nil)`
pub fn runtime_dir() -> Result(String, Nil) {
  case platform.os() {
    platform.Win32 | platform.Darwin -> Error(Nil)
    platform.Linux
    | platform.FreeBsd
    | platform.OpenBsd
    | platform.SunOs
    | platform.Aix -> check_dir_from_env(["XDG_RUNTIME_DIR"])
    platform.OtherOs(os) -> other_os_message(os)
  }
}

/// Returns the path to which user-specific state may be stored. 
/// 
/// The state directory contains data that should be retained between sessions (unlike the runtime directory), 
/// but may not be important/portable enough to be synchronized across machines (unlike the config/preferences/data directories).
/// 
/// On Linux and FreeBSD, it'll check $XDG_STATE_HOME and $HOME/.local/state, returning the first one that is a valid directory
/// 
/// On all other platforms, it'll always return `Error(Nil)`
pub fn state_dir() -> Result(String, Nil) {
  case platform.os() {
    platform.Win32 | platform.Darwin -> Error(Nil)
    platform.Linux
    | platform.FreeBsd
    | platform.OpenBsd
    | platform.SunOs
    | platform.Aix ->
      check_dirs([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")])
    platform.OtherOs(os) -> other_os_message(os)
  }
}
