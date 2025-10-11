import * as $envoy from "../envoy/envoy.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $platform from "../platform/platform.mjs";
import * as $simplifile from "../simplifile/simplifile.mjs";
import { Ok, Error, toList } from "./gleam.mjs";

/**
 * Return the first directory from the list that exists, or Nil
 * 
 * @ignore
 */
function check_dirs(paths) {
  let _pipe = paths;
  let _pipe$1 = $list.filter(
    _pipe,
    (a) => {
      return !$string.is_empty(a) && $result.unwrap(
        $simplifile.is_directory(a),
        false,
      );
    },
  );
  return $list.first(_pipe$1);
}

/**
 * Return the first environment variable from the list
 * that is set and is a valid directory
 * 
 * @ignore
 */
function check_dir_from_env(vars) {
  let _pipe = vars;
  let _pipe$1 = $list.filter_map(_pipe, $envoy.get);
  return check_dirs(_pipe$1);
}

function get_env(var$) {
  return $result.unwrap($envoy.get(var$), "");
}

function home_dir_path(path) {
  return get_env("HOME") + path;
}

function other_os_message(other_os) {
  $io.print_error(
    ("[WARN][directories] Operating system '" + other_os) + "' is not supported by this library",
  );
  return new Error(undefined);
}

/**
 * Returns the path to a temporary directory
 * 
 * It'll first check `%TMPDIR%`, `%TEMP%`, `%TMP%`, and return the first one that is a valid directory
 * 
 * If that fails, It'll check `C:\TEMP`, `C:\TMP`, `\TEMP`, `\TMP` on windows.
 * 
 * On MacOS, Linux, and FreeBSD, it'll check `/tmp`, `/var/tmp`, `/usr/tmp`,
 */
export function tmp_dir() {
  let $ = check_dir_from_env(toList(["TMPDIR", "TEMP", "TMP"]));
  if ($ instanceof Ok) {
    return $;
  } else {
    let $1 = $platform.os();
    if ($1 instanceof $platform.Aix) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.Darwin) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.FreeBsd) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.Linux) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.OpenBsd) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.SunOs) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.Win32) {
      return check_dirs(toList(["C:\\TEMP", "C:\\TMP", "\\TEMP", "\\TMP"]));
    } else {
      let os = $1[0];
      return other_os_message(os);
    }
  }
}

/**
 * Returns the path to the user's home directory
 * 
 * It'll check `%UserProfile%` and `%Profile%` on windows, returning first one that is a valid directory
 * 
 * On MacOS, Linux, and FreeBSD, it'll return the value of `$HOME` if it exists
 */
export function home_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Aix) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.Darwin) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.Linux) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.SunOs) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.Win32) {
    return check_dir_from_env(toList(["UserProfile", "Profile"]));
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

/**
 * Returns the path to the user-specific cache directory
 * 
 * On Windows, it'll return the value of `%APPDATA%` if it exists
 * 
 * On MacOS, it'll return value of `$HOME/Library/Caches` if it exists
 * 
 * On Linux and FreeBSD, it'll check `$XDG_CACHE_HOME` and `$HOME/.cache`, returning the first one that is a valid directory
 */
export function cache_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Aix) {
    return check_dirs(
      toList([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")]),
    );
  } else if ($ instanceof $platform.Darwin) {
    return check_dirs(toList([get_env("HOME") + "/Library/Caches"]));
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dirs(
      toList([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")]),
    );
  } else if ($ instanceof $platform.Linux) {
    return check_dirs(
      toList([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")]),
    );
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dirs(
      toList([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")]),
    );
  } else if ($ instanceof $platform.SunOs) {
    return check_dirs(
      toList([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")]),
    );
  } else if ($ instanceof $platform.Win32) {
    return check_dir_from_env(toList(["APPDATA"]));
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

/**
 * Returns the path to the user-specific config directory. This directory may be synced across computers
 * 
 * On Windows, it'll return the value of `%APPDATA%` if it exists
 * 
 * On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists
 * 
 * On Linux and FreeBSD, it'll check `$XDG_CONFIG_HOME` and `$HOME/.config`, returning the first one that is a valid directory
 */
export function config_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Aix) {
    return check_dirs(
      toList([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")]),
    );
  } else if ($ instanceof $platform.Darwin) {
    return check_dirs(
      toList([get_env("HOME") + "/Library/Application Support"]),
    );
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dirs(
      toList([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")]),
    );
  } else if ($ instanceof $platform.Linux) {
    return check_dirs(
      toList([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")]),
    );
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dirs(
      toList([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")]),
    );
  } else if ($ instanceof $platform.SunOs) {
    return check_dirs(
      toList([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")]),
    );
  } else if ($ instanceof $platform.Win32) {
    return check_dir_from_env(toList(["APPDATA"]));
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

/**
 * Returns the path to the user-specific local config directory. Similar to `config_dir`, except Windows won't sync it when connected to a domain with a roaming profile
 * 
 * On Windows, it'll return the value of `%LOCALAPPDATA%` if it exists
 * 
 * On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists
 * 
 * On Linux and FreeBSD, it'll check `$XDG_CONFIG_HOME` and `$HOME/.config`, returning the first one that is a valid directory
 */
export function config_local_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Win32) {
    return check_dir_from_env(toList(["LOCALAPPDATA"]));
  } else {
    return config_dir();
  }
}

/**
 * Returns the path to the user-specific data directory. This directory may be synced across computers
 * 
 * On Windows, it'll return the value of `%APPDATA%` if it exists
 * 
 * On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists
 * 
 * On Linux and FreeBSD, it'll check `$XDG_DATA_HOME``and $HOME/.local/share, returning the first one that is a valid directory
 */
export function data_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.FreeBsd) {
    return check_dirs(
      toList([get_env("XDG_DATA_HOME"), home_dir_path("/.local/share")]),
    );
  } else if ($ instanceof $platform.Linux) {
    return check_dirs(
      toList([get_env("XDG_DATA_HOME"), home_dir_path("/.local/share")]),
    );
  } else {
    return config_dir();
  }
}

/**
 * Returns the path to the user-specific data directory. Similar to `data_dir`, except Windows won't sync it when connected to a domain with a roaming profile
 * 
 * On Windows, it'll return the value of `%LOCALAPPDATA%` if it exists
 * 
 * On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists
 * 
 * On Linux and FreeBSD, it'll check DG_DATA_HOME ```$H`````ocal/share, r```g``` the first one that is a valid directory
 */
export function data_local_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Win32) {
    return check_dir_from_env(toList(["LOCALAPPDATA"]));
  } else {
    return data_dir();
  }
}

/**
 * Returns the path to which user-specific executable files may be written. 
 * 
 * On Linux and FreeBSD, it'll check $XDG_BIN_HOME, $HOME/.local/bin, $XDG_DATA_HOME/../bin and return the first one that is a valid directory
 * 
 * On all other platforms, it'll always return `Error(Nil)`
 */
export function executable_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Aix) {
    return check_dirs(
      toList([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") + "../bin",
      ]),
    );
  } else if ($ instanceof $platform.Darwin) {
    return new Error(undefined);
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dirs(
      toList([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") + "../bin",
      ]),
    );
  } else if ($ instanceof $platform.Linux) {
    return check_dirs(
      toList([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") + "../bin",
      ]),
    );
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dirs(
      toList([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") + "../bin",
      ]),
    );
  } else if ($ instanceof $platform.SunOs) {
    return check_dirs(
      toList([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") + "../bin",
      ]),
    );
  } else if ($ instanceof $platform.Win32) {
    return new Error(undefined);
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

/**
 * Returns the path to the user-specific preferences directory. This directory may be synced across computers
 * 
 * On Windows, it'll return the value of `%APPDATA%` if it exists
 * 
 * On MacOS, it'll return the value of `$HOME/Library/Preferences` if it exists
 * 
 * On Linux and FreeBSD, it'll check $XDG_CONFIG_HOME and $HOME/.config, returning the first one that is a valid directory
 */
export function preference_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Darwin) {
    return check_dirs(toList([home_dir_path("/Library/Preferences")]));
  } else {
    return config_dir();
  }
}

/**
 * Returns the path to which user-specific runtime files and other file objects may be placed. 
 * 
 * On Linux and FreeBSD, it'll check $XDG_RUNTIME_DIR if it is a valid directory
 * 
 * On all other platforms, it'll always return `Error(Nil)`
 */
export function runtime_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Aix) {
    return check_dir_from_env(toList(["XDG_RUNTIME_DIR"]));
  } else if ($ instanceof $platform.Darwin) {
    return new Error(undefined);
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dir_from_env(toList(["XDG_RUNTIME_DIR"]));
  } else if ($ instanceof $platform.Linux) {
    return check_dir_from_env(toList(["XDG_RUNTIME_DIR"]));
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dir_from_env(toList(["XDG_RUNTIME_DIR"]));
  } else if ($ instanceof $platform.SunOs) {
    return check_dir_from_env(toList(["XDG_RUNTIME_DIR"]));
  } else if ($ instanceof $platform.Win32) {
    return new Error(undefined);
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

/**
 * Returns the path to which user-specific state may be stored. 
 * 
 * The state directory contains data that should be retained between sessions (unlike the runtime directory), 
 * but may not be important/portable enough to be synchronized across machines (unlike the config/preferences/data directories).
 * 
 * On Linux and FreeBSD, it'll check $XDG_STATE_HOME and $HOME/.local/state, returning the first one that is a valid directory
 * 
 * On all other platforms, it'll always return `Error(Nil)`
 */
export function state_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Aix) {
    return check_dirs(
      toList([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")]),
    );
  } else if ($ instanceof $platform.Darwin) {
    return new Error(undefined);
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dirs(
      toList([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")]),
    );
  } else if ($ instanceof $platform.Linux) {
    return check_dirs(
      toList([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")]),
    );
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dirs(
      toList([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")]),
    );
  } else if ($ instanceof $platform.SunOs) {
    return check_dirs(
      toList([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")]),
    );
  } else if ($ instanceof $platform.Win32) {
    return new Error(undefined);
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}
