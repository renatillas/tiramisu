import * as $ansi from "../../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $glint from "../../glint/glint.mjs";
import * as $tom from "../../tom/tom.mjs";
import { Ok, Error, toList, Empty as $Empty, makeError } from "../gleam.mjs";
import * as $error from "../lustre_dev_tools/error.mjs";
import * as $project from "../lustre_dev_tools/project.mjs";

const FILEPATH = "src/lustre_dev_tools/cli.gleam";

export function log(message, quiet) {
  if (quiet) {
    return undefined;
  } else {
    return $io.println($ansi.grey(("   " + message) + "..."));
  }
}

export function success(message, quiet) {
  if (quiet) {
    return undefined;
  } else {
    return $io.println($ansi.green("✅ " + message));
  }
}

/**
 *
 */
export function string(name, path, project, description, continue$) {
  return flag(
    name,
    path,
    project,
    $glint.string_flag,
    $tom.get_string,
    description,
    continue$,
  );
}

/**
 *
 * 
 * @ignore
 */
function flag(name, path, project, read_flag, read_toml, description, continue$) {
  if (path instanceof $Empty) {
    if (name === "") {
      throw makeError(
        "panic",
        FILEPATH,
        "lustre_dev_tools/cli",
        119,
        "flag",
        "Invalid flag config. Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new",
        {}
      )
    } else {
      return $glint.flag(
        (() => {
          let _pipe = read_flag(name);
          return $glint.flag_help(
            _pipe,
            (() => {
              let _pipe$1 = description;
              return $string.trim(_pipe$1);
            })(),
          );
        })(),
        (from_flags) => {
          return continue$(
            (flags) => {
              let $ = from_flags(flags);
              if ($ instanceof Ok) {
                return $;
              } else {
                return new Error(
                  new $error.MissingRequiredFlag(toList(["--" + name])),
                );
              }
            },
          );
        },
      );
    }
  } else if (name === "") {
    return continue$(
      (_) => {
        let $ = read_toml(project.options, path);
        if ($ instanceof Ok) {
          return $;
        } else {
          return new Error(new $error.MissingRequiredFlag(path));
        }
      },
    );
  } else {
    return $glint.flag(
      (() => {
        let _pipe = read_flag(name);
        return $glint.flag_help(
          _pipe,
          (() => {
            let _pipe$1 = description;
            return $string.trim(_pipe$1);
          })(),
        );
      })(),
      (from_flags) => {
        return continue$(
          (flags) => {
            let $ = from_flags(flags);
            if ($ instanceof Ok) {
              return $;
            } else {
              let $1 = read_toml(project.options, path);
              if ($1 instanceof Ok) {
                return $1;
              } else {
                return new Error(new $error.MissingRequiredFlag(path));
              }
            }
          },
        );
      },
    );
  }
}

/**
 *
 */
export function bool(name, path, project, description, continue$) {
  return flag(
    name,
    path,
    project,
    $glint.bool_flag,
    $tom.get_bool,
    description,
    continue$,
  );
}

export function string_list(name, path, project, description, continue$) {
  let get_strings = (toml, path) => {
    let _pipe = $tom.get_array(toml, path);
    return $result.try$(
      _pipe,
      (_capture) => { return $list.try_map(_capture, $tom.as_string); },
    );
  };
  return flag(
    name,
    path,
    project,
    $glint.strings_flag,
    get_strings,
    description,
    continue$,
  );
}

/**
 *
 */
export function int(name, path, project, description, continue$) {
  return flag(
    name,
    path,
    project,
    $glint.int_flag,
    $tom.get_int,
    description,
    continue$,
  );
}
