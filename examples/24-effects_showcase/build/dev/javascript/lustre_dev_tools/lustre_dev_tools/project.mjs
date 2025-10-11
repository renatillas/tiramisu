import * as $filepath from "../../filepath/filepath.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import * as $tom from "../../tom/tom.mjs";
import { Ok, Error, toList, CustomType as $CustomType, makeError } from "../gleam.mjs";
import * as $error from "../lustre_dev_tools/error.mjs";

const FILEPATH = "src/lustre_dev_tools/project.gleam";

export class Project extends $CustomType {
  constructor(name, options, root, src, dev, assets, bin, build, has_node_modules) {
    super();
    this.name = name;
    this.options = options;
    this.root = root;
    this.src = src;
    this.dev = dev;
    this.assets = assets;
    this.bin = bin;
    this.build = build;
    this.has_node_modules = has_node_modules;
  }
}

function find_root(loop$path) {
  while (true) {
    let path = loop$path;
    let $ = $simplifile.is_file($filepath.join(path, "gleam.toml"));
    if ($ instanceof Ok) {
      let $1 = $[0];
      if ($1) {
        return path;
      } else {
        loop$path = $filepath.join(path, "../");
      }
    } else {
      loop$path = $filepath.join(path, "../");
    }
  }
}

/**
 *
 */
export function config() {
  let root = find_root("./");
  let bin = $filepath.join(root, ".lustre/bin");
  let src = $filepath.join(root, "src");
  let dev = $filepath.join(root, "dev");
  let assets = $filepath.join(root, "assets");
  let build = $filepath.join(root, ".lustre/build");
  let _block;
  let _pipe = $simplifile.is_directory($filepath.join(root, "node_modules"));
  _block = $result.unwrap(_pipe, false);
  let has_node_modules = _block;
  let $ = $simplifile.read($filepath.join(root, "gleam.toml"));
  let toml;
  if ($ instanceof Ok) {
    toml = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "lustre_dev_tools/project",
      93,
      "config",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 2377,
        end: 2449,
        pattern_start: 2388,
        pattern_end: 2396
      }
    )
  }
  let $1 = $tom.parse(toml);
  let config$1;
  if ($1 instanceof Ok) {
    config$1 = $1[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "lustre_dev_tools/project",
      94,
      "config",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $1,
        start: 2452,
        end: 2491,
        pattern_start: 2463,
        pattern_end: 2473
      }
    )
  }
  let $2 = $tom.get_string(config$1, toList(["name"]));
  let name;
  if ($2 instanceof Ok) {
    name = $2[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "lustre_dev_tools/project",
      95,
      "config",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $2,
        start: 2494,
        end: 2548,
        pattern_start: 2505,
        pattern_end: 2513
      }
    )
  }
  let _block$1;
  let _pipe$1 = $tom.get_table(config$1, toList(["tools", "lustre"]));
  _block$1 = $result.unwrap(_pipe$1, $dict.new$());
  let options = _block$1;
  return new Project(
    name,
    options,
    root,
    src,
    dev,
    assets,
    bin,
    build,
    has_node_modules,
  );
}

/**
 *
 */
export function initialise() {
  let project = config();
  return $result.try$(
    (() => {
      let $ = project.root;
      if ($ === "./") {
        return new Ok(undefined);
      } else if ($.startsWith("./")) {
        let path = $.slice(2);
        return new Error(new $error.MustBeProjectRoot(path));
      } else {
        let path = $;
        return new Error(new $error.MustBeProjectRoot(path));
      }
    })(),
    (_) => {
      return $result.try$(
        (() => {
          let _pipe = $simplifile.create_directory_all(project.bin);
          return $result.map_error(
            _pipe,
            (var0) => { return new $error.CouldNotInitialiseDevTools(var0); },
          );
        })(),
        (_) => {
          return $result.try$(
            (() => {
              let _pipe = $simplifile.create_directory_all(project.build);
              return $result.map_error(
                _pipe,
                (var0) => { return new $error.CouldNotInitialiseDevTools(var0); },
              );
            })(),
            (_) => {
              let gitignore_path = $filepath.join(project.root, ".gitignore");
              return $result.try$(
                (() => {
                  let $ = $simplifile.read(gitignore_path);
                  if ($ instanceof Ok) {
                    let gitignore = $[0];
                    let $1 = $string.contains(gitignore, ".lustre");
                    if ($1) {
                      return new Ok(undefined);
                    } else {
                      let _pipe = $simplifile.append(
                        gitignore_path,
                        "\n#Added automatically by Lustre Dev Tools\n/.lustre\n/dist\n",
                      );
                      let _pipe$1 = $result.map_error(
                        _pipe,
                        (var0) => {
                          return new $error.CouldNotInitialiseDevTools(var0);
                        },
                      );
                      return $result.replace(_pipe$1, undefined);
                    }
                  } else {
                    return new Ok(undefined);
                  }
                })(),
                (_) => { return new Ok(project); },
              );
            },
          );
        },
      );
    },
  );
}

export function exists(project, module) {
  let $ = $simplifile.is_file($filepath.join(project.src, module + ".gleam"));
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1) {
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
}
