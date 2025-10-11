import * as $filepath from "../../../filepath/filepath.mjs";
import * as $ansi from "../../../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $crypto from "../../../gleam_crypto/gleam/crypto.mjs";
import * as $http from "../../../gleam_http/gleam/http.mjs";
import { Get, Https } from "../../../gleam_http/gleam/http.mjs";
import * as $request from "../../../gleam_http/gleam/http/request.mjs";
import { Request } from "../../../gleam_http/gleam/http/request.mjs";
import * as $httpc from "../../../gleam_httpc/gleam/httpc.mjs";
import * as $regexp from "../../../gleam_regexp/gleam/regexp.mjs";
import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import * as $tom from "../../../tom/tom.mjs";
import { Ok, Error, toList, CustomType as $CustomType, makeError } from "../../gleam.mjs";
import * as $cli from "../../lustre_dev_tools/cli.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";
import * as $port from "../../lustre_dev_tools/port.mjs";
import * as $project from "../../lustre_dev_tools/project.mjs";
import * as $system from "../../lustre_dev_tools/system.mjs";

const FILEPATH = "src/lustre_dev_tools/bin/tailwind.gleam";

export class HasViableEntry extends $CustomType {}

export class HasTailwindEntry extends $CustomType {}

export class HasLegacyConfig extends $CustomType {}

export class Nothing extends $CustomType {}

/**
 *
 */
export function detect(project, entry) {
  let tailwind_config = $filepath.join(project.src, entry + ".css");
  let _block;
  let _pipe = tailwind_config;
  let _pipe$1 = $simplifile.is_file(_pipe);
  _block = $result.unwrap(_pipe$1, false);
  let has_tailwind_config = _block;
  let legacy_config = $filepath.join(project.root, "tailwind.config.js");
  let _block$1;
  let _pipe$2 = legacy_config;
  let _pipe$3 = $simplifile.is_file(_pipe$2);
  _block$1 = $result.unwrap(_pipe$3, false);
  let has_legacy_config = _block$1;
  if (has_tailwind_config) {
    return $result.try$(
      (() => {
        let _pipe$4 = $simplifile.read(tailwind_config);
        return $result.map_error(
          _pipe$4,
          (_capture) => {
            return new $error.CouldNotReadFile(tailwind_config, _capture);
          },
        );
      })(),
      (css) => {
        let $ = $regexp.from_string("^@import\\s+['\"]tailwindcss");
        let re;
        if ($ instanceof Ok) {
          re = $[0];
        } else {
          throw makeError(
            "let_assert",
            FILEPATH,
            "lustre_dev_tools/bin/tailwind",
            193,
            "detect",
            "Pattern match failed, no pattern matched the value.",
            {
              value: $,
              start: 5417,
              end: 5487,
              pattern_start: 5428,
              pattern_end: 5434
            }
          )
        }
        let $1 = $regexp.check(re, css);
        if ($1) {
          return new Ok(new HasTailwindEntry());
        } else {
          return new Ok(new HasViableEntry());
        }
      },
    );
  } else if (has_legacy_config) {
    return new Ok(new HasLegacyConfig());
  } else {
    return new Ok(new Nothing());
  }
}

const version = "4.1.13";

const hashes = /* @__PURE__ */ toList([
  [
    "tailwindcss-linux-arm64",
    "c90529475a398adbf3315898721c0f9fe85f434a2b3ea3eafada68867641819a",
  ],
  [
    "tailwindcss-linux-arm64-musl",
    "09624e1cb6295849020fb78344eb5dfa8196f57dfa6f81a0cb8442151d2f860d",
  ],
  [
    "tailwindcss-linux-x64",
    "b9ed9f8f640d3323711f9f68608aa266dff3adbc42e867c38ea2d009b973be11",
  ],
  [
    "tailwindcss-linux-x64-musl",
    "5785fbf6bc1e489e0d3c9743aa6cf0fe20b4633c1de6e8a4d6cdc0bf86716d71",
  ],
  [
    "tailwindcss-macos-arm64",
    "c47681e9948db20026a913a4aca4ee0269b4c0d4ef3f71343cb891dfdc1e97c9",
  ],
  [
    "tailwindcss-macos-x64",
    "c3b230bdbfaa46c94cad8db44da1f82773f10bac54f56fa196c8977d819c09e4",
  ],
  [
    "tailwindcss-windows-x64.exe",
    "ad16a528e13111e5df4e771b4b4981bd4b73e69140fa021f4102f46f02eeb86d",
  ],
]);

/**
 * Verifies the integrity of the downloaded archive by comparing its SHA-256
 * hash against the hashes provided by Tailwind:
 *
 *   https://github.com/tailwindlabs/tailwindcss/releases/download/v4.1.13/sha256sums.txt
 * 
 * @ignore
 */
function verify_integrity(archive, name) {
  let $ = $list.key_find(hashes, name);
  let expected;
  if ($ instanceof Ok) {
    expected = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "lustre_dev_tools/bin/tailwind",
      338,
      "verify_integrity",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 9579,
        end: 9632,
        pattern_start: 9590,
        pattern_end: 9602
      }
    )
  }
  let hash = $crypto.hash(new $crypto.Sha256(), archive);
  let _block;
  let _pipe = $bit_array.base16_encode(hash);
  _block = $string.lowercase(_pipe);
  let actual = _block;
  let $1 = actual === expected;
  if ($1) {
    return new Ok(undefined);
  } else {
    return new Error(new $error.CouldNotVerifyTailwindHash(expected, actual));
  }
}
