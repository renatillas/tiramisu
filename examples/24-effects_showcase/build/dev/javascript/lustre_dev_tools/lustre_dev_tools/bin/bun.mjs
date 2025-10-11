import * as $filepath from "../../../filepath/filepath.mjs";
import * as $crypto from "../../../gleam_crypto/gleam/crypto.mjs";
import * as $application from "../../../gleam_erlang/gleam/erlang/application.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $http from "../../../gleam_http/gleam/http.mjs";
import * as $request from "../../../gleam_http/gleam/http/request.mjs";
import { Request } from "../../../gleam_http/gleam/http/request.mjs";
import * as $httpc from "../../../gleam_httpc/gleam/httpc.mjs";
import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import * as $tom from "../../../tom/tom.mjs";
import { Ok, Error, toList, makeError } from "../../gleam.mjs";
import * as $cli from "../../lustre_dev_tools/cli.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";
import * as $port from "../../lustre_dev_tools/port.mjs";
import * as $project from "../../lustre_dev_tools/project.mjs";
import * as $system from "../../lustre_dev_tools/system.mjs";

const FILEPATH = "src/lustre_dev_tools/bin/bun.gleam";

const version = "1.2.22";

const hashes = /* @__PURE__ */ toList([
  [
    "bun-darwin-aarch64",
    "eb8c7e09cbea572414a0a367848e1acbf05294a946a594405a014b1fb3b3fc76",
  ],
  [
    "bun-darwin-x64",
    "a7484721a7ead45887c812e760b124047e663173cf2a3ba7c5aa1992cb22cd3e",
  ],
  [
    "bun-linux-aarch64-musl",
    "88c54cd66169aeb5ff31bc0c9d74a8017c7e6965597472ff49ecc355acb3a884",
  ],
  [
    "bun-linux-aarch64",
    "a97c687fb5e54de4e2fb0869a7ac9a2d9c3af75ac182e2b68138c1dd8f98131b",
  ],
  [
    "bun-linux-x64-baseline",
    "f753e8d9668078ab0f598ee26a9ac5acbbb822e057459dd50c191b86524d98e8",
  ],
  [
    "bun-linux-x64-musl-baseline",
    "4048e872b16fb3a296e89268769d3e41152f477b6f203eff58c672f69ed9f570",
  ],
  [
    "bun-linux-x64-musl",
    "dde5bd79f0e130cb9bf17f55ba1825e98a77f71ef78c575d8ca2ccae5431f47e",
  ],
  [
    "bun-linux-x64",
    "4c446af1a01d7b40e1e11baebc352f9b2bfd12887e51b97dd3b59879cee2743a",
  ],
  [
    "bun-windows-x64-baseline",
    "c44de73dc21c7140a8e15883c28abed60612196faaec9a60c275534280a49f59",
  ],
  [
    "bun-windows-x64",
    "3a28c685b47a159c5707d150accb5b4903c30f1e7b4dd01bb311d4112bdeb452",
  ],
]);

/**
 * Verifies the integrity of the downloaded archive by comparing its SHA-256
 * hash against the hashes provided by Bun:
 *
 *   https://github.com/oven-sh/bun/releases/download/bun-v1.2.17/SHASUMS256.txt
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
      "lustre_dev_tools/bin/bun",
      246,
      "verify_integrity",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 6903,
        end: 6956,
        pattern_start: 6914,
        pattern_end: 6926
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
    return new Error(new $error.CouldNotVerifyBunHash(expected, actual));
  }
}
