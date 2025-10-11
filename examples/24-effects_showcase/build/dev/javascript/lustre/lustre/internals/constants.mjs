import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $set from "../../../gleam_stdlib/gleam/set.mjs";
import { toList } from "../../gleam.mjs";
import { empty_dict, empty_set } from "./constants.ffi.mjs";

export { empty_dict, empty_set };

export const empty_list = /* @__PURE__ */ toList([]);

export const option_none = /* @__PURE__ */ new None();
