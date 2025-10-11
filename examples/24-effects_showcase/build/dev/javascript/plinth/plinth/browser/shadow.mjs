import * as $array from "../../../gleam_javascript/gleam/javascript/array.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $element from "../../plinth/browser/element.mjs";
import {
  appendChild as append_child,
  attachShadow as attach_shadow,
  shadowRoot as shadow_root,
  querySelector as query_selector,
  querySelectorAll as query_selector_all,
} from "../../shadow_ffi.mjs";

export {
  append_child,
  attach_shadow,
  query_selector,
  query_selector_all,
  shadow_root,
};

export class Open extends $CustomType {}

export class Closed extends $CustomType {}
