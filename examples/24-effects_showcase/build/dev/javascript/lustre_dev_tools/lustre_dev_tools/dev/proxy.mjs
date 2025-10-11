import * as $filepath from "../../../filepath/filepath.mjs";
import * as $request from "../../../gleam_http/gleam/http/request.mjs";
import { Request } from "../../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../../gleam_http/gleam/http/response.mjs";
import * as $httpc from "../../../gleam_httpc/gleam/httpc.mjs";
import * as $bytes_tree from "../../../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $uri from "../../../gleam_stdlib/gleam/uri.mjs";
import * as $wisp from "../../../wisp/wisp.mjs";
import { Ok, Error, CustomType as $CustomType } from "../../gleam.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";

export class Proxy extends $CustomType {
  constructor(from, to) {
    super();
    this.from = from;
    this.to = to;
  }
}

export class None extends $CustomType {}

export function new$(from, to) {
  if (from === "") {
    if (to === "") {
      return new Ok(new None());
    } else {
      return new Error(new $error.ProxyMissingFrom());
    }
  } else if (from.startsWith("/")) {
    if (to === "") {
      return new Error(new $error.ProxyMissingTo());
    } else {
      let $ = $uri.parse(to);
      if ($ instanceof Ok) {
        let uri = $[0];
        return new Ok(new Proxy(from, uri));
      } else {
        return new Error(new $error.ProxyInvalidTo());
      }
    }
  } else if (to === "") {
    return new Error(new $error.ProxyMissingTo());
  } else {
    let $ = $uri.parse(to);
    if ($ instanceof Ok) {
      let uri = $[0];
      return new Ok(new Proxy("/" + from, uri));
    } else {
      return new Error(new $error.ProxyInvalidTo());
    }
  }
}
