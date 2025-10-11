import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../../gleam.mjs";
import * as $http from "../../gleam/http.mjs";

export class Lax extends $CustomType {}

export class Strict extends $CustomType {}

export class None extends $CustomType {}

export class Attributes extends $CustomType {
  constructor(max_age, domain, path, secure, http_only, same_site) {
    super();
    this.max_age = max_age;
    this.domain = domain;
    this.path = path;
    this.secure = secure;
    this.http_only = http_only;
    this.same_site = same_site;
  }
}

function same_site_to_string(policy) {
  if (policy instanceof Lax) {
    return "Lax";
  } else if (policy instanceof Strict) {
    return "Strict";
  } else {
    return "None";
  }
}

/**
 * Helper to create sensible default attributes for a set cookie.
 *
 * https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#Attributes
 */
export function defaults(scheme) {
  return new Attributes(
    new $option.None(),
    new $option.None(),
    new $option.Some("/"),
    isEqual(scheme, new $http.Https()),
    true,
    new $option.Some(new Lax()),
  );
}

function check_token(token) {
  let contains_invalid_charachter = ((($string.contains(token, " ") || $string.contains(
    token,
    "\t",
  )) || $string.contains(token, "\r")) || $string.contains(token, "\n")) || $string.contains(
    token,
    "\f",
  );
  if (contains_invalid_charachter) {
    return new Error(undefined);
  } else {
    return new Ok(undefined);
  }
}

/**
 * Parse a list of cookies from a header string. Any malformed cookies will be
 * discarded.
 *
 * ## Backwards compatibility
 *
 * RFC 6265 states that cookies in the cookie header should be separated by a
 * `;`, however this function will also accept a `,` separator to remain
 * compatible with the now-deprecated RFC 2965, and any older software
 * following that specification.
 */
export function parse(cookie_string) {
  let _pipe = cookie_string;
  let _pipe$1 = $string.split(_pipe, ";");
  let _pipe$2 = $list.flat_map(
    _pipe$1,
    (_capture) => { return $string.split(_capture, ","); },
  );
  return $list.filter_map(
    _pipe$2,
    (pair) => {
      let $ = $string.split_once($string.trim(pair), "=");
      if ($ instanceof Ok) {
        let $1 = $[0][0];
        if ($1 === "") {
          return new Error(undefined);
        } else {
          let key = $1;
          let value = $[0][1];
          let key$1 = $string.trim(key);
          return $result.try$(
            check_token(key$1),
            (_) => {
              let value$1 = $string.trim(value);
              return $result.try$(
                check_token(value$1),
                (_) => { return new Ok([key$1, value$1]); },
              );
            },
          );
        }
      } else {
        return $;
      }
    },
  );
}

const epoch = "Expires=Thu, 01 Jan 1970 00:00:00 GMT";

function cookie_attributes_to_list(attributes) {
  let max_age;
  let domain;
  let path;
  let secure;
  let http_only;
  let same_site;
  max_age = attributes.max_age;
  domain = attributes.domain;
  path = attributes.path;
  secure = attributes.secure;
  http_only = attributes.http_only;
  same_site = attributes.same_site;
  let _pipe = toList([
    (() => {
      if (max_age instanceof $option.Some) {
        let $ = max_age[0];
        if ($ === 0) {
          return new $option.Some(epoch);
        } else {
          return new $option.None();
        }
      } else {
        return new $option.None();
      }
    })(),
    $option.map(
      max_age,
      (max_age) => { return "Max-Age=" + $int.to_string(max_age); },
    ),
    $option.map(domain, (domain) => { return "Domain=" + domain; }),
    $option.map(path, (path) => { return "Path=" + path; }),
    (() => {
      if (secure) {
        return new $option.Some("Secure");
      } else {
        return new $option.None();
      }
    })(),
    (() => {
      if (http_only) {
        return new $option.Some("HttpOnly");
      } else {
        return new $option.None();
      }
    })(),
    $option.map(
      same_site,
      (same_site) => { return "SameSite=" + same_site_to_string(same_site); },
    ),
  ]);
  return $list.filter_map(
    _pipe,
    (_capture) => { return $option.to_result(_capture, undefined); },
  );
}

export function set_header(name, value, attributes) {
  let _pipe = listPrepend(
    (name + "=") + value,
    cookie_attributes_to_list(attributes),
  );
  return $string.join(_pipe, "; ");
}
