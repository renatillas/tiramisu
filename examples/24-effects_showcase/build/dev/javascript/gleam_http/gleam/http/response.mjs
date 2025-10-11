import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import * as $http from "../../gleam/http.mjs";
import * as $cookie from "../../gleam/http/cookie.mjs";

export class Response extends $CustomType {
  constructor(status, headers, body) {
    super();
    this.status = status;
    this.headers = headers;
    this.body = body;
  }
}

/**
 * Construct an empty Response.
 *
 * The body type of the returned response is `String` and could be set with a
 * call to `set_body`.
 */
export function new$(status) {
  return new Response(status, toList([]), "");
}

/**
 * Get the value for a given header.
 *
 * If the response does not have that header then `Error(Nil)` is returned.
 */
export function get_header(response, key) {
  return $list.key_find(response.headers, $string.lowercase(key));
}

/**
 * Set the header with the given value under the given header key.
 *
 * If the response already has that key, it is replaced.
 *
 * Header keys are always lowercase in `gleam_http`. To use any uppercase
 * letter is invalid.
 */
export function set_header(response, key, value) {
  let headers = $list.key_set(response.headers, $string.lowercase(key), value);
  return new Response(response.status, headers, response.body);
}

/**
 * Prepend the header with the given value under the given header key.
 *
 * Similar to `set_header` except if the header already exists it prepends
 * another header with the same key.
 *
 * Header keys are always lowercase in `gleam_http`. To use any uppercase
 * letter is invalid.
 */
export function prepend_header(response, key, value) {
  let headers = listPrepend([$string.lowercase(key), value], response.headers);
  return new Response(response.status, headers, response.body);
}

/**
 * Set the body of the response, overwriting any existing body.
 */
export function set_body(response, body) {
  return new Response(response.status, response.headers, body);
}

/**
 * Update the body of a response using a given result returning function.
 *
 * If the given function returns an `Ok` value the body is set, if it returns
 * an `Error` value then the error is returned.
 */
export function try_map(response, transform) {
  return $result.try$(
    transform(response.body),
    (body) => { return new Ok(set_body(response, body)); },
  );
}

/**
 * Update the body of a response using a given function.
 */
export function map(response, transform) {
  let _pipe = response.body;
  let _pipe$1 = transform(_pipe);
  return ((_capture) => { return set_body(response, _capture); })(_pipe$1);
}

/**
 * Create a response that redirects to the given uri.
 */
export function redirect(uri) {
  return new Response(
    303,
    toList([["location", uri]]),
    $string.append("You are being redirected to ", uri),
  );
}

/**
 * Fetch the cookies sent in a response.
 *
 * Badly formed cookies will be discarded.
 */
export function get_cookies(resp) {
  let headers;
  headers = resp.headers;
  return $list.flat_map(
    headers,
    (header) => {
      let $ = header[0];
      if ($ === "set-cookie") {
        let value = header[1];
        return $cookie.parse(value);
      } else {
        return toList([]);
      }
    },
  );
}

/**
 * Set a cookie value for a client
 */
export function set_cookie(response, name, value, attributes) {
  return prepend_header(
    response,
    "set-cookie",
    $cookie.set_header(name, value, attributes),
  );
}

/**
 * Expire a cookie value for a client
 *
 * Note: The attributes value should be the same as when the response cookie was set.
 */
export function expire_cookie(response, name, attributes) {
  let attrs = new $cookie.Attributes(
    new $option.Some(0),
    attributes.domain,
    attributes.path,
    attributes.secure,
    attributes.http_only,
    attributes.same_site,
  );
  return set_cookie(response, name, "", attrs);
}
