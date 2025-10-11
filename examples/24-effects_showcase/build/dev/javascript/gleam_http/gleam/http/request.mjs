import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $uri from "../../../gleam_stdlib/gleam/uri.mjs";
import { Uri } from "../../../gleam_stdlib/gleam/uri.mjs";
import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import * as $http from "../../gleam/http.mjs";
import { Get } from "../../gleam/http.mjs";
import * as $cookie from "../../gleam/http/cookie.mjs";

export class Request extends $CustomType {
  constructor(method, headers, body, scheme, host, port, path, query) {
    super();
    this.method = method;
    this.headers = headers;
    this.body = body;
    this.scheme = scheme;
    this.host = host;
    this.port = port;
    this.path = path;
    this.query = query;
  }
}

/**
 * Return the uri that a request was sent to.
 */
export function to_uri(request) {
  return new Uri(
    new $option.Some($http.scheme_to_string(request.scheme)),
    new $option.None(),
    new $option.Some(request.host),
    request.port,
    request.path,
    request.query,
    new $option.None(),
  );
}

/**
 * Construct a request from a URI.
 */
export function from_uri(uri) {
  return $result.try$(
    (() => {
      let _pipe = uri.scheme;
      let _pipe$1 = $option.unwrap(_pipe, "");
      return $http.scheme_from_string(_pipe$1);
    })(),
    (scheme) => {
      return $result.try$(
        (() => {
          let _pipe = uri.host;
          return $option.to_result(_pipe, undefined);
        })(),
        (host) => {
          let req = new Request(
            new Get(),
            toList([]),
            "",
            scheme,
            host,
            uri.port,
            uri.path,
            uri.query,
          );
          return new Ok(req);
        },
      );
    },
  );
}

/**
 * Get the value for a given header.
 *
 * If the request does not have that header then `Error(Nil)` is returned.
 *
 * Header keys are always lowercase in `gleam_http`. To use any uppercase
 * letter is invalid.
 */
export function get_header(request, key) {
  return $list.key_find(request.headers, $string.lowercase(key));
}

/**
 * Set the header with the given value under the given header key.
 *
 * If already present, it is replaced.
 *
 * Header keys are always lowercase in `gleam_http`. To use any uppercase
 * letter is invalid.
 */
export function set_header(request, key, value) {
  let headers = $list.key_set(request.headers, $string.lowercase(key), value);
  return new Request(
    request.method,
    headers,
    request.body,
    request.scheme,
    request.host,
    request.port,
    request.path,
    request.query,
  );
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
export function prepend_header(request, key, value) {
  let headers = listPrepend([$string.lowercase(key), value], request.headers);
  return new Request(
    request.method,
    headers,
    request.body,
    request.scheme,
    request.host,
    request.port,
    request.path,
    request.query,
  );
}

/**
 * Set the body of the request, overwriting any existing body.
 */
export function set_body(req, body) {
  return new Request(
    req.method,
    req.headers,
    body,
    req.scheme,
    req.host,
    req.port,
    req.path,
    req.query,
  );
}

/**
 * Update the body of a request using a given function.
 */
export function map(request, transform) {
  let _pipe = request.body;
  let _pipe$1 = transform(_pipe);
  return ((_capture) => { return set_body(request, _capture); })(_pipe$1);
}

/**
 * Return the non-empty segments of a request path.
 *
 * # Examples
 *
 * ```gleam
 * > new()
 * > |> set_path("/one/two/three")
 * > |> path_segments
 * ["one", "two", "three"]
 * ```
 */
export function path_segments(request) {
  let _pipe = request.path;
  return $uri.path_segments(_pipe);
}

/**
 * Decode the query of a request.
 */
export function get_query(request) {
  let $ = request.query;
  if ($ instanceof $option.Some) {
    let query_string = $[0];
    return $uri.parse_query(query_string);
  } else {
    return new Ok(toList([]));
  }
}

/**
 * Set the query of the request.
 * Query params will be percent encoded before being added to the Request.
 */
export function set_query(req, query) {
  let _block;
  let _pipe = $list.map(
    query,
    (pair) => {
      let key;
      let value;
      key = pair[0];
      value = pair[1];
      return ($uri.percent_encode(key) + "=") + $uri.percent_encode(value);
    },
  );
  let _pipe$1 = $string.join(_pipe, "&");
  _block = new $option.Some(_pipe$1);
  let query$1 = _block;
  return new Request(
    req.method,
    req.headers,
    req.body,
    req.scheme,
    req.host,
    req.port,
    req.path,
    query$1,
  );
}

/**
 * Set the method of the request.
 */
export function set_method(req, method) {
  return new Request(
    method,
    req.headers,
    req.body,
    req.scheme,
    req.host,
    req.port,
    req.path,
    req.query,
  );
}

/**
 * A request with commonly used default values. This request can be used as
 * an initial value and then update to create the desired request.
 */
export function new$() {
  return new Request(
    new Get(),
    toList([]),
    "",
    new $http.Https(),
    "localhost",
    new $option.None(),
    "",
    new $option.None(),
  );
}

/**
 * Construct a request from a URL string
 */
export function to(url) {
  let _pipe = url;
  let _pipe$1 = $uri.parse(_pipe);
  return $result.try$(_pipe$1, from_uri);
}

/**
 * Set the scheme (protocol) of the request.
 */
export function set_scheme(req, scheme) {
  return new Request(
    req.method,
    req.headers,
    req.body,
    scheme,
    req.host,
    req.port,
    req.path,
    req.query,
  );
}

/**
 * Set the host of the request.
 */
export function set_host(req, host) {
  return new Request(
    req.method,
    req.headers,
    req.body,
    req.scheme,
    host,
    req.port,
    req.path,
    req.query,
  );
}

/**
 * Set the port of the request.
 */
export function set_port(req, port) {
  return new Request(
    req.method,
    req.headers,
    req.body,
    req.scheme,
    req.host,
    new $option.Some(port),
    req.path,
    req.query,
  );
}

/**
 * Set the path of the request.
 */
export function set_path(req, path) {
  return new Request(
    req.method,
    req.headers,
    req.body,
    req.scheme,
    req.host,
    req.port,
    path,
    req.query,
  );
}

/**
 * Set a cookie on a request, replacing any previous cookie with that name.
 *
 * All cookies should be stored in a single header named `cookie`.
 * There should be at most one header with the name `cookie`, otherwise this
 * function cannot guarentee that previous cookies with the same name are
 * replaced.
 */
export function set_cookie(req, name, value) {
  let _block;
  let _pipe = $list.key_pop(req.headers, "cookie");
  _block = $result.unwrap(_pipe, ["", req.headers]);
  let $ = _block;
  let cookies;
  let headers;
  cookies = $[0];
  headers = $[1];
  let _block$1;
  let _pipe$1 = $cookie.parse(cookies);
  let _pipe$2 = $list.key_set(_pipe$1, name, value);
  let _pipe$3 = $list.map(
    _pipe$2,
    (pair) => { return (pair[0] + "=") + pair[1]; },
  );
  _block$1 = $string.join(_pipe$3, "; ");
  let cookies$1 = _block$1;
  return new Request(
    req.method,
    listPrepend(["cookie", cookies$1], headers),
    req.body,
    req.scheme,
    req.host,
    req.port,
    req.path,
    req.query,
  );
}

/**
 * Fetch the cookies sent in a request.
 *
 * Note badly formed cookie pairs will be ignored.
 * RFC6265 specifies that invalid cookie names/attributes should be ignored.
 */
export function get_cookies(req) {
  let headers;
  headers = req.headers;
  return $list.flat_map(
    headers,
    (header) => {
      let $ = header[0];
      if ($ === "cookie") {
        let value = header[1];
        return $cookie.parse(value);
      } else {
        return toList([]);
      }
    },
  );
}

/**
 * Remove a cookie from a request
 *
 * Remove a cookie from the request. If no cookie is found return the request
 * unchanged. This will not remove the cookie from the client.
 */
export function remove_cookie(req, name) {
  let $ = $list.key_pop(req.headers, "cookie");
  if ($ instanceof Ok) {
    let cookies_string = $[0][0];
    let headers = $[0][1];
    let _block;
    let _pipe = $cookie.parse(cookies_string);
    let _pipe$1 = $list.filter_map(
      _pipe,
      (cookie) => {
        let cookie_name = cookie[0];
        if (cookie_name === name) {
          return new Error(undefined);
        } else {
          let name$1 = cookie[0];
          let value = cookie[1];
          return new Ok((name$1 + "=") + value);
        }
      },
    );
    _block = $string.join(_pipe$1, "; ");
    let new_cookies_string = _block;
    return new Request(
      req.method,
      listPrepend(["cookie", new_cookies_string], headers),
      req.body,
      req.scheme,
      req.host,
      req.port,
      req.path,
      req.query,
    );
  } else {
    return req;
  }
}
