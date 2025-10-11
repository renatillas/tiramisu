import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  isEqual,
  toBitArray,
  bitArraySlice,
  bitArraySliceToInt,
  stringBits,
} from "../gleam.mjs";

const FILEPATH = "src/gleam/http.gleam";

export class Get extends $CustomType {}

export class Post extends $CustomType {}

export class Head extends $CustomType {}

export class Put extends $CustomType {}

export class Delete extends $CustomType {}

export class Trace extends $CustomType {}

export class Connect extends $CustomType {}

export class Options extends $CustomType {}

export class Patch extends $CustomType {}

/**
 * Non-standard but valid HTTP methods.
 */
export class Other extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Http extends $CustomType {}

export class Https extends $CustomType {}

/**
 * The headers for the part have been fully parsed.
 * Header keys are all lowercase.
 */
export class MultipartHeaders extends $CustomType {
  constructor(headers, remaining) {
    super();
    this.headers = headers;
    this.remaining = remaining;
  }
}

/**
 * More input is required to parse the headers for this part.
 */
export class MoreRequiredForHeaders extends $CustomType {
  constructor(continuation) {
    super();
    this.continuation = continuation;
  }
}

/**
 * The body for the part has been fully parsed.
 */
export class MultipartBody extends $CustomType {
  constructor(chunk, done, remaining) {
    super();
    this.chunk = chunk;
    this.done = done;
    this.remaining = remaining;
  }
}

export class MoreRequiredForBody extends $CustomType {
  constructor(chunk, continuation) {
    super();
    this.chunk = chunk;
    this.continuation = continuation;
  }
}

export class ContentDisposition extends $CustomType {
  constructor($0, parameters) {
    super();
    this[0] = $0;
    this.parameters = parameters;
  }
}

function is_valid_token_loop(loop$token) {
  while (true) {
    let token = loop$token;
    if (token === "") {
      return true;
    } else if (token.startsWith("!")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("#")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("$")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("%")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("&")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("'")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("*")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("+")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("-")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith(".")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("^")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("_")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("`")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("|")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("~")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("0")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("1")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("2")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("3")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("4")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("5")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("6")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("7")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("8")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("9")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("A")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("B")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("C")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("D")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("E")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("F")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("G")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("H")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("I")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("J")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("K")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("L")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("M")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("N")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("O")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("P")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("Q")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("R")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("S")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("T")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("U")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("V")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("W")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("X")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("Y")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("Z")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("a")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("b")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("c")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("d")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("e")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("f")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("g")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("h")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("i")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("j")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("k")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("l")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("m")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("n")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("o")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("p")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("q")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("r")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("s")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("t")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("u")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("v")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("w")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("x")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("y")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else if (token.startsWith("z")) {
      let rest = token.slice(1);
      loop$token = rest;
    } else {
      return false;
    }
  }
}

function is_valid_token(token) {
  if (token === "") {
    return false;
  } else {
    return is_valid_token_loop(token);
  }
}

export function parse_method(method) {
  if (method === "CONNECT") {
    return new Ok(new Connect());
  } else if (method === "DELETE") {
    return new Ok(new Delete());
  } else if (method === "GET") {
    return new Ok(new Get());
  } else if (method === "HEAD") {
    return new Ok(new Head());
  } else if (method === "OPTIONS") {
    return new Ok(new Options());
  } else if (method === "PATCH") {
    return new Ok(new Patch());
  } else if (method === "POST") {
    return new Ok(new Post());
  } else if (method === "PUT") {
    return new Ok(new Put());
  } else if (method === "TRACE") {
    return new Ok(new Trace());
  } else {
    let method$1 = method;
    let $ = is_valid_token(method$1);
    if ($) {
      return new Ok(new Other(method$1));
    } else {
      return new Error(undefined);
    }
  }
}

export function method_to_string(method) {
  if (method instanceof Get) {
    return "GET";
  } else if (method instanceof Post) {
    return "POST";
  } else if (method instanceof Head) {
    return "HEAD";
  } else if (method instanceof Put) {
    return "PUT";
  } else if (method instanceof Delete) {
    return "DELETE";
  } else if (method instanceof Trace) {
    return "TRACE";
  } else if (method instanceof Connect) {
    return "CONNECT";
  } else if (method instanceof Options) {
    return "OPTIONS";
  } else if (method instanceof Patch) {
    return "PATCH";
  } else {
    let method$1 = method[0];
    return method$1;
  }
}

/**
 * Convert a scheme into a string.
 *
 * # Examples
 *
 * ```gleam
 * assert "http" == scheme_to_string(Http)
 * assert "https" == scheme_to_string(Https)
 * ```
 */
export function scheme_to_string(scheme) {
  if (scheme instanceof Http) {
    return "http";
  } else {
    return "https";
  }
}

/**
 * Parse a HTTP scheme from a string
 *
 * # Examples
 *
 * ```gleam
 * assert Ok(Http) == scheme_from_string("http")
 * assert Error(Nil) == scheme_from_string("ftp")
 * ```
 */
export function scheme_from_string(scheme) {
  let $ = $string.lowercase(scheme);
  if ($ === "http") {
    return new Ok(new Http());
  } else if ($ === "https") {
    return new Ok(new Https());
  } else {
    return new Error(undefined);
  }
}

function more_please_headers(existing, continuation) {
  return new Ok(
    new MoreRequiredForHeaders(
      (more) => {
        return $bool.guard(
          isEqual(more, toBitArray([])),
          new Error(undefined),
          () => { return continuation(toBitArray([existing, more])); },
        );
      },
    ),
  );
}

function more_please_body(chunk, existing, continuation) {
  return new Ok(
    new MoreRequiredForBody(
      chunk,
      (more) => {
        return $bool.guard(
          isEqual(more, toBitArray([])),
          new Error(undefined),
          () => { return continuation(toBitArray([existing, more])); },
        );
      },
    ),
  );
}

function parse_body_loop(
  loop$data,
  loop$boundary,
  loop$boundary_bytes,
  loop$body
) {
  while (true) {
    let data = loop$data;
    let boundary = loop$boundary;
    let boundary_bytes = loop$boundary_bytes;
    let body = loop$body;
    if (data.bitSize === 0) {
      return more_please_body(
        body,
        data,
        (data) => {
          return parse_body_loop(data, boundary, boundary_bytes, toBitArray([]));
        },
      );
    } else if (data.bitSize === 8) {
      if (data.byteAt(0) === 13) {
        return more_please_body(
          body,
          data,
          (data) => {
            return parse_body_loop(
              data,
              boundary,
              boundary_bytes,
              toBitArray([]),
            );
          },
        );
      } else {
        let char = data.byteAt(0);
        let data$1 = bitArraySlice(data, 8);
        loop$data = data$1;
        loop$boundary = boundary;
        loop$boundary_bytes = boundary_bytes;
        loop$body = toBitArray([body, char]);
      }
    } else if (data.bitSize >= 16) {
      if (data.byteAt(0) === 13 && data.byteAt(1) === 10) {
        let rest = bitArraySlice(data, 16);
        if (rest.bitSize >= 16) {
          if (rest.byteAt(0) === 45 && rest.byteAt(1) === 45) {
            if (
              boundary_bytes * 8 >= 0 &&
              rest.bitSize >= 16 + boundary_bytes * 8 &&
              rest.bitSize >= 24 + boundary_bytes * 8
            ) {
              if (
                bitArraySliceToInt(rest, 16 + boundary_bytes * 8, 24 + boundary_bytes * 8, true, false) === 13
              ) {
                if (rest.bitSize >= 32 + boundary_bytes * 8) {
                  if (
                    bitArraySliceToInt(rest, 24 + boundary_bytes * 8, 32 + boundary_bytes * 8, true, false) === 10
                  ) {
                    let found = bitArraySlice(rest, 16, 16 + boundary_bytes * 8);
                    if (isEqual(found, boundary)) {
                      return new Ok(new MultipartBody(body, false, rest));
                    } else if (
                      bitArraySliceToInt(rest, 16 + boundary_bytes * 8, 24 + boundary_bytes * 8, true, false) === 45 &&
                      bitArraySliceToInt(rest, 24 + boundary_bytes * 8, 32 + boundary_bytes * 8, true, false) === 45
                    ) {
                      let found$1 = bitArraySlice(rest, 16, 16 + boundary_bytes * 8);
                      if (isEqual(found$1, boundary)) {
                        let rest$1 = bitArraySlice(rest, 32 + boundary_bytes * 8);
                        return new Ok(new MultipartBody(body, true, rest$1));
                      } else {
                        loop$data = rest;
                        loop$boundary = boundary;
                        loop$boundary_bytes = boundary_bytes;
                        loop$body = toBitArray([body, stringBits("\r\n")]);
                      }
                    } else {
                      loop$data = rest;
                      loop$boundary = boundary;
                      loop$boundary_bytes = boundary_bytes;
                      loop$body = toBitArray([body, stringBits("\r\n")]);
                    }
                  } else if (
                    bitArraySliceToInt(rest, 16 + boundary_bytes * 8, 24 + boundary_bytes * 8, true, false) === 45 &&
                    bitArraySliceToInt(rest, 24 + boundary_bytes * 8, 32 + boundary_bytes * 8, true, false) === 45
                  ) {
                    let found = bitArraySlice(rest, 16, 16 + boundary_bytes * 8);
                    if (isEqual(found, boundary)) {
                      let rest$1 = bitArraySlice(rest, 32 + boundary_bytes * 8);
                      return new Ok(new MultipartBody(body, true, rest$1));
                    } else {
                      loop$data = rest;
                      loop$boundary = boundary;
                      loop$boundary_bytes = boundary_bytes;
                      loop$body = toBitArray([body, stringBits("\r\n")]);
                    }
                  } else {
                    loop$data = rest;
                    loop$boundary = boundary;
                    loop$boundary_bytes = boundary_bytes;
                    loop$body = toBitArray([body, stringBits("\r\n")]);
                  }
                } else {
                  return more_please_body(
                    body,
                    data,
                    (data) => {
                      return parse_body_loop(
                        data,
                        boundary,
                        boundary_bytes,
                        toBitArray([]),
                      );
                    },
                  );
                }
              } else if (
                bitArraySliceToInt(rest, 16 + boundary_bytes * 8, 24 + boundary_bytes * 8, true, false) === 45
              ) {
                if (rest.bitSize >= 32 + boundary_bytes * 8) {
                  if (
                    bitArraySliceToInt(rest, 24 + boundary_bytes * 8, 32 + boundary_bytes * 8, true, false) === 45
                  ) {
                    let found = bitArraySlice(rest, 16, 16 + boundary_bytes * 8);
                    if (isEqual(found, boundary)) {
                      let rest$1 = bitArraySlice(rest, 32 + boundary_bytes * 8);
                      return new Ok(new MultipartBody(body, true, rest$1));
                    } else {
                      loop$data = rest;
                      loop$boundary = boundary;
                      loop$boundary_bytes = boundary_bytes;
                      loop$body = toBitArray([body, stringBits("\r\n")]);
                    }
                  } else {
                    loop$data = rest;
                    loop$boundary = boundary;
                    loop$boundary_bytes = boundary_bytes;
                    loop$body = toBitArray([body, stringBits("\r\n")]);
                  }
                } else {
                  return more_please_body(
                    body,
                    data,
                    (data) => {
                      return parse_body_loop(
                        data,
                        boundary,
                        boundary_bytes,
                        toBitArray([]),
                      );
                    },
                  );
                }
              } else if (rest.bitSize >= 32 + boundary_bytes * 8) {
                loop$data = rest;
                loop$boundary = boundary;
                loop$boundary_bytes = boundary_bytes;
                loop$body = toBitArray([body, stringBits("\r\n")]);
              } else {
                return more_please_body(
                  body,
                  data,
                  (data) => {
                    return parse_body_loop(
                      data,
                      boundary,
                      boundary_bytes,
                      toBitArray([]),
                    );
                  },
                );
              }
            } else {
              return more_please_body(
                body,
                data,
                (data) => {
                  return parse_body_loop(
                    data,
                    boundary,
                    boundary_bytes,
                    toBitArray([]),
                  );
                },
              );
            }
          } else if (
            boundary_bytes * 8 >= 0 &&
            rest.bitSize >= 16 + boundary_bytes * 8 &&
            rest.bitSize >= 24 + boundary_bytes * 8 &&
            rest.bitSize >= 32 + boundary_bytes * 8
          ) {
            loop$data = rest;
            loop$boundary = boundary;
            loop$boundary_bytes = boundary_bytes;
            loop$body = toBitArray([body, stringBits("\r\n")]);
          } else {
            return more_please_body(
              body,
              data,
              (data) => {
                return parse_body_loop(
                  data,
                  boundary,
                  boundary_bytes,
                  toBitArray([]),
                );
              },
            );
          }
        } else {
          return more_please_body(
            body,
            data,
            (data) => {
              return parse_body_loop(
                data,
                boundary,
                boundary_bytes,
                toBitArray([]),
              );
            },
          );
        }
      } else {
        let char = data.byteAt(0);
        let data$1 = bitArraySlice(data, 8);
        loop$data = data$1;
        loop$boundary = boundary;
        loop$boundary_bytes = boundary_bytes;
        loop$body = toBitArray([body, char]);
      }
    } else if (data.bitSize >= 8) {
      let char = data.byteAt(0);
      let data$1 = bitArraySlice(data, 8);
      loop$data = data$1;
      loop$boundary = boundary;
      loop$boundary_bytes = boundary_bytes;
      loop$body = toBitArray([body, char]);
    } else {
      throw makeError(
        "panic",
        FILEPATH,
        "gleam/http",
        520,
        "parse_body_loop",
        "unreachable",
        {}
      )
    }
  }
}

function do_parse_multipart_body(data, boundary, boundary_bytes) {
  if (
    data.bitSize >= 16 &&
    data.byteAt(0) === 45 && data.byteAt(1) === 45 &&
    boundary_bytes * 8 >= 0 &&
    data.bitSize >= 16 + boundary_bytes * 8 &&
    (data.bitSize - (16 + boundary_bytes * 8)) % 8 === 0
  ) {
    let found = bitArraySlice(data, 16, 16 + boundary_bytes * 8);
    if (isEqual(found, boundary)) {
      return new Ok(new MultipartBody(toBitArray([]), false, data));
    } else {
      return parse_body_loop(data, boundary, boundary_bytes, toBitArray([]));
    }
  } else {
    return parse_body_loop(data, boundary, boundary_bytes, toBitArray([]));
  }
}

/**
 * Parse the body for part of a multipart message, as defined in RFC 2045. The
 * body is everything until the next boundary. This function is generally to be
 * called after calling `parse_multipart_headers` for a given part.
 *
 * This function will accept input of any size, it is up to the caller to limit
 * it if needed.
 *
 * To enable streaming parsing of multipart messages, this function will return
 * a continuation if there is not enough data to fully parse the body, along
 * with the data that has been parsed so far. Further information is available
 * in the documentation for `MultipartBody`.
 */
export function parse_multipart_body(data, boundary) {
  let boundary$1 = $bit_array.from_string(boundary);
  let boundary_bytes = $bit_array.byte_size(boundary$1);
  return do_parse_multipart_body(data, boundary$1, boundary_bytes);
}

function parse_rfc_2045_parameter_quoted_value(
  loop$header,
  loop$name,
  loop$value
) {
  while (true) {
    let header = loop$header;
    let name = loop$name;
    let value = loop$value;
    let $ = $string.pop_grapheme(header);
    if ($ instanceof Ok) {
      let $1 = $[0][0];
      if ($1 === "\"") {
        let rest = $[0][1];
        return new Ok([[name, value], rest]);
      } else if ($1 === "\\") {
        let rest = $[0][1];
        return $result.try$(
          $string.pop_grapheme(rest),
          (_use0) => {
            let grapheme;
            let rest$1;
            grapheme = _use0[0];
            rest$1 = _use0[1];
            return parse_rfc_2045_parameter_quoted_value(
              rest$1,
              name,
              value + grapheme,
            );
          },
        );
      } else {
        let grapheme = $1;
        let rest = $[0][1];
        loop$header = rest;
        loop$name = name;
        loop$value = value + grapheme;
      }
    } else {
      return $;
    }
  }
}

function parse_rfc_2045_parameter_unquoted_value(
  loop$header,
  loop$name,
  loop$value
) {
  while (true) {
    let header = loop$header;
    let name = loop$name;
    let value = loop$value;
    let $ = $string.pop_grapheme(header);
    if ($ instanceof Ok) {
      let $1 = $[0][0];
      if ($1 === ";") {
        let rest = $[0][1];
        return [[name, value], rest];
      } else if ($1 === " ") {
        let rest = $[0][1];
        return [[name, value], rest];
      } else if ($1 === "\t") {
        let rest = $[0][1];
        return [[name, value], rest];
      } else {
        let grapheme = $1;
        let rest = $[0][1];
        loop$header = rest;
        loop$name = name;
        loop$value = value + grapheme;
      }
    } else {
      return [[name, value], header];
    }
  }
}

function parse_rfc_2045_parameter_value(header, name) {
  let $ = $string.pop_grapheme(header);
  if ($ instanceof Ok) {
    let $1 = $[0][0];
    if ($1 === "\"") {
      let rest = $[0][1];
      return parse_rfc_2045_parameter_quoted_value(rest, name, "");
    } else {
      let grapheme = $1;
      let rest = $[0][1];
      return new Ok(
        parse_rfc_2045_parameter_unquoted_value(rest, name, grapheme),
      );
    }
  } else {
    return $;
  }
}

function parse_rfc_2045_parameter(header, name) {
  return $result.try$(
    $string.pop_grapheme(header),
    (_use0) => {
      let grapheme;
      let rest;
      grapheme = _use0[0];
      rest = _use0[1];
      if (grapheme === "=") {
        return parse_rfc_2045_parameter_value(rest, name);
      } else {
        return parse_rfc_2045_parameter(
          rest,
          name + $string.lowercase(grapheme),
        );
      }
    },
  );
}

function parse_rfc_2045_parameters(loop$header, loop$parameters) {
  while (true) {
    let header = loop$header;
    let parameters = loop$parameters;
    let $ = $string.pop_grapheme(header);
    if ($ instanceof Ok) {
      let $1 = $[0][0];
      if ($1 === ";") {
        let rest = $[0][1];
        loop$header = rest;
        loop$parameters = parameters;
      } else if ($1 === " ") {
        let rest = $[0][1];
        loop$header = rest;
        loop$parameters = parameters;
      } else if ($1 === "\t") {
        let rest = $[0][1];
        loop$header = rest;
        loop$parameters = parameters;
      } else {
        let grapheme = $1;
        let rest = $[0][1];
        let acc = $string.lowercase(grapheme);
        return $result.try$(
          parse_rfc_2045_parameter(rest, acc),
          (_use0) => {
            let parameter;
            let rest$1;
            parameter = _use0[0];
            rest$1 = _use0[1];
            return parse_rfc_2045_parameters(
              rest$1,
              listPrepend(parameter, parameters),
            );
          },
        );
      }
    } else {
      return new Ok($list.reverse(parameters));
    }
  }
}

function parse_content_disposition_type(loop$header, loop$name) {
  while (true) {
    let header = loop$header;
    let name = loop$name;
    let $ = $string.pop_grapheme(header);
    if ($ instanceof Ok) {
      let $1 = $[0][0];
      if ($1 === " ") {
        let rest = $[0][1];
        let result = parse_rfc_2045_parameters(rest, toList([]));
        return $result.map(
          result,
          (parameters) => { return new ContentDisposition(name, parameters); },
        );
      } else if ($1 === "\t") {
        let rest = $[0][1];
        let result = parse_rfc_2045_parameters(rest, toList([]));
        return $result.map(
          result,
          (parameters) => { return new ContentDisposition(name, parameters); },
        );
      } else if ($1 === ";") {
        let rest = $[0][1];
        let result = parse_rfc_2045_parameters(rest, toList([]));
        return $result.map(
          result,
          (parameters) => { return new ContentDisposition(name, parameters); },
        );
      } else {
        let grapheme = $1;
        let rest = $[0][1];
        loop$header = rest;
        loop$name = name + $string.lowercase(grapheme);
      }
    } else {
      return new Ok(new ContentDisposition(name, toList([])));
    }
  }
}

export function parse_content_disposition(header) {
  return parse_content_disposition_type(header, "");
}

function parse_header_value_loop(loop$data, loop$headers, loop$name, loop$value) {
  while (true) {
    let data = loop$data;
    let headers = loop$headers;
    let name = loop$name;
    let value = loop$value;
    if (data.bitSize === 0) {
      return more_please_headers(
        data,
        (data) => { return parse_header_value_loop(data, headers, name, value); },
      );
    } else if (data.bitSize === 8) {
      return more_please_headers(
        data,
        (data) => { return parse_header_value_loop(data, headers, name, value); },
      );
    } else if (data.bitSize >= 8) {
      if (data.bitSize === 16) {
        return more_please_headers(
          data,
          (data) => {
            return parse_header_value_loop(data, headers, name, value);
          },
        );
      } else if (data.bitSize >= 16) {
        if (data.bitSize === 24) {
          return more_please_headers(
            data,
            (data) => {
              return parse_header_value_loop(data, headers, name, value);
            },
          );
        } else if (data.bitSize >= 32) {
          if (
            data.byteAt(0) === 13 &&
              data.byteAt(1) === 10 &&
              data.byteAt(2) === 13 &&
              data.byteAt(3) === 10 &&
            (data.bitSize - 32) % 8 === 0
          ) {
            let data$1 = bitArraySlice(data, 32);
            return $result.map(
              $bit_array.to_string(value),
              (value) => {
                let headers$1 = $list.reverse(
                  listPrepend([$string.lowercase(name), value], headers),
                );
                return new MultipartHeaders(headers$1, data$1);
              },
            );
          } else if (
            data.byteAt(0) === 13 &&
              data.byteAt(1) === 10 &&
              data.byteAt(2) === 32
          ) {
            if ((data.bitSize - 24) % 8 === 0) {
              let data$1 = bitArraySlice(data, 24);
              loop$data = data$1;
              loop$headers = headers;
              loop$name = name;
              loop$value = value;
            } else if (
              data.byteAt(0) === 13 && data.byteAt(1) === 10 &&
              (data.bitSize - 16) % 8 === 0
            ) {
              let data$1 = bitArraySlice(data, 16);
              return $result.try$(
                $bit_array.to_string(value),
                (value) => {
                  let headers$1 = listPrepend(
                    [$string.lowercase(name), value],
                    headers,
                  );
                  return parse_header_name(data$1, headers$1);
                },
              );
            } else if ((data.bitSize - 8) % 8 === 0) {
              let char = data.byteAt(0);
              let rest = bitArraySlice(data, 8);
              loop$data = rest;
              loop$headers = headers;
              loop$name = name;
              loop$value = toBitArray([value, char]);
            } else {
              return new Error(undefined);
            }
          } else if (
            data.byteAt(0) === 13 &&
              data.byteAt(1) === 10 &&
              data.byteAt(2) === 9 &&
            (data.bitSize - 24) % 8 === 0
          ) {
            let data$1 = bitArraySlice(data, 24);
            loop$data = data$1;
            loop$headers = headers;
            loop$name = name;
            loop$value = value;
          } else if (
            data.byteAt(0) === 13 && data.byteAt(1) === 10 &&
            (data.bitSize - 16) % 8 === 0
          ) {
            let data$1 = bitArraySlice(data, 16);
            return $result.try$(
              $bit_array.to_string(value),
              (value) => {
                let headers$1 = listPrepend(
                  [$string.lowercase(name), value],
                  headers,
                );
                return parse_header_name(data$1, headers$1);
              },
            );
          } else if ((data.bitSize - 8) % 8 === 0) {
            let char = data.byteAt(0);
            let rest = bitArraySlice(data, 8);
            loop$data = rest;
            loop$headers = headers;
            loop$name = name;
            loop$value = toBitArray([value, char]);
          } else {
            return new Error(undefined);
          }
        } else if (data.bitSize >= 24) {
          if (
            data.byteAt(0) === 13 &&
              data.byteAt(1) === 10 &&
              data.byteAt(2) === 32
          ) {
            if ((data.bitSize - 24) % 8 === 0) {
              let data$1 = bitArraySlice(data, 24);
              loop$data = data$1;
              loop$headers = headers;
              loop$name = name;
              loop$value = value;
            } else if (
              data.byteAt(0) === 13 && data.byteAt(1) === 10 &&
              (data.bitSize - 16) % 8 === 0
            ) {
              let data$1 = bitArraySlice(data, 16);
              return $result.try$(
                $bit_array.to_string(value),
                (value) => {
                  let headers$1 = listPrepend(
                    [$string.lowercase(name), value],
                    headers,
                  );
                  return parse_header_name(data$1, headers$1);
                },
              );
            } else if ((data.bitSize - 8) % 8 === 0) {
              let char = data.byteAt(0);
              let rest = bitArraySlice(data, 8);
              loop$data = rest;
              loop$headers = headers;
              loop$name = name;
              loop$value = toBitArray([value, char]);
            } else {
              return new Error(undefined);
            }
          } else if (
            data.byteAt(0) === 13 &&
              data.byteAt(1) === 10 &&
              data.byteAt(2) === 9 &&
            (data.bitSize - 24) % 8 === 0
          ) {
            let data$1 = bitArraySlice(data, 24);
            loop$data = data$1;
            loop$headers = headers;
            loop$name = name;
            loop$value = value;
          } else if (
            data.byteAt(0) === 13 && data.byteAt(1) === 10 &&
            (data.bitSize - 16) % 8 === 0
          ) {
            let data$1 = bitArraySlice(data, 16);
            return $result.try$(
              $bit_array.to_string(value),
              (value) => {
                let headers$1 = listPrepend(
                  [$string.lowercase(name), value],
                  headers,
                );
                return parse_header_name(data$1, headers$1);
              },
            );
          } else if ((data.bitSize - 8) % 8 === 0) {
            let char = data.byteAt(0);
            let rest = bitArraySlice(data, 8);
            loop$data = rest;
            loop$headers = headers;
            loop$name = name;
            loop$value = toBitArray([value, char]);
          } else {
            return new Error(undefined);
          }
        } else if (
          data.byteAt(0) === 13 && data.byteAt(1) === 10 &&
          (data.bitSize - 16) % 8 === 0
        ) {
          let data$1 = bitArraySlice(data, 16);
          return $result.try$(
            $bit_array.to_string(value),
            (value) => {
              let headers$1 = listPrepend(
                [$string.lowercase(name), value],
                headers,
              );
              return parse_header_name(data$1, headers$1);
            },
          );
        } else if ((data.bitSize - 8) % 8 === 0) {
          let char = data.byteAt(0);
          let rest = bitArraySlice(data, 8);
          loop$data = rest;
          loop$headers = headers;
          loop$name = name;
          loop$value = toBitArray([value, char]);
        } else {
          return new Error(undefined);
        }
      } else if ((data.bitSize - 8) % 8 === 0) {
        let char = data.byteAt(0);
        let rest = bitArraySlice(data, 8);
        loop$data = rest;
        loop$headers = headers;
        loop$name = name;
        loop$value = toBitArray([value, char]);
      } else {
        return new Error(undefined);
      }
    } else {
      return new Error(undefined);
    }
  }
}

function parse_header_name(loop$data, loop$headers) {
  while (true) {
    let data = loop$data;
    let headers = loop$headers;
    if (data.bitSize >= 8) {
      if (data.byteAt(0) === 32) {
        let rest = bitArraySlice(data, 8);
        loop$data = rest;
        loop$headers = headers;
      } else if (data.byteAt(0) === 9) {
        let rest = bitArraySlice(data, 8);
        loop$data = rest;
        loop$headers = headers;
      } else {
        return parse_header_name_loop(data, headers, toBitArray([]));
      }
    } else {
      return more_please_headers(
        data,
        (_capture) => { return parse_header_name(_capture, headers); },
      );
    }
  }
}

function parse_header_name_loop(loop$data, loop$headers, loop$name) {
  while (true) {
    let data = loop$data;
    let headers = loop$headers;
    let name = loop$name;
    if (data.bitSize >= 8) {
      if (data.byteAt(0) === 58) {
        let data$1 = bitArraySlice(data, 8);
        let $ = $bit_array.to_string(name);
        if ($ instanceof Ok) {
          let name$1 = $[0];
          return parse_header_value(data$1, headers, name$1);
        } else {
          return $;
        }
      } else {
        let char = data.byteAt(0);
        let data$1 = bitArraySlice(data, 8);
        loop$data = data$1;
        loop$headers = headers;
        loop$name = toBitArray([name, char]);
      }
    } else {
      return more_please_headers(
        data,
        (_capture) => { return parse_header_name_loop(_capture, headers, name); },
      );
    }
  }
}

function parse_header_value(loop$data, loop$headers, loop$name) {
  while (true) {
    let data = loop$data;
    let headers = loop$headers;
    let name = loop$name;
    if (data.bitSize >= 8) {
      if (data.byteAt(0) === 32) {
        let rest = bitArraySlice(data, 8);
        loop$data = rest;
        loop$headers = headers;
        loop$name = name;
      } else if (data.byteAt(0) === 9) {
        let rest = bitArraySlice(data, 8);
        loop$data = rest;
        loop$headers = headers;
        loop$name = name;
      } else {
        return parse_header_value_loop(data, headers, name, toBitArray([]));
      }
    } else {
      return more_please_headers(
        data,
        (_capture) => { return parse_header_value(_capture, headers, name); },
      );
    }
  }
}

function do_parse_headers(data) {
  if (data.bitSize >= 32) {
    if (
      data.byteAt(0) === 13 &&
        data.byteAt(1) === 10 &&
        data.byteAt(2) === 13 &&
        data.byteAt(3) === 10 &&
      (data.bitSize - 32) % 8 === 0
    ) {
      let data$1 = bitArraySlice(data, 32);
      return new Ok(new MultipartHeaders(toList([]), data$1));
    } else if (
      data.byteAt(0) === 13 && data.byteAt(1) === 10 &&
      (data.bitSize - 16) % 8 === 0
    ) {
      let data$1 = bitArraySlice(data, 16);
      return parse_header_name(data$1, toList([]));
    } else {
      return new Error(undefined);
    }
  } else if (data.bitSize >= 16) {
    if (
      data.byteAt(0) === 13 && data.byteAt(1) === 10 &&
      (data.bitSize - 16) % 8 === 0
    ) {
      let data$1 = bitArraySlice(data, 16);
      return parse_header_name(data$1, toList([]));
    } else {
      return new Error(undefined);
    }
  } else if (data.bitSize === 8) {
    if (data.byteAt(0) === 13) {
      return more_please_headers(data, do_parse_headers);
    } else {
      return new Error(undefined);
    }
  } else if (data.bitSize === 0) {
    return more_please_headers(data, do_parse_headers);
  } else {
    return new Error(undefined);
  }
}

function skip_preamble(loop$data, loop$boundary, loop$boundary_bytes) {
  while (true) {
    let data = loop$data;
    let boundary = loop$boundary;
    let boundary_bytes = loop$boundary_bytes;
    if (data.bitSize >= 32) {
      if (
        data.byteAt(0) === 13 &&
          data.byteAt(1) === 10 &&
          data.byteAt(2) === 45 &&
          data.byteAt(3) === 45 &&
        (data.bitSize - 32) % 8 === 0
      ) {
        let rest = bitArraySlice(data, 32);
        if (boundary_bytes * 8 >= 0 && rest.bitSize >= boundary_bytes * 8) {
          let found = bitArraySlice(rest, 0, boundary_bytes * 8);
          if (isEqual(found, boundary)) {
            let rest$1 = bitArraySlice(rest, boundary_bytes * 8);
            return do_parse_headers(rest$1);
          } else {
            loop$data = rest;
            loop$boundary = boundary;
            loop$boundary_bytes = boundary_bytes;
          }
        } else {
          return more_please_headers(
            data,
            (_capture) => {
              return skip_preamble(_capture, boundary, boundary_bytes);
            },
          );
        }
      } else if ((data.bitSize - 8) % 8 === 0) {
        let data$1 = bitArraySlice(data, 8);
        loop$data = data$1;
        loop$boundary = boundary;
        loop$boundary_bytes = boundary_bytes;
      } else {
        throw makeError(
          "panic",
          FILEPATH,
          "gleam/http",
          341,
          "skip_preamble",
          "unreachable",
          {}
        )
      }
    } else if (data.bitSize === 0) {
      return more_please_headers(
        data,
        (_capture) => {
          return skip_preamble(_capture, boundary, boundary_bytes);
        },
      );
    } else if (data.bitSize === 8) {
      if (data.byteAt(0) === 13) {
        return more_please_headers(
          data,
          (_capture) => {
            return skip_preamble(_capture, boundary, boundary_bytes);
          },
        );
      } else if ((data.bitSize - 8) % 8 === 0) {
        let data$1 = bitArraySlice(data, 8);
        loop$data = data$1;
        loop$boundary = boundary;
        loop$boundary_bytes = boundary_bytes;
      } else {
        throw makeError(
          "panic",
          FILEPATH,
          "gleam/http",
          341,
          "skip_preamble",
          "unreachable",
          {}
        )
      }
    } else if (data.bitSize === 16) {
      if (data.byteAt(0) === 13 && data.byteAt(1) === 10) {
        return more_please_headers(
          data,
          (_capture) => {
            return skip_preamble(_capture, boundary, boundary_bytes);
          },
        );
      } else if ((data.bitSize - 8) % 8 === 0) {
        let data$1 = bitArraySlice(data, 8);
        loop$data = data$1;
        loop$boundary = boundary;
        loop$boundary_bytes = boundary_bytes;
      } else {
        throw makeError(
          "panic",
          FILEPATH,
          "gleam/http",
          341,
          "skip_preamble",
          "unreachable",
          {}
        )
      }
    } else if (data.bitSize === 24) {
      if (
        data.byteAt(0) === 13 && data.byteAt(1) === 10 && data.byteAt(2) === 45
      ) {
        return more_please_headers(
          data,
          (_capture) => {
            return skip_preamble(_capture, boundary, boundary_bytes);
          },
        );
      } else if ((data.bitSize - 8) % 8 === 0) {
        let data$1 = bitArraySlice(data, 8);
        loop$data = data$1;
        loop$boundary = boundary;
        loop$boundary_bytes = boundary_bytes;
      } else {
        throw makeError(
          "panic",
          FILEPATH,
          "gleam/http",
          341,
          "skip_preamble",
          "unreachable",
          {}
        )
      }
    } else if (data.bitSize >= 8 && (data.bitSize - 8) % 8 === 0) {
      let data$1 = bitArraySlice(data, 8);
      loop$data = data$1;
      loop$boundary = boundary;
      loop$boundary_bytes = boundary_bytes;
    } else {
      throw makeError(
        "panic",
        FILEPATH,
        "gleam/http",
        341,
        "skip_preamble",
        "unreachable",
        {}
      )
    }
  }
}

function do_parse_multipart_headers(data, boundary, boundary_bytes) {
  if (
    data.bitSize >= 16 &&
    data.byteAt(0) === 45 && data.byteAt(1) === 45 &&
    boundary_bytes * 8 >= 0 &&
    data.bitSize >= 16 + boundary_bytes * 8
  ) {
    let found = bitArraySlice(data, 16, 16 + boundary_bytes * 8);
    if (isEqual(found, boundary)) {
      let rest = bitArraySlice(data, 16 + boundary_bytes * 8);
      if (rest.bitSize >= 16) {
        if (rest.byteAt(0) === 45 && rest.byteAt(1) === 45) {
          let rest$1 = bitArraySlice(rest, 16);
          return new Ok(new MultipartHeaders(toList([]), rest$1));
        } else {
          return do_parse_headers(rest);
        }
      } else {
        return more_please_headers(
          data,
          (data) => {
            return do_parse_multipart_headers(data, boundary, boundary_bytes);
          },
        );
      }
    } else {
      return skip_preamble(data, boundary, boundary_bytes);
    }
  } else {
    return skip_preamble(data, boundary, boundary_bytes);
  }
}

/**
 * Parse the headers for part of a multipart message, as defined in RFC 2045.
 *
 * This function skips any preamble before the boundary. The preamble may be
 * retrieved using `parse_multipart_body`.
 *
 * This function will accept input of any size, it is up to the caller to limit
 * it if needed.
 *
 * To enable streaming parsing of multipart messages, this function will return
 * a continuation if there is not enough data to fully parse the headers.
 * Further information is available in the documentation for `MultipartBody`.
 */
export function parse_multipart_headers(data, boundary) {
  let boundary$1 = $bit_array.from_string(boundary);
  let boundary_bytes = $bit_array.byte_size(boundary$1);
  return do_parse_multipart_headers(data, boundary$1, boundary_bytes);
}
