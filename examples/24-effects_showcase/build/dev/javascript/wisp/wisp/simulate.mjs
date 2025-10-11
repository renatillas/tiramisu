import * as $crypto from "../../gleam_crypto/gleam/crypto.mjs";
import * as $http from "../../gleam_http/gleam/http.mjs";
import * as $request from "../../gleam_http/gleam/http/request.mjs";
import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bytes_tree from "../../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $uri from "../../gleam_stdlib/gleam/uri.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import {
  Ok,
  toList,
  CustomType as $CustomType,
  makeError,
  toBitArray,
  stringBits,
} from "../gleam.mjs";
import * as $wisp from "../wisp.mjs";
import { Bytes, File, Text } from "../wisp.mjs";

const FILEPATH = "src/wisp/simulate.gleam";

export class FileUpload extends $CustomType {
  constructor(file_name, content_type, content) {
    super();
    this.file_name = file_name;
    this.content_type = content_type;
    this.content = content;
  }
}

function build_multipart_body(form_values, files, boundary) {
  let _block;
  let _pipe = $list.fold(
    form_values,
    toBitArray([]),
    (acc, field) => {
      let name;
      let value;
      name = field[0];
      value = field[1];
      return toBitArray([
        acc,
        stringBits("--"),
        stringBits(boundary),
        stringBits("\r\n"),
        stringBits("Content-Disposition: form-data; name=\""),
        stringBits(name),
        stringBits("\"\r\n"),
        stringBits("\r\n"),
        stringBits(value),
        stringBits("\r\n"),
      ]);
    },
  );
  _block = ((_capture) => {
    return $list.fold(
      files,
      _capture,
      (acc, file) => {
        return toBitArray([
          acc,
          stringBits("--"),
          stringBits(boundary),
          stringBits("\r\n"),
          stringBits("Content-Disposition: form-data; name=\""),
          stringBits(file[0]),
          stringBits("\"; filename=\""),
          stringBits(file[1].file_name),
          stringBits("\"\r\n"),
          stringBits("Content-Type: "),
          stringBits(file[1].content_type),
          stringBits("\r\n"),
          stringBits("\r\n"),
          file[1].content,
          stringBits("\r\n"),
        ]);
      },
    );
  })(_pipe);
  let body = _block;
  return toBitArray([
    body,
    stringBits("--"),
    stringBits(boundary),
    stringBits("--\r\n"),
  ]);
}

/**
 * Read a text body from a response.
 *
 * # Panics
 *
 * This function will panic if the response body is a file and the file cannot
 * be read, or if it does not contain valid UTF-8.
 */
export function read_body(response) {
  let $ = response.body;
  if ($ instanceof Text) {
    let tree = $[0];
    return tree;
  } else if ($ instanceof Bytes) {
    let bytes = $[0];
    let data = $bytes_tree.to_bit_array(bytes);
    let $1 = $bit_array.to_string(data);
    let string;
    if ($1 instanceof Ok) {
      string = $1[0];
    } else {
      throw makeError(
        "let_assert",
        FILEPATH,
        "wisp/simulate",
        260,
        "read_body",
        "the response body was non-UTF8 binary data",
        {
          value: $1,
          start: 7570,
          end: 7619,
          pattern_start: 7581,
          pattern_end: 7591
        }
      )
    }
    return string;
  } else {
    let $1 = $.limit;
    if ($1 instanceof None) {
      let $2 = $.offset;
      if ($2 === 0) {
        let path = $.path;
        let $3 = $simplifile.read_bits(path);
        let data;
        if ($3 instanceof Ok) {
          data = $3[0];
        } else {
          throw makeError(
            "let_assert",
            FILEPATH,
            "wisp/simulate",
            265,
            "read_body",
            "the body was a file, but the file could not be read",
            {
              value: $3,
              start: 7746,
              end: 7794,
              pattern_start: 7757,
              pattern_end: 7765
            }
          )
        }
        let $4 = $bit_array.to_string(data);
        let contents;
        if ($4 instanceof Ok) {
          contents = $4[0];
        } else {
          throw makeError(
            "let_assert",
            FILEPATH,
            "wisp/simulate",
            267,
            "read_body",
            "the body file was not valid UTF-8",
            {
              value: $4,
              start: 7866,
              end: 7917,
              pattern_start: 7877,
              pattern_end: 7889
            }
          )
        }
        return contents;
      } else {
        let path = $.path;
        let offset = $2;
        let limit = $1;
        let $3 = $simplifile.read_bits(path);
        let data;
        if ($3 instanceof Ok) {
          data = $3[0];
        } else {
          throw makeError(
            "let_assert",
            FILEPATH,
            "wisp/simulate",
            272,
            "read_body",
            "the body was a file, but the file could not be read",
            {
              value: $3,
              start: 8030,
              end: 8078,
              pattern_start: 8041,
              pattern_end: 8049
            }
          )
        }
        let _block;
        let _pipe = limit;
        _block = $option.unwrap(_pipe, $bit_array.byte_size(data) - offset);
        let byte_length = _block;
        let $4 = $bit_array.slice(data, offset, byte_length);
        let slice;
        if ($4 instanceof Ok) {
          slice = $4[0];
        } else {
          throw makeError(
            "let_assert",
            FILEPATH,
            "wisp/simulate",
            276,
            "read_body",
            "the body was a file, but the limit and offset were invalid",
            {
              value: $4,
              start: 8241,
              end: 8306,
              pattern_start: 8252,
              pattern_end: 8261
            }
          )
        }
        let $5 = $bit_array.to_string(slice);
        let string;
        if ($5 instanceof Ok) {
          string = $5[0];
        } else {
          throw makeError(
            "let_assert",
            FILEPATH,
            "wisp/simulate",
            278,
            "read_body",
            "the body file range was not valid UTF-8",
            {
              value: $5,
              start: 8385,
              end: 8435,
              pattern_start: 8396,
              pattern_end: 8406
            }
          )
        }
        return string;
      }
    } else {
      let path = $.path;
      let offset = $.offset;
      let limit = $1;
      let $2 = $simplifile.read_bits(path);
      let data;
      if ($2 instanceof Ok) {
        data = $2[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "wisp/simulate",
          272,
          "read_body",
          "the body was a file, but the file could not be read",
          {
            value: $2,
            start: 8030,
            end: 8078,
            pattern_start: 8041,
            pattern_end: 8049
          }
        )
      }
      let _block;
      let _pipe = limit;
      _block = $option.unwrap(_pipe, $bit_array.byte_size(data) - offset);
      let byte_length = _block;
      let $3 = $bit_array.slice(data, offset, byte_length);
      let slice;
      if ($3 instanceof Ok) {
        slice = $3[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "wisp/simulate",
          276,
          "read_body",
          "the body was a file, but the limit and offset were invalid",
          {
            value: $3,
            start: 8241,
            end: 8306,
            pattern_start: 8252,
            pattern_end: 8261
          }
        )
      }
      let $4 = $bit_array.to_string(slice);
      let string;
      if ($4 instanceof Ok) {
        string = $4[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "wisp/simulate",
          278,
          "read_body",
          "the body file range was not valid UTF-8",
          {
            value: $4,
            start: 8385,
            end: 8435,
            pattern_start: 8396,
            pattern_end: 8406
          }
        )
      }
      return string;
    }
  }
}

/**
 * Read a binary data body from a response.
 *
 * # Panics
 *
 * This function will panic if the response body is a file and the file cannot
 * be read.
 */
export function read_body_bits(response) {
  let $ = response.body;
  if ($ instanceof Text) {
    let tree = $[0];
    return toBitArray([stringBits(tree)]);
  } else if ($ instanceof Bytes) {
    let tree = $[0];
    return $bytes_tree.to_bit_array(tree);
  } else {
    let $1 = $.limit;
    if ($1 instanceof None) {
      let $2 = $.offset;
      if ($2 === 0) {
        let path = $.path;
        let $3 = $simplifile.read_bits(path);
        let contents;
        if ($3 instanceof Ok) {
          contents = $3[0];
        } else {
          throw makeError(
            "let_assert",
            FILEPATH,
            "wisp/simulate",
            297,
            "read_body_bits",
            "the response body was a file, but the file could not be read",
            {
              value: $3,
              start: 8889,
              end: 8941,
              pattern_start: 8900,
              pattern_end: 8912
            }
          )
        }
        return contents;
      } else {
        let path = $.path;
        let offset = $2;
        let limit = $1;
        let $3 = $simplifile.read_bits(path);
        let contents;
        if ($3 instanceof Ok) {
          contents = $3[0];
        } else {
          throw makeError(
            "let_assert",
            FILEPATH,
            "wisp/simulate",
            302,
            "read_body_bits",
            "the body was a file, but the file could not be read",
            {
              value: $3,
              start: 9081,
              end: 9133,
              pattern_start: 9092,
              pattern_end: 9104
            }
          )
        }
        let _block;
        let _pipe = limit;
        _block = $option.unwrap(_pipe, $bit_array.byte_size(contents));
        let limit$1 = _block;
        let _block$1;
        let _pipe$1 = contents;
        _block$1 = $bit_array.slice(_pipe$1, offset, limit$1);
        let $4 = _block$1;
        let sliced;
        if ($4 instanceof Ok) {
          sliced = $4[0];
        } else {
          throw makeError(
            "let_assert",
            FILEPATH,
            "wisp/simulate",
            305,
            "read_body_bits",
            "the body was a file, but the limit and offset were invalid",
            {
              value: $4,
              start: 9277,
              end: 9343,
              pattern_start: 9288,
              pattern_end: 9298
            }
          )
        }
        return sliced;
      }
    } else {
      let path = $.path;
      let offset = $.offset;
      let limit = $1;
      let $2 = $simplifile.read_bits(path);
      let contents;
      if ($2 instanceof Ok) {
        contents = $2[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "wisp/simulate",
          302,
          "read_body_bits",
          "the body was a file, but the file could not be read",
          {
            value: $2,
            start: 9081,
            end: 9133,
            pattern_start: 9092,
            pattern_end: 9104
          }
        )
      }
      let _block;
      let _pipe = limit;
      _block = $option.unwrap(_pipe, $bit_array.byte_size(contents));
      let limit$1 = _block;
      let _block$1;
      let _pipe$1 = contents;
      _block$1 = $bit_array.slice(_pipe$1, offset, limit$1);
      let $3 = _block$1;
      let sliced;
      if ($3 instanceof Ok) {
        sliced = $3[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "wisp/simulate",
          305,
          "read_body_bits",
          "the body was a file, but the limit and offset were invalid",
          {
            value: $3,
            start: 9277,
            end: 9343,
            pattern_start: 9288,
            pattern_end: 9298
          }
        )
      }
      return sliced;
    }
  }
}

/**
 * Set a header on a request.
 */
export function header(request, name, value) {
  return $request.set_header(request, name, value);
}

/**
 * Continue a browser session from a previous request and response, adopting
 * the request cookies, and updating the cookies as specified by the response.
 */
export function session(next_request, previous_request, previous_response) {
  let _block;
  let $ = $list.key_find(previous_request.headers, "cookie");
  if ($ instanceof Ok) {
    let cookies = $[0];
    _block = header(next_request, "cookie", cookies);
  } else {
    _block = next_request;
  }
  let request$1 = _block;
  let _block$1;
  let _pipe = $list.key_filter(previous_response.headers, "set-cookie");
  let _pipe$1 = $list.map(
    _pipe,
    (cookie) => {
      let $1 = $string.split_once(cookie, ";");
      if ($1 instanceof Ok) {
        let cookie$1 = $1[0][0];
        let attributes = $1[0][1];
        let _block$2;
        let _pipe$1 = $string.split(attributes, ";");
        _block$2 = $list.map(_pipe$1, $string.trim);
        let attributes$1 = _block$2;
        return [cookie$1, attributes$1];
      } else {
        return [cookie, toList([])];
      }
    },
  );
  _block$1 = $list.filter_map(
    _pipe$1,
    (cookie) => {
      let _pipe$2 = $string.split_once(cookie[0], "=");
      return $result.map(
        _pipe$2,
        (split) => { return [split[0], split[1], cookie[1]]; },
      );
    },
  );
  let set_cookies = _block$1;
  return $list.fold(
    set_cookies,
    request$1,
    (request, cookie) => {
      let $1 = $list.contains(cookie[2], "Max-Age=0");
      if ($1) {
        return $request.remove_cookie(request, cookie[0]);
      } else {
        return $request.set_cookie(request, cookie[0], cookie[1]);
      }
    },
  );
}

/**
 * Set a cookie on the request.
 */
export function cookie(request, name, value, security) {
  let _block;
  if (security instanceof $wisp.PlainText) {
    _block = $bit_array.base64_encode(toBitArray([stringBits(value)]), false);
  } else {
    _block = $wisp.sign_message(
      request,
      toBitArray([stringBits(value)]),
      new $crypto.Sha512(),
    );
  }
  let value$1 = _block;
  return $request.set_cookie(request, name, value$1);
}

/**
 * The default secret key base used for test requests.
 * This should never be used outside of tests.
 */
export const default_secret_key_base = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";

/**
 * Add a text body to the request.
 * 
 * The `content-type` header is set to `text/plain`. You may want to override
 * this with `request.set_header`.
 */
export function string_body(request, text) {
  let _block;
  let _pipe = text;
  let _pipe$1 = $bit_array.from_string(_pipe);
  _block = $wisp.create_canned_connection(_pipe$1, default_secret_key_base);
  let body = _block;
  let _pipe$2 = request;
  let _pipe$3 = $request.set_body(_pipe$2, body);
  return $request.set_header(_pipe$3, "content-type", "text/plain");
}

/**
 * Add a binary body to the request.
 * 
 * The `content-type` header is set to `application/octet-stream`. You may
 * want to override/ this with `request.set_header`.
 */
export function bit_array_body(request, data) {
  let body = $wisp.create_canned_connection(data, default_secret_key_base);
  let _pipe = request;
  let _pipe$1 = $request.set_body(_pipe, body);
  return $request.set_header(
    _pipe$1,
    "content-type",
    "application/octet-stream",
  );
}

/**
 * Add HTML body to the request.
 * 
 * The `content-type` header is set to `text/html; charset=utf-8`.
 */
export function html_body(request, html) {
  let _block;
  let _pipe = html;
  let _pipe$1 = $bit_array.from_string(_pipe);
  _block = $wisp.create_canned_connection(_pipe$1, default_secret_key_base);
  let body = _block;
  let _pipe$2 = request;
  let _pipe$3 = $request.set_body(_pipe$2, body);
  return $request.set_header(
    _pipe$3,
    "content-type",
    "text/html; charset=utf-8",
  );
}

/**
 * Add a form data body to the request.
 * 
 * The `content-type` header is set to `application/x-www-form-urlencoded`.
 */
export function form_body(request, data) {
  let _block;
  let _pipe = $uri.query_to_string(data);
  let _pipe$1 = $bit_array.from_string(_pipe);
  _block = $wisp.create_canned_connection(_pipe$1, default_secret_key_base);
  let body = _block;
  let _pipe$2 = request;
  let _pipe$3 = $request.set_body(_pipe$2, body);
  return $request.set_header(
    _pipe$3,
    "content-type",
    "application/x-www-form-urlencoded",
  );
}

/**
 * Add a JSON body to the request.
 * 
 * The `content-type` header is set to `application/json`.
 */
export function json_body(request, data) {
  let _block;
  let _pipe = $json.to_string(data);
  let _pipe$1 = $bit_array.from_string(_pipe);
  _block = $wisp.create_canned_connection(_pipe$1, default_secret_key_base);
  let body = _block;
  let _pipe$2 = request;
  let _pipe$3 = $request.set_body(_pipe$2, body);
  return $request.set_header(_pipe$3, "content-type", "application/json");
}

/**
 * Add a multipart/form-data body to the request for testing file uploads
 * and form submissions.
 * 
 * The `content-type` header is set to `multipart/form-data` with an
 * appropriate boundary.
 * 
 * # Examples
 * 
 * ```gleam
 * let file = UploadedFile(
 *   file_name: "test.txt", 
 *   content_type: "text/plain",
 *   content: <<"Hello, world!":utf8>>
 * )
 * 
 * simulate.request(http.Post, "/upload")
 * |> simulate.multipart_body([#("user", "joe")], [#("file", file)])
 * ```
 */
export function multipart_body(request, values, files) {
  let _block;
  let _pipe = $crypto.strong_random_bytes(16);
  _block = $bit_array.base16_encode(_pipe);
  let boundary = _block;
  let body_data = build_multipart_body(values, files, boundary);
  let body = $wisp.create_canned_connection(body_data, default_secret_key_base);
  let _pipe$1 = request;
  let _pipe$2 = $request.set_body(_pipe$1, body);
  return $request.set_header(
    _pipe$2,
    "content-type",
    "multipart/form-data; boundary=" + boundary,
  );
}

/**
 * The default host for test requests.
 */
export const default_host = "wisp.example.com";

/**
 * The default headers for non-browser requests.
 */
export const default_headers = /* @__PURE__ */ toList([["host", default_host]]);

/**
 * Create a test request that can be used to test your request handler
 * functions.
 *
 * If you are testing handlers that are intended to be accessed from a browser
 * (such as those that use cookies) consider using `browser_request` instead.
 */
export function request(method, path) {
  let _block;
  let $1 = $string.split_once(path, "?");
  if ($1 instanceof Ok) {
    let path$1 = $1[0][0];
    let query = $1[0][1];
    _block = [path$1, new Some(query)];
  } else {
    _block = [path, new None()];
  }
  let $ = _block;
  let path$1;
  let query;
  path$1 = $[0];
  query = $[1];
  let connection = $wisp.create_canned_connection(
    toBitArray([]),
    default_secret_key_base,
  );
  return new $request.Request(
    method,
    default_headers,
    connection,
    new $http.Https(),
    default_host,
    new None(),
    path$1,
    query,
  );
}

/**
 * The default headers for browser requests.
 */
export const default_browser_headers = /* @__PURE__ */ toList([
  ["origin", "https://" + default_host],
  ["host", default_host],
]);

/**
 * Create a test request with browser-set headers that can be used to test
 * your request handler functions.
 *
 * The `origin` header is set when using this function.
 */
export function browser_request(method, path) {
  let _record = request(method, path);
  return new $request.Request(
    _record.method,
    default_browser_headers,
    _record.body,
    _record.scheme,
    _record.host,
    _record.port,
    _record.path,
    _record.query,
  );
}
