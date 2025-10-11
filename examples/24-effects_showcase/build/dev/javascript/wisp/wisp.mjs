import * as $exception from "../exception/exception.mjs";
import * as $filepath from "../filepath/filepath.mjs";
import * as $crypto from "../gleam_crypto/gleam/crypto.mjs";
import * as $application from "../gleam_erlang/gleam/erlang/application.mjs";
import * as $atom from "../gleam_erlang/gleam/erlang/atom.mjs";
import * as $http from "../gleam_http/gleam/http.mjs";
import * as $cookie from "../gleam_http/gleam/http/cookie.mjs";
import * as $request from "../gleam_http/gleam/http/request.mjs";
import * as $response from "../gleam_http/gleam/http/response.mjs";
import { Response as HttpResponse } from "../gleam_http/gleam/http/response.mjs";
import * as $json from "../gleam_json/gleam/json.mjs";
import * as $bit_array from "../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $bytes_tree from "../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $string_tree from "../gleam_stdlib/gleam/string_tree.mjs";
import * as $uri from "../gleam_stdlib/gleam/uri.mjs";
import * as $houdini from "../houdini/houdini.mjs";
import * as $logging from "../logging/logging.mjs";
import * as $marceau from "../marceau/marceau.mjs";
import * as $simplifile from "../simplifile/simplifile.mjs";
import {
  Ok,
  Error,
  toList,
  CustomType as $CustomType,
  makeError,
  isEqual,
  toBitArray,
  stringBits,
} from "./gleam.mjs";
import * as $internal from "./wisp/internal.mjs";

const FILEPATH = "src/wisp.gleam";

/**
 * A body of unicode text.
 *
 * If you have a `StringTree` you can use the `bytes_tree.from_string_tree`
 * function with the `Bytes` variant instead, as this will avoid the cost of
 * converting the tree into a string.
 */
export class Text extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * A body of binary data, stored as a `BytesTree`.
 *
 * If you have a `BitArray` you can use the `bytes_tree.from_bit_array`
 * function to convert it.
 *
 * If you have a `StringTree` you can use the `bytes_tree.from_string_tree`
 * function to convert it.
 */
export class Bytes extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * A body of the contents of a file.
 *
 * This will be sent efficiently using the `send_file` function of the
 * underlying HTTP server. The file will not be read into memory so it is
 * safe to send large files this way.
 */
export class File extends $CustomType {
  constructor(path, offset, limit) {
    super();
    this.path = path;
    this.offset = offset;
    this.limit = limit;
  }
}

class BufferedReader extends $CustomType {
  constructor(reader, buffer) {
    super();
    this.reader = reader;
    this.buffer = buffer;
  }
}

class Quotas extends $CustomType {
  constructor(body, files) {
    super();
    this.body = body;
    this.files = files;
  }
}

export class FormData extends $CustomType {
  constructor(values, files) {
    super();
    this.values = values;
    this.files = files;
  }
}

export class UploadedFile extends $CustomType {
  constructor(file_name, path) {
    super();
    this.file_name = file_name;
    this.path = path;
  }
}

class Errored extends $CustomType {}

class Thrown extends $CustomType {}

class Exited extends $CustomType {}

export class Range extends $CustomType {
  constructor(offset, limit) {
    super();
    this.offset = offset;
    this.limit = limit;
  }
}

export class EmergencyLevel extends $CustomType {}

export class AlertLevel extends $CustomType {}

export class CriticalLevel extends $CustomType {}

export class ErrorLevel extends $CustomType {}

export class WarningLevel extends $CustomType {}

export class NoticeLevel extends $CustomType {}

export class InfoLevel extends $CustomType {}

export class DebugLevel extends $CustomType {}

export class PlainText extends $CustomType {}

export class Signed extends $CustomType {}

/**
 * Create a response with the given status code.
 *
 * # Examples
 *
 * ```gleam
 * response(200)
 * // -> Response(200, [], Text(""))
 * ```
 */
export function response(status) {
  return new HttpResponse(status, toList([]), new Text(""));
}

/**
 * Set the body of a response.
 *
 * # Examples
 *
 * ```gleam
 * response(200)
 * |> set_body(File("/tmp/myfile.txt", option.None))
 * // -> Response(200, [], File("/tmp/myfile.txt", option.None))
 * ```
 */
export function set_body(response, body) {
  let _pipe = response;
  return $response.set_body(_pipe, body);
}

/**
 * Send a file from the disc as a file download.
 *
 * The operating system `send_file` function is used to efficiently send the
 * file over the network socket without reading the entire file into memory.
 *
 * The `content-disposition` header will be set to `attachment;
 * filename="name"` to ensure the file is downloaded by the browser. This is
 * especially good for files that the browser would otherwise attempt to open
 * as this can result in cross-site scripting vulnerabilities.
 *
 * If you wish to not set the `content-disposition` header you could use the
 * `set_body` function with the `File` body variant.
 *
 * # Examples
 *
 * ```gleam
 * response(200)
 * |> file_download(named: "myfile.txt", from: "/tmp/myfile.txt")
 * // -> Response(
 * //   200,
 * //   [#("content-disposition", "attachment; filename=\"myfile.txt\"")],
 * //   File("/tmp/myfile.txt", option.None),
 * // )
 * ```
 */
export function file_download(response, name, path) {
  let name$1 = $uri.percent_encode(name);
  let _pipe = response;
  let _pipe$1 = $response.set_header(
    _pipe,
    "content-disposition",
    ("attachment; filename=\"" + name$1) + "\"",
  );
  return $response.set_body(_pipe$1, new File(path, 0, new $option.None()));
}

/**
 * Send a file from memory as a file download.
 *
 * If your file is already on the disc use `file_download` instead, to avoid
 * having to read the file into memory to send it.
 *
 * The `content-disposition` header will be set to `attachment;
 * filename="name"` to ensure the file is downloaded by the browser. This is
 * especially good for files that the browser would otherwise attempt to open
 * as this can result in cross-site scripting vulnerabilities.
 *
 * # Examples
 *
 * ```gleam
 * let content = bytes_tree.from_string("Hello, Joe!")
 * response(200)
 * |> file_download_from_memory(named: "myfile.txt", containing: content)
 * // -> Response(
 * //   200,
 * //   [#("content-disposition", "attachment; filename=\"myfile.txt\"")],
 * //   File("/tmp/myfile.txt", option.None),
 * // )
 * ```
 */
export function file_download_from_memory(response, name, data) {
  let name$1 = $uri.percent_encode(name);
  let _pipe = response;
  let _pipe$1 = $response.set_header(
    _pipe,
    "content-disposition",
    ("attachment; filename=\"" + name$1) + "\"",
  );
  return $response.set_body(_pipe$1, new Bytes(data));
}

/**
 * Create a HTML response.
 *
 * The body is expected to be valid HTML, though this is not validated.
 * The `content-type` header will be set to `text/html; charset=utf-8`.
 *
 * # Examples
 *
 * ```gleam
 * html_response("<h1>Hello, Joe!</h1>", 200)
 * // -> Response(200, [#("content-type", "text/html; charset=utf-8")], Text(body))
 * ```
 */
export function html_response(html, status) {
  return new HttpResponse(
    status,
    toList([["content-type", "text/html; charset=utf-8"]]),
    new Text(html),
  );
}

/**
 * Create a JSON response.
 *
 * The body is expected to be valid JSON, though this is not validated.
 * The `content-type` header will be set to `application/json`.
 *
 * # Examples
 *
 * ```gleam
 * json_response("{\"name\": \"Joe\"}", 200)
 * // -> Response(200, [#("content-type", "application/json")], Text(body))
 * ```
 */
export function json_response(json, status) {
  return new HttpResponse(
    status,
    toList([["content-type", "application/json; charset=utf-8"]]),
    new Text(json),
  );
}

/**
 * Set the body of a response to a given HTML document, and set the
 * `content-type` header to `text/html`.
 *
 * The body is expected to be valid HTML, though this is not validated.
 *
 * # Examples
 *
 * ```gleam
 * response(201)
 * |> html_body("<h1>Hello, Joe!</h1>")
 * // -> Response(201, [#("content-type", "text/html; charset=utf-8")], Text(body))
 * ```
 */
export function html_body(response, html) {
  let _pipe = response;
  let _pipe$1 = $response.set_body(_pipe, new Text(html));
  return $response.set_header(
    _pipe$1,
    "content-type",
    "text/html; charset=utf-8",
  );
}

/**
 * Set the body of a response to a given JSON document, and set the
 * `content-type` header to `application/json`.
 *
 * The body is expected to be valid JSON, though this is not validated.
 *
 * # Examples
 *
 * ```gleam
 * response(201)
 * |> json_body("{\"name\": \"Joe\"}")
 * // -> Response(201, [#("content-type", "application/json; charset=utf-8")], Text(body))
 * ```
 */
export function json_body(response, json) {
  let _pipe = response;
  let _pipe$1 = $response.set_body(_pipe, new Text(json));
  return $response.set_header(
    _pipe$1,
    "content-type",
    "application/json; charset=utf-8",
  );
}

/**
 * Set the body of a response to a given string tree.
 *
 * You likely want to also set the request `content-type` header to an
 * appropriate value for the format of the content.
 *
 * # Examples
 *
 * ```gleam
 * let body = string_tree.from_string("Hello, Joe!")
 * response(201)
 * |> string_tree_body(body)
 * // -> Response(201, [], Text(body))
 * ```
 */
export function string_tree_body(response, content) {
  let _pipe = response;
  return $response.set_body(
    _pipe,
    new Bytes($bytes_tree.from_string_tree(content)),
  );
}

/**
 * Set the body of a response to a given string.
 *
 * You likely want to also set the request `content-type` header to an
 * appropriate value for the format of the content.
 *
 * # Examples
 *
 * ```gleam
 * let body =
 * response(201)
 * |> string_body("Hello, Joe!")
 * // -> Response(201, [], Text("Hello, Joe"))
 * ```
 */
export function string_body(response, content) {
  let _pipe = response;
  return $response.set_body(_pipe, new Text(content));
}

/**
 * Escape a string so that it can be safely included in a HTML document.
 *
 * Any content provided by the user should be escaped before being included in
 * a HTML document to prevent cross-site scripting attacks.
 *
 * # Examples
 *
 * ```gleam
 * escape_html("<h1>Hello, Joe!</h1>")
 * // -> "&lt;h1&gt;Hello, Joe!&lt;/h1&gt;"
 * ```
 */
export function escape_html(content) {
  return $houdini.escape(content);
}

/**
 * Create a response with status code 405: Method Not Allowed. Use this
 * when a request does not have an appropriate method to be handled.
 *
 * The `allow` header will be set to a comma separated list of the permitted
 * methods.
 *
 * # Examples
 *
 * ```gleam
 * method_not_allowed(allowed: [Get, Post])
 * // -> Response(405, [#("allow", "GET, POST")], Text("Method not allowed"))
 * ```
 */
export function method_not_allowed(methods) {
  let _block;
  let _pipe = methods;
  let _pipe$1 = $list.map(_pipe, $http.method_to_string);
  let _pipe$2 = $list.sort(_pipe$1, $string.compare);
  let _pipe$3 = $string.join(_pipe$2, ", ");
  _block = $string.uppercase(_pipe$3);
  let allowed = _block;
  return new HttpResponse(
    405,
    toList([["allow", allowed]]),
    new Text("Method not allowed"),
  );
}

/**
 * Create a response with status code 204: No content.
 *
 * # Examples
 *
 * ```gleam
 * no_content()
 * // -> Response(204, [], Text(""))
 * ```
 */
export function no_content() {
  return new HttpResponse(204, toList([]), new Text(""));
}

function buffered_read(reader, chunk_size) {
  let $ = reader.buffer;
  if ($.bitSize === 0) {
    return reader.reader(chunk_size);
  } else {
    return new Ok(new $internal.Chunk(reader.buffer, reader.reader));
  }
}

/**
 * Set the maximum permitted size of a request body of the request in bytes.
 *
 * If a body is larger than this size attempting to read the body will result
 * in a response with status code 413: Content too large will be returned to the
 * client.
 *
 * This limit only applies for headers and bodies that get read into memory.
 * Part of a multipart body that contain files and so are streamed to disc
 * instead use the `max_files_size` limit.
 */
export function set_max_body_size(request, size) {
  let _block;
  let _record = request.body;
  _block = new $internal.Connection(
    _record.reader,
    size,
    _record.max_files_size,
    _record.read_chunk_size,
    _record.secret_key_base,
    _record.temporary_directory,
  );
  let _pipe = _block;
  return ((_capture) => { return $request.set_body(request, _capture); })(_pipe);
}

/**
 * Get the maximum permitted size of a request body of the request in bytes.
 */
export function get_max_body_size(request) {
  return request.body.max_body_size;
}

/**
 * Set the secret key base used to sign cookies and other sensitive data.
 *
 * This key must be at least 64 bytes long and should be kept secret. Anyone
 * with this secret will be able to manipulate signed cookies and other sensitive
 * data.
 *
 * # Panics
 *
 * This function will panic if the key is less than 64 bytes long.
 */
export function set_secret_key_base(request, key) {
  let $ = $string.byte_size(key) < 64;
  if ($) {
    throw makeError(
      "panic",
      FILEPATH,
      "wisp",
      620,
      "set_secret_key_base",
      "Secret key base must be at least 64 bytes long",
      {}
    )
  } else {
    let _block;
    let _record = request.body;
    _block = new $internal.Connection(
      _record.reader,
      _record.max_body_size,
      _record.max_files_size,
      _record.read_chunk_size,
      key,
      _record.temporary_directory,
    );
    let _pipe = _block;
    return ((_capture) => { return $request.set_body(request, _capture); })(
      _pipe,
    );
  }
}

/**
 * Get the secret key base used to sign cookies and other sensitive data.
 */
export function get_secret_key_base(request) {
  return request.body.secret_key_base;
}

/**
 * Set the maximum permitted size of all files uploaded by a request, in bytes.
 *
 * If a request contains fails which are larger in total than this size
 * then attempting to read the body will result in a response with status code
 * 413: Content too large will be returned to the client.
 *
 * This limit only applies for files in a multipart body that get streamed to
 * disc. For headers and other content that gets read into memory use the
 * `max_body_size` limit.
 */
export function set_max_files_size(request, size) {
  let _block;
  let _record = request.body;
  _block = new $internal.Connection(
    _record.reader,
    _record.max_body_size,
    size,
    _record.read_chunk_size,
    _record.secret_key_base,
    _record.temporary_directory,
  );
  let _pipe = _block;
  return ((_capture) => { return $request.set_body(request, _capture); })(_pipe);
}

/**
 * Get the maximum permitted total size of a files uploaded by a request in
 * bytes.
 */
export function get_max_files_size(request) {
  return request.body.max_files_size;
}

/**
 * The the size limit for each chunk of the request body when read from the
 * client.
 *
 * This value is passed to the underlying web server when reading the body and
 * the exact size of chunks read depends on the server implementation. It most
 * likely will read chunks smaller than this size if not yet enough data has
 * been received from the client.
 */
export function set_read_chunk_size(request, size) {
  let _block;
  let _record = request.body;
  _block = new $internal.Connection(
    _record.reader,
    _record.max_body_size,
    _record.max_files_size,
    size,
    _record.secret_key_base,
    _record.temporary_directory,
  );
  let _pipe = _block;
  return ((_capture) => { return $request.set_body(request, _capture); })(_pipe);
}

/**
 * Get the size limit for each chunk of the request body when read from the
 * client.
 */
export function get_read_chunk_size(request) {
  return request.body.read_chunk_size;
}

/**
 * This middleware function ensures that the request has a specific HTTP
 * method, returning a response with status code 405: Method not allowed
 * if the method is not correct.
 *
 * # Examples
 *
 * ```gleam
 * fn handle_request(request: Request) -> Response {
 *   use <- wisp.require_method(request, http.Patch)
 *   // ...
 * }
 * ```
 */
export function require_method(request, method, next) {
  let $ = isEqual(request.method, method);
  if ($) {
    return next();
  } else {
    return method_not_allowed(toList([method]));
  }
}

/**
 * Parse the query parameters of a request into a list of key-value pairs. The
 * `key_find` function in the `gleam/list` stdlib module may be useful for
 * finding values in the list.
 *
 * Query parameter names do not have to be unique and so may appear multiple
 * times in the list.
 */
export function get_query(request) {
  let _pipe = $request.get_query(request);
  return $result.unwrap(_pipe, toList([]));
}

/**
 * This function overrides an incoming POST request with a method given in
 * the request's `_method` query paramerter. This is useful as web browsers
 * typically only support GET and POST requests, but our application may
 * expect other HTTP methods that are more semantically correct.
 *
 * The methods PUT, PATCH, and DELETE are accepted for overriding, all others
 * are ignored.
 *
 * The `_method` query paramerter can be specified in a HTML form like so:
 *
 * ```html
 * <form method="POST" action="/item/1?_method=DELETE">
 *   <button type="submit">Delete item</button>
 * </form>
 * ```
 *
 * # Examples
 *
 * ```gleam
 * fn handle_request(request: Request) -> Response {
 *   let request = wisp.method_override(request)
 *   // The method has now been overridden if appropriate
 * }
 * ```
 */
export function method_override(request) {
  return $bool.guard(
    !isEqual(request.method, new $http.Post()),
    request,
    () => {
      let _pipe = $result.try$(
        $request.get_query(request),
        (query) => {
          return $result.try$(
            $list.key_find(query, "_method"),
            (value) => {
              return $result.map(
                $http.parse_method(value),
                (method) => {
                  if (method instanceof $http.Put) {
                    return $request.set_method(request, method);
                  } else if (method instanceof $http.Delete) {
                    return $request.set_method(request, method);
                  } else if (method instanceof $http.Patch) {
                    return $request.set_method(request, method);
                  } else {
                    return request;
                  }
                },
              );
            },
          );
        },
      );
      return $result.unwrap(_pipe, request);
    },
  );
}

function read_body_loop(reader, read_chunk_size, max_body_size, accumulator) {
  return $result.try$(
    reader(read_chunk_size),
    (chunk) => {
      if (chunk instanceof $internal.Chunk) {
        let chunk$1 = chunk[0];
        let next = chunk.next;
        let accumulator$1 = $bit_array.append(accumulator, chunk$1);
        let $ = $bit_array.byte_size(accumulator$1) > max_body_size;
        if ($) {
          return new Error(undefined);
        } else {
          return read_body_loop(
            next,
            read_chunk_size,
            max_body_size,
            accumulator$1,
          );
        }
      } else {
        return new Ok(accumulator);
      }
    },
  );
}

/**
 * Read the entire body of the request as a bit array.
 *
 * You may instead wish to use the `require_bit_array_body` or the
 * `require_string_body` middleware functions instead.
 *
 * This function does not cache the body in any way, so if you call this
 * function (or any other body reading function) more than once it may hang or
 * return an incorrect value, depending on the underlying web server. It is the
 * responsibility of the caller to cache the body if it is needed multiple
 * times.
 *
 * If the body is larger than the `max_body_size` limit then a response
 * with status code 413: Content too large will be returned to the client.
 */
export function read_body_bits(request) {
  let connection = request.body;
  return read_body_loop(
    connection.reader,
    connection.read_chunk_size,
    connection.max_body_size,
    toBitArray([]),
  );
}

function sort_keys(pairs) {
  return $list.sort(pairs, (a, b) => { return $string.compare(a[0], b[0]); });
}

/**
 * Parses the content of a range header.
 *
 * # Example
 *
 * ```gleam
 * wisp.parse_range_header("bytes=-64")
 * // -> Ok(Range(offset: -64, limit: option.None))
 * ```
 */
export function parse_range_header(range_header) {
  if (range_header.startsWith("bytes=")) {
    let range = range_header.slice(6);
    return $result.try$(
      (() => {
        let _pipe = range;
        return $string.split_once(_pipe, "-");
      })(),
      (_use0) => {
        let start_str;
        let end_str;
        start_str = _use0[0];
        end_str = _use0[1];
        if (start_str === "") {
          let _pipe = $int.parse(end_str);
          return $result.map(
            _pipe,
            (tail_offset) => {
              return new Range(- tail_offset, new $option.None());
            },
          );
        } else if (end_str === "") {
          let _pipe = $int.parse(start_str);
          return $result.map(
            _pipe,
            (offset) => { return new Range(offset, new $option.None()); },
          );
        } else {
          return $result.try$(
            $int.parse(start_str),
            (offset) => {
              return $result.try$(
                $int.parse(end_str),
                (end) => {
                  return new Ok(
                    new Range(offset, new $option.Some((end - offset) + 1)),
                  );
                },
              );
            },
          );
        }
      },
    );
  } else {
    return new Error(undefined);
  }
}

/**
 * A middleware function that converts `HEAD` requests to `GET` requests,
 * handles the request, and then discards the response body. This is useful so
 * that your application can handle `HEAD` requests without having to implement
 * handlers for them.
 *
 * The `x-original-method` header is set to `"HEAD"` for requests that were
 * originally `HEAD` requests.
 *
 * # Examples
 *
 * ```gleam
 * fn handle_request(req: Request) -> Response {
 *   use req <- wisp.handle_head(req)
 *   // ...
 * }
 * ```
 */
export function handle_head(req, handler) {
  let $ = req.method;
  if ($ instanceof $http.Head) {
    let _pipe = req;
    let _pipe$1 = $request.set_method(_pipe, new $http.Get());
    let _pipe$2 = $request.prepend_header(_pipe$1, "x-original-method", "HEAD");
    return handler(_pipe$2);
  } else {
    return handler(req);
  }
}

/**
 * Create a new temporary directory for the given request.
 *
 * If you are using the Mist adapter or another compliant web server
 * adapter then this file will be deleted for you when the request is complete.
 * Otherwise you will need to call the `delete_temporary_files` function
 * yourself.
 */
export function new_temporary_file(request) {
  let directory = request.body.temporary_directory;
  return $result.try$(
    $simplifile.create_directory_all(directory),
    (_) => {
      let path = $filepath.join(directory, $internal.random_slug());
      return $result.map($simplifile.create_file(path), (_) => { return path; });
    },
  );
}

/**
 * Delete any temporary files created for the given request.
 *
 * If you are using the Mist adapter or another compliant web server
 * adapter then this file will be deleted for you when the request is complete.
 * Otherwise you will need to call this function yourself.
 */
export function delete_temporary_files(request) {
  let $ = $simplifile.delete$(request.body.temporary_directory);
  if ($ instanceof Error) {
    let $1 = $[0];
    if ($1 instanceof $simplifile.Enoent) {
      return new Ok(undefined);
    } else {
      return $;
    }
  } else {
    return $;
  }
}

function log_level_to_logging_log_level(log_level) {
  if (log_level instanceof EmergencyLevel) {
    return new $logging.Emergency();
  } else if (log_level instanceof AlertLevel) {
    return new $logging.Alert();
  } else if (log_level instanceof CriticalLevel) {
    return new $logging.Critical();
  } else if (log_level instanceof ErrorLevel) {
    return new $logging.Error();
  } else if (log_level instanceof WarningLevel) {
    return new $logging.Warning();
  } else if (log_level instanceof NoticeLevel) {
    return new $logging.Notice();
  } else if (log_level instanceof InfoLevel) {
    return new $logging.Info();
  } else {
    return new $logging.Debug();
  }
}

/**
 * Generate a random string of the given length.
 */
export function random_string(length) {
  return $internal.random_string(length);
}

/**
 * Sign a message which can later be verified using the `verify_signed_message`
 * function to detect if the message has been tampered with.
 *
 * Signed messages are not encrypted and can be read by anyone. They are not
 * suitable for storing sensitive information.
 *
 * This function uses the secret key base from the request. If the secret
 * changes then the signature will no longer be verifiable.
 */
export function sign_message(request, message, algorithm) {
  return $crypto.sign_message(
    message,
    toBitArray([stringBits(request.body.secret_key_base)]),
    algorithm,
  );
}

/**
 * Verify a signed message which was signed using the `sign_message` function.
 *
 * Returns the content of the message if the signature is valid, otherwise
 * returns an error.
 *
 * This function uses the secret key base from the request. If the secret
 * changes then the signature will no longer be verifiable.
 */
export function verify_signed_message(request, message) {
  return $crypto.verify_signed_message(
    message,
    toBitArray([stringBits(request.body.secret_key_base)]),
  );
}

/**
 * Set a cookie on the response. After `max_age` seconds the cookie will be
 * expired by the client.
 *
 * If you wish for more control over the cookie attributes then you may want
 * to use the `gleam/http/cookie` module from the `gleam_http` package.
 *
 * # Security
 *
 * - `PlainText`: the cookie value is base64 encoded. This permits use of any
 *    characters in the cookie, but it is possible for the client to edit the
 *    cookie, potentially maliciously.
 * - `Signed`: the cookie value will be signed with `sign_message` and so
 *    cannot be tampered with by the client.
 *
 * # `Secure` cookie attribute
 *
 * This function sets the `Secure` cookie attribute (with one exception detailed
 * below), ensuring that browsers will only send the cookie over HTTPS
 * connections.
 *
 * Most browsers consider localhost to be secure and permit `Secure` cookies
 * for those requests as well, but Safari does not. For cookies to work in
 * development for programmers using a browser like Safari the `Secure`
 * attribute will not be set if all these conditions are met:
 *
 * - The request scheme is `http://`.
 * - The request host is `localhost`, `127.0.0.1`, or `[::1]`.
 * - The `x-forwarded-proto` header has not been set, indicating that the
 *   request is not from a reverse proxy such as Caddy or Nginx.
 *
 * # Examples
 *
 * Setting a plain text cookie that the client can read and modify:
 *
 * ```gleam
 * wisp.ok()
 * |> wisp.set_cookie(request, "id", "123", wisp.PlainText, 60 * 60)
 * ```
 *
 * Setting a signed cookie that the client can read but not modify:
 *
 * ```gleam
 * wisp.ok()
 * |> wisp.set_cookie(request, "id", value, wisp.Signed, 60 * 60)
 * ```
 */
export function set_cookie(response, request, name, value, security, max_age) {
  let _block;
  let $ = request.host;
  if ($ === "localhost" && isEqual(request.scheme, new $http.Http())) {
    let $1 = $request.get_header(request, "x-forwarded-proto");
    if ($1 instanceof Ok) {
      _block = new $http.Https();
    } else {
      _block = new $http.Http();
    }
  } else if ($ === "127.0.0.1" && isEqual(request.scheme, new $http.Http())) {
    let $1 = $request.get_header(request, "x-forwarded-proto");
    if ($1 instanceof Ok) {
      _block = new $http.Https();
    } else {
      _block = new $http.Http();
    }
  } else if ($ === "[::1]" && isEqual(request.scheme, new $http.Http())) {
    let $1 = $request.get_header(request, "x-forwarded-proto");
    if ($1 instanceof Ok) {
      _block = new $http.Https();
    } else {
      _block = new $http.Http();
    }
  } else {
    _block = new $http.Https();
  }
  let scheme = _block;
  let _block$1;
  let _record = $cookie.defaults(scheme);
  _block$1 = new $cookie.Attributes(
    new $option.Some(max_age),
    _record.domain,
    _record.path,
    _record.secure,
    _record.http_only,
    _record.same_site,
  );
  let attributes = _block$1;
  let _block$2;
  if (security instanceof PlainText) {
    _block$2 = $bit_array.base64_encode(toBitArray([stringBits(value)]), false);
  } else {
    _block$2 = sign_message(
      request,
      toBitArray([stringBits(value)]),
      new $crypto.Sha512(),
    );
  }
  let value$1 = _block$2;
  let _pipe = response;
  return $response.set_cookie(_pipe, name, value$1, attributes);
}

/**
 * Get a cookie from the request.
 *
 * If a cookie is missing, found to be malformed, or the signature is invalid
 * for a signed cookie, then `Error(Nil)` is returned.
 *
 * # Security
 *
 * - `PlainText`: the cookie value is expected to be base64 encoded.
 * - `Signed`: the cookie value is expected to be signed with `sign_message`.
 *
 * # Examples
 *
 * ```gleam
 * wisp.get_cookie(request, "group", wisp.PlainText)
 * // -> Ok("A")
 * ```
 */
export function get_cookie(request, name, security) {
  return $result.try$(
    (() => {
      let _pipe = request;
      let _pipe$1 = $request.get_cookies(_pipe);
      return $list.key_find(_pipe$1, name);
    })(),
    (value) => {
      return $result.try$(
        (() => {
          if (security instanceof PlainText) {
            return $bit_array.base64_decode(value);
          } else {
            return verify_signed_message(request, value);
          }
        })(),
        (value) => { return $bit_array.to_string(value); },
      );
    },
  );
}

export function create_canned_connection(body, secret_key_base) {
  return $internal.make_connection(
    (_) => {
      return new Ok(
        new $internal.Chunk(
          body,
          (_) => { return new Ok(new $internal.ReadingFinished()); },
        ),
      );
    },
    secret_key_base,
  );
}

const content_text = ["content-type", "text/plain"];

/**
 * Create a response with status code 200: OK.
 *
 * # Examples
 *
 * ```gleam
 * ok()
 * // -> Response(200, [#("content-type", "text/plain")], Text("OK"))
 * ```
 */
export function ok() {
  return new HttpResponse(200, toList([content_text]), new Text("OK"));
}

/**
 * Create a response with status code 201: Created.
 *
 * # Examples
 *
 * ```gleam
 * created()
 * // -> Response(201, [#("content-type", "text/plain")], Text("Created"))
 * ```
 */
export function created() {
  return new HttpResponse(201, toList([content_text]), new Text("Created"));
}

/**
 * Create a response with status code 202: Accepted.
 *
 * # Examples
 *
 * ```gleam
 * accepted()
 * // -> Response(202, [#("content-type", "text/plain")], Text("Accepted"))
 * ```
 */
export function accepted() {
  return new HttpResponse(202, toList([content_text]), new Text("Accepted"));
}

/**
 * Create a response with status code 303: See Other, and the `location`
 * header set to the given URL. Used to redirect the client to another page.
 *
 * # Examples
 *
 * ```gleam
 * redirect(to: "https://example.com")
 * // -> Response(
 * //   303,
 * //   [#("location", "https://example.com"), #("context-type", "text/plain")],
 * //   Text("You are being redirected: https://example.com"),
 * // )
 * ```
 */
export function redirect(url) {
  return new HttpResponse(
    303,
    toList([["location", url], content_text]),
    new Text("You are being redirected: " + url),
  );
}

/**
 * Create a response with status code 308: Permanent redirect, and the
 * `location` header set to the given URL. Used to redirect the client to
 * another page.
 *
 * This redirect is permanent and the client is expected to cache the new
 * location, using it for future requests.
 *
 * # Examples
 *
 * ```gleam
 * moved_permanently(to: "https://example.com")
 * // -> Response(
 * //   303,
 * //   [#("location", "https://example.com")],
 * //   Text("You are being redirected: https://example.com"),
 * // )
 * ```
 */
export function permanent_redirect(url) {
  return new HttpResponse(
    308,
    toList([["location", url], content_text]),
    new Text("You are being redirected: " + url),
  );
}

/**
 * Create a response with status code 404: Not found.
 *
 * # Examples
 *
 * ```gleam
 * not_found()
 * // -> Response(404, [#("content-type", "text/plain")], Text("Not found"))
 * ```
 */
export function not_found() {
  return new HttpResponse(404, toList([content_text]), new Text("Not found"));
}

/**
 * Create a response with status code 400: Bad request.
 *
 * # Examples
 *
 * ```gleam
 * bad_request("Invalid JSON")
 * // -> Response(400, [#("content-type", "text/plain")], Text("Bad request: Invalid JSON"))
 * ```
 */
export function bad_request(detail) {
  let _block;
  if (detail === "") {
    _block = "Bad request";
  } else {
    _block = "Bad request: " + detail;
  }
  let body = _block;
  return new HttpResponse(400, toList([content_text]), new Text(body));
}

/**
 * Create a response with status code 413: Content too large.
 *
 * # Examples
 *
 * ```gleam
 * content_too_large()
 * // -> Response(413, [#("content-type", "text/plain")], Text("Content too large"))
 * ```
 */
export function content_too_large() {
  return new HttpResponse(
    413,
    toList([content_text]),
    new Text("Content too large"),
  );
}

/**
 * Create a response with status code 415: Unsupported media type.
 *
 * The `allow` header will be set to a comma separated list of the permitted
 * content-types.
 *
 * # Examples
 *
 * ```gleam
 * unsupported_media_type(accept: ["application/json", "text/plain"])
 * // -> Response(415, [#("allow", "application/json, text/plain")], Text("Unsupported media type"))
 * ```
 */
export function unsupported_media_type(acceptable) {
  let acceptable$1 = $string.join(acceptable, ", ");
  return new HttpResponse(
    415,
    toList([["accept", acceptable$1], content_text]),
    new Text("Unsupported media type"),
  );
}

/**
 * Create a response with status code 422: Unprocessable content.
 *
 * # Examples
 *
 * ```gleam
 * unprocessable_content()
 * // -> Response(422, [#("content-type", "text/plain")], Text("Unprocessable content"))
 * ```
 */
export function unprocessable_content() {
  return new HttpResponse(
    422,
    toList([content_text]),
    new Text("Unprocessable content"),
  );
}

/**
 * Create a response with status code 500: Internal server error.
 *
 * # Examples
 *
 * ```gleam
 * internal_server_error()
 * // -> Response(500, [#("content-type", "text/plain")], Text("Internal server error"))
 * ```
 */
export function internal_server_error() {
  return new HttpResponse(
    500,
    toList([content_text]),
    new Text("Internal server error"),
  );
}

function decrement_body_quota(quotas, size) {
  let quotas$1 = new Quotas(quotas.body - size, quotas.files);
  let $ = quotas$1.body < 0;
  if ($) {
    return new Error(content_too_large());
  } else {
    return new Ok(quotas$1);
  }
}

function decrement_quota(quota, size) {
  let $ = quota - size;
  let quota$1 = $;
  if (quota$1 < 0) {
    return new Error(content_too_large());
  } else {
    let quota$2 = $;
    return new Ok(quota$2);
  }
}

/**
 * A middleware function which reads the entire body of the request as a bit
 * string.
 *
 * This function does not cache the body in any way, so if you call this
 * function (or any other body reading function) more than once it may hang or
 * return an incorrect value, depending on the underlying web server. It is the
 * responsibility of the caller to cache the body if it is needed multiple
 * times.
 *
 * If the body is larger than the `max_body_size` limit then a response
 * with status code 413: Content too large will be returned to the client.
 *
 * # Examples
 *
 * ```gleam
 * fn handle_request(request: Request) -> Response {
 *   use body <- wisp.require_string_body(request)
 *   // ...
 * }
 * ```
 */
export function require_bit_array_body(request, next) {
  let $ = read_body_bits(request);
  if ($ instanceof Ok) {
    let body = $[0];
    return next(body);
  } else {
    return content_too_large();
  }
}

/**
 * This middleware function ensures that the request has a value for the
 * `content-type` header, returning a response with status code 415:
 * Unsupported media type if the header is not the expected value
 *
 * # Examples
 *
 * ```gleam
 * fn handle_request(request: Request) -> Response {
 *   use <- wisp.require_content_type(request, "application/json")
 *   // ...
 * }
 * ```
 */
export function require_content_type(request, expected, next) {
  let $ = $list.key_find(request.headers, "content-type");
  if ($ instanceof Ok) {
    let content_type = $[0];
    let $1 = $string.split_once(content_type, ";");
    if ($1 instanceof Ok) {
      let content_type$1 = $1[0][0];
      if (content_type$1 === expected) {
        return next();
      } else if (content_type$1 === expected) {
        return next();
      } else {
        return unsupported_media_type(toList([expected]));
      }
    } else if (content_type === expected) {
      return next();
    } else {
      return unsupported_media_type(toList([expected]));
    }
  } else {
    return unsupported_media_type(toList([expected]));
  }
}

function fn_with_bad_request_error(f, error) {
  return (a) => {
    let $ = f(a);
    if ($ instanceof Ok) {
      return $;
    } else {
      return new Error(bad_request(error));
    }
  };
}

const invalid_json = "Invalid JSON";

const invalid_form = "Invalid form encoding";

const invalid_utf8 = "Invalid UTF-8";

/**
 * A middleware function which reads the entire body of the request as a string.
 *
 * This function does not cache the body in any way, so if you call this
 * function (or any other body reading function) more than once it may hang or
 * return an incorrect value, depending on the underlying web server. It is the
 * responsibility of the caller to cache the body if it is needed multiple
 * times.
 *
 * If the body is larger than the `max_body_size` limit then a response
 * with status code 413: Content too large will be returned to the client.
 *
 * If the body is found not to be valid UTF-8 then a response with
 * status code 400: Bad request will be returned to the client.
 *
 * # Examples
 *
 * ```gleam
 * fn handle_request(request: Request) -> Response {
 *   use body <- wisp.require_string_body(request)
 *   // ...
 * }
 * ```
 */
export function require_string_body(request, next) {
  let $ = read_body_bits(request);
  if ($ instanceof Ok) {
    let body = $[0];
    let $1 = $bit_array.to_string(body);
    if ($1 instanceof Ok) {
      let body$1 = $1[0];
      return next(body$1);
    } else {
      return bad_request(invalid_utf8);
    }
  } else {
    return content_too_large();
  }
}

/**
 * A middleware which extracts JSON from the body of a request.
 *
 * ```gleam
 * fn handle_request(request: Request) -> Response {
 *   use json <- wisp.require_json(request)
 *   // decode and use JSON here...
 * }
 * ```
 *
 * The `set_max_body_size` and `set_read_chunk_size` can be used to configure
 * the reading of the request body.
 *
 * If the request does not have the `content-type` set to `application/json` a
 * response with status code 415: Unsupported media type will be returned
 * to the client.
 *
 * If the request body is larger than the `max_body_size` or `max_files_size`
 * limits then a response with status code 413: Content too large will be
 * returned to the client.
 *
 * If the body cannot be parsed successfully then a response with status
 * code 400: Bad request will be returned to the client.
 */
export function require_json(request, next) {
  return require_content_type(
    request,
    "application/json",
    () => {
      return require_string_body(
        request,
        (body) => {
          let $ = $json.parse(body, $decode.dynamic);
          if ($ instanceof Ok) {
            let json = $[0];
            return next(json);
          } else {
            return bad_request(invalid_json);
          }
        },
      );
    },
  );
}

function require_urlencoded_form(request, next) {
  return require_string_body(
    request,
    (body) => {
      let $ = $uri.parse_query(body);
      if ($ instanceof Ok) {
        let pairs = $[0];
        let pairs$1 = sort_keys(pairs);
        return next(new FormData(pairs$1, toList([])));
      } else {
        return bad_request(invalid_form);
      }
    },
  );
}

const invalid_range = "Invalid range";

/**
 * Checks for the `range` header and handles partial file reads.
 *
 * If the range request header is present, it will set the `accept-ranges`,
 * `content-range`, and `content-length` response headers. If the range
 * request header has a range that is out of bounds of the file, it will
 * respond with a `416 Range Not Satisfiable`.
 *
 * If the header isn't present, it returns the input response without changes.
 * 
 * @ignore
 */
function handle_file_range_header(resp, req, file_info, path) {
  let result = $result.try$(
    (() => {
      let _pipe = $request.get_header(req, "range");
      return $result.replace_error(_pipe, resp);
    })(),
    (raw_range) => {
      return $result.try$(
        (() => {
          let _pipe = parse_range_header(raw_range);
          return $result.replace_error(_pipe, bad_request(invalid_range));
        })(),
        (range) => {
          let _block;
          let $ = range.offset < 0;
          if ($) {
            _block = new Range(file_info.size + range.offset, range.limit);
          } else {
            _block = range;
          }
          let range$1 = _block;
          let _block$1;
          let _pipe = range$1.limit;
          let _pipe$1 = $option.map(
            _pipe,
            (end) => {
              return ((end < 0) || (end >= file_info.size)) || (end < range$1.offset);
            },
          );
          _block$1 = $option.unwrap(_pipe$1, false);
          let end_is_invalid = _block$1;
          return $bool.guard(
            ((range$1.offset < 0) || (range$1.offset >= file_info.size)) || end_is_invalid,
            new Error(
              (() => {
                let _pipe$2 = response(416);
                return $response.prepend_header(_pipe$2, "range", "bytes=*");
              })(),
            ),
            () => {
              let _block$2;
              {
                let _block$3;
                let $1 = range$1.limit;
                if ($1 instanceof $option.Some) {
                  let l = $1[0];
                  let _pipe$2 = ((range$1.offset + l) - 1);
                  _block$3 = $int.to_string(_pipe$2);
                } else {
                  let _pipe$2 = (file_info.size - 1);
                  let _pipe$3 = $int.max(_pipe$2, 0);
                  _block$3 = $int.to_string(_pipe$3);
                }
                let end = _block$3;
                _block$2 = (((("bytes " + $int.to_string(range$1.offset)) + "-") + end) + "/") + $int.to_string(
                  file_info.size,
                );
              }
              let content_range = _block$2;
              let _block$3;
              let $1 = range$1.limit;
              if ($1 instanceof $option.Some) {
                let l = $1[0];
                _block$3 = $int.to_string(l);
              } else {
                _block$3 = $int.to_string(file_info.size - range$1.offset);
              }
              let content_length = _block$3;
              let _pipe$2 = new $response.Response(
                206,
                resp.headers,
                new File(path, range$1.offset, range$1.limit),
              );
              let _pipe$3 = $response.set_header(
                _pipe$2,
                "content-length",
                content_length,
              );
              let _pipe$4 = $response.set_header(
                _pipe$3,
                "accept-ranges",
                "bytes",
              );
              let _pipe$5 = $response.set_header(
                _pipe$4,
                "content-range",
                content_range,
              );
              return new Ok(_pipe$5);
            },
          );
        },
      );
    },
  );
  if (result instanceof Ok) {
    let response$1 = result[0];
    return response$1;
  } else {
    let response$1 = result[0];
    return response$1;
  }
}

const invalid_content_disposition = "Invalid content-disposition";

function multipart_content_disposition(headers) {
  let _pipe = $result.try$(
    $list.key_find(headers, "content-disposition"),
    (header) => {
      return $result.try$(
        $http.parse_content_disposition(header),
        (header) => {
          return $result.map(
            $list.key_find(header.parameters, "name"),
            (name) => {
              let filename = $option.from_result(
                $list.key_find(header.parameters, "filename"),
              );
              return [name, filename];
            },
          );
        },
      );
    },
  );
  return $result.replace_error(_pipe, bad_request(invalid_content_disposition));
}

const invalid_host = "Invalid host";

const invalid_origin = "Invalid origin";

const unexpected_end = "Unexpected end of request body";

function read_chunk(reader, chunk_size) {
  let $ = buffered_read(reader, chunk_size);
  if ($ instanceof Ok) {
    let chunk = $[0];
    if (chunk instanceof $internal.Chunk) {
      let chunk$1 = chunk[0];
      let next = chunk.next;
      return new Ok([chunk$1, next]);
    } else {
      return new Error(bad_request(unexpected_end));
    }
  } else {
    return new Error(bad_request(unexpected_end));
  }
}

function multipart_body(
  reader,
  parse,
  boundary,
  chunk_size,
  quota,
  append,
  data
) {
  return $result.try$(
    read_chunk(reader, chunk_size),
    (_use0) => {
      let chunk;
      let reader$1;
      chunk = _use0[0];
      reader$1 = _use0[1];
      let size_read = $bit_array.byte_size(chunk);
      return $result.try$(
        parse(chunk),
        (output) => {
          if (output instanceof $http.MultipartBody) {
            let parsed = output.chunk;
            let done = output.done;
            let remaining = output.remaining;
            let used = (size_read - $bit_array.byte_size(remaining)) - 2;
            let _block;
            if (done) {
              _block = (used - 4) - $string.byte_size(boundary);
            } else {
              _block = used;
            }
            let used$1 = _block;
            return $result.try$(
              decrement_quota(quota, used$1),
              (quota) => {
                let reader$2 = new BufferedReader(reader$1, remaining);
                let _block$1;
                if (done) {
                  _block$1 = new $option.None();
                } else {
                  _block$1 = new $option.Some(reader$2);
                }
                let reader$3 = _block$1;
                return $result.map(
                  append(data, parsed),
                  (value) => { return [reader$3, quota, value]; },
                );
              },
            );
          } else {
            let chunk$1 = output.chunk;
            let parse$1 = output.continuation;
            let parse$2 = fn_with_bad_request_error(parse$1, invalid_form);
            let reader$2 = new BufferedReader(reader$1, toBitArray([]));
            return $result.try$(
              append(data, chunk$1),
              (data) => {
                return multipart_body(
                  reader$2,
                  parse$2,
                  boundary,
                  chunk_size,
                  quota,
                  append,
                  data,
                );
              },
            );
          }
        },
      );
    },
  );
}

function multipart_headers(reader, parse, chunk_size, quotas) {
  return $result.try$(
    read_chunk(reader, chunk_size),
    (_use0) => {
      let chunk;
      let reader$1;
      chunk = _use0[0];
      reader$1 = _use0[1];
      return $result.try$(
        parse(chunk),
        (headers) => {
          if (headers instanceof $http.MultipartHeaders) {
            let headers$1 = headers.headers;
            let remaining = headers.remaining;
            let used = $bit_array.byte_size(chunk) - $bit_array.byte_size(
              remaining,
            );
            return $result.map(
              decrement_body_quota(quotas, used),
              (quotas) => {
                let reader$2 = new BufferedReader(reader$1, remaining);
                return [headers$1, reader$2, quotas];
              },
            );
          } else {
            let parse$1 = headers.continuation;
            let parse$2 = (chunk) => {
              let $ = parse$1(chunk);
              if ($ instanceof Ok) {
                return $;
              } else {
                return new Error(bad_request(invalid_form));
              }
            };
            let reader$2 = new BufferedReader(reader$1, toBitArray([]));
            return multipart_headers(reader$2, parse$2, chunk_size, quotas);
          }
        },
      );
    },
  );
}

/**
 * Return the non-empty segments of a request path.
 *
 * # Examples
 *
 * ```gleam
 * > request.new()
 * > |> request.set_path("/one/two/three")
 * > |> wisp.path_segments
 * ["one", "two", "three"]
 * ```
 */
export const path_segments = $request.path_segments;

/**
 * Set a given header to a given value, replacing any existing value.
 *
 * # Examples
 *
 * ```gleam
 * > wisp.ok()
 * > |> wisp.set_header("content-type", "application/json")
 * Request(200, [#("content-type", "application/json")], Text("OK"))
 * ```
 */
export const set_header = $response.set_header;

/**
 * Calculates etag for requested file and then checks for the request header `if-none-match`.
 *
 * If the header isn't present or the value doesn't match the newly generated etag, it returns the file with the newly generated etag.
 * Otherwise if the etag matches, it returns status 304 without the file, allowing the browser to use the cached version.
 * 
 * @ignore
 */
function handle_etag(resp, req, file_info) {
  let etag = $internal.generate_etag(file_info.size, file_info.mtime_seconds);
  let $ = $request.get_header(req, "if-none-match");
  if ($ instanceof Ok) {
    let old_etag = $[0];
    if (old_etag === etag) {
      let _pipe = response(304);
      return set_header(_pipe, "etag", etag);
    } else {
      return $response.set_header(resp, "etag", etag);
    }
  } else {
    return $response.set_header(resp, "etag", etag);
  }
}

/**
 * A middleware function that serves files from a directory, along with a
 * suitable `content-type` header for known file extensions.
 *
 * Files are sent using the `File` response body type, so they will be sent
 * directly to the client from the disc, without being read into memory.
 *
 * The `under` parameter is the request path prefix that must match for the
 * file to be served.
 *
 * | `under`   | `from`  | `request.path`     | `file`                  |
 * |-----------|---------|--------------------|-------------------------|
 * | `/static` | `/data` | `/static/file.txt` | `/data/file.txt`        |
 * | ``        | `/data` | `/static/file.txt` | `/data/static/file.txt` |
 * | `/static` | ``      | `/static/file.txt` | `file.txt`              |
 *
 * This middleware will discard any `..` path segments in the request path to
 * prevent the client from accessing files outside of the directory. It is
 * advised not to serve a directory that contains your source code, application
 * configuration, database, or other private files.
 *
 * # Examples
 *
 * ```gleam
 * fn handle_request(req: Request) -> Response {
 *   use <- wisp.serve_static(req, under: "/static", from: "/public")
 *   // ...
 * }
 * ```
 *
 * Typically you static assets may be kept in your project in a directory
 * called `priv`. The `priv_directory` function can be used to get a path to
 * this directory.
 *
 * ```gleam
 * fn handle_request(req: Request) -> Response {
 *   let assert Ok(priv) = priv_directory("my_application")
 *   use <- wisp.serve_static(req, under: "/static", from: priv)
 *   // ...
 * }
 * ```
 */
export function serve_static(req, prefix, directory, handler) {
  let path = $internal.remove_preceeding_slashes(req.path);
  let prefix$1 = $internal.remove_preceeding_slashes(prefix);
  let $ = req.method;
  let $1 = $string.starts_with(path, prefix$1);
  if ($1 && $ instanceof $http.Get) {
    let _block;
    let _pipe = path;
    let _pipe$1 = $string.drop_start(_pipe, $string.length(prefix$1));
    let _pipe$2 = $string.replace(_pipe$1, "..", "");
    _block = ((_capture) => { return $filepath.join(directory, _capture); })(
      _pipe$2,
    );
    let path$1 = _block;
    let _block$1;
    let _pipe$3 = req.path;
    let _pipe$4 = $string.split(_pipe$3, ".");
    let _pipe$5 = $list.last(_pipe$4);
    _block$1 = $result.unwrap(_pipe$5, "");
    let file_type = _block$1;
    let mime_type = $marceau.extension_to_mime_type(file_type);
    let _block$2;
    if (mime_type === "application/json") {
      _block$2 = mime_type + "; charset=utf-8";
    } else if (mime_type.startsWith("text/")) {
      _block$2 = mime_type + "; charset=utf-8";
    } else {
      _block$2 = mime_type;
    }
    let content_type = _block$2;
    let $2 = $simplifile.file_info(path$1);
    if ($2 instanceof Ok) {
      let file_info = $2[0];
      let $3 = $simplifile.file_info_type(file_info);
      if ($3 instanceof $simplifile.File) {
        let _pipe$6 = $response.new$(200);
        let _pipe$7 = $response.set_header(
          _pipe$6,
          "content-type",
          content_type,
        );
        let _pipe$8 = $response.set_body(
          _pipe$7,
          new File(path$1, 0, new $option.None()),
        );
        let _pipe$9 = handle_etag(_pipe$8, req, file_info);
        return handle_file_range_header(_pipe$9, req, file_info, path$1);
      } else {
        return handler();
      }
    } else {
      return handler();
    }
  } else {
    return handler();
  }
}

/**
 * Returns the path of a package's `priv` directory, where extra non-Gleam
 * or Erlang files are typically kept.
 *
 * Returns an error if no package was found with the given name.
 *
 * # Example
 *
 * ```gleam
 * > erlang.priv_directory("my_app")
 * // -> Ok("/some/location/my_app/priv")
 * ```
 */
export const priv_directory = $application.priv_directory;
