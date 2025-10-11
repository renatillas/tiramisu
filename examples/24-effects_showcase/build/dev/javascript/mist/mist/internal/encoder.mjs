import * as $http from "../../../gleam_http/gleam/http.mjs";
import * as $response from "../../../gleam_http/gleam/http/response.mjs";
import * as $bytes_tree from "../../../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import { toBitArray, stringBits } from "../../gleam.mjs";

export function status_to_bit_array(status) {
  if (status === 100) {
    return toBitArray([stringBits("Continue")]);
  } else if (status === 101) {
    return toBitArray([stringBits("Switching Protocols")]);
  } else if (status === 103) {
    return toBitArray([stringBits("Early Hints")]);
  } else if (status === 200) {
    return toBitArray([stringBits("OK")]);
  } else if (status === 201) {
    return toBitArray([stringBits("Created")]);
  } else if (status === 202) {
    return toBitArray([stringBits("Accepted")]);
  } else if (status === 203) {
    return toBitArray([stringBits("Non-Authoritative Information")]);
  } else if (status === 204) {
    return toBitArray([stringBits("No Content")]);
  } else if (status === 205) {
    return toBitArray([stringBits("Reset Content")]);
  } else if (status === 206) {
    return toBitArray([stringBits("Partial Content")]);
  } else if (status === 300) {
    return toBitArray([stringBits("Multiple Choices")]);
  } else if (status === 301) {
    return toBitArray([stringBits("Moved Permanently")]);
  } else if (status === 302) {
    return toBitArray([stringBits("Found")]);
  } else if (status === 303) {
    return toBitArray([stringBits("See Other")]);
  } else if (status === 304) {
    return toBitArray([stringBits("Not Modified")]);
  } else if (status === 307) {
    return toBitArray([stringBits("Temporary Redirect")]);
  } else if (status === 308) {
    return toBitArray([stringBits("Permanent Redirect")]);
  } else if (status === 400) {
    return toBitArray([stringBits("Bad Request")]);
  } else if (status === 401) {
    return toBitArray([stringBits("Unauthorized")]);
  } else if (status === 402) {
    return toBitArray([stringBits("Payment Required")]);
  } else if (status === 403) {
    return toBitArray([stringBits("Forbidden")]);
  } else if (status === 404) {
    return toBitArray([stringBits("Not Found")]);
  } else if (status === 405) {
    return toBitArray([stringBits("Method Not Allowed")]);
  } else if (status === 406) {
    return toBitArray([stringBits("Not Acceptable")]);
  } else if (status === 407) {
    return toBitArray([stringBits("Proxy Authentication Required")]);
  } else if (status === 408) {
    return toBitArray([stringBits("Request Timeout")]);
  } else if (status === 409) {
    return toBitArray([stringBits("Conflict")]);
  } else if (status === 410) {
    return toBitArray([stringBits("Gone")]);
  } else if (status === 411) {
    return toBitArray([stringBits("Length Required")]);
  } else if (status === 412) {
    return toBitArray([stringBits("Precondition Failed")]);
  } else if (status === 413) {
    return toBitArray([stringBits("Payload Too Large")]);
  } else if (status === 414) {
    return toBitArray([stringBits("URI Too Long")]);
  } else if (status === 415) {
    return toBitArray([stringBits("Unsupported Media Type")]);
  } else if (status === 416) {
    return toBitArray([stringBits("Range Not Satisfiable")]);
  } else if (status === 417) {
    return toBitArray([stringBits("Expectation Failed")]);
  } else if (status === 418) {
    return toBitArray([stringBits("I'm a teapot")]);
  } else if (status === 422) {
    return toBitArray([stringBits("Unprocessable Entity")]);
  } else if (status === 425) {
    return toBitArray([stringBits("Too Early")]);
  } else if (status === 426) {
    return toBitArray([stringBits("Upgrade Required")]);
  } else if (status === 428) {
    return toBitArray([stringBits("Precondition Required")]);
  } else if (status === 429) {
    return toBitArray([stringBits("Too Many Requests")]);
  } else if (status === 431) {
    return toBitArray([stringBits("Request Header Fields Too Large")]);
  } else if (status === 451) {
    return toBitArray([stringBits("Unavailable For Legal Reasons")]);
  } else if (status === 500) {
    return toBitArray([stringBits("Internal Server Error")]);
  } else if (status === 501) {
    return toBitArray([stringBits("Not Implemented")]);
  } else if (status === 502) {
    return toBitArray([stringBits("Bad Gateway")]);
  } else if (status === 503) {
    return toBitArray([stringBits("Service Unavailable")]);
  } else if (status === 504) {
    return toBitArray([stringBits("Gateway Timeout")]);
  } else if (status === 505) {
    return toBitArray([stringBits("HTTP Version Not Supported")]);
  } else if (status === 506) {
    return toBitArray([stringBits("Variant Also Negotiates")]);
  } else if (status === 507) {
    return toBitArray([stringBits("Insufficient Storage")]);
  } else if (status === 508) {
    return toBitArray([stringBits("Loop Detected")]);
  } else if (status === 510) {
    return toBitArray([stringBits("Not Extended")]);
  } else if (status === 511) {
    return toBitArray([stringBits("Network Authentication Required")]);
  } else {
    return toBitArray([stringBits("Unknown HTTP Status")]);
  }
}

export function encode_headers(headers) {
  return $list.fold(
    headers,
    $bytes_tree.new$(),
    (builder, tup) => {
      let header;
      let value;
      header = tup[0];
      value = tup[1];
      let _pipe = builder;
      let _pipe$1 = $bytes_tree.append_string(_pipe, header);
      let _pipe$2 = $bytes_tree.append(_pipe$1, toBitArray([stringBits(": ")]));
      let _pipe$3 = $bytes_tree.append_string(_pipe$2, value);
      return $bytes_tree.append(_pipe$3, toBitArray([stringBits("\r\n")]));
    },
  );
}

export function response_builder(status, headers, version) {
  let _block;
  let _pipe = status;
  let _pipe$1 = $int.to_string(_pipe);
  let _pipe$2 = $bytes_tree.from_string(_pipe$1);
  let _pipe$3 = $bytes_tree.append(_pipe$2, toBitArray([stringBits(" ")]));
  _block = $bytes_tree.append(_pipe$3, status_to_bit_array(status));
  let status_string = _block;
  let _pipe$4 = $bytes_tree.new$();
  let _pipe$5 = $bytes_tree.append(
    _pipe$4,
    toBitArray([stringBits("HTTP/"), stringBits(version), stringBits(" ")]),
  );
  let _pipe$6 = $bytes_tree.append_tree(_pipe$5, status_string);
  let _pipe$7 = $bytes_tree.append(_pipe$6, toBitArray([stringBits("\r\n")]));
  let _pipe$8 = $bytes_tree.append_tree(_pipe$7, encode_headers(headers));
  return $bytes_tree.append(_pipe$8, toBitArray([stringBits("\r\n")]));
}

/**
 * Turns an HTTP response into a TCP message
 */
export function to_bytes_tree(resp, version) {
  let _pipe = resp.status;
  let _pipe$1 = response_builder(_pipe, resp.headers, version);
  return $bytes_tree.append_tree(_pipe$1, resp.body);
}
