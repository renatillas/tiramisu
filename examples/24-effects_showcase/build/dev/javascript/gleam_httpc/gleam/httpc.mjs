import * as $charlist from "../../gleam_erlang/gleam/erlang/charlist.mjs";
import * as $http from "../../gleam_http/gleam/http.mjs";
import * as $request from "../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../gleam_http/gleam/http/response.mjs";
import { Response } from "../../gleam_http/gleam/http/response.mjs";
import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $uri from "../../gleam_stdlib/gleam/uri.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";

export class InvalidUtf8Response extends $CustomType {}

/**
 * It was not possible to connect to the host.
 */
export class FailedToConnect extends $CustomType {
  constructor(ip4, ip6) {
    super();
    this.ip4 = ip4;
    this.ip6 = ip6;
  }
}

export class ResponseTimeout extends $CustomType {}

export class Posix extends $CustomType {
  constructor(code) {
    super();
    this.code = code;
  }
}

export class TlsAlert extends $CustomType {
  constructor(code, detail) {
    super();
    this.code = code;
    this.detail = detail;
  }
}

class Ssl extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Autoredirect extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Timeout extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Binary extends $CustomType {}

class BodyFormat extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class SocketOpts extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Ipfamily extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Inet6fb4 extends $CustomType {}

class Verify extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class VerifyNone extends $CustomType {}

class Builder extends $CustomType {
  constructor(verify_tls, follow_redirects, timeout) {
    super();
    this.verify_tls = verify_tls;
    this.follow_redirects = follow_redirects;
    this.timeout = timeout;
  }
}

/**
 * Create a new configuration with the default settings.
 *
 * # Defaults
 *
 * - TLS is verified.
 * - Redirects are not followed.
 * - The timeout for the response to be received is 30 seconds from when the
 *   request is sent.
 */
export function configure() {
  return new Builder(true, false, 30_000);
}

/**
 * Set whether to verify the TLS certificate of the server.
 *
 * This defaults to `True`, meaning that the TLS certificate will be verified
 * unless you call this function with `False`.
 *
 * Setting this to `False` can make your application vulnerable to
 * man-in-the-middle attacks and other security risks. Do not do this unless
 * you are sure and you understand the risks.
 */
export function verify_tls(config, which) {
  return new Builder(which, config.follow_redirects, config.timeout);
}

/**
 * Set whether redirects should be followed automatically.
 */
export function follow_redirects(config, which) {
  return new Builder(config.verify_tls, which, config.timeout);
}

/**
 * Set the timeout in milliseconds, the default being 30 seconds.
 *
 * If the response is not recieved within this amount of time then the
 * client disconnects and an error is returned.
 */
export function timeout(config, timeout) {
  return new Builder(config.verify_tls, config.follow_redirects, timeout);
}
