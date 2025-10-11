import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $response from "../../../gleam_http/gleam/http/response.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $glisten from "../../../glisten/glisten.mjs";
import { Packet, User } from "../../../glisten/glisten.mjs";
import * as $transport from "../../../glisten/glisten/transport.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $http from "../../mist/internal/http.mjs";
import {
  Bytes,
  Chunked,
  Connection,
  DiscardPacket,
  File,
  Initial,
  ServerSentEvents,
  Websocket,
} from "../../mist/internal/http.mjs";
import * as $http_handler from "../../mist/internal/http/handler.mjs";
import * as $http2 from "../../mist/internal/http2.mjs";
import * as $http2_handler from "../../mist/internal/http2/handler.mjs";
import * as $stream from "../../mist/internal/http2/stream.mjs";
import { Send } from "../../mist/internal/http2/stream.mjs";

export class InvalidRequest extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NotFound extends $CustomType {}

export class Http1 extends $CustomType {
  constructor(state, self) {
    super();
    this.state = state;
    this.self = self;
  }
}

export class Http2 extends $CustomType {
  constructor(state) {
    super();
    this.state = state;
  }
}

export function new_state(subj) {
  return new Http1($http_handler.initial_state(), subj);
}
