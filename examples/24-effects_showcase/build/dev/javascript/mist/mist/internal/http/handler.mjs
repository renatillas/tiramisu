import * as $exception from "../../../../exception/exception.mjs";
import * as $process from "../../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $ghttp from "../../../../gleam_http/gleam/http.mjs";
import * as $request from "../../../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../../../gleam_http/gleam/http/response.mjs";
import * as $bytes_tree from "../../../../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $int from "../../../../gleam_stdlib/gleam/int.mjs";
import * as $option from "../../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../../gleam_stdlib/gleam/string.mjs";
import * as $yielder from "../../../../gleam_yielder/gleam/yielder.mjs";
import * as $handler from "../../../../glisten/glisten/internal/handler.mjs";
import { Close, Internal } from "../../../../glisten/glisten/internal/handler.mjs";
import * as $socket from "../../../../glisten/glisten/socket.mjs";
import { Badarg } from "../../../../glisten/glisten/socket.mjs";
import * as $transport from "../../../../glisten/glisten/transport.mjs";
import * as $logging from "../../../../logging/logging.mjs";
import { CustomType as $CustomType } from "../../../gleam.mjs";
import * as $encoder from "../../../mist/internal/encoder.mjs";
import * as $file from "../../../mist/internal/file.mjs";
import * as $http from "../../../mist/internal/http.mjs";
import { Bytes, Chunked, File, ServerSentEvents, Websocket } from "../../../mist/internal/http.mjs";

export class State extends $CustomType {
  constructor(idle_timer) {
    super();
    this.idle_timer = idle_timer;
  }
}

export function initial_state() {
  return new State(new None());
}
