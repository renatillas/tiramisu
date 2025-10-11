import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $supervisor from "../../../gleam_otp/gleam/otp/static_supervisor.mjs";
import * as $supervision from "../../../gleam_otp/gleam/otp/supervision.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $handler from "../../glisten/internal/handler.mjs";
import { Handler, Internal, Ready } from "../../glisten/internal/handler.mjs";
import * as $listener from "../../glisten/internal/listener.mjs";
import * as $socket from "../../glisten/socket.mjs";
import * as $options from "../../glisten/socket/options.mjs";
import * as $transport from "../../glisten/transport.mjs";

export class AcceptConnection extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class AcceptError extends $CustomType {}

export class HandlerError extends $CustomType {}

export class ControlError extends $CustomType {}

export class AcceptorState extends $CustomType {
  constructor(sender, socket, transport) {
    super();
    this.sender = sender;
    this.socket = socket;
    this.transport = transport;
  }
}

export class Pool extends $CustomType {
  constructor(handler, pool_count, on_init, on_close, transport) {
    super();
    this.handler = handler;
    this.pool_count = pool_count;
    this.on_init = on_init;
    this.on_close = on_close;
    this.transport = transport;
  }
}
