import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $socket from "../../glisten/socket.mjs";
import * as $options from "../../glisten/socket/options.mjs";
import * as $transport from "../../glisten/transport.mjs";

export class Close extends $CustomType {}

export class Ready extends $CustomType {}

export class ReceiveMessage extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class SslClosed extends $CustomType {}

export class TcpClosed extends $CustomType {}

export class Internal extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class User extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Packet extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Custom extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class LoopState extends $CustomType {
  constructor(client_ip, socket, sender, transport, state) {
    super();
    this.client_ip = client_ip;
    this.socket = socket;
    this.sender = sender;
    this.transport = transport;
    this.state = state;
  }
}

export class Connection extends $CustomType {
  constructor(client_ip, socket, transport, sender) {
    super();
    this.client_ip = client_ip;
    this.socket = socket;
    this.transport = transport;
    this.sender = sender;
  }
}

export class Continue extends $CustomType {
  constructor(state, selector) {
    super();
    this.state = state;
    this.selector = selector;
  }
}

export class NormalStop extends $CustomType {}

export class AbnormalStop extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class Handler extends $CustomType {
  constructor(socket, loop, on_init, on_close, transport) {
    super();
    this.socket = socket;
    this.loop = loop;
    this.on_init = on_init;
    this.on_close = on_close;
    this.transport = transport;
  }
}

export function continue$(state) {
  return new Continue(state, new None());
}

export function with_selector(next, selector) {
  if (next instanceof Continue) {
    let state = next.state;
    return new Continue(state, new Some(selector));
  } else {
    return next;
  }
}

export function stop() {
  return new NormalStop();
}

export function stop_abnormal(reason) {
  return new AbnormalStop(reason);
}
