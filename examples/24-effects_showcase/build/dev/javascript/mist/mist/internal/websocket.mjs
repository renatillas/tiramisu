import * as $exception from "../../../exception/exception.mjs";
import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $glisten from "../../../glisten/glisten.mjs";
import * as $options from "../../../glisten/glisten/socket/options.mjs";
import * as $transport from "../../../glisten/glisten/transport.mjs";
import * as $websocket from "../../../gramps/gramps/websocket.mjs";
import { CloseFrame, Control, PingFrame } from "../../../gramps/gramps/websocket.mjs";
import * as $compression from "../../../gramps/gramps/websocket/compression.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $next from "../../mist/internal/next.mjs";
import { AbnormalStop, Continue, NormalStop } from "../../mist/internal/next.mjs";

export class SocketMessage extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class SocketClosedMessage extends $CustomType {}

export class UserMessage extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Valid extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Invalid extends $CustomType {}

export class WebsocketConnection extends $CustomType {
  constructor(socket, transport, deflate) {
    super();
    this.socket = socket;
    this.transport = transport;
    this.deflate = deflate;
  }
}

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

export class WebsocketState extends $CustomType {
  constructor(buffer, user, permessage_deflate) {
    super();
    this.buffer = buffer;
    this.user = user;
    this.permessage_deflate = permessage_deflate;
  }
}
