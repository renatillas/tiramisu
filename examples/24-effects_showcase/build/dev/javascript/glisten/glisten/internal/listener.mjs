import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $socket from "../../glisten/socket.mjs";
import * as $options from "../../glisten/socket/options.mjs";
import * as $transport from "../../glisten/transport.mjs";

export class Info extends $CustomType {
  constructor(caller) {
    super();
    this.caller = caller;
  }
}

export class State extends $CustomType {
  constructor(listen_socket, port, ip_address) {
    super();
    this.listen_socket = listen_socket;
    this.port = port;
    this.ip_address = ip_address;
  }
}
