import * as $atom from "../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $port from "../../gleam_erlang/gleam/erlang/port.mjs";
import * as $process from "../../gleam_erlang/gleam/erlang/process.mjs";
import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $actor from "../../gleam_otp/gleam/otp/actor.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";

class Send extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Data extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Exit extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Unknown extends $CustomType {}
