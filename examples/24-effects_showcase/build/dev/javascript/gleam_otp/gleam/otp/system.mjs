import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";

export class Running extends $CustomType {}

export class Suspended extends $CustomType {}

export class NoDebug extends $CustomType {}

export class StatusInfo extends $CustomType {
  constructor(module, parent, mode, debug_state, state) {
    super();
    this.module = module;
    this.parent = parent;
    this.mode = mode;
    this.debug_state = debug_state;
    this.state = state;
  }
}

export class Resume extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Suspend extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class GetState extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class GetStatus extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
