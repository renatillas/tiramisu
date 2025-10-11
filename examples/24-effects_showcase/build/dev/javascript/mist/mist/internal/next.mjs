import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";

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
