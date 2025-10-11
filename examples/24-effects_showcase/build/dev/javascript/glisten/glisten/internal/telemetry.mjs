import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { toList, CustomType as $CustomType } from "../../gleam.mjs";

export class Data extends $CustomType {
  constructor(latency, metadata) {
    super();
    this.latency = latency;
    this.metadata = metadata;
  }
}

export class Start extends $CustomType {}

export class Stop extends $CustomType {}

export class Glisten extends $CustomType {}

export class Handshake extends $CustomType {}

export class HandlerLoop extends $CustomType {}

export class Listener extends $CustomType {}

export class Acceptor extends $CustomType {}

export class HandlerStart extends $CustomType {}

export class HandlerInit extends $CustomType {}

class Native extends $CustomType {}

class Microsecond extends $CustomType {}

export const events = /* @__PURE__ */ toList([
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Glisten(),
    /* @__PURE__ */ new Handshake(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Glisten(),
    /* @__PURE__ */ new HandlerLoop(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Glisten(),
    /* @__PURE__ */ new Acceptor(),
    /* @__PURE__ */ new HandlerStart(),
    /* @__PURE__ */ new Stop(),
  ]),
]);
