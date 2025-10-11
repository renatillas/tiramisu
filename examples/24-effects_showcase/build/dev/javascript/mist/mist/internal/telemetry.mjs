import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { toList, CustomType as $CustomType } from "../../gleam.mjs";

export class Start extends $CustomType {}

export class Stop extends $CustomType {}

export class Mist extends $CustomType {}

export class ParseRequest extends $CustomType {}

export class ParseRequest2 extends $CustomType {}

export class DecodePacket extends $CustomType {}

export class ConvertPath extends $CustomType {}

export class ParseMethod extends $CustomType {}

export class ParseHeaders extends $CustomType {}

export class ParseRest extends $CustomType {}

export class ParsePath extends $CustomType {}

export class ParseTransport extends $CustomType {}

export class ParseHost extends $CustomType {}

export class ParsePort extends $CustomType {}

export class BuildRequest extends $CustomType {}

export class ReadData extends $CustomType {}

export class Http1Handler extends $CustomType {}

export class HttpUpgrade extends $CustomType {}

export class Http2Handler extends $CustomType {}

class Native extends $CustomType {}

class Microsecond extends $CustomType {}

export const events = /* @__PURE__ */ toList([
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ParseRequest(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ParseRequest2(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new Http1Handler(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new HttpUpgrade(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new Http2Handler(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new DecodePacket(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ConvertPath(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ParseMethod(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ParseHeaders(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ParseRest(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ParsePath(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ParseTransport(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ParseHost(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ParsePort(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new BuildRequest(),
    /* @__PURE__ */ new Stop(),
  ]),
  /* @__PURE__ */ toList([
    /* @__PURE__ */ new Mist(),
    /* @__PURE__ */ new ReadData(),
    /* @__PURE__ */ new Stop(),
  ]),
]);
