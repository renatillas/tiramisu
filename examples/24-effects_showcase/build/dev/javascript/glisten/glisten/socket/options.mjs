import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import { toList, CustomType as $CustomType } from "../../gleam.mjs";

export class Binary extends $CustomType {}

export class Once extends $CustomType {}

export class Passive extends $CustomType {}

export class Count extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Active extends $CustomType {}

export class Address extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Any extends $CustomType {}

export class Loopback extends $CustomType {}

export class CertKeyFiles extends $CustomType {
  constructor(certfile, keyfile) {
    super();
    this.certfile = certfile;
    this.keyfile = keyfile;
  }
}

export class Backlog extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Nodelay extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Linger extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class SendTimeout extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class SendTimeoutClose extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Reuseaddr extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class ActiveMode extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Mode extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class CertKeyConfig extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class AlpnPreferredProtocols extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Ipv6 extends $CustomType {}

export class Buffer extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Ip extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class IpV4 extends $CustomType {
  constructor($0, $1, $2, $3) {
    super();
    this[0] = $0;
    this[1] = $1;
    this[2] = $2;
    this[3] = $3;
  }
}

export class IpV6 extends $CustomType {
  constructor($0, $1, $2, $3, $4, $5, $6, $7) {
    super();
    this[0] = $0;
    this[1] = $1;
    this[2] = $2;
    this[3] = $3;
    this[4] = $4;
    this[5] = $5;
    this[6] = $6;
    this[7] = $7;
  }
}

export const default_options = /* @__PURE__ */ toList([
  /* @__PURE__ */ new Backlog(1024),
  /* @__PURE__ */ new Nodelay(true),
  /* @__PURE__ */ new SendTimeout(30_000),
  /* @__PURE__ */ new SendTimeoutClose(true),
  /* @__PURE__ */ new Reuseaddr(true),
  /* @__PURE__ */ new Mode(/* @__PURE__ */ new Binary()),
  /* @__PURE__ */ new ActiveMode(/* @__PURE__ */ new Passive()),
]);
