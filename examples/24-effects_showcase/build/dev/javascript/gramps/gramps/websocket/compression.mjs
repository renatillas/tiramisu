import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bytes_tree from "../../../gleam_stdlib/gleam/bytes_tree.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";

export class Context extends $CustomType {
  constructor(context, no_takeover) {
    super();
    this.context = context;
    this.no_takeover = no_takeover;
  }
}

class Sync extends $CustomType {}

class Deflated extends $CustomType {}

class Default extends $CustomType {}

export class ContextTakeover extends $CustomType {
  constructor(no_client, no_server) {
    super();
    this.no_client = no_client;
    this.no_server = no_server;
  }
}

export class Compression extends $CustomType {
  constructor(inflate, deflate) {
    super();
    this.inflate = inflate;
    this.deflate = deflate;
  }
}
