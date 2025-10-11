import * as $bytes_tree from "../../../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $glisten from "../../../glisten/glisten.mjs";
import * as $transport from "../../../glisten/glisten/transport.mjs";
import { Ssl, Tcp } from "../../../glisten/glisten/transport.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";

export class IsDir extends $CustomType {}

export class NoAccess extends $CustomType {}

export class NoEntry extends $CustomType {}

export class UnknownFileError extends $CustomType {}

export class FileErr extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class SocketErr extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class File extends $CustomType {
  constructor(descriptor, file_size) {
    super();
    this.descriptor = descriptor;
    this.file_size = file_size;
  }
}
