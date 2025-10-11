import { CustomType as $CustomType } from "../../../gleam.mjs";

export class Ok extends $CustomType {
  constructor($0, $1) {
    super();
    this[0] = $0;
    this[1] = $1;
  }
}

export class Error extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
