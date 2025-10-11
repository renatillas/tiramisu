import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import { rescue, defer, on_crash } from "./exception_ffi.mjs";
import { CustomType as $CustomType } from "./gleam.mjs";

export { defer, on_crash, rescue };

/**
 * An error was raised.
 * On Erlang this would be caused by calling the `erlang:error/1` function,
 * or some other runtime error.
 * On JavaScript this would be caused by throwing an `Error` object.
 */
export class Errored extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * A value was thrown.
 * On Erlang this would be caused by calling the `erlang:throw/1` function.
 * On JavaScript this would be caused by throwing any non-`Error` value.
 */
export class Thrown extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * A process exited.
 * On Erlang this would be caused by calling the `erlang:exit/1` function.
 * On JavaScript this variant is not used.
 */
export class Exited extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
