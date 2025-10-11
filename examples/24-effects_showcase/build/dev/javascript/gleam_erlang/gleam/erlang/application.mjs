import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $node from "../../gleam/erlang/node.mjs";

export class Normal extends $CustomType {}

/**
 * The application is distributed and started at the current node because of
 * a takeover from Node, either because Erlang's `application:takeover/2`
 * function has been called, or because the current node has higher priority
 * than the previous node.
 */
export class Takeover extends $CustomType {
  constructor(previous) {
    super();
    this.previous = previous;
  }
}

/**
 * The application is distributed and started at the current node because of
 * a failover from the previous node.
 */
export class Failover extends $CustomType {
  constructor(previous) {
    super();
    this.previous = previous;
  }
}
