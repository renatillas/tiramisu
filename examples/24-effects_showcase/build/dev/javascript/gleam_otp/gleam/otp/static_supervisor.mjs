import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import { Ok, toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import * as $actor from "../../gleam/otp/actor.mjs";
import * as $supervision from "../../gleam/otp/supervision.mjs";

class Supervisor extends $CustomType {
  constructor(pid) {
    super();
    this.pid = pid;
  }
}

export class OneForOne extends $CustomType {}

export class OneForAll extends $CustomType {}

export class RestForOne extends $CustomType {}

export class Never extends $CustomType {}

export class AnySignificant extends $CustomType {}

export class AllSignificant extends $CustomType {}

class Builder extends $CustomType {
  constructor(strategy, intensity, period, auto_shutdown, children) {
    super();
    this.strategy = strategy;
    this.intensity = intensity;
    this.period = period;
    this.auto_shutdown = auto_shutdown;
    this.children = children;
  }
}

class Strategy extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Intensity extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Period extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class AutoShutdown extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Id extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Start extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Restart extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Significant extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Type extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Shutdown extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Create a new supervisor builder, ready for further configuration.
 */
export function new$(strategy) {
  return new Builder(strategy, 2, 5, new Never(), toList([]));
}

/**
 * To prevent a supervisor from getting into an infinite loop of child
 * process terminations and restarts, a maximum restart tolerance is
 * defined using two integer values specified with keys intensity and
 * period in the above map. Assuming the values MaxR for intensity and MaxT
 * for period, then, if more than MaxR restarts occur within MaxT seconds,
 * the supervisor terminates all child processes and then itself. The
 * termination reason for the supervisor itself in that case will be
 * shutdown. 
 *
 * Intensity defaults to 2 and period defaults to 5.
 */
export function restart_tolerance(builder, intensity, period) {
  return new Builder(
    builder.strategy,
    intensity,
    period,
    builder.auto_shutdown,
    builder.children,
  );
}

/**
 * A supervisor can be configured to automatically shut itself down with
 * exit reason shutdown when significant children terminate.
 */
export function auto_shutdown(builder, value) {
  return new Builder(
    builder.strategy,
    builder.intensity,
    builder.period,
    value,
    builder.children,
  );
}

/**
 * Add a child to the supervisor.
 */
export function add(builder, child) {
  return new Builder(
    builder.strategy,
    builder.intensity,
    builder.period,
    builder.auto_shutdown,
    listPrepend(
      $supervision.map_data(child, (_) => { return undefined; }),
      builder.children,
    ),
  );
}

export function init(start_data) {
  return new Ok(start_data);
}

export function start_child_callback(start) {
  let $ = start();
  if ($ instanceof Ok) {
    let started = $[0];
    return new Ok(started.pid);
  } else {
    return $;
  }
}
