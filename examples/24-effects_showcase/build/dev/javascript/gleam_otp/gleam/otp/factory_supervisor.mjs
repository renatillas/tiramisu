import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Ok, CustomType as $CustomType } from "../../gleam.mjs";
import * as $actor from "../../gleam/otp/actor.mjs";
import * as $result2 from "../../gleam/otp/internal/result2.mjs";
import * as $supervision from "../../gleam/otp/supervision.mjs";

class Supervisor extends $CustomType {
  constructor(pid) {
    super();
    this.pid = pid;
  }
}

class NamedSupervisor extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

class Builder extends $CustomType {
  constructor(child_type, template, restart_strategy, intensity, period, name) {
    super();
    this.child_type = child_type;
    this.template = template;
    this.restart_strategy = restart_strategy;
    this.intensity = intensity;
    this.period = period;
    this.name = name;
  }
}

class Local extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class SimpleOneForOne extends $CustomType {}

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
 * Get a reference to a supervisor using its registered name.
 *
 * If no supervisor has been started using this name then functions
 * using this reference will fail.
 *
 * # Panics
 *
 * Functions using the `Supervisor` reference returned by this function
 * will panic if there is no factory supervisor registered with the name
 * when they are called. Always make sure your supervisors are themselves
 * supervised.
 */
export function get_by_name(name) {
  return new NamedSupervisor(name);
}

/**
 * Provide a name for the supervisor to be registered with when started,
 * enabling it be more easily contacted by other processes. This is useful for
 * enabling processes that can take over from an older one that has exited due
 * to a failure.
 *
 * If the name is already registered to another process then the factory
 * supervisor will fail to start.
 */
export function named(builder, name) {
  return new Builder(
    builder.child_type,
    builder.template,
    builder.restart_strategy,
    builder.intensity,
    builder.period,
    new $option.Some(name),
  );
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
    builder.child_type,
    builder.template,
    builder.restart_strategy,
    intensity,
    period,
    builder.name,
  );
}

/**
 * Configure the amount of milliseconds a child has to shut down before
 * being brutal killed by the supervisor.
 *
 * If not set the default for a child is 5000ms.
 *
 * This will be ignored if the child is a supervisor itself.
 */
export function timeout(builder, ms) {
  let $ = builder.child_type;
  if ($ instanceof $supervision.Worker) {
    return new Builder(
      new $supervision.Worker(ms),
      builder.template,
      builder.restart_strategy,
      builder.intensity,
      builder.period,
      builder.name,
    );
  } else {
    return builder;
  }
}

/**
 * Configure the strategy for restarting children when they exit. See the
 * documentation for the `supervision.Restart` for details.
 *
 * If not set the default strategy is `supervision.Transient`, so children
 * will be restarted if they terminate abnormally.
 */
export function restart_strategy(builder, restart_strategy) {
  let $ = builder.child_type;
  if ($ instanceof $supervision.Worker) {
    return new Builder(
      builder.child_type,
      builder.template,
      restart_strategy,
      builder.intensity,
      builder.period,
      builder.name,
    );
  } else {
    return builder;
  }
}

export function init(start_data) {
  return new Ok(start_data);
}

export function start_child_callback(start, argument) {
  let $ = start(argument);
  if ($ instanceof Ok) {
    let started = $[0];
    return new $result2.Ok(started.pid, started.data);
  } else {
    let error = $[0];
    return new $result2.Error(error);
  }
}

const default_intensity = 2;

const default_period = 5;

const default_restart_strategy = /* @__PURE__ */ new $supervision.Transient();

/**
 * Configure a supervisor with a child-starting template function.
 *
 * You should use this unless the child processes are also supervisors.
 *
 * The default shutdown timeout is 5000ms. This can be changed with the
 * `timeout` function.
 */
export function worker_child(template) {
  return new Builder(
    new $supervision.Worker(5000),
    template,
    default_restart_strategy,
    default_intensity,
    default_period,
    new $option.None(),
  );
}

/**
 * Configure a supervisor with a template that will start children that are
 * also supervisors.
 *
 * You should only use this if the child processes are also supervisors.
 *
 * Supervisor children have an unlimited amount of time to shutdown, there is
 * no timeout.
 */
export function supervisor_child(template) {
  return new Builder(
    new $supervision.Supervisor(),
    template,
    default_restart_strategy,
    default_intensity,
    default_period,
    new $option.None(),
  );
}
