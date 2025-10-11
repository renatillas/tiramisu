import { Ok, CustomType as $CustomType } from "../../gleam.mjs";
import * as $actor from "../../gleam/otp/actor.mjs";

export class Permanent extends $CustomType {}

export class Transient extends $CustomType {}

export class Temporary extends $CustomType {}

/**
 * A worker child has to shut-down within a given amount of time.
 */
export class Worker extends $CustomType {
  constructor(shutdown_ms) {
    super();
    this.shutdown_ms = shutdown_ms;
  }
}

export class Supervisor extends $CustomType {}

export class ChildSpecification extends $CustomType {
  constructor(start, restart, significant, child_type) {
    super();
    this.start = start;
    this.restart = restart;
    this.significant = significant;
    this.child_type = child_type;
  }
}

/**
 * A regular child process.
 *
 * You should use this unless your process is also a supervisor.
 *
 * The default shutdown timeout is 5000ms. This can be changed with the
 * `timeout` function.
 */
export function worker(start) {
  return new ChildSpecification(start, new Permanent(), false, new Worker(5000));
}

/**
 * A special child that is a supervisor itself.
 *
 * Supervisor children have an unlimited shutdown time, there is no timeout.
 */
export function supervisor(start) {
  return new ChildSpecification(start, new Permanent(), false, new Supervisor());
}

/**
 * This defines if a child is considered significant for automatic
 * self-shutdown of the supervisor.
 *
 * You most likely do not want to consider any children significant.
 *
 * This will be ignored if the supervisor auto shutdown is set to `Never`,
 * which is the default.
 *
 * The default value for significance is `False`.
 */
export function significant(child, significant) {
  return new ChildSpecification(
    child.start,
    child.restart,
    significant,
    child.child_type,
  );
}

/**
 * This defines the amount of milliseconds a child has to shut down before
 * being brutal killed by the supervisor.
 *
 * If not set the default for a child is 5000ms.
 *
 * This will be ignored if the child is a supervisor itself.
 */
export function timeout(child, ms) {
  let $ = child.child_type;
  if ($ instanceof Worker) {
    return new ChildSpecification(
      child.start,
      child.restart,
      child.significant,
      new Worker(ms),
    );
  } else {
    return child;
  }
}

/**
 * When the child is to be restarted. See the `Restart` documentation for
 * more.
 *
 * The default value for restart is `Permanent`.
 */
export function restart(child, restart) {
  return new ChildSpecification(
    child.start,
    restart,
    child.significant,
    child.child_type,
  );
}

/**
 * Transform the data of the started child process.
 */
export function map_data(child, transform) {
  return new ChildSpecification(
    () => {
      let $ = child.start();
      if ($ instanceof Ok) {
        let started = $[0];
        return new Ok(new $actor.Started(started.pid, transform(started.data)));
      } else {
        return $;
      }
    },
    child.restart,
    child.significant,
    child.child_type,
  );
}
