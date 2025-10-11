import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $charlist from "../../../gleam_erlang/gleam/erlang/charlist.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import { Abnormal, Killed } from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, CustomType as $CustomType } from "../../gleam.mjs";
import * as $system from "../../gleam/otp/system.mjs";
import { GetState, GetStatus, Resume, Running, StatusInfo, Suspend, Suspended } from "../../gleam/otp/system.mjs";

/**
 * A regular message excepted by the process
 * 
 * @ignore
 */
class Message extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * An OTP system message, for debugging or maintenance
 * 
 * @ignore
 */
class System extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * An unexpected message
 * 
 * @ignore
 */
class Unexpected extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Continue handling messages.
 *
 * An optional selector can be provided to changes the messages that the
 * actor is handling. This replaces any selector that was previously given
 * in the actor's `init` callback, or in any previous `Next` value.
 */
class Continue extends $CustomType {
  constructor(state, selector) {
    super();
    this.state = state;
    this.selector = selector;
  }
}

/**
 * Stop handling messages and shut down.
 */
class Stop extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Self extends $CustomType {
  constructor(mode, parent, state, selector, debug_state, message_handler) {
    super();
    this.mode = mode;
    this.parent = parent;
    this.state = state;
    this.selector = selector;
    this.debug_state = debug_state;
    this.message_handler = message_handler;
  }
}

export class Started extends $CustomType {
  constructor(pid, data) {
    super();
    this.pid = pid;
    this.data = data;
  }
}

class Initialised extends $CustomType {
  constructor(state, selector, return$) {
    super();
    this.state = state;
    this.selector = selector;
    this.return = return$;
  }
}

class Builder extends $CustomType {
  constructor(initialise, initialisation_timeout, on_message, name) {
    super();
    this.initialise = initialise;
    this.initialisation_timeout = initialisation_timeout;
    this.on_message = on_message;
    this.name = name;
  }
}

export class InitTimeout extends $CustomType {}

export class InitFailed extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class InitExited extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Ack extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Mon extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Indicate the actor should continue, processing any waiting or future messages.
 */
export function continue$(state) {
  return new Continue(state, new None());
}

/**
 * Indicate the actor should stop and shut-down, handling no futher messages.
 *
 * The reason for exiting is `Normal`.
 */
export function stop() {
  return new Stop(new $process.Normal());
}

/**
 * Indicate the actor is in a bad state and should shut down. It will not
 * handle any new messages, and any linked processes will also exit abnormally.
 *
 * The provided reason will be given and propagated.
 */
export function stop_abnormal(reason) {
  return new Stop(new $process.Abnormal($dynamic.string(reason)));
}

/**
 * Provide a selector to change the messages that the actor is handling
 * going forward. This replaces any selector that was previously given
 * in the actor's `init` callback, or in any previous `Next` value.
 */
export function with_selector(value, selector) {
  if (value instanceof Continue) {
    let state = value.state;
    return new Continue(state, new Some(selector));
  } else {
    return value;
  }
}

/**
 * Takes the post-initialisation state of the actor. This state will be passed
 * to the `on_message` callback each time a message is received.
 */
export function initialised(state) {
  return new Initialised(state, new None(), undefined);
}

/**
 * Add a selector for the actor to receive messages with.
 *
 * If a message is received by the actor but not selected for with the
 * selector then the actor will discard it and log a warning.
 */
export function selecting(initialised, selector) {
  return new Initialised(
    initialised.state,
    new Some(selector),
    initialised.return,
  );
}

/**
 * Add the data to return to the parent process. This might be a subject that
 * the actor will receive messages over.
 */
export function returning(initialised, return$) {
  return new Initialised(initialised.state, initialised.selector, return$);
}

/**
 * Create a builder for an actor without a custom initialiser. The actor
 * returns a subject to the parent that can be used to send messages to the
 * actor.
 *
 * If the actor has been given a name with the `named` function then the
 * subject is a named subject.
 *
 * If you wish to create an actor with some other initialisation logic that
 * runs before it starts handling messages, see `new_with_initialiser`.
 */
export function new$(state) {
  let initialise = (subject) => {
    let _pipe = initialised(state);
    let _pipe$1 = returning(_pipe, subject);
    return new Ok(_pipe$1);
  };
  return new Builder(
    initialise,
    1000,
    (state, _) => { return continue$(state); },
    new $option.None(),
  );
}

/**
 * Create a builder for an actor with a custom initialiser that runs before
 * the start function returns to the parent, and before the actor starts
 * handling messages.
 *
 * The first argument is a number of milliseconds that the initialiser
 * function is expected to return within. If it takes longer the initialiser
 * is considered to have failed and the actor will be killed, and an error
 * will be returned to the parent.
 *
 * The actor's default subject is passed to the initialiser function. You can
 * chose to return it to the parent with `returning`, use it in some other
 * way, or ignore it completely.
 *
 * If a custom selector is given using the `selecting` function then this
 * overwrites the default selector, which selects for the default subject, so
 * you will need to add the subject to the custom selector yourself.
 */
export function new_with_initialiser(timeout, initialise) {
  return new Builder(
    initialise,
    timeout,
    (state, _) => { return continue$(state); },
    new $option.None(),
  );
}

/**
 * Set the message handler for the actor. This callback function will be
 * called each time the actor receives a message.
 *
 * Actors handle messages sequentially, later messages being handled after the
 * previous one has been handled.
 */
export function on_message(builder, handler) {
  return new Builder(
    builder.initialise,
    builder.initialisation_timeout,
    handler,
    builder.name,
  );
}

/**
 * Provide a name for the actor to be registered with when started, enabling
 * it to receive messages via a named subject. This is useful for making
 * processes that can take over from an older one that has exited due to a
 * failure, or to avoid passing subjects from receiver processes to sender
 * processes.
 *
 * If the name is already registered to another process then the actor will
 * fail to start.
 *
 * When this function is used the actor's default subject will be a named
 * subject using this name.
 */
export function named(builder, name) {
  return new Builder(
    builder.initialise,
    builder.initialisation_timeout,
    builder.on_message,
    new $option.Some(name),
  );
}
