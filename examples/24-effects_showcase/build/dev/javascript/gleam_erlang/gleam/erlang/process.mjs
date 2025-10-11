import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, CustomType as $CustomType } from "../../gleam.mjs";
import * as $atom from "../../gleam/erlang/atom.mjs";
import * as $port from "../../gleam/erlang/port.mjs";
import * as $reference from "../../gleam/erlang/reference.mjs";

class Subject extends $CustomType {
  constructor(owner, tag) {
    super();
    this.owner = owner;
    this.tag = tag;
  }
}

class NamedSubject extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class ExitMessage extends $CustomType {
  constructor(pid, reason) {
    super();
    this.pid = pid;
    this.reason = reason;
  }
}

export class Normal extends $CustomType {}

export class Killed extends $CustomType {}

export class Abnormal extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

class Anything extends $CustomType {}

class Process extends $CustomType {}

export class ProcessDown extends $CustomType {
  constructor(monitor, pid, reason) {
    super();
    this.monitor = monitor;
    this.pid = pid;
    this.reason = reason;
  }
}

export class PortDown extends $CustomType {
  constructor(monitor, port, reason) {
    super();
    this.monitor = monitor;
    this.port = port;
    this.reason = reason;
  }
}

export class TimerNotFound extends $CustomType {}

/**
 * The timer was found and cancelled before it triggered.
 *
 * The amount of remaining time before the timer was due to be triggered is
 * returned in milliseconds.
 */
export class Cancelled extends $CustomType {
  constructor(time_remaining) {
    super();
    this.time_remaining = time_remaining;
  }
}

class Kill extends $CustomType {}

/**
 * Create a subject for the given process with the give tag. This is unsafe!
 * There's nothing here that verifies that the message the subject receives is
 * expected and that the tag is not already in use.
 *
 * You should almost certainly not use this function.
 * 
 * @ignore
 */
export function unsafely_create_subject(owner, tag) {
  return new Subject(owner, tag);
}

/**
 * Create a subject for a name, which can be used to send and receive messages.
 *
 * All subjects created for the same name behave identically and can be used
 * interchangably.
 */
export function named_subject(name) {
  return new NamedSubject(name);
}

/**
 * Get the name of a subject, returning an error if it doesn't have one.
 */
export function subject_name(subject) {
  if (subject instanceof Subject) {
    return new Error(undefined);
  } else {
    let name = subject.name;
    return new Ok(name);
  }
}
