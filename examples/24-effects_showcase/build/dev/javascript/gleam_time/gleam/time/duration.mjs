import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $order from "../../../gleam_stdlib/gleam/order.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { CustomType as $CustomType, remainderInt, divideInt } from "../../gleam.mjs";

class Duration extends $CustomType {
  constructor(seconds, nanoseconds) {
    super();
    this.seconds = seconds;
    this.nanoseconds = nanoseconds;
  }
}

export class Nanosecond extends $CustomType {}

export class Microsecond extends $CustomType {}

export class Millisecond extends $CustomType {}

export class Second extends $CustomType {}

export class Minute extends $CustomType {}

export class Hour extends $CustomType {}

export class Day extends $CustomType {}

export class Week extends $CustomType {}

export class Month extends $CustomType {}

export class Year extends $CustomType {}

/**
 * Ensure the duration is represented with `nanoseconds` being positive and
 * less than 1 second.
 *
 * This function does not change the amount of time that the duratoin refers
 * to, it only adjusts the values used to represent the time.
 * 
 * @ignore
 */
function normalise(duration) {
  let multiplier = 1_000_000_000;
  let nanoseconds$1 = remainderInt(duration.nanoseconds, multiplier);
  let overflow = duration.nanoseconds - nanoseconds$1;
  let seconds$1 = duration.seconds + (divideInt(overflow, multiplier));
  let $ = nanoseconds$1 >= 0;
  if ($) {
    return new Duration(seconds$1, nanoseconds$1);
  } else {
    return new Duration(seconds$1 - 1, multiplier + nanoseconds$1);
  }
}

/**
 * Convert a duration to a number of the largest number of a unit, serving as
 * a rough description of the duration that a human can understand.
 *
 * The size used for each unit are described in the documentation for the
 * `Unit` type.
 *
 * ```gleam
 * seconds(125)
 * |> approximate
 * // -> #(2, Minute)
 * ```
 *
 * This function rounds _towards zero_. This means that if a duration is just
 * short of 2 days then it will approximate to 1 day.
 *
 * ```gleam
 * hours(47)
 * |> approximate
 * // -> #(1, Day)
 * ```
 */
export function approximate(duration) {
  let s;
  let ns;
  s = duration.seconds;
  ns = duration.nanoseconds;
  let minute = 60;
  let hour = minute * 60;
  let day = hour * 24;
  let week = day * 7;
  let year = day * 365 + hour * 6;
  let month = globalThis.Math.trunc(year / 12);
  let microsecond = 1000;
  let millisecond = microsecond * 1000;
  let $ = undefined;
  if (s < 0) {
    let _block;
    let _pipe = new Duration(- s, - ns);
    let _pipe$1 = normalise(_pipe);
    _block = approximate(_pipe$1);
    let $1 = _block;
    let amount;
    let unit;
    amount = $1[0];
    unit = $1[1];
    return [- amount, unit];
  } else if (s >= year) {
    return [divideInt(s, year), new Year()];
  } else if (s >= month) {
    return [divideInt(s, month), new Month()];
  } else if (s >= week) {
    return [divideInt(s, week), new Week()];
  } else if (s >= day) {
    return [divideInt(s, day), new Day()];
  } else if (s >= hour) {
    return [divideInt(s, hour), new Hour()];
  } else if (s >= minute) {
    return [divideInt(s, minute), new Minute()];
  } else if (s > 0) {
    return [s, new Second()];
  } else if (ns >= millisecond) {
    return [divideInt(ns, millisecond), new Millisecond()];
  } else if (ns >= microsecond) {
    return [divideInt(ns, microsecond), new Microsecond()];
  } else {
    return [ns, new Nanosecond()];
  }
}

/**
 * Compare one duration to another, indicating whether the first spans a
 * larger amount of time (and so is greater) or smaller amount of time (and so
 * is lesser) than the second.
 *
 * # Examples
 *
 * ```gleam
 * compare(seconds(1), seconds(2))
 * // -> order.Lt
 * ```
 *
 * Whether a duration is negative or positive doesn't matter for comparing
 * them, only the amount of time spanned matters.
 *
 * ```gleam
 * compare(seconds(-2), seconds(1))
 * // -> order.Gt
 * ```
 */
export function compare(left, right) {
  let parts = (x) => {
    let $ = x.seconds >= 0;
    if ($) {
      return [x.seconds, x.nanoseconds];
    } else {
      return [x.seconds * -1 - 1, 1_000_000_000 - x.nanoseconds];
    }
  };
  let $ = parts(left);
  let ls;
  let lns;
  ls = $[0];
  lns = $[1];
  let $1 = parts(right);
  let rs;
  let rns;
  rs = $1[0];
  rns = $1[1];
  let _pipe = $int.compare(ls, rs);
  return $order.break_tie(_pipe, $int.compare(lns, rns));
}

/**
 * Calculate the difference between two durations.
 *
 * This is effectively substracting the first duration from the second.
 *
 * # Examples
 *
 * ```gleam
 * difference(seconds(1), seconds(5))
 * // -> seconds(4)
 * ```
 */
export function difference(left, right) {
  let _pipe = new Duration(
    right.seconds - left.seconds,
    right.nanoseconds - left.nanoseconds,
  );
  return normalise(_pipe);
}

/**
 * Add two durations together.
 *
 * # Examples
 *
 * ```gleam
 * add(seconds(1), seconds(5))
 * // -> seconds(6)
 * ```
 */
export function add(left, right) {
  let _pipe = new Duration(
    left.seconds + right.seconds,
    left.nanoseconds + right.nanoseconds,
  );
  return normalise(_pipe);
}

function nanosecond_digits(loop$n, loop$position, loop$acc) {
  while (true) {
    let n = loop$n;
    let position = loop$position;
    let acc = loop$acc;
    if (position === 9) {
      return acc;
    } else if ((acc === "") && ((remainderInt(n, 10)) === 0)) {
      loop$n = globalThis.Math.trunc(n / 10);
      loop$position = position + 1;
      loop$acc = acc;
    } else {
      let acc$1 = $int.to_string(n % 10) + acc;
      loop$n = globalThis.Math.trunc(n / 10);
      loop$position = position + 1;
      loop$acc = acc$1;
    }
  }
}

/**
 * Convert the duration to an [ISO8601][1] formatted duration string.
 *
 * The ISO8601 duration format is ambiguous without context due to months and
 * years having different lengths, and because of leap seconds. This function
 * encodes the duration as days, hours, and seconds without any leap seconds.
 * Be sure to take this into account when using the duration strings.
 *
 * [1]: https://en.wikipedia.org/wiki/ISO_8601#Durations
 */
export function to_iso8601_string(duration) {
  let split = (total, limit) => {
    let amount = remainderInt(total, limit);
    let remainder = divideInt((total - amount), limit);
    return [amount, remainder];
  };
  let $ = split(duration.seconds, 60);
  let seconds$1;
  let rest;
  seconds$1 = $[0];
  rest = $[1];
  let $1 = split(rest, 60);
  let minutes$1;
  let rest$1;
  minutes$1 = $1[0];
  rest$1 = $1[1];
  let $2 = split(rest$1, 24);
  let hours$1;
  let rest$2;
  hours$1 = $2[0];
  rest$2 = $2[1];
  let days = rest$2;
  let add$1 = (out, value, unit) => {
    if (value === 0) {
      return out;
    } else {
      return (out + $int.to_string(value)) + unit;
    }
  };
  let _block;
  let _pipe = "P";
  let _pipe$1 = add$1(_pipe, days, "D");
  let _pipe$2 = $string.append(_pipe$1, "T");
  let _pipe$3 = add$1(_pipe$2, hours$1, "H");
  _block = add$1(_pipe$3, minutes$1, "M");
  let output = _block;
  let $3 = duration.nanoseconds;
  if ($3 === 0) {
    if (seconds$1 === 0) {
      return output;
    } else {
      return (output + $int.to_string(seconds$1)) + "S";
    }
  } else {
    let f = nanosecond_digits(duration.nanoseconds, 0, "");
    return (((output + $int.to_string(seconds$1)) + ".") + f) + "S";
  }
}

/**
 * Create a duration of a number of seconds.
 */
export function seconds(amount) {
  return new Duration(amount, 0);
}

/**
 * Create a duration of a number of minutes.
 */
export function minutes(amount) {
  return seconds(amount * 60);
}

/**
 * Create a duration of a number of hours.
 */
export function hours(amount) {
  return seconds(amount * 60 * 60);
}

/**
 * Create a duration of a number of milliseconds.
 */
export function milliseconds(amount) {
  let remainder = amount % 1000;
  let overflow = amount - remainder;
  let nanoseconds$1 = remainder * 1_000_000;
  let seconds$1 = globalThis.Math.trunc(overflow / 1000);
  let _pipe = new Duration(seconds$1, nanoseconds$1);
  return normalise(_pipe);
}

/**
 * Create a duration of a number of nanoseconds.
 *
 * # JavaScript int limitations
 *
 * Remember that JavaScript can only perfectly represent ints between positive
 * and negative 9,007,199,254,740,991! If you use a single call to this
 * function to create durations larger than that number of nanoseconds then
 * you will likely not get exactly the value you expect. Use `seconds` and
 * `milliseconds` as much as possible for large durations.
 */
export function nanoseconds(amount) {
  let _pipe = new Duration(0, amount);
  return normalise(_pipe);
}

/**
 * Convert the duration to a number of seconds.
 *
 * There may be some small loss of precision due to `Duration` being
 * nanosecond accurate and `Float` not being able to represent this.
 */
export function to_seconds(duration) {
  let seconds$1 = $int.to_float(duration.seconds);
  let nanoseconds$1 = $int.to_float(duration.nanoseconds);
  return seconds$1 + (nanoseconds$1 / 1_000_000_000.0);
}

/**
 * Convert the duration to a number of seconds and nanoseconds. There is no
 * loss of precision with this conversion on any target.
 */
export function to_seconds_and_nanoseconds(duration) {
  return [duration.seconds, duration.nanoseconds];
}

export const empty = /* @__PURE__ */ new Duration(0, 0);
