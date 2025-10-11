import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $float from "../../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../../gleam_stdlib/gleam/order.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  remainderInt,
  divideFloat,
  divideInt,
  toBitArray,
  bitArraySlice,
} from "../../gleam.mjs";
import * as $calendar from "../../gleam/time/calendar.mjs";
import * as $duration from "../../gleam/time/duration.mjs";
import { system_time as get_system_time } from "../../gleam_time_ffi.mjs";

class Timestamp extends $CustomType {
  constructor(seconds, nanoseconds) {
    super();
    this.seconds = seconds;
    this.nanoseconds = nanoseconds;
  }
}

/**
 * Ensure the time is represented with `nanoseconds` being positive and less
 * than 1 second.
 *
 * This function does not change the time that the timestamp refers to, it
 * only adjusts the values used to represent the time.
 * 
 * @ignore
 */
function normalise(timestamp) {
  let multiplier = 1_000_000_000;
  let nanoseconds = remainderInt(timestamp.nanoseconds, multiplier);
  let overflow = timestamp.nanoseconds - nanoseconds;
  let seconds = timestamp.seconds + (divideInt(overflow, multiplier));
  let $ = nanoseconds >= 0;
  if ($) {
    return new Timestamp(seconds, nanoseconds);
  } else {
    return new Timestamp(seconds - 1, multiplier + nanoseconds);
  }
}

/**
 * Compare one timestamp to another, indicating whether the first is further
 * into the future (greater) or further into the past (lesser) than the
 * second.
 *
 * # Examples
 *
 * ```gleam
 * compare(from_unix_seconds(1), from_unix_seconds(2))
 * // -> order.Lt
 * ```
 */
export function compare(left, right) {
  return $order.break_tie(
    $int.compare(left.seconds, right.seconds),
    $int.compare(left.nanoseconds, right.nanoseconds),
  );
}

/**
 * Get the current system time.
 *
 * Note this time is not unique or monotonic, it could change at any time or
 * even go backwards! The exact behaviour will depend on the runtime used. See
 * the module documentation for more information.
 *
 * On Erlang this uses [`erlang:system_time/1`][1]. On JavaScript this uses
 * [`Date.now`][2].
 *
 * [1]: https://www.erlang.org/doc/apps/erts/erlang#system_time/1
 * [2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now
 */
export function system_time() {
  let $ = get_system_time();
  let seconds;
  let nanoseconds;
  seconds = $[0];
  nanoseconds = $[1];
  return normalise(new Timestamp(seconds, nanoseconds));
}

/**
 * Calculate the difference between two timestamps.
 *
 * This is effectively substracting the first timestamp from the second.
 *
 * # Examples
 *
 * ```gleam
 * difference(from_unix_seconds(1), from_unix_seconds(5))
 * // -> duration.seconds(4)
 * ```
 */
export function difference(left, right) {
  let seconds = $duration.seconds(right.seconds - left.seconds);
  let nanoseconds = $duration.nanoseconds(right.nanoseconds - left.nanoseconds);
  return $duration.add(seconds, nanoseconds);
}

/**
 * Add a duration to a timestamp.
 *
 * # Examples
 *
 * ```gleam
 * add(from_unix_seconds(1000), duration.seconds(5))
 * // -> from_unix_seconds(1005)
 * ```
 */
export function add(timestamp, duration) {
  let $ = $duration.to_seconds_and_nanoseconds(duration);
  let seconds;
  let nanoseconds;
  seconds = $[0];
  nanoseconds = $[1];
  let _pipe = new Timestamp(
    timestamp.seconds + seconds,
    timestamp.nanoseconds + nanoseconds,
  );
  return normalise(_pipe);
}

function pad_digit(digit, desired_length) {
  let _pipe = $int.to_string(digit);
  return $string.pad_start(_pipe, desired_length, "0");
}

function duration_to_minutes(duration) {
  return $float.round($duration.to_seconds(duration) / 60.0);
}

function modulo(n, m) {
  let $ = $int.modulo(n, m);
  if ($ instanceof Ok) {
    let n$1 = $[0];
    return n$1;
  } else {
    return 0;
  }
}

function floored_div(numerator, denominator) {
  let n = divideFloat($int.to_float(numerator), denominator);
  return $float.round($float.floor(n));
}

function to_civil(minutes) {
  let raw_day = floored_div(minutes, (60.0 * 24.0)) + 719_468;
  let _block;
  let $ = raw_day >= 0;
  if ($) {
    _block = globalThis.Math.trunc(raw_day / 146_097);
  } else {
    _block = globalThis.Math.trunc((raw_day - 146_096) / 146_097);
  }
  let era = _block;
  let day_of_era = raw_day - era * 146_097;
  let year_of_era = globalThis.Math.trunc(
    (((day_of_era - (globalThis.Math.trunc(day_of_era / 1460))) + (globalThis.Math.trunc(
      day_of_era / 36_524
    ))) - (globalThis.Math.trunc(day_of_era / 146_096))) / 365
  );
  let year = year_of_era + era * 400;
  let day_of_year = day_of_era - ((365 * year_of_era + (globalThis.Math.trunc(
    year_of_era / 4
  ))) - (globalThis.Math.trunc(year_of_era / 100)));
  let mp = globalThis.Math.trunc((5 * day_of_year + 2) / 153);
  let _block$1;
  let $1 = mp < 10;
  if ($1) {
    _block$1 = mp + 3;
  } else {
    _block$1 = mp - 9;
  }
  let month = _block$1;
  let day = (day_of_year - (globalThis.Math.trunc((153 * mp + 2) / 5))) + 1;
  let _block$2;
  let $2 = month <= 2;
  if ($2) {
    _block$2 = year + 1;
  } else {
    _block$2 = year;
  }
  let year$1 = _block$2;
  return [year$1, month, day];
}

function to_calendar_from_offset(timestamp, offset) {
  let total = timestamp.seconds + offset * 60;
  let seconds = modulo(total, 60);
  let total_minutes = floored_div(total, 60.0);
  let minutes = globalThis.Math.trunc(modulo(total, 60 * 60) / 60);
  let hours = divideInt(modulo(total, 24 * 60 * 60), 60 * 60);
  let $ = to_civil(total_minutes);
  let year;
  let month;
  let day;
  year = $[0];
  month = $[1];
  day = $[2];
  return [year, month, day, hours, minutes, seconds];
}

/**
 * Convert a `Timestamp` to calendar time, suitable for presenting to a human
 * to read.
 *
 * If you want a machine to use the time value then you should not use this
 * function and should instead keep it as a timestamp. See the documentation
 * for the `gleam/time/calendar` module for more information.
 *
 * # Examples
 *
 * ```gleam
 * timestamp.from_unix_seconds(0)
 * |> timestamp.to_calendar(calendar.utc_offset)
 * // -> #(Date(1970, January, 1), TimeOfDay(0, 0, 0, 0))
 * ```
 */
export function to_calendar(timestamp, offset) {
  let offset$1 = duration_to_minutes(offset);
  let $ = to_calendar_from_offset(timestamp, offset$1);
  let year;
  let month;
  let day;
  let hours;
  let minutes;
  let seconds;
  year = $[0];
  month = $[1];
  day = $[2];
  hours = $[3];
  minutes = $[4];
  seconds = $[5];
  let _block;
  if (month === 1) {
    _block = new $calendar.January();
  } else if (month === 2) {
    _block = new $calendar.February();
  } else if (month === 3) {
    _block = new $calendar.March();
  } else if (month === 4) {
    _block = new $calendar.April();
  } else if (month === 5) {
    _block = new $calendar.May();
  } else if (month === 6) {
    _block = new $calendar.June();
  } else if (month === 7) {
    _block = new $calendar.July();
  } else if (month === 8) {
    _block = new $calendar.August();
  } else if (month === 9) {
    _block = new $calendar.September();
  } else if (month === 10) {
    _block = new $calendar.October();
  } else if (month === 11) {
    _block = new $calendar.November();
  } else {
    _block = new $calendar.December();
  }
  let month$1 = _block;
  let nanoseconds = timestamp.nanoseconds;
  let date = new $calendar.Date(year, month$1, day);
  let time = new $calendar.TimeOfDay(hours, minutes, seconds, nanoseconds);
  return [date, time];
}

function do_remove_trailing_zeros(loop$reversed_digits) {
  while (true) {
    let reversed_digits = loop$reversed_digits;
    if (reversed_digits instanceof $Empty) {
      return reversed_digits;
    } else {
      let digit = reversed_digits.head;
      if (digit === 0) {
        let digits = reversed_digits.tail;
        loop$reversed_digits = digits;
      } else {
        let reversed_digits$1 = reversed_digits;
        return $list.reverse(reversed_digits$1);
      }
    }
  }
}

/**
 * Given a list of digits, return new list with any trailing zeros removed.
 * 
 * @ignore
 */
function remove_trailing_zeros(digits) {
  let reversed_digits = $list.reverse(digits);
  return do_remove_trailing_zeros(reversed_digits);
}

function do_get_zero_padded_digits(loop$number, loop$digits, loop$count) {
  while (true) {
    let number = loop$number;
    let digits = loop$digits;
    let count = loop$count;
    let number$1 = number;
    if ((number$1 <= 0) && (count >= 9)) {
      return digits;
    } else {
      let number$2 = number;
      if (number$2 <= 0) {
        loop$number = number$2;
        loop$digits = listPrepend(0, digits);
        loop$count = count + 1;
      } else {
        let number$3 = number;
        let digit = number$3 % 10;
        let number$4 = floored_div(number$3, 10.0);
        loop$number = number$4;
        loop$digits = listPrepend(digit, digits);
        loop$count = count + 1;
      }
    }
  }
}

/**
 * Returns the list of digits of `number`.  If the number of digits is less 
 * than 9, the result is zero-padded at the front.
 * 
 * @ignore
 */
function get_zero_padded_digits(number) {
  return do_get_zero_padded_digits(number, toList([]), 0);
}

/**
 * Converts nanoseconds into a `String` representation of fractional seconds.
 * 
 * Assumes that `nanoseconds < 1_000_000_000`, which will be true for any 
 * normalised timestamp.
 * 
 * @ignore
 */
function show_second_fraction(nanoseconds) {
  let $ = $int.compare(nanoseconds, 0);
  if ($ instanceof $order.Lt) {
    return "";
  } else if ($ instanceof $order.Eq) {
    return "";
  } else {
    let _block;
    let _pipe = nanoseconds;
    let _pipe$1 = get_zero_padded_digits(_pipe);
    let _pipe$2 = remove_trailing_zeros(_pipe$1);
    let _pipe$3 = $list.map(_pipe$2, $int.to_string);
    _block = $string.join(_pipe$3, "");
    let second_fraction_part = _block;
    return "." + second_fraction_part;
  }
}

/**
 * Convert a timestamp to a RFC 3339 formatted time string, with an offset
 * supplied as an additional argument.
 *
 * The output of this function is also ISO 8601 compatible so long as the
 * offset not negative. Offsets have at-most minute precision, so an offset
 * with higher precision will be rounded to the nearest minute.
 *
 * If you are making an API such as a HTTP JSON API you are encouraged to use
 * Unix timestamps instead of this format or ISO 8601. Unix timestamps are a
 * better choice as they don't contain offset information. Consider:
 *
 * - UTC offsets are not time zones. This does not and cannot tell us the time
 *   zone in which the date was recorded. So what are we supposed to do with
 *   this information?
 * - Users typically want dates formatted according to their local time zone.
 *   What if the provided UTC offset is different from the current user's time
 *   zone? What are we supposed to do with it then?
 * - Despite it being useless (or worse, a source of bugs), the UTC offset
 *   creates a larger payload to transfer.
 *
 * They also uses more memory than a unix timestamp. The way they are better
 * than Unix timestamp is that it is easier for a human to read them, but
 * this is a hinderance that tooling can remedy, and APIs are not primarily
 * for humans.
 *
 * # Examples
 *
 * ```gleam
 * timestamp.from_unix_seconds_and_nanoseconds(1000, 123_000_000)
 * |> to_rfc3339(calendar.utc_offset)
 * // -> "1970-01-01T00:16:40.123Z"
 * ```
 *
 * ```gleam
 * timestamp.from_unix_seconds(1000)
 * |> to_rfc3339(duration.seconds(3600))
 * // -> "1970-01-01T01:16:40+01:00"
 * ```
 */
export function to_rfc3339(timestamp, offset) {
  let offset$1 = duration_to_minutes(offset);
  let $ = to_calendar_from_offset(timestamp, offset$1);
  let years;
  let months;
  let days;
  let hours;
  let minutes;
  let seconds;
  years = $[0];
  months = $[1];
  days = $[2];
  hours = $[3];
  minutes = $[4];
  seconds = $[5];
  let offset_minutes = modulo(offset$1, 60);
  let offset_hours = $int.absolute_value(floored_div(offset$1, 60.0));
  let n2 = (_capture) => { return pad_digit(_capture, 2); };
  let n4 = (_capture) => { return pad_digit(_capture, 4); };
  let out = "";
  let out$1 = ((((out + n4(years)) + "-") + n2(months)) + "-") + n2(days);
  let out$2 = out$1 + "T";
  let out$3 = ((((out$2 + n2(hours)) + ":") + n2(minutes)) + ":") + n2(seconds);
  let out$4 = out$3 + show_second_fraction(timestamp.nanoseconds);
  let $1 = $int.compare(offset$1, 0);
  if ($1 instanceof $order.Lt) {
    return (((out$4 + "-") + n2(offset_hours)) + ":") + n2(offset_minutes);
  } else if ($1 instanceof $order.Eq) {
    return out$4 + "Z";
  } else {
    return (((out$4 + "+") + n2(offset_hours)) + ":") + n2(offset_minutes);
  }
}

function is_leap_year(year) {
  return ((year % 4) === 0) && (((year % 100) !== 0) || ((year % 400) === 0));
}

function parse_sign(bytes) {
  if (bytes.bitSize >= 8) {
    if (bytes.byteAt(0) === 43) {
      if ((bytes.bitSize - 8) % 8 === 0) {
        let remaining_bytes = bitArraySlice(bytes, 8);
        return new Ok(["+", remaining_bytes]);
      } else {
        return new Error(undefined);
      }
    } else if (bytes.byteAt(0) === 45 && (bytes.bitSize - 8) % 8 === 0) {
      let remaining_bytes = bitArraySlice(bytes, 8);
      return new Ok(["-", remaining_bytes]);
    } else {
      return new Error(undefined);
    }
  } else {
    return new Error(undefined);
  }
}

/**
 * Accept the given value from `bytes` and move past it if found.
 * 
 * @ignore
 */
function accept_byte(bytes, value) {
  if (bytes.bitSize >= 8 && (bytes.bitSize - 8) % 8 === 0) {
    let byte = bytes.byteAt(0);
    if (byte === value) {
      let remaining_bytes = bitArraySlice(bytes, 8);
      return new Ok(remaining_bytes);
    } else {
      return new Error(undefined);
    }
  } else {
    return new Error(undefined);
  }
}

function accept_empty(bytes) {
  if (bytes.bitSize === 0) {
    return new Ok(undefined);
  } else {
    return new Error(undefined);
  }
}

/**
 * Note: It is the callers responsibility to ensure the inputs are valid.
 * 
 * See https://www.tondering.dk/claus/cal/julperiod.php#formula
 * 
 * @ignore
 */
function julian_day_from_ymd(year, month, day) {
  let adjustment = globalThis.Math.trunc((14 - month) / 12);
  let adjusted_year = (year + 4800) - adjustment;
  let adjusted_month = (month + 12 * adjustment) - 3;
  return (((((day + (globalThis.Math.trunc((153 * adjusted_month + 2) / 5))) + 365 * adjusted_year) + (globalThis.Math.trunc(
    adjusted_year / 4
  ))) - (globalThis.Math.trunc(adjusted_year / 100))) + (globalThis.Math.trunc(
    adjusted_year / 400
  ))) - 32_045;
}

/**
 * Create a timestamp from a number of seconds since 00:00:00 UTC on 1 January
 * 1970.
 */
export function from_unix_seconds(seconds) {
  return new Timestamp(seconds, 0);
}

/**
 * Create a timestamp from a number of seconds and nanoseconds since 00:00:00
 * UTC on 1 January 1970.
 *
 * # JavaScript int limitations
 *
 * Remember that JavaScript can only perfectly represent ints between positive
 * and negative 9,007,199,254,740,991! If you only use the nanosecond field
 * then you will almost certainly not get the date value you want due to this
 * loss of precision. Always use seconds primarily and then use nanoseconds
 * for the final sub-second adjustment.
 */
export function from_unix_seconds_and_nanoseconds(seconds, nanoseconds) {
  let _pipe = new Timestamp(seconds, nanoseconds);
  return normalise(_pipe);
}

/**
 * Convert the timestamp to a number of seconds since 00:00:00 UTC on 1
 * January 1970.
 *
 * There may be some small loss of precision due to `Timestamp` being
 * nanosecond accurate and `Float` not being able to represent this.
 */
export function to_unix_seconds(timestamp) {
  let seconds = $int.to_float(timestamp.seconds);
  let nanoseconds = $int.to_float(timestamp.nanoseconds);
  return seconds + (nanoseconds / 1_000_000_000.0);
}

/**
 * Convert the timestamp to a number of seconds and nanoseconds since 00:00:00
 * UTC on 1 January 1970. There is no loss of precision with this conversion
 * on any target.
 */
export function to_unix_seconds_and_nanoseconds(timestamp) {
  return [timestamp.seconds, timestamp.nanoseconds];
}

const seconds_per_day = 86_400;

const seconds_per_hour = 3600;

const seconds_per_minute = 60;

function offset_to_seconds(sign, hours, minutes) {
  let abs_seconds = hours * seconds_per_hour + minutes * seconds_per_minute;
  if (sign === "-") {
    return - abs_seconds;
  } else {
    return abs_seconds;
  }
}

/**
 * `julian_seconds_from_parts(year, month, day, hours, minutes, seconds)` 
 * returns the number of Julian 
 * seconds represented by the given arguments.
 * 
 * Note: It is the callers responsibility to ensure the inputs are valid.
 * 
 * See https://www.tondering.dk/claus/cal/julperiod.php#formula
 * 
 * @ignore
 */
function julian_seconds_from_parts(year, month, day, hours, minutes, seconds) {
  let julian_day_seconds = julian_day_from_ymd(year, month, day) * seconds_per_day;
  return ((julian_day_seconds + hours * seconds_per_hour) + minutes * seconds_per_minute) + seconds;
}

const nanoseconds_per_second = 1_000_000_000;

/**
 * The `:` character as a byte
 * 
 * @ignore
 */
const byte_colon = 0x3A;

/**
 * The `-` character as a byte
 * 
 * @ignore
 */
const byte_minus = 0x2D;

/**
 * The `0` character as a byte
 * 
 * @ignore
 */
const byte_zero = 0x30;

/**
 * The `9` character as a byte
 * 
 * @ignore
 */
const byte_nine = 0x39;

function do_parse_second_fraction_as_nanoseconds(
  loop$bytes,
  loop$acc,
  loop$power
) {
  while (true) {
    let bytes = loop$bytes;
    let acc = loop$acc;
    let power = loop$power;
    let power$1 = globalThis.Math.trunc(power / 10);
    if (bytes.bitSize >= 8 && (bytes.bitSize - 8) % 8 === 0) {
      let byte = bytes.byteAt(0);
      if (((0x30 <= byte) && (byte <= 0x39)) && (power$1 < 1)) {
        let remaining_bytes = bitArraySlice(bytes, 8);
        loop$bytes = remaining_bytes;
        loop$acc = acc;
        loop$power = power$1;
      } else {
        let byte$1 = bytes.byteAt(0);
        if ((0x30 <= byte$1) && (byte$1 <= 0x39)) {
          let remaining_bytes = bitArraySlice(bytes, 8);
          let digit = byte$1 - 0x30;
          loop$bytes = remaining_bytes;
          loop$acc = acc + digit * power$1;
          loop$power = power$1;
        } else {
          return new Ok([acc, bytes]);
        }
      }
    } else {
      return new Ok([acc, bytes]);
    }
  }
}

function parse_second_fraction_as_nanoseconds(bytes) {
  if (bytes.bitSize >= 8 && bytes.byteAt(0) === 46) {
    if (bytes.bitSize >= 16 && (bytes.bitSize - 16) % 8 === 0) {
      let byte = bytes.byteAt(1);
      if ((0x30 <= byte) && (byte <= 0x39)) {
        let remaining_bytes = bitArraySlice(bytes, 16);
        return do_parse_second_fraction_as_nanoseconds(
          toBitArray([byte, remaining_bytes]),
          0,
          nanoseconds_per_second,
        );
      } else if ((bytes.bitSize - 8) % 8 === 0) {
        return new Error(undefined);
      } else {
        return new Ok([0, bytes]);
      }
    } else if ((bytes.bitSize - 8) % 8 === 0) {
      return new Error(undefined);
    } else {
      return new Ok([0, bytes]);
    }
  } else {
    return new Ok([0, bytes]);
  }
}

function do_parse_digits(loop$bytes, loop$count, loop$acc, loop$k) {
  while (true) {
    let bytes = loop$bytes;
    let count = loop$count;
    let acc = loop$acc;
    let k = loop$k;
    if (k >= count) {
      return new Ok([acc, bytes]);
    } else if (bytes.bitSize >= 8 && (bytes.bitSize - 8) % 8 === 0) {
      let byte = bytes.byteAt(0);
      if ((0x30 <= byte) && (byte <= 0x39)) {
        let remaining_bytes = bitArraySlice(bytes, 8);
        loop$bytes = remaining_bytes;
        loop$count = count;
        loop$acc = acc * 10 + (byte - 0x30);
        loop$k = k + 1;
      } else {
        return new Error(undefined);
      }
    } else {
      return new Error(undefined);
    }
  }
}

/**
 * Parse and return the given number of digits from the given bytes.
 * 
 * @ignore
 */
function parse_digits(bytes, count) {
  return do_parse_digits(bytes, count, 0, 0);
}

function parse_year(bytes) {
  return parse_digits(bytes, 4);
}

function parse_month(bytes) {
  return $result.try$(
    parse_digits(bytes, 2),
    (_use0) => {
      let month;
      let bytes$1;
      month = _use0[0];
      bytes$1 = _use0[1];
      let $ = (1 <= month) && (month <= 12);
      if ($) {
        return new Ok([month, bytes$1]);
      } else {
        return new Error(undefined);
      }
    },
  );
}

function parse_day(bytes, year, month) {
  return $result.try$(
    parse_digits(bytes, 2),
    (_use0) => {
      let day;
      let bytes$1;
      day = _use0[0];
      bytes$1 = _use0[1];
      return $result.try$(
        (() => {
          if (month === 1) {
            return new Ok(31);
          } else if (month === 3) {
            return new Ok(31);
          } else if (month === 5) {
            return new Ok(31);
          } else if (month === 7) {
            return new Ok(31);
          } else if (month === 8) {
            return new Ok(31);
          } else if (month === 10) {
            return new Ok(31);
          } else if (month === 12) {
            return new Ok(31);
          } else if (month === 4) {
            return new Ok(30);
          } else if (month === 6) {
            return new Ok(30);
          } else if (month === 9) {
            return new Ok(30);
          } else if (month === 11) {
            return new Ok(30);
          } else if (month === 2) {
            let $ = is_leap_year(year);
            if ($) {
              return new Ok(29);
            } else {
              return new Ok(28);
            }
          } else {
            return new Error(undefined);
          }
        })(),
        (max_day) => {
          let $ = (1 <= day) && (day <= max_day);
          if ($) {
            return new Ok([day, bytes$1]);
          } else {
            return new Error(undefined);
          }
        },
      );
    },
  );
}

function parse_hours(bytes) {
  return $result.try$(
    parse_digits(bytes, 2),
    (_use0) => {
      let hours;
      let bytes$1;
      hours = _use0[0];
      bytes$1 = _use0[1];
      let $ = (0 <= hours) && (hours <= 23);
      if ($) {
        return new Ok([hours, bytes$1]);
      } else {
        return new Error(undefined);
      }
    },
  );
}

function parse_minutes(bytes) {
  return $result.try$(
    parse_digits(bytes, 2),
    (_use0) => {
      let minutes;
      let bytes$1;
      minutes = _use0[0];
      bytes$1 = _use0[1];
      let $ = (0 <= minutes) && (minutes <= 59);
      if ($) {
        return new Ok([minutes, bytes$1]);
      } else {
        return new Error(undefined);
      }
    },
  );
}

function parse_seconds(bytes) {
  return $result.try$(
    parse_digits(bytes, 2),
    (_use0) => {
      let seconds;
      let bytes$1;
      seconds = _use0[0];
      bytes$1 = _use0[1];
      let $ = (0 <= seconds) && (seconds <= 60);
      if ($) {
        return new Ok([seconds, bytes$1]);
      } else {
        return new Error(undefined);
      }
    },
  );
}

function parse_numeric_offset(bytes) {
  return $result.try$(
    parse_sign(bytes),
    (_use0) => {
      let sign;
      let bytes$1;
      sign = _use0[0];
      bytes$1 = _use0[1];
      return $result.try$(
        parse_hours(bytes$1),
        (_use0) => {
          let hours;
          let bytes$2;
          hours = _use0[0];
          bytes$2 = _use0[1];
          return $result.try$(
            accept_byte(bytes$2, byte_colon),
            (bytes) => {
              return $result.try$(
                parse_minutes(bytes),
                (_use0) => {
                  let minutes;
                  let bytes$1;
                  minutes = _use0[0];
                  bytes$1 = _use0[1];
                  let offset_seconds = offset_to_seconds(sign, hours, minutes);
                  return new Ok([offset_seconds, bytes$1]);
                },
              );
            },
          );
        },
      );
    },
  );
}

function parse_offset(bytes) {
  if (bytes.bitSize >= 8) {
    if (bytes.byteAt(0) === 90) {
      if ((bytes.bitSize - 8) % 8 === 0) {
        let remaining_bytes = bitArraySlice(bytes, 8);
        return new Ok([0, remaining_bytes]);
      } else {
        return parse_numeric_offset(bytes);
      }
    } else if (bytes.byteAt(0) === 122 && (bytes.bitSize - 8) % 8 === 0) {
      let remaining_bytes = bitArraySlice(bytes, 8);
      return new Ok([0, remaining_bytes]);
    } else {
      return parse_numeric_offset(bytes);
    }
  } else {
    return parse_numeric_offset(bytes);
  }
}

/**
 * The `t` character as a byte
 * 
 * @ignore
 */
const byte_t_lowercase = 0x74;

/**
 * The `T` character as a byte
 * 
 * @ignore
 */
const byte_t_uppercase = 0x54;

/**
 * The `T` character as a byte
 * 
 * @ignore
 */
const byte_space = 0x20;

function accept_date_time_separator(bytes) {
  if (bytes.bitSize >= 8 && (bytes.bitSize - 8) % 8 === 0) {
    let byte = bytes.byteAt(0);
    if (((byte === 0x54) || (byte === 0x74)) || (byte === 0x20)) {
      let remaining_bytes = bitArraySlice(bytes, 8);
      return new Ok(remaining_bytes);
    } else {
      return new Error(undefined);
    }
  } else {
    return new Error(undefined);
  }
}

/**
 * The Julian seconds of the UNIX epoch (Julian day is 2_440_588)
 * 
 * @ignore
 */
const julian_seconds_unix_epoch = 210_866_803_200;

/**
 * Note: The caller of this function must ensure that all inputs are valid.
 * 
 * @ignore
 */
function from_date_time(
  year,
  month,
  day,
  hours,
  minutes,
  seconds,
  second_fraction_as_nanoseconds,
  offset_seconds
) {
  let julian_seconds = julian_seconds_from_parts(
    year,
    month,
    day,
    hours,
    minutes,
    seconds,
  );
  let julian_seconds_since_epoch = julian_seconds - julian_seconds_unix_epoch;
  let _pipe = new Timestamp(
    julian_seconds_since_epoch - offset_seconds,
    second_fraction_as_nanoseconds,
  );
  return normalise(_pipe);
}

/**
 * Create a `Timestamp` from a human-readable calendar time.
 *
 * # Examples
 *
 * ```gleam
 * timestamp.from_calendar(
 *   date: calendar.Date(2024, calendar.December, 25),
 *   time: calendar.TimeOfDay(12, 30, 50, 0),
 *   offset: calendar.utc_offset,
 * )
 * |> timestamp.to_rfc3339(calendar.utc_offset)
 * // -> "2024-12-25T12:30:50Z"
 * ```
 */
export function from_calendar(date, time, offset) {
  let _block;
  let $ = date.month;
  if ($ instanceof $calendar.January) {
    _block = 1;
  } else if ($ instanceof $calendar.February) {
    _block = 2;
  } else if ($ instanceof $calendar.March) {
    _block = 3;
  } else if ($ instanceof $calendar.April) {
    _block = 4;
  } else if ($ instanceof $calendar.May) {
    _block = 5;
  } else if ($ instanceof $calendar.June) {
    _block = 6;
  } else if ($ instanceof $calendar.July) {
    _block = 7;
  } else if ($ instanceof $calendar.August) {
    _block = 8;
  } else if ($ instanceof $calendar.September) {
    _block = 9;
  } else if ($ instanceof $calendar.October) {
    _block = 10;
  } else if ($ instanceof $calendar.November) {
    _block = 11;
  } else {
    _block = 12;
  }
  let month = _block;
  return from_date_time(
    date.year,
    month,
    date.day,
    time.hours,
    time.minutes,
    time.seconds,
    time.nanoseconds,
    $float.round($duration.to_seconds(offset)),
  );
}

/**
 * Parses an [RFC 3339 formatted time string][spec] into a `Timestamp`.
 *
 * [spec]: https://datatracker.ietf.org/doc/html/rfc3339#section-5.6
 * 
 * # Examples
 *
 * ```gleam
 * let assert Ok(ts) = timestamp.parse_rfc3339("1970-01-01T00:00:01Z")
 * timestamp.to_unix_seconds_and_nanoseconds(ts)
 * // -> #(1, 0)
 * ```
 * 
 * Parsing an invalid timestamp returns an error.
 * 
 * ```gleam
 * let assert Error(Nil) = timestamp.parse_rfc3339("1995-10-31")
 * ```
 *
 * # Notes
 * 
 * - Follows the grammar specified in section 5.6 Internet Date/Time Format of 
 *   RFC 3339 <https://datatracker.ietf.org/doc/html/rfc3339#section-5.6>.
 * - The `T` and `Z` characters may alternatively be lower case `t` or `z`, 
 *   respectively.
 * - Full dates and full times must be separated by `T` or `t`. A space is also 
 *   permitted.
 * - Leap seconds rules are not considered.  That is, any timestamp may 
 *   specify digts `00` - `60` for the seconds.
 * - Any part of a fractional second that cannot be represented in the 
 *   nanosecond precision is tructated.  That is, for the time string, 
 *   `"1970-01-01T00:00:00.1234567899Z"`, the fractional second `.1234567899` 
 *   will be represented as `123_456_789` in the `Timestamp`.
 */
export function parse_rfc3339(input) {
  let bytes = $bit_array.from_string(input);
  return $result.try$(
    parse_year(bytes),
    (_use0) => {
      let year;
      let bytes$1;
      year = _use0[0];
      bytes$1 = _use0[1];
      return $result.try$(
        accept_byte(bytes$1, byte_minus),
        (bytes) => {
          return $result.try$(
            parse_month(bytes),
            (_use0) => {
              let month;
              let bytes$1;
              month = _use0[0];
              bytes$1 = _use0[1];
              return $result.try$(
                accept_byte(bytes$1, byte_minus),
                (bytes) => {
                  return $result.try$(
                    parse_day(bytes, year, month),
                    (_use0) => {
                      let day;
                      let bytes$1;
                      day = _use0[0];
                      bytes$1 = _use0[1];
                      return $result.try$(
                        accept_date_time_separator(bytes$1),
                        (bytes) => {
                          return $result.try$(
                            parse_hours(bytes),
                            (_use0) => {
                              let hours;
                              let bytes$1;
                              hours = _use0[0];
                              bytes$1 = _use0[1];
                              return $result.try$(
                                accept_byte(bytes$1, byte_colon),
                                (bytes) => {
                                  return $result.try$(
                                    parse_minutes(bytes),
                                    (_use0) => {
                                      let minutes;
                                      let bytes$1;
                                      minutes = _use0[0];
                                      bytes$1 = _use0[1];
                                      return $result.try$(
                                        accept_byte(bytes$1, byte_colon),
                                        (bytes) => {
                                          return $result.try$(
                                            parse_seconds(bytes),
                                            (_use0) => {
                                              let seconds;
                                              let bytes$1;
                                              seconds = _use0[0];
                                              bytes$1 = _use0[1];
                                              return $result.try$(
                                                parse_second_fraction_as_nanoseconds(
                                                  bytes$1,
                                                ),
                                                (_use0) => {
                                                  let second_fraction_as_nanoseconds;
                                                  let bytes$2;
                                                  second_fraction_as_nanoseconds = _use0[0];
                                                  bytes$2 = _use0[1];
                                                  return $result.try$(
                                                    parse_offset(bytes$2),
                                                    (_use0) => {
                                                      let offset_seconds;
                                                      let bytes$3;
                                                      offset_seconds = _use0[0];
                                                      bytes$3 = _use0[1];
                                                      return $result.try$(
                                                        accept_empty(bytes$3),
                                                        (_use0) => {
                                                          
                                                          return new Ok(
                                                            from_date_time(
                                                              year,
                                                              month,
                                                              day,
                                                              hours,
                                                              minutes,
                                                              seconds,
                                                              second_fraction_as_nanoseconds,
                                                              offset_seconds,
                                                            ),
                                                          );
                                                        },
                                                      );
                                                    },
                                                  );
                                                },
                                              );
                                            },
                                          );
                                        },
                                      );
                                    },
                                  );
                                },
                              );
                            },
                          );
                        },
                      );
                    },
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}
