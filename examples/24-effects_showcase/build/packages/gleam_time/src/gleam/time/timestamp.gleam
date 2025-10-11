//// Welcome to the timestamp module! This module and its `Timestamp` type are
//// what you will be using most commonly when working with time in Gleam.
////
//// A timestamp represents a moment in time, represented as an amount of time
//// since the calendar time 00:00:00 UTC on 1 January 1970, also known as the
//// _Unix epoch_.
////
//// # Wall clock time and monotonicity
////
//// Time is very complicated, especially on computers! While they generally do
//// a good job of keeping track of what the time is, computers can get
//// out-of-sync and start to report a time that is too late or too early. Most
//// computers use "network time protocol" to tell each other what they think
//// the time is, and computers that realise they are running too fast or too
//// slow will adjust their clock to correct it. When this happens it can seem
//// to your program that the current time has changed, and it may have even
//// jumped backwards in time!
////
//// This measure of time is called _wall clock time_, and it is what people
//// commonly think of when they think of time. It is important to be aware that
//// it can go backwards, and your program must not rely on it only ever going
//// forwards at a steady rate. For example, for tracking what order events happen
//// in. 
////
//// This module uses wall clock time. If your program needs time values to always
//// increase you will need a _monotonic_ time instead. It's uncommon that you
//// would need monotonic time, one example might be if you're making a
//// benchmarking framework.
////
//// The exact way that time works will depend on what runtime you use. The
//// Erlang documentation on time has a lot of detail about time generally as well
//// as how it works on the BEAM, it is worth reading.
//// <https://www.erlang.org/doc/apps/erts/time_correction>.
////
//// # Converting to local time
////
//// Timestamps don't take into account time zones, so a moment in time will
//// have the same timestamp value regardless of where you are in the world. To
//// convert them to local time you will need to know the offset for the time
//// zone you wish to use, likely from a time zone database. See the
//// `gleam/time/calendar` module for more information.
////

import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string
import gleam/time/calendar
import gleam/time/duration.{type Duration}

const seconds_per_day: Int = 86_400

const seconds_per_hour: Int = 3600

const seconds_per_minute: Int = 60

const nanoseconds_per_second: Int = 1_000_000_000

/// The `:` character as a byte
const byte_colon: Int = 0x3A

/// The `-` character as a byte
const byte_minus: Int = 0x2D

/// The `0` character as a byte
const byte_zero: Int = 0x30

/// The `9` character as a byte
const byte_nine: Int = 0x39

/// The `t` character as a byte
const byte_t_lowercase: Int = 0x74

/// The `T` character as a byte
const byte_t_uppercase: Int = 0x54

/// The `T` character as a byte
const byte_space: Int = 0x20

/// The Julian seconds of the UNIX epoch (Julian day is 2_440_588)
const julian_seconds_unix_epoch: Int = 210_866_803_200

/// The main time type, which you should favour over other types such as
/// calendar time types. It is efficient, unambiguous, and it is not possible
/// to construct an invalid timestamp.
///
/// The most common situation in which you may need a different time data
/// structure is when you need to display time to human for them to read. When
/// you need to do this convert the timestamp to calendar time when presenting
/// it, but internally always keep the time as a timestamp.
///
pub opaque type Timestamp {
  // When compiling to JavaScript ints have limited precision and size. This
  // means that if we were to store the the timestamp in a single int the
  // timestamp would not be able to represent times far in the future or in the
  // past, or distinguish between two times that are close together. Timestamps
  // are instead represented as a number of seconds and a number of nanoseconds.
  //
  // If you have manually adjusted the seconds and nanoseconds values the
  // `normalise` function can be used to ensure the time is represented the
  // intended way, with `nanoseconds` being positive and less than 1 second.
  //
  // The timestamp is the sum of the seconds and the nanoseconds.
  Timestamp(seconds: Int, nanoseconds: Int)
}

/// Ensure the time is represented with `nanoseconds` being positive and less
/// than 1 second.
///
/// This function does not change the time that the timestamp refers to, it
/// only adjusts the values used to represent the time.
///
fn normalise(timestamp: Timestamp) -> Timestamp {
  let multiplier = 1_000_000_000
  let nanoseconds = timestamp.nanoseconds % multiplier
  let overflow = timestamp.nanoseconds - nanoseconds
  let seconds = timestamp.seconds + overflow / multiplier
  case nanoseconds >= 0 {
    True -> Timestamp(seconds, nanoseconds)
    False -> Timestamp(seconds - 1, multiplier + nanoseconds)
  }
}

/// Compare one timestamp to another, indicating whether the first is further
/// into the future (greater) or further into the past (lesser) than the
/// second.
///
/// # Examples
///
/// ```gleam
/// compare(from_unix_seconds(1), from_unix_seconds(2))
/// // -> order.Lt
/// ```
///
pub fn compare(left: Timestamp, right: Timestamp) -> order.Order {
  order.break_tie(
    int.compare(left.seconds, right.seconds),
    int.compare(left.nanoseconds, right.nanoseconds),
  )
}

/// Get the current system time.
///
/// Note this time is not unique or monotonic, it could change at any time or
/// even go backwards! The exact behaviour will depend on the runtime used. See
/// the module documentation for more information.
///
/// On Erlang this uses [`erlang:system_time/1`][1]. On JavaScript this uses
/// [`Date.now`][2].
///
/// [1]: https://www.erlang.org/doc/apps/erts/erlang#system_time/1
/// [2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now
///
pub fn system_time() -> Timestamp {
  let #(seconds, nanoseconds) = get_system_time()
  normalise(Timestamp(seconds, nanoseconds))
}

@external(erlang, "gleam_time_ffi", "system_time")
@external(javascript, "../../gleam_time_ffi.mjs", "system_time")
fn get_system_time() -> #(Int, Int)

/// Calculate the difference between two timestamps.
///
/// This is effectively substracting the first timestamp from the second.
///
/// # Examples
///
/// ```gleam
/// difference(from_unix_seconds(1), from_unix_seconds(5))
/// // -> duration.seconds(4)
/// ```
///
pub fn difference(left: Timestamp, right: Timestamp) -> Duration {
  let seconds = duration.seconds(right.seconds - left.seconds)
  let nanoseconds = duration.nanoseconds(right.nanoseconds - left.nanoseconds)
  duration.add(seconds, nanoseconds)
}

/// Add a duration to a timestamp.
///
/// # Examples
///
/// ```gleam
/// add(from_unix_seconds(1000), duration.seconds(5))
/// // -> from_unix_seconds(1005)
/// ```
///
pub fn add(timestamp: Timestamp, duration: Duration) -> Timestamp {
  let #(seconds, nanoseconds) = duration.to_seconds_and_nanoseconds(duration)
  Timestamp(timestamp.seconds + seconds, timestamp.nanoseconds + nanoseconds)
  |> normalise
}

/// Convert a timestamp to a RFC 3339 formatted time string, with an offset
/// supplied as an additional argument.
///
/// The output of this function is also ISO 8601 compatible so long as the
/// offset not negative. Offsets have at-most minute precision, so an offset
/// with higher precision will be rounded to the nearest minute.
///
/// If you are making an API such as a HTTP JSON API you are encouraged to use
/// Unix timestamps instead of this format or ISO 8601. Unix timestamps are a
/// better choice as they don't contain offset information. Consider:
///
/// - UTC offsets are not time zones. This does not and cannot tell us the time
///   zone in which the date was recorded. So what are we supposed to do with
///   this information?
/// - Users typically want dates formatted according to their local time zone.
///   What if the provided UTC offset is different from the current user's time
///   zone? What are we supposed to do with it then?
/// - Despite it being useless (or worse, a source of bugs), the UTC offset
///   creates a larger payload to transfer.
///
/// They also uses more memory than a unix timestamp. The way they are better
/// than Unix timestamp is that it is easier for a human to read them, but
/// this is a hinderance that tooling can remedy, and APIs are not primarily
/// for humans.
///
/// # Examples
///
/// ```gleam
/// timestamp.from_unix_seconds_and_nanoseconds(1000, 123_000_000)
/// |> to_rfc3339(calendar.utc_offset)
/// // -> "1970-01-01T00:16:40.123Z"
/// ```
///
/// ```gleam
/// timestamp.from_unix_seconds(1000)
/// |> to_rfc3339(duration.seconds(3600))
/// // -> "1970-01-01T01:16:40+01:00"
/// ```
///
pub fn to_rfc3339(timestamp: Timestamp, offset: Duration) -> String {
  let offset = duration_to_minutes(offset)
  let #(years, months, days, hours, minutes, seconds) =
    to_calendar_from_offset(timestamp, offset)

  let offset_minutes = modulo(offset, 60)
  let offset_hours = int.absolute_value(floored_div(offset, 60.0))

  let n2 = pad_digit(_, to: 2)
  let n4 = pad_digit(_, to: 4)
  let out = ""
  let out = out <> n4(years) <> "-" <> n2(months) <> "-" <> n2(days)
  let out = out <> "T"
  let out = out <> n2(hours) <> ":" <> n2(minutes) <> ":" <> n2(seconds)
  let out = out <> show_second_fraction(timestamp.nanoseconds)
  case int.compare(offset, 0) {
    order.Eq -> out <> "Z"
    order.Gt -> out <> "+" <> n2(offset_hours) <> ":" <> n2(offset_minutes)
    order.Lt -> out <> "-" <> n2(offset_hours) <> ":" <> n2(offset_minutes)
  }
}

fn pad_digit(digit: Int, to desired_length: Int) -> String {
  int.to_string(digit) |> string.pad_start(desired_length, "0")
}

/// Convert a `Timestamp` to calendar time, suitable for presenting to a human
/// to read.
///
/// If you want a machine to use the time value then you should not use this
/// function and should instead keep it as a timestamp. See the documentation
/// for the `gleam/time/calendar` module for more information.
///
/// # Examples
///
/// ```gleam
/// timestamp.from_unix_seconds(0)
/// |> timestamp.to_calendar(calendar.utc_offset)
/// // -> #(Date(1970, January, 1), TimeOfDay(0, 0, 0, 0))
/// ```
///
pub fn to_calendar(
  timestamp: Timestamp,
  offset: Duration,
) -> #(calendar.Date, calendar.TimeOfDay) {
  let offset = duration_to_minutes(offset)
  let #(year, month, day, hours, minutes, seconds) =
    to_calendar_from_offset(timestamp, offset)
  let month = case month {
    1 -> calendar.January
    2 -> calendar.February
    3 -> calendar.March
    4 -> calendar.April
    5 -> calendar.May
    6 -> calendar.June
    7 -> calendar.July
    8 -> calendar.August
    9 -> calendar.September
    10 -> calendar.October
    11 -> calendar.November
    _ -> calendar.December
  }
  let nanoseconds = timestamp.nanoseconds
  let date = calendar.Date(year:, month:, day:)
  let time = calendar.TimeOfDay(hours:, minutes:, seconds:, nanoseconds:)
  #(date, time)
}

fn duration_to_minutes(duration: duration.Duration) -> Int {
  float.round(duration.to_seconds(duration) /. 60.0)
}

fn to_calendar_from_offset(
  timestamp: Timestamp,
  offset: Int,
) -> #(Int, Int, Int, Int, Int, Int) {
  let total = timestamp.seconds + { offset * 60 }
  let seconds = modulo(total, 60)
  let total_minutes = floored_div(total, 60.0)
  let minutes = modulo(total, 60 * 60) / 60
  let hours = modulo(total, 24 * 60 * 60) / { 60 * 60 }
  let #(year, month, day) = to_civil(total_minutes)
  #(year, month, day, hours, minutes, seconds)
}

/// Create a `Timestamp` from a human-readable calendar time.
///
/// # Examples
///
/// ```gleam
/// timestamp.from_calendar(
///   date: calendar.Date(2024, calendar.December, 25),
///   time: calendar.TimeOfDay(12, 30, 50, 0),
///   offset: calendar.utc_offset,
/// )
/// |> timestamp.to_rfc3339(calendar.utc_offset)
/// // -> "2024-12-25T12:30:50Z"
/// ```
///
pub fn from_calendar(
  date date: calendar.Date,
  time time: calendar.TimeOfDay,
  offset offset: Duration,
) -> Timestamp {
  let month = case date.month {
    calendar.January -> 1
    calendar.February -> 2
    calendar.March -> 3
    calendar.April -> 4
    calendar.May -> 5
    calendar.June -> 6
    calendar.July -> 7
    calendar.August -> 8
    calendar.September -> 9
    calendar.October -> 10
    calendar.November -> 11
    calendar.December -> 12
  }
  from_date_time(
    year: date.year,
    month:,
    day: date.day,
    hours: time.hours,
    minutes: time.minutes,
    seconds: time.seconds,
    second_fraction_as_nanoseconds: time.nanoseconds,
    offset_seconds: float.round(duration.to_seconds(offset)),
  )
}

fn modulo(n: Int, m: Int) -> Int {
  case int.modulo(n, m) {
    Ok(n) -> n
    Error(_) -> 0
  }
}

fn floored_div(numerator: Int, denominator: Float) -> Int {
  let n = int.to_float(numerator) /. denominator
  float.round(float.floor(n))
}

// Adapted from Elm's Time module
fn to_civil(minutes: Int) -> #(Int, Int, Int) {
  let raw_day = floored_div(minutes, { 60.0 *. 24.0 }) + 719_468
  let era = case raw_day >= 0 {
    True -> raw_day / 146_097
    False -> { raw_day - 146_096 } / 146_097
  }
  let day_of_era = raw_day - era * 146_097
  let year_of_era =
    {
      day_of_era
      - { day_of_era / 1460 }
      + { day_of_era / 36_524 }
      - { day_of_era / 146_096 }
    }
    / 365
  let year = year_of_era + era * 400
  let day_of_year =
    day_of_era
    - { 365 * year_of_era + { year_of_era / 4 } - { year_of_era / 100 } }
  let mp = { 5 * day_of_year + 2 } / 153
  let month = case mp < 10 {
    True -> mp + 3
    False -> mp - 9
  }
  let day = day_of_year - { 153 * mp + 2 } / 5 + 1
  let year = case month <= 2 {
    True -> year + 1
    False -> year
  }
  #(year, month, day)
}

/// Converts nanoseconds into a `String` representation of fractional seconds.
/// 
/// Assumes that `nanoseconds < 1_000_000_000`, which will be true for any 
/// normalised timestamp.
/// 
fn show_second_fraction(nanoseconds: Int) -> String {
  case int.compare(nanoseconds, 0) {
    // Zero fractional seconds are not shown.
    order.Lt | order.Eq -> ""
    order.Gt -> {
      let second_fraction_part = {
        nanoseconds
        |> get_zero_padded_digits
        |> remove_trailing_zeros
        |> list.map(int.to_string)
        |> string.join("")
      }

      "." <> second_fraction_part
    }
  }
}

/// Given a list of digits, return new list with any trailing zeros removed.
/// 
fn remove_trailing_zeros(digits: List(Int)) -> List(Int) {
  let reversed_digits = list.reverse(digits)

  do_remove_trailing_zeros(reversed_digits)
}

fn do_remove_trailing_zeros(reversed_digits) {
  case reversed_digits {
    [] -> []
    [digit, ..digits] if digit == 0 -> do_remove_trailing_zeros(digits)
    reversed_digits -> list.reverse(reversed_digits)
  }
}

/// Returns the list of digits of `number`.  If the number of digits is less 
/// than 9, the result is zero-padded at the front.
/// 
fn get_zero_padded_digits(number: Int) -> List(Int) {
  do_get_zero_padded_digits(number, [], 0)
}

fn do_get_zero_padded_digits(
  number: Int,
  digits: List(Int),
  count: Int,
) -> List(Int) {
  case number {
    number if number <= 0 && count >= 9 -> digits
    number if number <= 0 ->
      // Zero-pad the digits at the front until we have at least 9 digits.
      do_get_zero_padded_digits(number, [0, ..digits], count + 1)
    number -> {
      let digit = number % 10
      let number = floored_div(number, 10.0)
      do_get_zero_padded_digits(number, [digit, ..digits], count + 1)
    }
  }
}

/// Parses an [RFC 3339 formatted time string][spec] into a `Timestamp`.
///
/// [spec]: https://datatracker.ietf.org/doc/html/rfc3339#section-5.6
/// 
/// # Examples
///
/// ```gleam
/// let assert Ok(ts) = timestamp.parse_rfc3339("1970-01-01T00:00:01Z")
/// timestamp.to_unix_seconds_and_nanoseconds(ts)
/// // -> #(1, 0)
/// ```
/// 
/// Parsing an invalid timestamp returns an error.
/// 
/// ```gleam
/// let assert Error(Nil) = timestamp.parse_rfc3339("1995-10-31")
/// ```
///
/// # Notes
/// 
/// - Follows the grammar specified in section 5.6 Internet Date/Time Format of 
///   RFC 3339 <https://datatracker.ietf.org/doc/html/rfc3339#section-5.6>.
/// - The `T` and `Z` characters may alternatively be lower case `t` or `z`, 
///   respectively.
/// - Full dates and full times must be separated by `T` or `t`. A space is also 
///   permitted.
/// - Leap seconds rules are not considered.  That is, any timestamp may 
///   specify digts `00` - `60` for the seconds.
/// - Any part of a fractional second that cannot be represented in the 
///   nanosecond precision is tructated.  That is, for the time string, 
///   `"1970-01-01T00:00:00.1234567899Z"`, the fractional second `.1234567899` 
///   will be represented as `123_456_789` in the `Timestamp`.
/// 
pub fn parse_rfc3339(input: String) -> Result(Timestamp, Nil) {
  let bytes = bit_array.from_string(input)

  // Date 
  use #(year, bytes) <- result.try(parse_year(from: bytes))
  use bytes <- result.try(accept_byte(from: bytes, value: byte_minus))
  use #(month, bytes) <- result.try(parse_month(from: bytes))
  use bytes <- result.try(accept_byte(from: bytes, value: byte_minus))
  use #(day, bytes) <- result.try(parse_day(from: bytes, year:, month:))

  use bytes <- result.try(accept_date_time_separator(from: bytes))

  // Time 
  use #(hours, bytes) <- result.try(parse_hours(from: bytes))
  use bytes <- result.try(accept_byte(from: bytes, value: byte_colon))
  use #(minutes, bytes) <- result.try(parse_minutes(from: bytes))
  use bytes <- result.try(accept_byte(from: bytes, value: byte_colon))
  use #(seconds, bytes) <- result.try(parse_seconds(from: bytes))
  use #(second_fraction_as_nanoseconds, bytes) <- result.try(
    parse_second_fraction_as_nanoseconds(from: bytes),
  )

  // Offset
  use #(offset_seconds, bytes) <- result.try(parse_offset(from: bytes))

  // Done
  use Nil <- result.try(accept_empty(bytes))

  Ok(from_date_time(
    year:,
    month:,
    day:,
    hours:,
    minutes:,
    seconds:,
    second_fraction_as_nanoseconds:,
    offset_seconds:,
  ))
}

fn parse_year(from bytes: BitArray) -> Result(#(Int, BitArray), Nil) {
  parse_digits(from: bytes, count: 4)
}

fn parse_month(from bytes: BitArray) -> Result(#(Int, BitArray), Nil) {
  use #(month, bytes) <- result.try(parse_digits(from: bytes, count: 2))
  case 1 <= month && month <= 12 {
    True -> Ok(#(month, bytes))
    False -> Error(Nil)
  }
}

fn parse_day(
  from bytes: BitArray,
  year year,
  month month,
) -> Result(#(Int, BitArray), Nil) {
  use #(day, bytes) <- result.try(parse_digits(from: bytes, count: 2))

  use max_day <- result.try(case month {
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> Ok(31)
    4 | 6 | 9 | 11 -> Ok(30)
    2 -> {
      case is_leap_year(year) {
        True -> Ok(29)
        False -> Ok(28)
      }
    }
    _ -> Error(Nil)
  })

  case 1 <= day && day <= max_day {
    True -> Ok(#(day, bytes))
    False -> Error(Nil)
  }
}

// Implementation from RFC 3339 Appendix C
fn is_leap_year(year: Int) -> Bool {
  year % 4 == 0 && { year % 100 != 0 || year % 400 == 0 }
}

fn parse_hours(from bytes: BitArray) -> Result(#(Int, BitArray), Nil) {
  use #(hours, bytes) <- result.try(parse_digits(from: bytes, count: 2))
  case 0 <= hours && hours <= 23 {
    True -> Ok(#(hours, bytes))
    False -> Error(Nil)
  }
}

fn parse_minutes(from bytes: BitArray) -> Result(#(Int, BitArray), Nil) {
  use #(minutes, bytes) <- result.try(parse_digits(from: bytes, count: 2))
  case 0 <= minutes && minutes <= 59 {
    True -> Ok(#(minutes, bytes))
    False -> Error(Nil)
  }
}

fn parse_seconds(from bytes: BitArray) -> Result(#(Int, BitArray), Nil) {
  use #(seconds, bytes) <- result.try(parse_digits(from: bytes, count: 2))
  // Max of 60 for leap seconds.  We don't bother to check if this leap second
  // actually occurred in the past or not.
  case 0 <= seconds && seconds <= 60 {
    True -> Ok(#(seconds, bytes))
    False -> Error(Nil)
  }
}

// Truncates any part of the fraction that is beyond the nanosecond precision.
fn parse_second_fraction_as_nanoseconds(from bytes: BitArray) {
  case bytes {
    <<".", byte, remaining_bytes:bytes>>
      if byte_zero <= byte && byte <= byte_nine
    -> {
      do_parse_second_fraction_as_nanoseconds(
        from: <<byte, remaining_bytes:bits>>,
        acc: 0,
        power: nanoseconds_per_second,
      )
    }
    // bytes starts with a ".", which should introduce a fraction, but it does
    // not, and so it is an ill-formed input.
    <<".", _:bytes>> -> Error(Nil)
    // bytes does not start with a "." so there is no fraction.  Call this 0
    // nanoseconds.
    _ -> Ok(#(0, bytes))
  }
}

fn do_parse_second_fraction_as_nanoseconds(
  from bytes: BitArray,
  acc acc: Int,
  power power: Int,
) -> Result(#(Int, BitArray), a) {
  // Each digit place to the left in the fractional second is 10x fewer
  // nanoseconds.
  let power = power / 10

  case bytes {
    <<byte, remaining_bytes:bytes>>
      if byte_zero <= byte && byte <= byte_nine && power < 1
    -> {
      // We already have the max precision for nanoseconds. Truncate any
      // remaining digits.
      do_parse_second_fraction_as_nanoseconds(
        from: remaining_bytes,
        acc:,
        power:,
      )
    }
    <<byte, remaining_bytes:bytes>> if byte_zero <= byte && byte <= byte_nine -> {
      // We have not yet reached the precision limit. Parse the next digit.
      let digit = byte - 0x30
      do_parse_second_fraction_as_nanoseconds(
        from: remaining_bytes,
        acc: acc + digit * power,
        power:,
      )
    }
    _ -> Ok(#(acc, bytes))
  }
}

fn parse_offset(from bytes: BitArray) -> Result(#(Int, BitArray), Nil) {
  case bytes {
    <<"Z", remaining_bytes:bytes>> | <<"z", remaining_bytes:bytes>> ->
      Ok(#(0, remaining_bytes))
    _ -> parse_numeric_offset(bytes)
  }
}

fn parse_numeric_offset(from bytes: BitArray) -> Result(#(Int, BitArray), Nil) {
  use #(sign, bytes) <- result.try(parse_sign(from: bytes))
  use #(hours, bytes) <- result.try(parse_hours(from: bytes))
  use bytes <- result.try(accept_byte(from: bytes, value: byte_colon))
  use #(minutes, bytes) <- result.try(parse_minutes(from: bytes))

  let offset_seconds = offset_to_seconds(sign, hours:, minutes:)

  Ok(#(offset_seconds, bytes))
}

fn parse_sign(from bytes) {
  case bytes {
    <<"+", remaining_bytes:bytes>> -> Ok(#("+", remaining_bytes))
    <<"-", remaining_bytes:bytes>> -> Ok(#("-", remaining_bytes))
    _ -> Error(Nil)
  }
}

fn offset_to_seconds(sign, hours hours, minutes minutes) {
  let abs_seconds = hours * seconds_per_hour + minutes * seconds_per_minute

  case sign {
    "-" -> -abs_seconds
    _ -> abs_seconds
  }
}

/// Parse and return the given number of digits from the given bytes.
/// 
fn parse_digits(
  from bytes: BitArray,
  count count: Int,
) -> Result(#(Int, BitArray), Nil) {
  do_parse_digits(from: bytes, count:, acc: 0, k: 0)
}

fn do_parse_digits(
  from bytes: BitArray,
  count count: Int,
  acc acc: Int,
  k k: Int,
) -> Result(#(Int, BitArray), Nil) {
  case bytes {
    _ if k >= count -> Ok(#(acc, bytes))
    <<byte, remaining_bytes:bytes>> if byte_zero <= byte && byte <= byte_nine ->
      do_parse_digits(
        from: remaining_bytes,
        count:,
        acc: acc * 10 + { byte - 0x30 },
        k: k + 1,
      )
    _ -> Error(Nil)
  }
}

/// Accept the given value from `bytes` and move past it if found.
/// 
fn accept_byte(from bytes: BitArray, value value: Int) -> Result(BitArray, Nil) {
  case bytes {
    <<byte, remaining_bytes:bytes>> if byte == value -> Ok(remaining_bytes)
    _ -> Error(Nil)
  }
}

fn accept_date_time_separator(from bytes: BitArray) -> Result(BitArray, Nil) {
  case bytes {
    <<byte, remaining_bytes:bytes>>
      if byte == byte_t_uppercase
      || byte == byte_t_lowercase
      || byte == byte_space
    -> Ok(remaining_bytes)
    _ -> Error(Nil)
  }
}

fn accept_empty(from bytes: BitArray) -> Result(Nil, Nil) {
  case bytes {
    <<>> -> Ok(Nil)
    _ -> Error(Nil)
  }
}

/// Note: The caller of this function must ensure that all inputs are valid.
/// 
fn from_date_time(
  year year: Int,
  month month: Int,
  day day: Int,
  hours hours: Int,
  minutes minutes: Int,
  seconds seconds: Int,
  second_fraction_as_nanoseconds second_fraction_as_nanoseconds: Int,
  offset_seconds offset_seconds: Int,
) -> Timestamp {
  let julian_seconds =
    julian_seconds_from_parts(year:, month:, day:, hours:, minutes:, seconds:)

  let julian_seconds_since_epoch = julian_seconds - julian_seconds_unix_epoch

  Timestamp(
    seconds: julian_seconds_since_epoch - offset_seconds,
    nanoseconds: second_fraction_as_nanoseconds,
  )
  |> normalise
}

/// `julian_seconds_from_parts(year, month, day, hours, minutes, seconds)` 
/// returns the number of Julian 
/// seconds represented by the given arguments.
/// 
/// Note: It is the callers responsibility to ensure the inputs are valid.
/// 
/// See https://www.tondering.dk/claus/cal/julperiod.php#formula
/// 
fn julian_seconds_from_parts(
  year year: Int,
  month month: Int,
  day day: Int,
  hours hours: Int,
  minutes minutes: Int,
  seconds seconds: Int,
) {
  let julian_day_seconds =
    julian_day_from_ymd(year:, month:, day:) * seconds_per_day

  julian_day_seconds
  + { hours * seconds_per_hour }
  + { minutes * seconds_per_minute }
  + seconds
}

/// Note: It is the callers responsibility to ensure the inputs are valid.
/// 
/// See https://www.tondering.dk/claus/cal/julperiod.php#formula
/// 
fn julian_day_from_ymd(year year: Int, month month: Int, day day: Int) -> Int {
  let adjustment = { 14 - month } / 12
  let adjusted_year = year + 4800 - adjustment
  let adjusted_month = month + 12 * adjustment - 3

  day
  + { { 153 * adjusted_month } + 2 }
  / 5
  + 365
  * adjusted_year
  + { adjusted_year / 4 }
  - { adjusted_year / 100 }
  + { adjusted_year / 400 }
  - 32_045
}

/// Create a timestamp from a number of seconds since 00:00:00 UTC on 1 January
/// 1970.
///
pub fn from_unix_seconds(seconds: Int) -> Timestamp {
  Timestamp(seconds, 0)
}

/// Create a timestamp from a number of seconds and nanoseconds since 00:00:00
/// UTC on 1 January 1970.
///
/// # JavaScript int limitations
///
/// Remember that JavaScript can only perfectly represent ints between positive
/// and negative 9,007,199,254,740,991! If you only use the nanosecond field
/// then you will almost certainly not get the date value you want due to this
/// loss of precision. Always use seconds primarily and then use nanoseconds
/// for the final sub-second adjustment.
///
pub fn from_unix_seconds_and_nanoseconds(
  seconds seconds: Int,
  nanoseconds nanoseconds: Int,
) -> Timestamp {
  Timestamp(seconds, nanoseconds)
  |> normalise
}

/// Convert the timestamp to a number of seconds since 00:00:00 UTC on 1
/// January 1970.
///
/// There may be some small loss of precision due to `Timestamp` being
/// nanosecond accurate and `Float` not being able to represent this.
///
pub fn to_unix_seconds(timestamp: Timestamp) -> Float {
  let seconds = int.to_float(timestamp.seconds)
  let nanoseconds = int.to_float(timestamp.nanoseconds)
  seconds +. { nanoseconds /. 1_000_000_000.0 }
}

/// Convert the timestamp to a number of seconds and nanoseconds since 00:00:00
/// UTC on 1 January 1970. There is no loss of precision with this conversion
/// on any target.
pub fn to_unix_seconds_and_nanoseconds(timestamp: Timestamp) -> #(Int, Int) {
  #(timestamp.seconds, timestamp.nanoseconds)
}
