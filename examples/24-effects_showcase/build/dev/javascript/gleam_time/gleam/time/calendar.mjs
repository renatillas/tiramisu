import { Ok, Error, CustomType as $CustomType } from "../../gleam.mjs";
import * as $duration from "../../gleam/time/duration.mjs";
import { local_time_offset_seconds } from "../../gleam_time_ffi.mjs";

export class Date extends $CustomType {
  constructor(year, month, day) {
    super();
    this.year = year;
    this.month = month;
    this.day = day;
  }
}

export class TimeOfDay extends $CustomType {
  constructor(hours, minutes, seconds, nanoseconds) {
    super();
    this.hours = hours;
    this.minutes = minutes;
    this.seconds = seconds;
    this.nanoseconds = nanoseconds;
  }
}

export class January extends $CustomType {}

export class February extends $CustomType {}

export class March extends $CustomType {}

export class April extends $CustomType {}

export class May extends $CustomType {}

export class June extends $CustomType {}

export class July extends $CustomType {}

export class August extends $CustomType {}

export class September extends $CustomType {}

export class October extends $CustomType {}

export class November extends $CustomType {}

export class December extends $CustomType {}

/**
 * Get the offset for the computer's currently configured time zone.
 *
 * Note this may not be the time zone that is correct to use for your user.
 * For example, if you are making a web application that runs on a server you
 * want _their_ computer's time zone, not yours.
 *
 * This is the _current local_ offset, not the current local time zone. This
 * means that while it will result in the expected outcome for the current
 * time, it may result in unexpected output if used with other timestamps. For
 * example: a timestamp that would locally be during daylight savings time if
 * is it not currently daylight savings time when this function is called.
 */
export function local_offset() {
  return $duration.seconds(local_time_offset_seconds());
}

/**
 * Returns the English name for a month.
 *
 * # Examples
 *
 * ```gleam
 * month_to_string(April)
 * // -> "April"
 * ```
 */
export function month_to_string(month) {
  if (month instanceof January) {
    return "January";
  } else if (month instanceof February) {
    return "February";
  } else if (month instanceof March) {
    return "March";
  } else if (month instanceof April) {
    return "April";
  } else if (month instanceof May) {
    return "May";
  } else if (month instanceof June) {
    return "June";
  } else if (month instanceof July) {
    return "July";
  } else if (month instanceof August) {
    return "August";
  } else if (month instanceof September) {
    return "September";
  } else if (month instanceof October) {
    return "October";
  } else if (month instanceof November) {
    return "November";
  } else {
    return "December";
  }
}

/**
 * Returns the number for the month, where January is 1 and December is 12.
 *
 * # Examples
 *
 * ```gleam
 * month_to_int(January)
 * // -> 1
 * ```
 */
export function month_to_int(month) {
  if (month instanceof January) {
    return 1;
  } else if (month instanceof February) {
    return 2;
  } else if (month instanceof March) {
    return 3;
  } else if (month instanceof April) {
    return 4;
  } else if (month instanceof May) {
    return 5;
  } else if (month instanceof June) {
    return 6;
  } else if (month instanceof July) {
    return 7;
  } else if (month instanceof August) {
    return 8;
  } else if (month instanceof September) {
    return 9;
  } else if (month instanceof October) {
    return 10;
  } else if (month instanceof November) {
    return 11;
  } else {
    return 12;
  }
}

/**
 * Returns the month for a given number, where January is 1 and December is 12.
 *
 * # Examples
 *
 * ```gleam
 * month_from_int(1)
 * // -> Ok(January)
 * ```
 */
export function month_from_int(month) {
  if (month === 1) {
    return new Ok(new January());
  } else if (month === 2) {
    return new Ok(new February());
  } else if (month === 3) {
    return new Ok(new March());
  } else if (month === 4) {
    return new Ok(new April());
  } else if (month === 5) {
    return new Ok(new May());
  } else if (month === 6) {
    return new Ok(new June());
  } else if (month === 7) {
    return new Ok(new July());
  } else if (month === 8) {
    return new Ok(new August());
  } else if (month === 9) {
    return new Ok(new September());
  } else if (month === 10) {
    return new Ok(new October());
  } else if (month === 11) {
    return new Ok(new November());
  } else if (month === 12) {
    return new Ok(new December());
  } else {
    return new Error(undefined);
  }
}

/**
 * Determines if a given year is a leap year.
 *
 * A leap year occurs every 4 years, except for years divisible by 100,
 * unless they are also divisible by 400.
 *
 * # Examples
 *
 * ```gleam
 * is_leap_year(2024)
 * // -> True
 * ```
 *
 * ```gleam
 * is_leap_year(2023)
 * // -> False
 * ```
 */
export function is_leap_year(year) {
  let $ = (year % 400) === 0;
  if ($) {
    return $;
  } else {
    let $1 = (year % 100) === 0;
    if ($1) {
      return false;
    } else {
      return (year % 4) === 0;
    }
  }
}

/**
 * Checks if a given date is valid.
 *
 * This function properly accounts for leap years when validating February days.
 * A leap year occurs every 4 years, except for years divisible by 100,
 * unless they are also divisible by 400.
 *
 * # Examples
 *
 * ```gleam
 * is_valid_date(Date(2023, April, 15))
 * // -> True
 * ```
 *
 * ```gleam
 * is_valid_date(Date(2023, April, 31))
 * // -> False
 * ```
 *
 * ```gleam
 * is_valid_date(Date(2024, February, 29))
 * // -> True (2024 is a leap year)
 * ```
 */
export function is_valid_date(date) {
  let year;
  let month;
  let day;
  year = date.year;
  month = date.month;
  day = date.day;
  let $ = day < 1;
  if ($) {
    return false;
  } else {
    if (month instanceof January) {
      return day <= 31;
    } else if (month instanceof February) {
      let _block;
      let $1 = is_leap_year(year);
      if ($1) {
        _block = 29;
      } else {
        _block = 28;
      }
      let max_february_days = _block;
      return day <= max_february_days;
    } else if (month instanceof March) {
      return day <= 31;
    } else if (month instanceof April) {
      return day <= 30;
    } else if (month instanceof May) {
      return day <= 31;
    } else if (month instanceof June) {
      return day <= 30;
    } else if (month instanceof July) {
      return day <= 31;
    } else if (month instanceof August) {
      return day <= 31;
    } else if (month instanceof September) {
      return day <= 30;
    } else if (month instanceof October) {
      return day <= 31;
    } else if (month instanceof November) {
      return day <= 30;
    } else {
      return day <= 31;
    }
  }
}

/**
 * Checks if a time of day is valid.
 *
 * Validates that hours are 0-23, minutes are 0-59, seconds are 0-59,
 * and nanoseconds are 0-999,999,999.
 *
 * # Examples
 *
 * ```gleam
 * is_valid_time_of_day(TimeOfDay(12, 30, 45, 123456789))
 * // -> True
 * ```
 */
export function is_valid_time_of_day(time) {
  let hours;
  let minutes;
  let seconds;
  let nanoseconds;
  hours = time.hours;
  minutes = time.minutes;
  seconds = time.seconds;
  nanoseconds = time.nanoseconds;
  return (((((((hours >= 0) && (hours <= 23)) && (minutes >= 0)) && (minutes <= 59)) && (seconds >= 0)) && (seconds <= 59)) && (nanoseconds >= 0)) && (nanoseconds <= 999_999_999);
}

/**
 * The offset for the [Coordinated Universal Time (UTC)](https://en.wikipedia.org/wiki/Coordinated_Universal_Time)
 * time zone.
 *
 * The utc zone has no time adjustments, it is always zero. It never observes
 * daylight-saving time and it never shifts around based on political
 * restructuring.
 */
export const utc_offset = $duration.empty;
