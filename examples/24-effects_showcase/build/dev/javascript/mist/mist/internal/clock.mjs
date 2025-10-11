import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { CustomType as $CustomType, makeError } from "../../gleam.mjs";

const FILEPATH = "src/mist/internal/clock.gleam";

export class SetTime extends $CustomType {}

class MistClock extends $CustomType {}

class DateHeader extends $CustomType {}

export class Set extends $CustomType {}

export class Protected extends $CustomType {}

export class NamedTable extends $CustomType {}

export class ReadConcurrency extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

function weekday_to_short_string(weekday) {
  if (weekday === 1) {
    return "Mon";
  } else if (weekday === 2) {
    return "Tue";
  } else if (weekday === 3) {
    return "Wed";
  } else if (weekday === 4) {
    return "Thu";
  } else if (weekday === 5) {
    return "Fri";
  } else if (weekday === 6) {
    return "Sat";
  } else if (weekday === 7) {
    return "Sun";
  } else {
    throw makeError(
      "panic",
      FILEPATH,
      "mist/internal/clock",
      98,
      "weekday_to_short_string",
      "erlang weekday outside of 1-7 range",
      {}
    )
  }
}

function month_to_short_string(month) {
  if (month === 1) {
    return "Jan";
  } else if (month === 2) {
    return "Feb";
  } else if (month === 3) {
    return "Mar";
  } else if (month === 4) {
    return "Apr";
  } else if (month === 5) {
    return "May";
  } else if (month === 6) {
    return "Jun";
  } else if (month === 7) {
    return "Jul";
  } else if (month === 8) {
    return "Aug";
  } else if (month === 9) {
    return "Sep";
  } else if (month === 10) {
    return "Oct";
  } else if (month === 11) {
    return "Nov";
  } else if (month === 12) {
    return "Dec";
  } else {
    throw makeError(
      "panic",
      FILEPATH,
      "mist/internal/clock",
      116,
      "month_to_short_string",
      "erlang month outside of 1-12 range",
      {}
    )
  }
}
