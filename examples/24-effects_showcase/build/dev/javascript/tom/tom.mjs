import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $float from "../gleam_stdlib/gleam/float.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $calendar from "../gleam_time/gleam/time/calendar.mjs";
import * as $duration from "../gleam_time/gleam/time/duration.mjs";
import * as $timestamp from "../gleam_time/gleam/time/timestamp.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "./gleam.mjs";

const FILEPATH = "src/tom.gleam";

export class Int extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Float extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Infinity is a valid number in TOML but Gleam does not support it, so this
 * variant represents the infinity values.
 */
export class Infinity extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * NaN is a valid number in TOML but Gleam does not support it, so this
 * variant represents the NaN values.
 */
export class Nan extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Bool extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class String extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Date extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Time extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class DateTime extends $CustomType {
  constructor(date, time, offset) {
    super();
    this.date = date;
    this.time = time;
    this.offset = offset;
  }
}

export class Array extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class ArrayOfTables extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Table extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class InlineTable extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Local extends $CustomType {}

export class Offset extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Positive extends $CustomType {}

export class Negative extends $CustomType {}

/**
 * An unexpected character was encountered when parsing the document.
 */
export class Unexpected extends $CustomType {
  constructor(got, expected) {
    super();
    this.got = got;
    this.expected = expected;
  }
}

/**
 * More than one items have the same key in the document.
 */
export class KeyAlreadyInUse extends $CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
}

export class NumberInt extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NumberFloat extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NumberInfinity extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NumberNan extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * There was no value at the given key.
 */
export class NotFound extends $CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
}

/**
 * The value at the given key was not of the expected type.
 */
export class WrongType extends $CustomType {
  constructor(key, expected, got) {
    super();
    this.key = key;
    this.expected = expected;
    this.got = got;
  }
}

function classify(toml) {
  if (toml instanceof Int) {
    return "Int";
  } else if (toml instanceof Float) {
    return "Float";
  } else if (toml instanceof Infinity) {
    let $ = toml[0];
    if ($ instanceof Positive) {
      return "Infinity";
    } else {
      return "Negative Infinity";
    }
  } else if (toml instanceof Nan) {
    let $ = toml[0];
    if ($ instanceof Positive) {
      return "NaN";
    } else {
      return "Negative NaN";
    }
  } else if (toml instanceof Bool) {
    return "Bool";
  } else if (toml instanceof String) {
    return "String";
  } else if (toml instanceof Date) {
    return "Date";
  } else if (toml instanceof Time) {
    return "Time";
  } else if (toml instanceof DateTime) {
    return "DateTime";
  } else if (toml instanceof Array) {
    return "Array";
  } else if (toml instanceof ArrayOfTables) {
    return "Array";
  } else if (toml instanceof Table) {
    return "Table";
  } else {
    return "Table";
  }
}

function push_key(result, key) {
  if (result instanceof Ok) {
    return result;
  } else {
    let $ = result[0];
    if ($ instanceof NotFound) {
      let path = $.key;
      return new Error(new NotFound(listPrepend(key, path)));
    } else {
      let path = $.key;
      let expected = $.expected;
      let got = $.got;
      return new Error(new WrongType(listPrepend(key, path), expected, got));
    }
  }
}

/**
 * Get a value of any type from a TOML document dictionary.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = 1")
 * get(parsed, ["a", "b", "c"])
 * // -> Ok(Int(1))
 * ```
 */
export function get(toml, key) {
  if (key instanceof $Empty) {
    return new Error(new NotFound(toList([])));
  } else {
    let $ = key.tail;
    if ($ instanceof $Empty) {
      let k = key.head;
      return $result.replace_error(
        $dict.get(toml, k),
        new NotFound(toList([k])),
      );
    } else {
      let k = key.head;
      let key$1 = $;
      let $1 = $dict.get(toml, k);
      if ($1 instanceof Ok) {
        let $2 = $1[0];
        if ($2 instanceof Table) {
          let t = $2[0];
          return push_key(get(t, key$1), k);
        } else if ($2 instanceof InlineTable) {
          let t = $2[0];
          return push_key(get(t, key$1), k);
        } else {
          let other = $2;
          return new Error(new WrongType(toList([k]), "Table", classify(other)));
        }
      } else {
        return new Error(new NotFound(toList([k])));
      }
    }
  }
}

/**
 * Get an int from a TOML document dictionary.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = 1")
 * get_int(parsed, ["a", "b", "c"])
 * // -> Ok(1)
 * ```
 */
export function get_int(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof Int) {
      let i = $1[0];
      return new Ok(i);
    } else {
      let other = $1;
      return new Error(new WrongType(key, "Int", classify(other)));
    }
  } else {
    return $;
  }
}

/**
 * Get a float from a TOML document dictionary.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = 1.1")
 * get_float(parsed, ["a", "b", "c"])
 * // -> Ok(1.1)
 * ```
 */
export function get_float(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof Float) {
      let i = $1[0];
      return new Ok(i);
    } else {
      let other = $1;
      return new Error(new WrongType(key, "Float", classify(other)));
    }
  } else {
    return $;
  }
}

/**
 * Get a bool from a TOML document dictionary.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = true")
 * get_bool(parsed, ["a", "b", "c"])
 * // -> Ok(True)
 * ```
 */
export function get_bool(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof Bool) {
      let i = $1[0];
      return new Ok(i);
    } else {
      let other = $1;
      return new Error(new WrongType(key, "Bool", classify(other)));
    }
  } else {
    return $;
  }
}

/**
 * Get a string from a TOML document dictionary.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = \"ok\"")
 * get_string(parsed, ["a", "b", "c"])
 * // -> Ok("ok")
 * ```
 */
export function get_string(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof String) {
      let i = $1[0];
      return new Ok(i);
    } else {
      let other = $1;
      return new Error(new WrongType(key, "String", classify(other)));
    }
  } else {
    return $;
  }
}

/**
 * Get a date from a TOML document dictionary.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = 1979-05-27")
 * get_date(parsed, ["a", "b", "c"])
 * // -> Ok(Date(1979, May, 27))
 * ```
 */
export function get_date(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof Date) {
      let i = $1[0];
      return new Ok(i);
    } else {
      let other = $1;
      return new Error(new WrongType(key, "Date", classify(other)));
    }
  } else {
    return $;
  }
}

/**
 * Get a time from a TOML document dictionary.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = 07:32:00")
 * get_time_of_day(parsed, ["a", "b", "c"])
 * // -> Ok(TimeOfDay(7, 32, 0, 0))
 * ```
 */
export function get_time_of_day(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof Time) {
      let i = $1[0];
      return new Ok(i);
    } else {
      let other = $1;
      return new Error(new WrongType(key, "Time", classify(other)));
    }
  } else {
    return $;
  }
}

/**
 * Get a date-time from a TOML document dictionary.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = 1979-05-27T07:32:00")
 * get_calendar_time(parsed, ["a", "b", "c"])
 * // -> Ok(#(Date(1979, May, 27), TimeOfDay(7, 32, 0, 0), Local))
 * ```
 */
export function get_calendar_time(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof DateTime) {
      let d = $1.date;
      let t = $1.time;
      let o = $1.offset;
      return new Ok([d, t, o]);
    } else {
      let other = $1;
      return new Error(new WrongType(key, "DateTime", classify(other)));
    }
  } else {
    return $;
  }
}

/**
 * Get an unambiguous time from a TOML document dictionary.
 *
 * If a TOML date time has no offset it is ambiguous and cannot be converted
 * into a timestamp. There's no way to know what actual point in time it would
 * be as it would be different in different time zones.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = 1970-00-00T00:00:00Z")
 * get_timestamp(parsed, ["a", "b", "c"])
 * // -> Ok(timestamp.from_unix_seconds(0))
 * ```
 */
export function get_timestamp(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof DateTime) {
      let $2 = $1.offset;
      if ($2 instanceof Offset) {
        let date = $1.date;
        let time = $1.time;
        let offset = $2[0];
        return new Ok($timestamp.from_calendar(date, time, offset));
      } else {
        let other = $1;
        return new Error(
          new WrongType(key, "DateTime with offset", classify(other)),
        );
      }
    } else {
      let other = $1;
      return new Error(
        new WrongType(key, "DateTime with offset", classify(other)),
      );
    }
  } else {
    return $;
  }
}

/**
 * Get an array from a TOML document dictionary.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = [1, 2]")
 * get_array(parsed, ["a", "b", "c"])
 * // -> Ok([Int(1), Int(2)])
 * ```
 */
export function get_array(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof Array) {
      let i = $1[0];
      return new Ok(i);
    } else if ($1 instanceof ArrayOfTables) {
      let i = $1[0];
      return new Ok($list.map(i, (var0) => { return new Table(var0); }));
    } else {
      let other = $1;
      return new Error(new WrongType(key, "Array", classify(other)));
    }
  } else {
    return $;
  }
}

/**
 * Get a table from a TOML document dictionary.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = { d = 1 }")
 * get_table(parsed, ["a", "b", "c"])
 * // -> Ok(dict.from_list([#("d", Int(1))]))
 * ```
 */
export function get_table(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof Table) {
      let i = $1[0];
      return new Ok(i);
    } else if ($1 instanceof InlineTable) {
      let i = $1[0];
      return new Ok(i);
    } else {
      let other = $1;
      return new Error(new WrongType(key, "Table", classify(other)));
    }
  } else {
    return $;
  }
}

/**
 * Get a number of any kind from a TOML document dictionary.
 * This could be an int, a float, a NaN, or an infinity.
 *
 * ## Examples
 * 
 * ```gleam
 * let assert Ok(parsed) = parse("a.b.c = { d = inf }")
 * get_number(parsed, ["a", "b", "c"])
 * // -> Ok(NumberInfinity(Positive)))
 * ```
 */
export function get_number(toml, key) {
  let $ = get(toml, key);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof Int) {
      let x = $1[0];
      return new Ok(new NumberInt(x));
    } else if ($1 instanceof Float) {
      let x = $1[0];
      return new Ok(new NumberFloat(x));
    } else if ($1 instanceof Infinity) {
      let x = $1[0];
      return new Ok(new NumberInfinity(x));
    } else if ($1 instanceof Nan) {
      let x = $1[0];
      return new Ok(new NumberNan(x));
    } else {
      let other = $1;
      return new Error(new WrongType(key, "Number", classify(other)));
    }
  } else {
    return $;
  }
}

function merge(table, key, old, new$) {
  if (new$ instanceof ArrayOfTables && old instanceof ArrayOfTables) {
    let new$1 = new$[0];
    let tables = old[0];
    return new Ok(
      $dict.insert(table, key, new ArrayOfTables($list.append(new$1, tables))),
    );
  } else {
    return new Error(toList([key]));
  }
}

function insert_loop(table, key, value) {
  if (key instanceof $Empty) {
    throw makeError(
      "panic",
      FILEPATH,
      "tom",
      529,
      "insert_loop",
      "unreachable",
      {}
    )
  } else {
    let $ = key.tail;
    if ($ instanceof $Empty) {
      let k = key.head;
      let $1 = $dict.get(table, k);
      if ($1 instanceof Ok) {
        let old = $1[0];
        return merge(table, k, old, value);
      } else {
        return new Ok($dict.insert(table, k, value));
      }
    } else {
      let k = key.head;
      let key$1 = $;
      let $1 = $dict.get(table, k);
      if ($1 instanceof Ok) {
        let $2 = $1[0];
        if ($2 instanceof ArrayOfTables) {
          let $3 = $2[0];
          if ($3 instanceof $Empty) {
            return new Error(toList([k]));
          } else {
            let inner = $3.head;
            let rest = $3.tail;
            let $4 = insert_loop(inner, key$1, value);
            if ($4 instanceof Ok) {
              let inner$1 = $4[0];
              return new Ok(
                $dict.insert(
                  table,
                  k,
                  new ArrayOfTables(listPrepend(inner$1, rest)),
                ),
              );
            } else {
              let path = $4[0];
              return new Error(listPrepend(k, path));
            }
          }
        } else if ($2 instanceof Table) {
          let inner = $2[0];
          let $3 = insert_loop(inner, key$1, value);
          if ($3 instanceof Ok) {
            let inner$1 = $3[0];
            return new Ok($dict.insert(table, k, new Table(inner$1)));
          } else {
            let path = $3[0];
            return new Error(listPrepend(k, path));
          }
        } else {
          return new Error(toList([k]));
        }
      } else {
        let $2 = insert_loop($dict.new$(), key$1, value);
        if ($2 instanceof Ok) {
          let inner = $2[0];
          return new Ok($dict.insert(table, k, new Table(inner)));
        } else {
          let path = $2[0];
          return new Error(listPrepend(k, path));
        }
      }
    }
  }
}

function insert(table, key, value) {
  let $ = insert_loop(table, key, value);
  if ($ instanceof Ok) {
    return $;
  } else {
    let path = $[0];
    return new Error(new KeyAlreadyInUse(path));
  }
}

function expect_end_of_line(input, next) {
  if (input instanceof $Empty) {
    return new Error(new Unexpected("EOF", "\n"));
  } else {
    let $ = input.head;
    if ($ === "\n") {
      let input$1 = input.tail;
      return next(input$1);
    } else if ($ === "\r\n") {
      let input$1 = input.tail;
      return next(input$1);
    } else {
      let g = $;
      return new Error(new Unexpected(g, "\n"));
    }
  }
}

function parse_key_quoted(loop$input, loop$close, loop$name) {
  while (true) {
    let input = loop$input;
    let close = loop$close;
    let name = loop$name;
    if (input instanceof $Empty) {
      return new Error(new Unexpected("EOF", close));
    } else {
      let g = input.head;
      if (g === close) {
        let input$1 = input.tail;
        return new Ok([name, input$1]);
      } else {
        let g$1 = input.head;
        let input$1 = input.tail;
        loop$input = input$1;
        loop$close = close;
        loop$name = name + g$1;
      }
    }
  }
}

function parse_key_bare(loop$input, loop$name) {
  while (true) {
    let input = loop$input;
    let name = loop$name;
    if (input instanceof $Empty) {
      return new Error(new Unexpected("EOF", "key"));
    } else {
      let $ = input.head;
      if ($ === " " && name !== "") {
        let input$1 = input.tail;
        return new Ok([name, input$1]);
      } else if ($ === "=" && name !== "") {
        return new Ok([name, input]);
      } else if ($ === "." && name !== "") {
        return new Ok([name, input]);
      } else if ($ === "]") {
        if (name !== "") {
          return new Ok([name, input]);
        } else {
          return new Error(new Unexpected("]", "key"));
        }
      } else if ($ === ",") {
        if (name !== "") {
          return new Error(new Unexpected(",", "="));
        } else {
          return new Error(new Unexpected(",", "key"));
        }
      } else if ($ === "\n") {
        if (name !== "") {
          return new Error(new Unexpected("\n", "="));
        } else {
          return new Error(new Unexpected("\n", "key"));
        }
      } else if ($ === "\r\n") {
        if (name !== "") {
          return new Error(new Unexpected("\r\n", "="));
        } else {
          return new Error(new Unexpected("\r\n", "key"));
        }
      } else {
        let g = $;
        let input$1 = input.tail;
        loop$input = input$1;
        loop$name = name + g;
      }
    }
  }
}

function skip_line_whitespace(input) {
  return $list.drop_while(input, (g) => { return (g === " ") || (g === "\t"); });
}

function parse_key_segment(input) {
  let input$1 = skip_line_whitespace(input);
  if (input$1 instanceof $Empty) {
    return parse_key_bare(input$1, "");
  } else {
    let $ = input$1.head;
    if ($ === "=") {
      return new Error(new Unexpected("=", "Key"));
    } else if ($ === "\n") {
      return new Error(new Unexpected("\n", "Key"));
    } else if ($ === "\r\n") {
      return new Error(new Unexpected("\r\n", "Key"));
    } else if ($ === "[") {
      return new Error(new Unexpected("[", "Key"));
    } else if ($ === "\"") {
      let input$2 = input$1.tail;
      return parse_key_quoted(input$2, "\"", "");
    } else if ($ === "'") {
      let input$2 = input$1.tail;
      return parse_key_quoted(input$2, "'", "");
    } else {
      return parse_key_bare(input$1, "");
    }
  }
}

function skip_whitespace(loop$input) {
  while (true) {
    let input = loop$input;
    if (input instanceof $Empty) {
      return input;
    } else {
      let $ = input.head;
      if ($ === " ") {
        let input$1 = input.tail;
        loop$input = input$1;
      } else if ($ === "\t") {
        let input$1 = input.tail;
        loop$input = input$1;
      } else if ($ === "\n") {
        let input$1 = input.tail;
        loop$input = input$1;
      } else if ($ === "\r\n") {
        let input$1 = input.tail;
        loop$input = input$1;
      } else {
        return input;
      }
    }
  }
}

function drop_comments(loop$input, loop$acc, loop$in_string) {
  while (true) {
    let input = loop$input;
    let acc = loop$acc;
    let in_string = loop$in_string;
    if (input instanceof $Empty) {
      return $list.reverse(acc);
    } else {
      let $ = input.head;
      if ($ === "\\") {
        let $1 = input.tail;
        if ($1 instanceof $Empty) {
          let g = $;
          let input$1 = $1;
          loop$input = input$1;
          loop$acc = listPrepend(g, acc);
          loop$in_string = in_string;
        } else {
          let $2 = $1.head;
          if ($2 === "\"" && in_string) {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$acc = listPrepend("\"", listPrepend("\\", acc));
            loop$in_string = in_string;
          } else {
            let g = $;
            let input$1 = $1;
            loop$input = input$1;
            loop$acc = listPrepend(g, acc);
            loop$in_string = in_string;
          }
        }
      } else if ($ === "\"") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$acc = listPrepend("\"", acc);
        loop$in_string = !in_string;
      } else if ($ === "#") {
        if (in_string) {
          let input$1 = input.tail;
          loop$input = input$1;
          loop$acc = listPrepend("#", acc);
          loop$in_string = in_string;
        } else if (!in_string) {
          let input$1 = input.tail;
          let _pipe = input$1;
          let _pipe$1 = $list.drop_while(_pipe, (g) => { return g !== "\n"; });
          loop$input = _pipe$1;
          loop$acc = acc;
          loop$in_string = in_string;
        } else {
          let g = $;
          let input$1 = input.tail;
          loop$input = input$1;
          loop$acc = listPrepend(g, acc);
          loop$in_string = in_string;
        }
      } else {
        let g = $;
        let input$1 = input.tail;
        loop$input = input$1;
        loop$acc = listPrepend(g, acc);
        loop$in_string = in_string;
      }
    }
  }
}

function do$(result, next) {
  if (result instanceof Ok) {
    let a = result[0][0];
    let input = result[0][1];
    return next(a, input);
  } else {
    return result;
  }
}

function parse_key(input, segments) {
  return do$(
    parse_key_segment(input),
    (segment, input) => {
      let segments$1 = listPrepend(segment, segments);
      let input$1 = skip_line_whitespace(input);
      if (input$1 instanceof $Empty) {
        return new Ok([$list.reverse(segments$1), input$1]);
      } else {
        let $ = input$1.head;
        if ($ === ".") {
          let input$2 = input$1.tail;
          return parse_key(input$2, segments$1);
        } else {
          return new Ok([$list.reverse(segments$1), input$1]);
        }
      }
    },
  );
}

function expect(input, expected, next) {
  if (input instanceof $Empty) {
    return new Error(new Unexpected("EOF", expected));
  } else {
    let g = input.head;
    if (g === expected) {
      let input$1 = input.tail;
      return next(input$1);
    } else {
      let g$1 = input.head;
      return new Error(new Unexpected(g$1, expected));
    }
  }
}

function parse_table_header(input) {
  let input$1 = skip_line_whitespace(input);
  return do$(
    parse_key(input$1, toList([])),
    (key, input) => {
      return expect(
        input,
        "]",
        (input) => {
          let input$1 = skip_line_whitespace(input);
          return expect_end_of_line(
            input$1,
            (input) => { return new Ok([key, input]); },
          );
        },
      );
    },
  );
}

function parse_hex(loop$input, loop$number, loop$sign) {
  while (true) {
    let input = loop$input;
    let number = loop$number;
    let sign = loop$sign;
    if (input instanceof $Empty) {
      let input$1 = input;
      let _block;
      if (sign instanceof Positive) {
        _block = number;
      } else {
        _block = - number;
      }
      let number$1 = _block;
      return new Ok([new Int(number$1), input$1]);
    } else {
      let $ = input.head;
      if ($ === "_") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number;
        loop$sign = sign;
      } else if ($ === "0") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 0;
        loop$sign = sign;
      } else if ($ === "1") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 1;
        loop$sign = sign;
      } else if ($ === "2") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 2;
        loop$sign = sign;
      } else if ($ === "3") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 3;
        loop$sign = sign;
      } else if ($ === "4") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 4;
        loop$sign = sign;
      } else if ($ === "5") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 5;
        loop$sign = sign;
      } else if ($ === "6") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 6;
        loop$sign = sign;
      } else if ($ === "7") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 7;
        loop$sign = sign;
      } else if ($ === "8") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 8;
        loop$sign = sign;
      } else if ($ === "9") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 9;
        loop$sign = sign;
      } else if ($ === "a") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 10;
        loop$sign = sign;
      } else if ($ === "b") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 11;
        loop$sign = sign;
      } else if ($ === "c") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 12;
        loop$sign = sign;
      } else if ($ === "d") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 13;
        loop$sign = sign;
      } else if ($ === "e") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 14;
        loop$sign = sign;
      } else if ($ === "f") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 15;
        loop$sign = sign;
      } else if ($ === "A") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 10;
        loop$sign = sign;
      } else if ($ === "B") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 11;
        loop$sign = sign;
      } else if ($ === "C") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 12;
        loop$sign = sign;
      } else if ($ === "D") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 13;
        loop$sign = sign;
      } else if ($ === "E") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 14;
        loop$sign = sign;
      } else if ($ === "F") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 16 + 15;
        loop$sign = sign;
      } else {
        let input$1 = input;
        let _block;
        if (sign instanceof Positive) {
          _block = number;
        } else {
          _block = - number;
        }
        let number$1 = _block;
        return new Ok([new Int(number$1), input$1]);
      }
    }
  }
}

function parse_octal(loop$input, loop$number, loop$sign) {
  while (true) {
    let input = loop$input;
    let number = loop$number;
    let sign = loop$sign;
    if (input instanceof $Empty) {
      let input$1 = input;
      let _block;
      if (sign instanceof Positive) {
        _block = number;
      } else {
        _block = - number;
      }
      let number$1 = _block;
      return new Ok([new Int(number$1), input$1]);
    } else {
      let $ = input.head;
      if ($ === "_") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number;
        loop$sign = sign;
      } else if ($ === "0") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 8 + 0;
        loop$sign = sign;
      } else if ($ === "1") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 8 + 1;
        loop$sign = sign;
      } else if ($ === "2") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 8 + 2;
        loop$sign = sign;
      } else if ($ === "3") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 8 + 3;
        loop$sign = sign;
      } else if ($ === "4") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 8 + 4;
        loop$sign = sign;
      } else if ($ === "5") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 8 + 5;
        loop$sign = sign;
      } else if ($ === "6") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 8 + 6;
        loop$sign = sign;
      } else if ($ === "7") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 8 + 7;
        loop$sign = sign;
      } else {
        let input$1 = input;
        let _block;
        if (sign instanceof Positive) {
          _block = number;
        } else {
          _block = - number;
        }
        let number$1 = _block;
        return new Ok([new Int(number$1), input$1]);
      }
    }
  }
}

function parse_binary(loop$input, loop$number, loop$sign) {
  while (true) {
    let input = loop$input;
    let number = loop$number;
    let sign = loop$sign;
    if (input instanceof $Empty) {
      let input$1 = input;
      let _block;
      if (sign instanceof Positive) {
        _block = number;
      } else {
        _block = - number;
      }
      let number$1 = _block;
      return new Ok([new Int(number$1), input$1]);
    } else {
      let $ = input.head;
      if ($ === "_") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number;
        loop$sign = sign;
      } else if ($ === "0") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 2 + 0;
        loop$sign = sign;
      } else if ($ === "1") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 2 + 1;
        loop$sign = sign;
      } else {
        let input$1 = input;
        let _block;
        if (sign instanceof Positive) {
          _block = number;
        } else {
          _block = - number;
        }
        let number$1 = _block;
        return new Ok([new Int(number$1), input$1]);
      }
    }
  }
}

function parse_exponent(loop$input, loop$n, loop$n_sign, loop$ex, loop$ex_sign) {
  while (true) {
    let input = loop$input;
    let n = loop$n;
    let n_sign = loop$n_sign;
    let ex = loop$ex;
    let ex_sign = loop$ex_sign;
    if (input instanceof $Empty) {
      let input$1 = input;
      let _block;
      if (n_sign instanceof Positive) {
        _block = n;
      } else {
        _block = n * -1.0;
      }
      let number = _block;
      let exponent = $int.to_float(
        (() => {
          if (ex_sign instanceof Positive) {
            return ex;
          } else {
            return - ex;
          }
        })(),
      );
      let _block$1;
      let $ = $float.power(10.0, exponent);
      if ($ instanceof Ok) {
        let multiplier = $[0];
        _block$1 = multiplier;
      } else {
        _block$1 = 1.0;
      }
      let multiplier = _block$1;
      return new Ok([new Float(number * multiplier), input$1]);
    } else {
      let $ = input.head;
      if ($ === "_") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex;
        loop$ex_sign = ex_sign;
      } else if ($ === "0") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex * 10;
        loop$ex_sign = ex_sign;
      } else if ($ === "1") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex * 10 + 1;
        loop$ex_sign = ex_sign;
      } else if ($ === "2") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex * 10 + 2;
        loop$ex_sign = ex_sign;
      } else if ($ === "3") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex * 10 + 3;
        loop$ex_sign = ex_sign;
      } else if ($ === "4") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex * 10 + 4;
        loop$ex_sign = ex_sign;
      } else if ($ === "5") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex * 10 + 5;
        loop$ex_sign = ex_sign;
      } else if ($ === "6") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex * 10 + 6;
        loop$ex_sign = ex_sign;
      } else if ($ === "7") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex * 10 + 7;
        loop$ex_sign = ex_sign;
      } else if ($ === "8") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex * 10 + 8;
        loop$ex_sign = ex_sign;
      } else if ($ === "9") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$n = n;
        loop$n_sign = n_sign;
        loop$ex = ex * 10 + 9;
        loop$ex_sign = ex_sign;
      } else {
        let input$1 = input;
        let _block;
        if (n_sign instanceof Positive) {
          _block = n;
        } else {
          _block = n * -1.0;
        }
        let number = _block;
        let exponent = $int.to_float(
          (() => {
            if (ex_sign instanceof Positive) {
              return ex;
            } else {
              return - ex;
            }
          })(),
        );
        let _block$1;
        let $1 = $float.power(10.0, exponent);
        if ($1 instanceof Ok) {
          let multiplier = $1[0];
          _block$1 = multiplier;
        } else {
          _block$1 = 1.0;
        }
        let multiplier = _block$1;
        return new Ok([new Float(number * multiplier), input$1]);
      }
    }
  }
}

function parse_float(loop$input, loop$number, loop$sign, loop$unit) {
  while (true) {
    let input = loop$input;
    let number = loop$number;
    let sign = loop$sign;
    let unit = loop$unit;
    if (input instanceof $Empty) {
      let input$1 = input;
      let _block;
      if (sign instanceof Positive) {
        _block = number;
      } else {
        _block = number * -1.0;
      }
      let number$1 = _block;
      return new Ok([new Float(number$1), input$1]);
    } else {
      let $ = input.head;
      if ($ === "_") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number;
        loop$sign = sign;
        loop$unit = unit;
      } else if ($ === "0") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number;
        loop$sign = sign;
        loop$unit = unit * 0.1;
      } else if ($ === "1") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number + (1.0 * unit);
        loop$sign = sign;
        loop$unit = unit * 0.1;
      } else if ($ === "2") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number + (2.0 * unit);
        loop$sign = sign;
        loop$unit = unit * 0.1;
      } else if ($ === "3") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number + (3.0 * unit);
        loop$sign = sign;
        loop$unit = unit * 0.1;
      } else if ($ === "4") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number + (4.0 * unit);
        loop$sign = sign;
        loop$unit = unit * 0.1;
      } else if ($ === "5") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number + (5.0 * unit);
        loop$sign = sign;
        loop$unit = unit * 0.1;
      } else if ($ === "6") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number + (6.0 * unit);
        loop$sign = sign;
        loop$unit = unit * 0.1;
      } else if ($ === "7") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number + (7.0 * unit);
        loop$sign = sign;
        loop$unit = unit * 0.1;
      } else if ($ === "8") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number + (8.0 * unit);
        loop$sign = sign;
        loop$unit = unit * 0.1;
      } else if ($ === "9") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number + (9.0 * unit);
        loop$sign = sign;
        loop$unit = unit * 0.1;
      } else if ($ === "e") {
        let $1 = input.tail;
        if ($1 instanceof $Empty) {
          let input$1 = $1;
          return parse_exponent(input$1, number, sign, 0, new Positive());
        } else {
          let $2 = $1.head;
          if ($2 === "+") {
            let input$1 = $1.tail;
            return parse_exponent(input$1, number, sign, 0, new Positive());
          } else if ($2 === "-") {
            let input$1 = $1.tail;
            return parse_exponent(input$1, number, sign, 0, new Negative());
          } else {
            let input$1 = $1;
            return parse_exponent(input$1, number, sign, 0, new Positive());
          }
        }
      } else if ($ === "E") {
        let $1 = input.tail;
        if ($1 instanceof $Empty) {
          let input$1 = $1;
          return parse_exponent(input$1, number, sign, 0, new Positive());
        } else {
          let $2 = $1.head;
          if ($2 === "+") {
            let input$1 = $1.tail;
            return parse_exponent(input$1, number, sign, 0, new Positive());
          } else if ($2 === "-") {
            let input$1 = $1.tail;
            return parse_exponent(input$1, number, sign, 0, new Negative());
          } else {
            let input$1 = $1;
            return parse_exponent(input$1, number, sign, 0, new Positive());
          }
        }
      } else {
        let input$1 = input;
        let _block;
        if (sign instanceof Positive) {
          _block = number;
        } else {
          _block = number * -1.0;
        }
        let number$1 = _block;
        return new Ok([new Float(number$1), input$1]);
      }
    }
  }
}

function parse_string(loop$input, loop$string) {
  while (true) {
    let input = loop$input;
    let string = loop$string;
    if (input instanceof $Empty) {
      return new Error(new Unexpected("EOF", "\""));
    } else {
      let $ = input.head;
      if ($ === "\"") {
        let input$1 = input.tail;
        return new Ok([new String(string), input$1]);
      } else if ($ === "\\") {
        let $1 = input.tail;
        if ($1 instanceof $Empty) {
          let g = $;
          let input$1 = $1;
          loop$input = input$1;
          loop$string = string + g;
        } else {
          let $2 = $1.head;
          if ($2 === "t") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\t";
          } else if ($2 === "e") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\u{001b}";
          } else if ($2 === "b") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\u{0008}";
          } else if ($2 === "n") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\n";
          } else if ($2 === "r") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\r";
          } else if ($2 === "f") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\f";
          } else if ($2 === "\"") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\"";
          } else if ($2 === "\\") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\\";
          } else {
            let g = $;
            let input$1 = $1;
            loop$input = input$1;
            loop$string = string + g;
          }
        }
      } else if ($ === "\n") {
        return new Error(new Unexpected("\n", "\""));
      } else if ($ === "\r\n") {
        return new Error(new Unexpected("\r\n", "\""));
      } else {
        let g = $;
        let input$1 = input.tail;
        loop$input = input$1;
        loop$string = string + g;
      }
    }
  }
}

function parse_multi_line_string(loop$input, loop$string) {
  while (true) {
    let input = loop$input;
    let string = loop$string;
    if (input instanceof $Empty) {
      return new Error(new Unexpected("EOF", "\""));
    } else {
      let $ = input.head;
      if ($ === "\"") {
        let $1 = input.tail;
        if ($1 instanceof $Empty) {
          let g = $;
          let input$1 = $1;
          loop$input = input$1;
          loop$string = string + g;
        } else {
          let $2 = $1.tail;
          if ($2 instanceof $Empty) {
            let g = $;
            let input$1 = $1;
            loop$input = input$1;
            loop$string = string + g;
          } else {
            let $3 = $2.head;
            if ($3 === "\"") {
              let $4 = $1.head;
              if ($4 === "\"") {
                let input$1 = $2.tail;
                return new Ok([new String(string), input$1]);
              } else {
                let g = $;
                let input$1 = $1;
                loop$input = input$1;
                loop$string = string + g;
              }
            } else {
              let g = $;
              let input$1 = $1;
              loop$input = input$1;
              loop$string = string + g;
            }
          }
        }
      } else if ($ === "\\") {
        let $1 = input.tail;
        if ($1 instanceof $Empty) {
          let g = $;
          let input$1 = $1;
          loop$input = input$1;
          loop$string = string + g;
        } else {
          let $2 = $1.head;
          if ($2 === "\n") {
            let input$1 = $1.tail;
            loop$input = skip_whitespace(input$1);
            loop$string = string;
          } else if ($2 === "\r\n") {
            let input$1 = $1.tail;
            loop$input = skip_whitespace(input$1);
            loop$string = string;
          } else if ($2 === "t") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\t";
          } else if ($2 === "n") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\n";
          } else if ($2 === "r") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\r";
          } else if ($2 === "\"") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\"";
          } else if ($2 === "\\") {
            let input$1 = $1.tail;
            loop$input = input$1;
            loop$string = string + "\\";
          } else {
            let g = $;
            let input$1 = $1;
            loop$input = input$1;
            loop$string = string + g;
          }
        }
      } else if ($ === "\r\n") {
        if (string === "") {
          let input$1 = input.tail;
          loop$input = input$1;
          loop$string = string;
        } else if (string === "") {
          let input$1 = input.tail;
          loop$input = input$1;
          loop$string = string;
        } else {
          let g = $;
          let input$1 = input.tail;
          loop$input = input$1;
          loop$string = string + g;
        }
      } else if ($ === "\n" && string === "") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$string = string;
      } else {
        let g = $;
        let input$1 = input.tail;
        loop$input = input$1;
        loop$string = string + g;
      }
    }
  }
}

function parse_multi_line_literal_string(loop$input, loop$string) {
  while (true) {
    let input = loop$input;
    let string = loop$string;
    if (input instanceof $Empty) {
      return new Error(new Unexpected("EOF", "\""));
    } else {
      let $ = input.head;
      if ($ === "'") {
        let $1 = input.tail;
        if ($1 instanceof $Empty) {
          let g = $;
          let input$1 = $1;
          loop$input = input$1;
          loop$string = string + g;
        } else {
          let $2 = $1.tail;
          if ($2 instanceof $Empty) {
            let g = $;
            let input$1 = $1;
            loop$input = input$1;
            loop$string = string + g;
          } else {
            let $3 = $2.head;
            if ($3 === "'") {
              let $4 = $1.head;
              if ($4 === "'") {
                let $5 = $2.tail;
                if ($5 instanceof $Empty) {
                  let input$1 = $5;
                  return new Ok([new String(string), input$1]);
                } else {
                  let $6 = $5.head;
                  if ($6 === "'") {
                    return new Error(new Unexpected("''''", "'''"));
                  } else {
                    let input$1 = $5;
                    return new Ok([new String(string), input$1]);
                  }
                }
              } else {
                let g = $;
                let input$1 = $1;
                loop$input = input$1;
                loop$string = string + g;
              }
            } else {
              let g = $;
              let input$1 = $1;
              loop$input = input$1;
              loop$string = string + g;
            }
          }
        }
      } else if ($ === "\n" && string === "") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$string = string;
      } else if ($ === "\r\n" && string === "") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$string = string;
      } else {
        let g = $;
        let input$1 = input.tail;
        loop$input = input$1;
        loop$string = string + g;
      }
    }
  }
}

function parse_literal_string(loop$input, loop$string) {
  while (true) {
    let input = loop$input;
    let string = loop$string;
    if (input instanceof $Empty) {
      return new Error(new Unexpected("EOF", "\""));
    } else {
      let $ = input.head;
      if ($ === "\n") {
        return new Error(new Unexpected("\n", "'"));
      } else if ($ === "\r\n") {
        return new Error(new Unexpected("\r\n", "'"));
      } else if ($ === "'") {
        let input$1 = input.tail;
        return new Ok([new String(string), input$1]);
      } else {
        let g = $;
        let input$1 = input.tail;
        loop$input = input$1;
        loop$string = string + g;
      }
    }
  }
}

function parse_time_ns(loop$input, loop$seconds, loop$ns, loop$digits_count) {
  while (true) {
    let input = loop$input;
    let seconds = loop$seconds;
    let ns = loop$ns;
    let digits_count = loop$digits_count;
    if (input instanceof $Empty) {
      let exponent = $int.to_float(9 - digits_count);
      let $ = $float.power(10.0, exponent);
      let multiplier;
      if ($ instanceof Ok) {
        multiplier = $[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "tom",
          1200,
          "parse_time_ns",
          "Pattern match failed, no pattern matched the value.",
          {
            value: $,
            start: 37108,
            end: 37163,
            pattern_start: 37119,
            pattern_end: 37133
          }
        )
      }
      return new Ok([[seconds, ns * $float.truncate(multiplier)], input]);
    } else {
      let $ = input.head;
      if ($ === "0" && digits_count < 9) {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$seconds = seconds;
        loop$ns = ns * 10 + 0;
        loop$digits_count = digits_count + 1;
      } else if ($ === "1" && digits_count < 9) {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$seconds = seconds;
        loop$ns = ns * 10 + 1;
        loop$digits_count = digits_count + 1;
      } else if ($ === "2" && digits_count < 9) {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$seconds = seconds;
        loop$ns = ns * 10 + 2;
        loop$digits_count = digits_count + 1;
      } else if ($ === "3" && digits_count < 9) {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$seconds = seconds;
        loop$ns = ns * 10 + 3;
        loop$digits_count = digits_count + 1;
      } else if ($ === "4" && digits_count < 9) {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$seconds = seconds;
        loop$ns = ns * 10 + 4;
        loop$digits_count = digits_count + 1;
      } else if ($ === "5" && digits_count < 9) {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$seconds = seconds;
        loop$ns = ns * 10 + 5;
        loop$digits_count = digits_count + 1;
      } else if ($ === "6" && digits_count < 9) {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$seconds = seconds;
        loop$ns = ns * 10 + 6;
        loop$digits_count = digits_count + 1;
      } else if ($ === "7" && digits_count < 9) {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$seconds = seconds;
        loop$ns = ns * 10 + 7;
        loop$digits_count = digits_count + 1;
      } else if ($ === "8" && digits_count < 9) {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$seconds = seconds;
        loop$ns = ns * 10 + 8;
        loop$digits_count = digits_count + 1;
      } else if ($ === "9" && digits_count < 9) {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$seconds = seconds;
        loop$ns = ns * 10 + 9;
        loop$digits_count = digits_count + 1;
      } else {
        let exponent = $int.to_float(9 - digits_count);
        let $1 = $float.power(10.0, exponent);
        let multiplier;
        if ($1 instanceof Ok) {
          multiplier = $1[0];
        } else {
          throw makeError(
            "let_assert",
            FILEPATH,
            "tom",
            1200,
            "parse_time_ns",
            "Pattern match failed, no pattern matched the value.",
            {
              value: $1,
              start: 37108,
              end: 37163,
              pattern_start: 37119,
              pattern_end: 37133
            }
          )
        }
        return new Ok([[seconds, ns * $float.truncate(multiplier)], input]);
      }
    }
  }
}

function parse_number_under_60(input, expected) {
  if (input instanceof $Empty) {
    return new Error(new Unexpected("EOF", expected));
  } else {
    let $ = input.tail;
    if ($ instanceof $Empty) {
      let g = input.head;
      return new Error(new Unexpected(g, expected));
    } else {
      let $1 = $.head;
      if ($1 === "0") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return new Ok([0, input$1]);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return new Ok([10, input$1]);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return new Ok([20, input$1]);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return new Ok([30, input$1]);
        } else if ($2 === "4") {
          let input$1 = $.tail;
          return new Ok([40, input$1]);
        } else if ($2 === "5") {
          let input$1 = $.tail;
          return new Ok([50, input$1]);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, expected));
        }
      } else if ($1 === "1") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return new Ok([1, input$1]);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return new Ok([11, input$1]);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return new Ok([21, input$1]);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return new Ok([31, input$1]);
        } else if ($2 === "4") {
          let input$1 = $.tail;
          return new Ok([41, input$1]);
        } else if ($2 === "5") {
          let input$1 = $.tail;
          return new Ok([51, input$1]);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, expected));
        }
      } else if ($1 === "2") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return new Ok([2, input$1]);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return new Ok([12, input$1]);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return new Ok([22, input$1]);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return new Ok([32, input$1]);
        } else if ($2 === "4") {
          let input$1 = $.tail;
          return new Ok([42, input$1]);
        } else if ($2 === "5") {
          let input$1 = $.tail;
          return new Ok([52, input$1]);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, expected));
        }
      } else if ($1 === "3") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return new Ok([3, input$1]);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return new Ok([13, input$1]);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return new Ok([23, input$1]);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return new Ok([33, input$1]);
        } else if ($2 === "4") {
          let input$1 = $.tail;
          return new Ok([43, input$1]);
        } else if ($2 === "5") {
          let input$1 = $.tail;
          return new Ok([53, input$1]);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, expected));
        }
      } else if ($1 === "4") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return new Ok([4, input$1]);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return new Ok([14, input$1]);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return new Ok([24, input$1]);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return new Ok([34, input$1]);
        } else if ($2 === "4") {
          let input$1 = $.tail;
          return new Ok([44, input$1]);
        } else if ($2 === "5") {
          let input$1 = $.tail;
          return new Ok([54, input$1]);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, expected));
        }
      } else if ($1 === "5") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return new Ok([5, input$1]);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return new Ok([15, input$1]);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return new Ok([25, input$1]);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return new Ok([35, input$1]);
        } else if ($2 === "4") {
          let input$1 = $.tail;
          return new Ok([45, input$1]);
        } else if ($2 === "5") {
          let input$1 = $.tail;
          return new Ok([55, input$1]);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, expected));
        }
      } else if ($1 === "6") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return new Ok([6, input$1]);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return new Ok([16, input$1]);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return new Ok([26, input$1]);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return new Ok([36, input$1]);
        } else if ($2 === "4") {
          let input$1 = $.tail;
          return new Ok([46, input$1]);
        } else if ($2 === "5") {
          let input$1 = $.tail;
          return new Ok([56, input$1]);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, expected));
        }
      } else if ($1 === "7") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return new Ok([7, input$1]);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return new Ok([17, input$1]);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return new Ok([27, input$1]);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return new Ok([37, input$1]);
        } else if ($2 === "4") {
          let input$1 = $.tail;
          return new Ok([47, input$1]);
        } else if ($2 === "5") {
          let input$1 = $.tail;
          return new Ok([57, input$1]);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, expected));
        }
      } else if ($1 === "8") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return new Ok([8, input$1]);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return new Ok([18, input$1]);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return new Ok([28, input$1]);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return new Ok([38, input$1]);
        } else if ($2 === "4") {
          let input$1 = $.tail;
          return new Ok([48, input$1]);
        } else if ($2 === "5") {
          let input$1 = $.tail;
          return new Ok([58, input$1]);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, expected));
        }
      } else if ($1 === "9") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return new Ok([9, input$1]);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return new Ok([19, input$1]);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return new Ok([29, input$1]);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return new Ok([39, input$1]);
        } else if ($2 === "4") {
          let input$1 = $.tail;
          return new Ok([49, input$1]);
        } else if ($2 === "5") {
          let input$1 = $.tail;
          return new Ok([59, input$1]);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, expected));
        }
      } else {
        let g = input.head;
        return new Error(new Unexpected(g, expected));
      }
    }
  }
}

function parse_hour_minute(input) {
  return do$(
    (() => {
      if (input instanceof $Empty) {
        return new Error(new Unexpected("EOF", "time"));
      } else {
        let $ = input.tail;
        if ($ instanceof $Empty) {
          let g = input.head;
          return new Error(new Unexpected(g, "time"));
        } else {
          let $1 = $.tail;
          if ($1 instanceof $Empty) {
            let g = input.head;
            return new Error(new Unexpected(g, "time"));
          } else {
            let $2 = $1.head;
            if ($2 === ":") {
              let $3 = $.head;
              if ($3 === "0") {
                let $4 = input.head;
                if ($4 === "0") {
                  let input$1 = $1.tail;
                  return new Ok([0, input$1]);
                } else if ($4 === "1") {
                  let input$1 = $1.tail;
                  return new Ok([10, input$1]);
                } else if ($4 === "2") {
                  let input$1 = $1.tail;
                  return new Ok([20, input$1]);
                } else {
                  let g = $4;
                  return new Error(new Unexpected(g, "time"));
                }
              } else if ($3 === "1") {
                let $4 = input.head;
                if ($4 === "0") {
                  let input$1 = $1.tail;
                  return new Ok([1, input$1]);
                } else if ($4 === "1") {
                  let input$1 = $1.tail;
                  return new Ok([11, input$1]);
                } else if ($4 === "2") {
                  let input$1 = $1.tail;
                  return new Ok([21, input$1]);
                } else {
                  let g = $4;
                  return new Error(new Unexpected(g, "time"));
                }
              } else if ($3 === "2") {
                let $4 = input.head;
                if ($4 === "0") {
                  let input$1 = $1.tail;
                  return new Ok([2, input$1]);
                } else if ($4 === "1") {
                  let input$1 = $1.tail;
                  return new Ok([12, input$1]);
                } else if ($4 === "2") {
                  let input$1 = $1.tail;
                  return new Ok([22, input$1]);
                } else {
                  let g = $4;
                  return new Error(new Unexpected(g, "time"));
                }
              } else if ($3 === "3") {
                let $4 = input.head;
                if ($4 === "0") {
                  let input$1 = $1.tail;
                  return new Ok([3, input$1]);
                } else if ($4 === "1") {
                  let input$1 = $1.tail;
                  return new Ok([13, input$1]);
                } else if ($4 === "2") {
                  let input$1 = $1.tail;
                  return new Ok([23, input$1]);
                } else {
                  let g = $4;
                  return new Error(new Unexpected(g, "time"));
                }
              } else if ($3 === "4") {
                let $4 = input.head;
                if ($4 === "0") {
                  let input$1 = $1.tail;
                  return new Ok([4, input$1]);
                } else if ($4 === "1") {
                  let input$1 = $1.tail;
                  return new Ok([14, input$1]);
                } else {
                  let g = $4;
                  return new Error(new Unexpected(g, "time"));
                }
              } else if ($3 === "5") {
                let $4 = input.head;
                if ($4 === "0") {
                  let input$1 = $1.tail;
                  return new Ok([5, input$1]);
                } else if ($4 === "1") {
                  let input$1 = $1.tail;
                  return new Ok([15, input$1]);
                } else {
                  let g = $4;
                  return new Error(new Unexpected(g, "time"));
                }
              } else if ($3 === "6") {
                let $4 = input.head;
                if ($4 === "0") {
                  let input$1 = $1.tail;
                  return new Ok([6, input$1]);
                } else if ($4 === "1") {
                  let input$1 = $1.tail;
                  return new Ok([16, input$1]);
                } else {
                  let g = $4;
                  return new Error(new Unexpected(g, "time"));
                }
              } else if ($3 === "7") {
                let $4 = input.head;
                if ($4 === "0") {
                  let input$1 = $1.tail;
                  return new Ok([7, input$1]);
                } else if ($4 === "1") {
                  let input$1 = $1.tail;
                  return new Ok([17, input$1]);
                } else {
                  let g = $4;
                  return new Error(new Unexpected(g, "time"));
                }
              } else if ($3 === "8") {
                let $4 = input.head;
                if ($4 === "0") {
                  let input$1 = $1.tail;
                  return new Ok([8, input$1]);
                } else if ($4 === "1") {
                  let input$1 = $1.tail;
                  return new Ok([18, input$1]);
                } else {
                  let g = $4;
                  return new Error(new Unexpected(g, "time"));
                }
              } else if ($3 === "9") {
                let $4 = input.head;
                if ($4 === "0") {
                  let input$1 = $1.tail;
                  return new Ok([9, input$1]);
                } else if ($4 === "1") {
                  let input$1 = $1.tail;
                  return new Ok([19, input$1]);
                } else {
                  let g = $4;
                  return new Error(new Unexpected(g, "time"));
                }
              } else {
                let g = input.head;
                return new Error(new Unexpected(g, "time"));
              }
            } else {
              let g = input.head;
              return new Error(new Unexpected(g, "time"));
            }
          }
        }
      }
    })(),
    (hours, input) => {
      return do$(
        parse_number_under_60(input, "minutes"),
        (minutes, input) => { return new Ok([[hours, minutes], input]); },
      );
    },
  );
}

function parse_time_s_ns(input) {
  if (input instanceof $Empty) {
    return new Ok([[0, 0], input]);
  } else {
    let $ = input.head;
    if ($ === ":") {
      let input$1 = input.tail;
      return do$(
        parse_number_under_60(input$1, "seconds"),
        (seconds, input) => {
          if (input instanceof $Empty) {
            return new Ok([[seconds, 0], input]);
          } else {
            let $1 = input.head;
            if ($1 === ".") {
              let input$1 = input.tail;
              return parse_time_ns(input$1, seconds, 0, 0);
            } else {
              return new Ok([[seconds, 0], input]);
            }
          }
        },
      );
    } else {
      return new Ok([[0, 0], input]);
    }
  }
}

function parse_time_minute(input, hours) {
  return do$(
    parse_number_under_60(input, "minutes"),
    (minutes, input) => {
      return do$(
        parse_time_s_ns(input),
        (_use0, input) => {
          let seconds;
          let ns;
          seconds = _use0[0];
          ns = _use0[1];
          let time = new $calendar.TimeOfDay(hours, minutes, seconds, ns);
          return new Ok([new Time(time), input]);
        },
      );
    },
  );
}

function parse_time_value(input) {
  return do$(
    parse_hour_minute(input),
    (_use0, input) => {
      let hours;
      let minutes;
      hours = _use0[0];
      minutes = _use0[1];
      return do$(
        parse_time_s_ns(input),
        (_use0, input) => {
          let seconds;
          let ns;
          seconds = _use0[0];
          ns = _use0[1];
          let time = new $calendar.TimeOfDay(hours, minutes, seconds, ns);
          return new Ok([time, input]);
        },
      );
    },
  );
}

function parse_offset_hours(input, sign) {
  return do$(
    parse_hour_minute(input),
    (_use0, input) => {
      let hours;
      let minutes;
      hours = _use0[0];
      minutes = _use0[1];
      let _block;
      if (sign instanceof Positive) {
        _block = $duration.add(
          $duration.hours(hours),
          $duration.minutes(minutes),
        );
      } else {
        _block = $duration.add(
          $duration.hours(- hours),
          $duration.minutes(- minutes),
        );
      }
      let duration = _block;
      return new Ok([new Offset(duration), input]);
    },
  );
}

function parse_offset(input) {
  if (input instanceof $Empty) {
    return new Ok([new Local(), input]);
  } else {
    let $ = input.head;
    if ($ === "Z") {
      let input$1 = input.tail;
      return new Ok([new Offset($calendar.utc_offset), input$1]);
    } else if ($ === "+") {
      let input$1 = input.tail;
      return parse_offset_hours(input$1, new Positive());
    } else if ($ === "-") {
      let input$1 = input.tail;
      return parse_offset_hours(input$1, new Negative());
    } else {
      return new Ok([new Local(), input]);
    }
  }
}

function parse_date_end(input, year, month, day) {
  let date = new $calendar.Date(year, month, day);
  if (input instanceof $Empty) {
    return new Ok([new Date(date), input]);
  } else {
    let $ = input.head;
    if ($ === " ") {
      let input$1 = input.tail;
      return do$(
        parse_time_value(input$1),
        (time, input) => {
          return do$(
            parse_offset(input),
            (offset, input) => {
              return new Ok([new DateTime(date, time, offset), input]);
            },
          );
        },
      );
    } else if ($ === "T") {
      let input$1 = input.tail;
      return do$(
        parse_time_value(input$1),
        (time, input) => {
          return do$(
            parse_offset(input),
            (offset, input) => {
              return new Ok([new DateTime(date, time, offset), input]);
            },
          );
        },
      );
    } else {
      return new Ok([new Date(date), input]);
    }
  }
}

function parse_date_day(input, year, month) {
  if (input instanceof $Empty) {
    return new Error(new Unexpected("EOF", "date day"));
  } else {
    let $ = input.tail;
    if ($ instanceof $Empty) {
      let g = input.head;
      return new Error(new Unexpected(g, "date day"));
    } else {
      let $1 = $.head;
      if ($1 === "1") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 1);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 11);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 21);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 31);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, "date day"));
        }
      } else if ($1 === "2") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 2);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 12);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 22);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, "date day"));
        }
      } else if ($1 === "3") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 3);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 13);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 23);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, "date day"));
        }
      } else if ($1 === "4") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 4);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 14);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 24);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, "date day"));
        }
      } else if ($1 === "5") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 5);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 15);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 25);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, "date day"));
        }
      } else if ($1 === "6") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 6);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 16);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 26);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, "date day"));
        }
      } else if ($1 === "7") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 7);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 17);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 27);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, "date day"));
        }
      } else if ($1 === "8") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 8);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 18);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 28);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, "date day"));
        }
      } else if ($1 === "9") {
        let $2 = input.head;
        if ($2 === "0") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 9);
        } else if ($2 === "1") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 19);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 29);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, "date day"));
        }
      } else if ($1 === "0") {
        let $2 = input.head;
        if ($2 === "1") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 10);
        } else if ($2 === "2") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 20);
        } else if ($2 === "3") {
          let input$1 = $.tail;
          return parse_date_end(input$1, year, month, 30);
        } else {
          let g = $2;
          return new Error(new Unexpected(g, "date day"));
        }
      } else {
        let g = input.head;
        return new Error(new Unexpected(g, "date day"));
      }
    }
  }
}

function parse_date(input, year) {
  if (input instanceof $Empty) {
    return new Error(new Unexpected("EOF", "date month"));
  } else {
    let $ = input.tail;
    if ($ instanceof $Empty) {
      let g = input.head;
      return new Error(new Unexpected(g, "date month"));
    } else {
      let $1 = $.tail;
      if ($1 instanceof $Empty) {
        let g = input.head;
        return new Error(new Unexpected(g, "date month"));
      } else {
        let $2 = $1.head;
        if ($2 === "-") {
          let $3 = $.head;
          if ($3 === "1") {
            let $4 = input.head;
            if ($4 === "0") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.January());
            } else if ($4 === "1") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.November());
            } else {
              let g = $4;
              return new Error(new Unexpected(g, "date month"));
            }
          } else if ($3 === "2") {
            let $4 = input.head;
            if ($4 === "0") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.February());
            } else if ($4 === "1") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.December());
            } else {
              let g = $4;
              return new Error(new Unexpected(g, "date month"));
            }
          } else if ($3 === "3") {
            let $4 = input.head;
            if ($4 === "0") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.March());
            } else {
              let g = $4;
              return new Error(new Unexpected(g, "date month"));
            }
          } else if ($3 === "4") {
            let $4 = input.head;
            if ($4 === "0") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.April());
            } else {
              let g = $4;
              return new Error(new Unexpected(g, "date month"));
            }
          } else if ($3 === "5") {
            let $4 = input.head;
            if ($4 === "0") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.May());
            } else {
              let g = $4;
              return new Error(new Unexpected(g, "date month"));
            }
          } else if ($3 === "6") {
            let $4 = input.head;
            if ($4 === "0") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.June());
            } else {
              let g = $4;
              return new Error(new Unexpected(g, "date month"));
            }
          } else if ($3 === "7") {
            let $4 = input.head;
            if ($4 === "0") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.July());
            } else {
              let g = $4;
              return new Error(new Unexpected(g, "date month"));
            }
          } else if ($3 === "8") {
            let $4 = input.head;
            if ($4 === "0") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.August());
            } else {
              let g = $4;
              return new Error(new Unexpected(g, "date month"));
            }
          } else if ($3 === "9") {
            let $4 = input.head;
            if ($4 === "0") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.September());
            } else {
              let g = $4;
              return new Error(new Unexpected(g, "date month"));
            }
          } else if ($3 === "0") {
            let $4 = input.head;
            if ($4 === "1") {
              let input$1 = $1.tail;
              return parse_date_day(input$1, year, new $calendar.October());
            } else {
              let g = $4;
              return new Error(new Unexpected(g, "date month"));
            }
          } else {
            let g = input.head;
            return new Error(new Unexpected(g, "date month"));
          }
        } else {
          let g = input.head;
          return new Error(new Unexpected(g, "date month"));
        }
      }
    }
  }
}

function parse_number(loop$input, loop$number, loop$sign) {
  while (true) {
    let input = loop$input;
    let number = loop$number;
    let sign = loop$sign;
    if (input instanceof $Empty) {
      let input$1 = input;
      let _block;
      if (sign instanceof Positive) {
        _block = number;
      } else {
        _block = - number;
      }
      let number$1 = _block;
      return new Ok([new Int(number$1), input$1]);
    } else {
      let $ = input.head;
      if ($ === "_") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number;
        loop$sign = sign;
      } else if ($ === "0") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 10 + 0;
        loop$sign = sign;
      } else if ($ === "1") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 10 + 1;
        loop$sign = sign;
      } else if ($ === "2") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 10 + 2;
        loop$sign = sign;
      } else if ($ === "3") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 10 + 3;
        loop$sign = sign;
      } else if ($ === "4") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 10 + 4;
        loop$sign = sign;
      } else if ($ === "5") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 10 + 5;
        loop$sign = sign;
      } else if ($ === "6") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 10 + 6;
        loop$sign = sign;
      } else if ($ === "7") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 10 + 7;
        loop$sign = sign;
      } else if ($ === "8") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 10 + 8;
        loop$sign = sign;
      } else if ($ === "9") {
        let input$1 = input.tail;
        loop$input = input$1;
        loop$number = number * 10 + 9;
        loop$sign = sign;
      } else if ($ === "-") {
        let input$1 = input.tail;
        return parse_date(input$1, number);
      } else if ($ === ":" && number < 24) {
        let input$1 = input.tail;
        return parse_time_minute(input$1, number);
      } else if ($ === ".") {
        let input$1 = input.tail;
        return parse_float(input$1, $int.to_float(number), sign, 0.1);
      } else if ($ === "e") {
        let $1 = input.tail;
        if ($1 instanceof $Empty) {
          let input$1 = $1;
          return parse_exponent(
            input$1,
            $int.to_float(number),
            sign,
            0,
            new Positive(),
          );
        } else {
          let $2 = $1.head;
          if ($2 === "+") {
            let input$1 = $1.tail;
            return parse_exponent(
              input$1,
              $int.to_float(number),
              sign,
              0,
              new Positive(),
            );
          } else if ($2 === "-") {
            let input$1 = $1.tail;
            return parse_exponent(
              input$1,
              $int.to_float(number),
              sign,
              0,
              new Negative(),
            );
          } else {
            let input$1 = $1;
            return parse_exponent(
              input$1,
              $int.to_float(number),
              sign,
              0,
              new Positive(),
            );
          }
        }
      } else if ($ === "E") {
        let $1 = input.tail;
        if ($1 instanceof $Empty) {
          let input$1 = $1;
          return parse_exponent(
            input$1,
            $int.to_float(number),
            sign,
            0,
            new Positive(),
          );
        } else {
          let $2 = $1.head;
          if ($2 === "+") {
            let input$1 = $1.tail;
            return parse_exponent(
              input$1,
              $int.to_float(number),
              sign,
              0,
              new Positive(),
            );
          } else if ($2 === "-") {
            let input$1 = $1.tail;
            return parse_exponent(
              input$1,
              $int.to_float(number),
              sign,
              0,
              new Negative(),
            );
          } else {
            let input$1 = $1;
            return parse_exponent(
              input$1,
              $int.to_float(number),
              sign,
              0,
              new Positive(),
            );
          }
        }
      } else {
        let input$1 = input;
        let _block;
        if (sign instanceof Positive) {
          _block = number;
        } else {
          _block = - number;
        }
        let number$1 = _block;
        return new Ok([new Int(number$1), input$1]);
      }
    }
  }
}

/**
 * Get a int from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_int(Int(1))
 * // -> Ok(1)
 * ```
 *
 * ```gleam
 * as_int(Float(1.4))
 * // -> Error(WrongType([], "Int", "Float"))
 * ```
 */
export function as_int(toml) {
  if (toml instanceof Int) {
    let f = toml[0];
    return new Ok(f);
  } else {
    let other = toml;
    return new Error(new WrongType(toList([]), "Int", classify(other)));
  }
}

/**
 * Get a float from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_float(Float(1.5))
 * // -> Ok(1.5)
 * ```
 *
 * ```gleam
 * as_float(Int(1))
 * // -> Error(WrongType([], "Float", "Int"))
 * ```
 */
export function as_float(toml) {
  if (toml instanceof Float) {
    let f = toml[0];
    return new Ok(f);
  } else {
    let other = toml;
    return new Error(new WrongType(toList([]), "Float", classify(other)));
  }
}

/**
 * Get a bool from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_bool(Bool(true))
 * // -> Ok(true)
 * ```
 *
 * ```gleam
 * as_bool(Int(1))
 * // -> Error(WrongType([], "Bool", "Int"))
 * ```
 */
export function as_bool(toml) {
  if (toml instanceof Bool) {
    let b = toml[0];
    return new Ok(b);
  } else {
    let other = toml;
    return new Error(new WrongType(toList([]), "Bool", classify(other)));
  }
}

/**
 * Get a string from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_string(String("hello"))
 * // -> Ok("hello")
 * ```
 *
 * ```gleam
 * as_string(Int(1))
 * // -> Error(WrongType([], "String", "Int"))
 * ```
 */
export function as_string(toml) {
  if (toml instanceof String) {
    let s = toml[0];
    return new Ok(s);
  } else {
    let other = toml;
    return new Error(new WrongType(toList([]), "String", classify(other)));
  }
}

/**
 * Get a date from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_date(Date(date))
 * // -> Ok(date)
 * ```
 *
 * ```gleam
 * as_date(Int(1))
 * // -> Error(WrongType([], "Date", "Int"))
 * ```
 */
export function as_date(toml) {
  if (toml instanceof Date) {
    let d = toml[0];
    return new Ok(d);
  } else {
    let other = toml;
    return new Error(new WrongType(toList([]), "Date", classify(other)));
  }
}

/**
 * Get a time from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_time_of_day(Time(time))
 * // -> Ok(time)
 * ```
 *
 * ```gleam
 * as_time_of_day(Int(1))
 * // -> Error(WrongType([], "Time", "Int"))
 * ```
 */
export function as_time_of_day(toml) {
  if (toml instanceof Time) {
    let t = toml[0];
    return new Ok(t);
  } else {
    let other = toml;
    return new Error(new WrongType(toList([]), "Time", classify(other)));
  }
}

/**
 * Get an unambiguous time from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_timestamp(Int(1))
 * // -> Error(WrongType([], "DateTime with offset", "Int"))
 * ```
 */
export function as_timestamp(toml) {
  if (toml instanceof DateTime) {
    let $ = toml.offset;
    if ($ instanceof Offset) {
      let date = toml.date;
      let time = toml.time;
      let offset = $[0];
      return new Ok($timestamp.from_calendar(date, time, offset));
    } else {
      let other = toml;
      return new Error(
        new WrongType(toList([]), "DateTime with offset", classify(other)),
      );
    }
  } else {
    let other = toml;
    return new Error(
      new WrongType(toList([]), "DateTime with offset", classify(other)),
    );
  }
}

/**
 * Get a datetime from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_calendar_time(DateTime(datetime))
 * // -> Ok(datetime)
 * ```
 *
 * ```gleam
 * as_calendar_time(Int(1))
 * // -> Error(WrongType([], "DateTime", "Int"))
 * ```
 */
export function as_calendar_time(toml) {
  if (toml instanceof DateTime) {
    let d = toml.date;
    let t = toml.time;
    let o = toml.offset;
    return new Ok([d, t, o]);
  } else {
    let other = toml;
    return new Error(new WrongType(toList([]), "DateTime", classify(other)));
  }
}

/**
 * Get an array from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_array(Array([]))
 * // -> Ok([])
 * ```
 *
 * ```gleam
 * as_array(Int(1))
 * // -> Error(WrongType([], "Array", "Int"))
 * ```
 */
export function as_array(toml) {
  if (toml instanceof Array) {
    let arr = toml[0];
    return new Ok(arr);
  } else {
    let other = toml;
    return new Error(new WrongType(toList([]), "Array", classify(other)));
  }
}

/**
 * Get a table from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_table(Table(dict.new()))
 * // -> Ok(dict.new())
 * ```
 *
 * ```gleam
 * as_table(Int(1))
 * // -> Error(WrongType([], "Table", "Int"))
 * ```
 */
export function as_table(toml) {
  if (toml instanceof Table) {
    let tbl = toml[0];
    return new Ok(tbl);
  } else if (toml instanceof InlineTable) {
    let tbl = toml[0];
    return new Ok(tbl);
  } else {
    let other = toml;
    return new Error(new WrongType(toList([]), "Table", classify(other)));
  }
}

/**
 * Get a number (int or float) from a TOML document.
 *
 * ## Examples
 *
 * ```gleam
 * as_number(Int(1))
 * // -> Ok(NumberInt(1))
 * ```
 *
 * ```gleam
 * as_number(Float(1.5))
 * // -> Ok(NumberFloat(1.5))
 * ```
 *
 * ```gleam
 * as_number(Bool(true))
 * // -> Error(WrongType([], "Number", "Bool"))
 * ```
 */
export function as_number(toml) {
  if (toml instanceof Int) {
    let x = toml[0];
    return new Ok(new NumberInt(x));
  } else if (toml instanceof Float) {
    let x = toml[0];
    return new Ok(new NumberFloat(x));
  } else if (toml instanceof Infinity) {
    let x = toml[0];
    return new Ok(new NumberInfinity(x));
  } else if (toml instanceof Nan) {
    let x = toml[0];
    return new Ok(new NumberNan(x));
  } else {
    let other = toml;
    return new Error(new WrongType(toList([]), "Number", classify(other)));
  }
}

function reverse_arrays_of_tables(toml) {
  if (toml instanceof ArrayOfTables) {
    let tables = toml[0];
    return new ArrayOfTables(reverse_arrays_of_tables_array(tables, toList([])));
  } else if (toml instanceof Table) {
    let table = toml[0];
    return new Table(reverse_arrays_of_tables_table(table));
  } else {
    return toml;
  }
}

function reverse_arrays_of_tables_table(table) {
  return $dict.map_values(
    table,
    (_, v) => { return reverse_arrays_of_tables(v); },
  );
}

function reverse_arrays_of_tables_array(loop$array, loop$acc) {
  while (true) {
    let array = loop$array;
    let acc = loop$acc;
    if (array instanceof $Empty) {
      return acc;
    } else {
      let first = array.head;
      let rest = array.tail;
      let first$1 = reverse_arrays_of_tables_table(first);
      loop$array = rest;
      loop$acc = listPrepend(first$1, acc);
    }
  }
}

function parse_inline_table_property(input, properties) {
  let input$1 = skip_whitespace(input);
  return do$(
    parse_key(input$1, toList([])),
    (key, input) => {
      let input$1 = skip_line_whitespace(input);
      return expect(
        input$1,
        "=",
        (input) => {
          let input$1 = skip_line_whitespace(input);
          return do$(
            parse_value(input$1),
            (value, input) => {
              let $ = insert(properties, key, value);
              if ($ instanceof Ok) {
                let properties$1 = $[0];
                return new Ok([properties$1, input]);
              } else {
                return $;
              }
            },
          );
        },
      );
    },
  );
}

function parse_value(input) {
  if (input instanceof $Empty) {
    return new Error(new Unexpected("EOF", "value"));
  } else {
    let $ = input.head;
    if ($ === "t") {
      let $1 = input.tail;
      if ($1 instanceof $Empty) {
        let g = $;
        return new Error(new Unexpected(g, "value"));
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let g = $;
          return new Error(new Unexpected(g, "value"));
        } else {
          let $3 = $2.tail;
          if ($3 instanceof $Empty) {
            let g = $;
            return new Error(new Unexpected(g, "value"));
          } else {
            let $4 = $3.head;
            if ($4 === "e") {
              let $5 = $2.head;
              if ($5 === "u") {
                let $6 = $1.head;
                if ($6 === "r") {
                  let input$1 = $3.tail;
                  return new Ok([new Bool(true), input$1]);
                } else {
                  let g = $;
                  return new Error(new Unexpected(g, "value"));
                }
              } else {
                let g = $;
                return new Error(new Unexpected(g, "value"));
              }
            } else {
              let g = $;
              return new Error(new Unexpected(g, "value"));
            }
          }
        }
      }
    } else if ($ === "f") {
      let $1 = input.tail;
      if ($1 instanceof $Empty) {
        let g = $;
        return new Error(new Unexpected(g, "value"));
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let g = $;
          return new Error(new Unexpected(g, "value"));
        } else {
          let $3 = $2.tail;
          if ($3 instanceof $Empty) {
            let g = $;
            return new Error(new Unexpected(g, "value"));
          } else {
            let $4 = $3.tail;
            if ($4 instanceof $Empty) {
              let g = $;
              return new Error(new Unexpected(g, "value"));
            } else {
              let $5 = $4.head;
              if ($5 === "e") {
                let $6 = $3.head;
                if ($6 === "s") {
                  let $7 = $2.head;
                  if ($7 === "l") {
                    let $8 = $1.head;
                    if ($8 === "a") {
                      let input$1 = $4.tail;
                      return new Ok([new Bool(false), input$1]);
                    } else {
                      let g = $;
                      return new Error(new Unexpected(g, "value"));
                    }
                  } else {
                    let g = $;
                    return new Error(new Unexpected(g, "value"));
                  }
                } else {
                  let g = $;
                  return new Error(new Unexpected(g, "value"));
                }
              } else {
                let g = $;
                return new Error(new Unexpected(g, "value"));
              }
            }
          }
        }
      }
    } else if ($ === "n") {
      let $1 = input.tail;
      if ($1 instanceof $Empty) {
        let g = $;
        return new Error(new Unexpected(g, "value"));
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let g = $;
          return new Error(new Unexpected(g, "value"));
        } else {
          let $3 = $2.head;
          if ($3 === "n") {
            let $4 = $1.head;
            if ($4 === "a") {
              let input$1 = $2.tail;
              return new Ok([new Nan(new Positive()), input$1]);
            } else {
              let g = $;
              return new Error(new Unexpected(g, "value"));
            }
          } else {
            let g = $;
            return new Error(new Unexpected(g, "value"));
          }
        }
      }
    } else if ($ === "+") {
      let $1 = input.tail;
      if ($1 instanceof $Empty) {
        let input$1 = $1;
        return parse_number(input$1, 0, new Positive());
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let input$1 = $1;
          return parse_number(input$1, 0, new Positive());
        } else {
          let $3 = $2.head;
          if ($3 === "a") {
            let $4 = $2.tail;
            if ($4 instanceof $Empty) {
              let input$1 = $1;
              return parse_number(input$1, 0, new Positive());
            } else {
              let $5 = $4.head;
              if ($5 === "n") {
                let $6 = $1.head;
                if ($6 === "n") {
                  let input$1 = $4.tail;
                  return new Ok([new Nan(new Positive()), input$1]);
                } else {
                  let input$1 = $1;
                  return parse_number(input$1, 0, new Positive());
                }
              } else {
                let input$1 = $1;
                return parse_number(input$1, 0, new Positive());
              }
            }
          } else if ($3 === "n") {
            let $4 = $2.tail;
            if ($4 instanceof $Empty) {
              let input$1 = $1;
              return parse_number(input$1, 0, new Positive());
            } else {
              let $5 = $4.head;
              if ($5 === "f") {
                let $6 = $1.head;
                if ($6 === "i") {
                  let input$1 = $4.tail;
                  return new Ok([new Infinity(new Positive()), input$1]);
                } else {
                  let input$1 = $1;
                  return parse_number(input$1, 0, new Positive());
                }
              } else {
                let input$1 = $1;
                return parse_number(input$1, 0, new Positive());
              }
            }
          } else if ($3 === "x") {
            let $4 = $1.head;
            if ($4 === "0") {
              let input$1 = $2.tail;
              return parse_hex(input$1, 0, new Positive());
            } else {
              let input$1 = $1;
              return parse_number(input$1, 0, new Positive());
            }
          } else if ($3 === "o") {
            let $4 = $1.head;
            if ($4 === "0") {
              let input$1 = $2.tail;
              return parse_octal(input$1, 0, new Positive());
            } else {
              let input$1 = $1;
              return parse_number(input$1, 0, new Positive());
            }
          } else if ($3 === "b") {
            let $4 = $1.head;
            if ($4 === "0") {
              let input$1 = $2.tail;
              return parse_binary(input$1, 0, new Positive());
            } else {
              let input$1 = $1;
              return parse_number(input$1, 0, new Positive());
            }
          } else {
            let input$1 = $1;
            return parse_number(input$1, 0, new Positive());
          }
        }
      }
    } else if ($ === "-") {
      let $1 = input.tail;
      if ($1 instanceof $Empty) {
        let input$1 = $1;
        return parse_number(input$1, 0, new Negative());
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let input$1 = $1;
          return parse_number(input$1, 0, new Negative());
        } else {
          let $3 = $2.head;
          if ($3 === "a") {
            let $4 = $2.tail;
            if ($4 instanceof $Empty) {
              let input$1 = $1;
              return parse_number(input$1, 0, new Negative());
            } else {
              let $5 = $4.head;
              if ($5 === "n") {
                let $6 = $1.head;
                if ($6 === "n") {
                  let input$1 = $4.tail;
                  return new Ok([new Nan(new Negative()), input$1]);
                } else {
                  let input$1 = $1;
                  return parse_number(input$1, 0, new Negative());
                }
              } else {
                let input$1 = $1;
                return parse_number(input$1, 0, new Negative());
              }
            }
          } else if ($3 === "n") {
            let $4 = $2.tail;
            if ($4 instanceof $Empty) {
              let input$1 = $1;
              return parse_number(input$1, 0, new Negative());
            } else {
              let $5 = $4.head;
              if ($5 === "f") {
                let $6 = $1.head;
                if ($6 === "i") {
                  let input$1 = $4.tail;
                  return new Ok([new Infinity(new Negative()), input$1]);
                } else {
                  let input$1 = $1;
                  return parse_number(input$1, 0, new Negative());
                }
              } else {
                let input$1 = $1;
                return parse_number(input$1, 0, new Negative());
              }
            }
          } else if ($3 === "x") {
            let $4 = $1.head;
            if ($4 === "0") {
              let input$1 = $2.tail;
              return parse_hex(input$1, 0, new Negative());
            } else {
              let input$1 = $1;
              return parse_number(input$1, 0, new Negative());
            }
          } else if ($3 === "o") {
            let $4 = $1.head;
            if ($4 === "0") {
              let input$1 = $2.tail;
              return parse_octal(input$1, 0, new Negative());
            } else {
              let input$1 = $1;
              return parse_number(input$1, 0, new Negative());
            }
          } else if ($3 === "b") {
            let $4 = $1.head;
            if ($4 === "0") {
              let input$1 = $2.tail;
              return parse_binary(input$1, 0, new Negative());
            } else {
              let input$1 = $1;
              return parse_number(input$1, 0, new Negative());
            }
          } else {
            let input$1 = $1;
            return parse_number(input$1, 0, new Negative());
          }
        }
      }
    } else if ($ === "i") {
      let $1 = input.tail;
      if ($1 instanceof $Empty) {
        let g = $;
        return new Error(new Unexpected(g, "value"));
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let g = $;
          return new Error(new Unexpected(g, "value"));
        } else {
          let $3 = $2.head;
          if ($3 === "f") {
            let $4 = $1.head;
            if ($4 === "n") {
              let input$1 = $2.tail;
              return new Ok([new Infinity(new Positive()), input$1]);
            } else {
              let g = $;
              return new Error(new Unexpected(g, "value"));
            }
          } else {
            let g = $;
            return new Error(new Unexpected(g, "value"));
          }
        }
      }
    } else if ($ === "[") {
      let input$1 = input.tail;
      return parse_array(input$1, toList([]));
    } else if ($ === "{") {
      let input$1 = input.tail;
      return parse_inline_table(input$1, $dict.new$());
    } else if ($ === "0") {
      let $1 = input.tail;
      if ($1 instanceof $Empty) {
        return parse_number(input, 0, new Positive());
      } else {
        let $2 = $1.head;
        if ($2 === "x") {
          let input$1 = $1.tail;
          return parse_hex(input$1, 0, new Positive());
        } else if ($2 === "o") {
          let input$1 = $1.tail;
          return parse_octal(input$1, 0, new Positive());
        } else if ($2 === "b") {
          let input$1 = $1.tail;
          return parse_binary(input$1, 0, new Positive());
        } else {
          return parse_number(input, 0, new Positive());
        }
      }
    } else if ($ === "1") {
      return parse_number(input, 0, new Positive());
    } else if ($ === "2") {
      return parse_number(input, 0, new Positive());
    } else if ($ === "3") {
      return parse_number(input, 0, new Positive());
    } else if ($ === "4") {
      return parse_number(input, 0, new Positive());
    } else if ($ === "5") {
      return parse_number(input, 0, new Positive());
    } else if ($ === "6") {
      return parse_number(input, 0, new Positive());
    } else if ($ === "7") {
      return parse_number(input, 0, new Positive());
    } else if ($ === "8") {
      return parse_number(input, 0, new Positive());
    } else if ($ === "9") {
      return parse_number(input, 0, new Positive());
    } else if ($ === "\"") {
      let $1 = input.tail;
      if ($1 instanceof $Empty) {
        let input$1 = $1;
        return parse_string(input$1, "");
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let input$1 = $1;
          return parse_string(input$1, "");
        } else {
          let $3 = $2.head;
          if ($3 === "\"") {
            let $4 = $1.head;
            if ($4 === "\"") {
              let input$1 = $2.tail;
              return parse_multi_line_string(input$1, "");
            } else {
              let input$1 = $1;
              return parse_string(input$1, "");
            }
          } else {
            let input$1 = $1;
            return parse_string(input$1, "");
          }
        }
      }
    } else if ($ === "'") {
      let $1 = input.tail;
      if ($1 instanceof $Empty) {
        let input$1 = $1;
        return parse_literal_string(input$1, "");
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let input$1 = $1;
          return parse_literal_string(input$1, "");
        } else {
          let $3 = $2.head;
          if ($3 === "'") {
            let $4 = $1.head;
            if ($4 === "'") {
              let input$1 = $2.tail;
              return parse_multi_line_literal_string(input$1, "");
            } else {
              let input$1 = $1;
              return parse_literal_string(input$1, "");
            }
          } else {
            let input$1 = $1;
            return parse_literal_string(input$1, "");
          }
        }
      }
    } else {
      let g = $;
      return new Error(new Unexpected(g, "value"));
    }
  }
}

function parse_inline_table(loop$input, loop$properties) {
  while (true) {
    let input = loop$input;
    let properties = loop$properties;
    let input$1 = skip_whitespace(input);
    if (input$1 instanceof $Empty) {
      let $ = parse_inline_table_property(input$1, properties);
      if ($ instanceof Ok) {
        let properties$1 = $[0][0];
        let input$2 = $[0][1];
        let input$3 = skip_whitespace(input$2);
        if (input$3 instanceof $Empty) {
          return new Error(new Unexpected("EOF", "}"));
        } else {
          let $1 = input$3.head;
          if ($1 === "}") {
            let input$4 = input$3.tail;
            return new Ok([new InlineTable(properties$1), input$4]);
          } else if ($1 === ",") {
            let input$4 = input$3.tail;
            let input$5 = skip_whitespace(input$4);
            loop$input = input$5;
            loop$properties = properties$1;
          } else {
            let g = $1;
            return new Error(new Unexpected(g, "}"));
          }
        }
      } else {
        return $;
      }
    } else {
      let $ = input$1.head;
      if ($ === "}") {
        let input$2 = input$1.tail;
        return new Ok([new InlineTable(properties), input$2]);
      } else {
        let $1 = parse_inline_table_property(input$1, properties);
        if ($1 instanceof Ok) {
          let properties$1 = $1[0][0];
          let input$2 = $1[0][1];
          let input$3 = skip_whitespace(input$2);
          if (input$3 instanceof $Empty) {
            return new Error(new Unexpected("EOF", "}"));
          } else {
            let $2 = input$3.head;
            if ($2 === "}") {
              let input$4 = input$3.tail;
              return new Ok([new InlineTable(properties$1), input$4]);
            } else if ($2 === ",") {
              let input$4 = input$3.tail;
              let input$5 = skip_whitespace(input$4);
              loop$input = input$5;
              loop$properties = properties$1;
            } else {
              let g = $2;
              return new Error(new Unexpected(g, "}"));
            }
          }
        } else {
          return $1;
        }
      }
    }
  }
}

function parse_key_value(input, toml) {
  return do$(
    parse_key(input, toList([])),
    (key, input) => {
      let input$1 = skip_line_whitespace(input);
      return expect(
        input$1,
        "=",
        (input) => {
          let input$1 = skip_line_whitespace(input);
          return do$(
            parse_value(input$1),
            (value, input) => {
              let $ = insert(toml, key, value);
              if ($ instanceof Ok) {
                let toml$1 = $[0];
                return new Ok([toml$1, input]);
              } else {
                return $;
              }
            },
          );
        },
      );
    },
  );
}

function parse_table(loop$input, loop$toml) {
  while (true) {
    let input = loop$input;
    let toml = loop$toml;
    let input$1 = skip_whitespace(input);
    if (input$1 instanceof $Empty) {
      return new Ok([toml, input$1]);
    } else {
      let $ = input$1.head;
      if ($ === "[") {
        return new Ok([toml, input$1]);
      } else {
        let $1 = parse_key_value(input$1, toml);
        if ($1 instanceof Ok) {
          let toml$1 = $1[0][0];
          let input$2 = $1[0][1];
          let $2 = skip_line_whitespace(input$2);
          if ($2 instanceof $Empty) {
            return new Ok([toml$1, toList([])]);
          } else {
            let $3 = $2.head;
            if ($3 === "\n") {
              let in$ = $2.tail;
              loop$input = in$;
              loop$toml = toml$1;
            } else if ($3 === "\r\n") {
              let in$ = $2.tail;
              loop$input = in$;
              loop$toml = toml$1;
            } else {
              let g = $3;
              return new Error(new Unexpected(g, "\n"));
            }
          }
        } else {
          return $1;
        }
      }
    }
  }
}

function parse_array_of_tables(input) {
  let input$1 = skip_line_whitespace(input);
  return do$(
    parse_key(input$1, toList([])),
    (key, input) => {
      return expect(
        input,
        "]",
        (input) => {
          return expect(
            input,
            "]",
            (input) => {
              return do$(
                parse_table(input, $dict.new$()),
                (table, input) => { return new Ok([[key, table], input]); },
              );
            },
          );
        },
      );
    },
  );
}

function parse_table_and_header(input) {
  return do$(
    parse_table_header(input),
    (key, input) => {
      return do$(
        parse_table(input, $dict.new$()),
        (table, input) => { return new Ok([[key, table], input]); },
      );
    },
  );
}

function parse_tables(loop$input, loop$toml) {
  while (true) {
    let input = loop$input;
    let toml = loop$toml;
    if (input instanceof $Empty) {
      return new Ok(toml);
    } else {
      let $ = input.head;
      if ($ === "[") {
        let $1 = input.tail;
        if ($1 instanceof $Empty) {
          let input$1 = $1;
          let $2 = parse_table_and_header(input$1);
          if ($2 instanceof Ok) {
            let input$2 = $2[0][1];
            let key = $2[0][0][0];
            let table = $2[0][0][1];
            let $3 = insert(toml, key, new Table(table));
            if ($3 instanceof Ok) {
              let toml$1 = $3[0];
              loop$input = input$2;
              loop$toml = toml$1;
            } else {
              return $3;
            }
          } else {
            return $2;
          }
        } else {
          let $2 = $1.head;
          if ($2 === "[") {
            let input$1 = $1.tail;
            let $3 = parse_array_of_tables(input$1);
            if ($3 instanceof Ok) {
              let input$2 = $3[0][1];
              let key = $3[0][0][0];
              let table = $3[0][0][1];
              let $4 = insert(toml, key, new ArrayOfTables(toList([table])));
              if ($4 instanceof Ok) {
                let toml$1 = $4[0];
                loop$input = input$2;
                loop$toml = toml$1;
              } else {
                return $4;
              }
            } else {
              return $3;
            }
          } else {
            let input$1 = $1;
            let $3 = parse_table_and_header(input$1);
            if ($3 instanceof Ok) {
              let input$2 = $3[0][1];
              let key = $3[0][0][0];
              let table = $3[0][0][1];
              let $4 = insert(toml, key, new Table(table));
              if ($4 instanceof Ok) {
                let toml$1 = $4[0];
                loop$input = input$2;
                loop$toml = toml$1;
              } else {
                return $4;
              }
            } else {
              return $3;
            }
          }
        }
      } else {
        let g = $;
        return new Error(new Unexpected(g, "["));
      }
    }
  }
}

export function parse(input) {
  let input$1 = $string.to_graphemes(input);
  let input$2 = drop_comments(input$1, toList([]), false);
  let input$3 = skip_whitespace(input$2);
  return do$(
    parse_table(input$3, $dict.new$()),
    (toml, input) => {
      let $ = parse_tables(input, toml);
      if ($ instanceof Ok) {
        let toml$1 = $[0];
        return new Ok(reverse_arrays_of_tables_table(toml$1));
      } else {
        return $;
      }
    },
  );
}

function parse_array(input, elements) {
  let input$1 = skip_whitespace(input);
  if (input$1 instanceof $Empty) {
    return do$(
      parse_value(input$1),
      (element, input) => {
        let elements$1 = listPrepend(element, elements);
        let input$1 = skip_whitespace(input);
        if (input$1 instanceof $Empty) {
          return new Error(new Unexpected("EOF", "]"));
        } else {
          let $ = input$1.head;
          if ($ === "]") {
            let input$2 = input$1.tail;
            return new Ok([new Array($list.reverse(elements$1)), input$2]);
          } else if ($ === ",") {
            let input$2 = input$1.tail;
            let input$3 = skip_whitespace(input$2);
            return parse_array(input$3, elements$1);
          } else {
            let g = $;
            return new Error(new Unexpected(g, "]"));
          }
        }
      },
    );
  } else {
    let $ = input$1.head;
    if ($ === "]") {
      let input$2 = input$1.tail;
      return new Ok([new Array($list.reverse(elements)), input$2]);
    } else {
      return do$(
        parse_value(input$1),
        (element, input) => {
          let elements$1 = listPrepend(element, elements);
          let input$1 = skip_whitespace(input);
          if (input$1 instanceof $Empty) {
            return new Error(new Unexpected("EOF", "]"));
          } else {
            let $1 = input$1.head;
            if ($1 === "]") {
              let input$2 = input$1.tail;
              return new Ok([new Array($list.reverse(elements$1)), input$2]);
            } else if ($1 === ",") {
              let input$2 = input$1.tail;
              let input$3 = skip_whitespace(input$2);
              return parse_array(input$3, elements$1);
            } else {
              let g = $1;
              return new Error(new Unexpected(g, "]"));
            }
          }
        },
      );
    }
  }
}
