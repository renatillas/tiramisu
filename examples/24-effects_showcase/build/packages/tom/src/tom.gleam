//// A pure Gleam TOML parser!
////
//// ```gleam
//// import tom
//// 
//// const config = "
////   [person]
////   name = \"Lucy\"
////   is_cool = true
//// "
//// 
//// pub fn main() {
////   // Parse a string of TOML
////   let assert Ok(parsed) = tom.parse(config)
//// 
////   // Now you can work with the data directly, or you can use the `get_*`
////   // functions to retrieve values.
//// 
////   tom.get_string(parsed, ["person", "name"])
////   // -> Ok("Lucy")
//// 
////   let is_cool = tom.get_bool(parsed, ["person", "is_cool"])
////   // -> Ok(True)
//// }
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleam/time/calendar
import gleam/time/duration
import gleam/time/timestamp

/// A TOML document.
pub type Toml {
  Int(Int)
  Float(Float)
  /// Infinity is a valid number in TOML but Gleam does not support it, so this
  /// variant represents the infinity values.
  Infinity(Sign)
  /// NaN is a valid number in TOML but Gleam does not support it, so this
  /// variant represents the NaN values.
  Nan(Sign)
  Bool(Bool)
  String(String)
  Date(calendar.Date)
  Time(calendar.TimeOfDay)
  DateTime(date: calendar.Date, time: calendar.TimeOfDay, offset: Offset)
  Array(List(Toml))
  ArrayOfTables(List(Dict(String, Toml)))
  Table(Dict(String, Toml))
  InlineTable(Dict(String, Toml))
}

pub type Offset {
  Local
  Offset(duration.Duration)
}

pub type Sign {
  Positive
  Negative
}

/// An error that can occur when parsing a TOML document.
pub type ParseError {
  /// An unexpected character was encountered when parsing the document.
  Unexpected(got: String, expected: String)
  /// More than one items have the same key in the document.
  KeyAlreadyInUse(key: List(String))
}

type Tokens =
  List(String)

type Parsed(a) =
  Result(#(a, Tokens), ParseError)

/// A number of any kind, returned by the `get_number` function.
pub type Number {
  NumberInt(Int)
  NumberFloat(Float)
  NumberInfinity(Sign)
  NumberNan(Sign)
}

/// An error that can occur when retrieving a value from a TOML document dictionary with
/// one of the `get_*` functions.
pub type GetError {
  /// There was no value at the given key.
  NotFound(key: List(String))
  /// The value at the given key was not of the expected type.
  WrongType(key: List(String), expected: String, got: String)
}

// TODO: test
/// Get a value of any type from a TOML document dictionary.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = 1")
/// get(parsed, ["a", "b", "c"])
/// // -> Ok(Int(1))
/// ```
///
pub fn get(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(Toml, GetError) {
  case key {
    [] -> Error(NotFound([]))
    [k] -> result.replace_error(dict.get(toml, k), NotFound([k]))
    [k, ..key] -> {
      case dict.get(toml, k) {
        Ok(Table(t)) -> push_key(get(t, key), k)
        Ok(InlineTable(t)) -> push_key(get(t, key), k)
        Ok(other) -> Error(WrongType([k], "Table", classify(other)))
        Error(_) -> Error(NotFound([k]))
      }
    }
  }
}

// TODO: test
/// Get an int from a TOML document dictionary.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = 1")
/// get_int(parsed, ["a", "b", "c"])
/// // -> Ok(1)
/// ```
///
pub fn get_int(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(Int, GetError) {
  case get(toml, key) {
    Ok(Int(i)) -> Ok(i)
    Ok(other) -> Error(WrongType(key, "Int", classify(other)))
    Error(e) -> Error(e)
  }
}

// TODO: test
/// Get a float from a TOML document dictionary.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = 1.1")
/// get_float(parsed, ["a", "b", "c"])
/// // -> Ok(1.1)
/// ```
///
pub fn get_float(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(Float, GetError) {
  case get(toml, key) {
    Ok(Float(i)) -> Ok(i)
    Ok(other) -> Error(WrongType(key, "Float", classify(other)))
    Error(e) -> Error(e)
  }
}

// TODO: test
/// Get a bool from a TOML document dictionary.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = true")
/// get_bool(parsed, ["a", "b", "c"])
/// // -> Ok(True)
/// ```
///
pub fn get_bool(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(Bool, GetError) {
  case get(toml, key) {
    Ok(Bool(i)) -> Ok(i)
    Ok(other) -> Error(WrongType(key, "Bool", classify(other)))
    Error(e) -> Error(e)
  }
}

// TODO: test
/// Get a string from a TOML document dictionary.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = \"ok\"")
/// get_string(parsed, ["a", "b", "c"])
/// // -> Ok("ok")
/// ```
///
pub fn get_string(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(String, GetError) {
  case get(toml, key) {
    Ok(String(i)) -> Ok(i)
    Ok(other) -> Error(WrongType(key, "String", classify(other)))
    Error(e) -> Error(e)
  }
}

/// Get a date from a TOML document dictionary.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = 1979-05-27")
/// get_date(parsed, ["a", "b", "c"])
/// // -> Ok(Date(1979, May, 27))
/// ```
///
pub fn get_date(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(calendar.Date, GetError) {
  case get(toml, key) {
    Ok(Date(i)) -> Ok(i)
    Ok(other) -> Error(WrongType(key, "Date", classify(other)))
    Error(e) -> Error(e)
  }
}

/// Get a time from a TOML document dictionary.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = 07:32:00")
/// get_time_of_day(parsed, ["a", "b", "c"])
/// // -> Ok(TimeOfDay(7, 32, 0, 0))
/// ```
///
pub fn get_time_of_day(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(calendar.TimeOfDay, GetError) {
  case get(toml, key) {
    Ok(Time(i)) -> Ok(i)
    Ok(other) -> Error(WrongType(key, "Time", classify(other)))
    Error(e) -> Error(e)
  }
}

/// Get a date-time from a TOML document dictionary.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = 1979-05-27T07:32:00")
/// get_calendar_time(parsed, ["a", "b", "c"])
/// // -> Ok(#(Date(1979, May, 27), TimeOfDay(7, 32, 0, 0), Local))
/// ```
///
pub fn get_calendar_time(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(#(calendar.Date, calendar.TimeOfDay, Offset), GetError) {
  case get(toml, key) {
    Ok(DateTime(d, t, o)) -> Ok(#(d, t, o))
    Ok(other) -> Error(WrongType(key, "DateTime", classify(other)))
    Error(e) -> Error(e)
  }
}

/// Get an unambiguous time from a TOML document dictionary.
///
/// If a TOML date time has no offset it is ambiguous and cannot be converted
/// into a timestamp. There's no way to know what actual point in time it would
/// be as it would be different in different time zones.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = 1970-00-00T00:00:00Z")
/// get_timestamp(parsed, ["a", "b", "c"])
/// // -> Ok(timestamp.from_unix_seconds(0))
/// ```
///
pub fn get_timestamp(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(timestamp.Timestamp, GetError) {
  case get(toml, key) {
    Ok(DateTime(date:, time:, offset: Offset(offset))) ->
      Ok(timestamp.from_calendar(date, time, offset))
    Ok(other) -> Error(WrongType(key, "DateTime with offset", classify(other)))
    Error(e) -> Error(e)
  }
}

// TODO: test
/// Get an array from a TOML document dictionary.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = [1, 2]")
/// get_array(parsed, ["a", "b", "c"])
/// // -> Ok([Int(1), Int(2)])
/// ```
///
pub fn get_array(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(List(Toml), GetError) {
  case get(toml, key) {
    Ok(Array(i)) -> Ok(i)
    Ok(ArrayOfTables(i)) -> Ok(list.map(i, Table))
    Ok(other) -> Error(WrongType(key, "Array", classify(other)))
    Error(e) -> Error(e)
  }
}

// TODO: test
/// Get a table from a TOML document dictionary.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = { d = 1 }")
/// get_table(parsed, ["a", "b", "c"])
/// // -> Ok(dict.from_list([#("d", Int(1))]))
/// ```
///
pub fn get_table(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(Dict(String, Toml), GetError) {
  case get(toml, key) {
    Ok(Table(i)) -> Ok(i)
    Ok(InlineTable(i)) -> Ok(i)
    Ok(other) -> Error(WrongType(key, "Table", classify(other)))
    Error(e) -> Error(e)
  }
}

// TODO: test
/// Get a number of any kind from a TOML document dictionary.
/// This could be an int, a float, a NaN, or an infinity.
///
/// ## Examples
/// 
/// ```gleam
/// let assert Ok(parsed) = parse("a.b.c = { d = inf }")
/// get_number(parsed, ["a", "b", "c"])
/// // -> Ok(NumberInfinity(Positive)))
/// ```
///
pub fn get_number(
  toml: Dict(String, Toml),
  key: List(String),
) -> Result(Number, GetError) {
  case get(toml, key) {
    Ok(Int(x)) -> Ok(NumberInt(x))
    Ok(Float(x)) -> Ok(NumberFloat(x))
    Ok(Nan(x)) -> Ok(NumberNan(x))
    Ok(Infinity(x)) -> Ok(NumberInfinity(x))
    Ok(other) -> Error(WrongType(key, "Number", classify(other)))
    Error(e) -> Error(e)
  }
}

fn classify(toml: Toml) -> String {
  case toml {
    Int(_) -> "Int"
    Float(_) -> "Float"
    Nan(Positive) -> "NaN"
    Nan(Negative) -> "Negative NaN"
    Infinity(Positive) -> "Infinity"
    Infinity(Negative) -> "Negative Infinity"
    Bool(_) -> "Bool"
    String(_) -> "String"
    Date(_) -> "Date"
    Time(_) -> "Time"
    DateTime(_, _, _) -> "DateTime"
    Array(_) -> "Array"
    ArrayOfTables(_) -> "Array"
    Table(_) -> "Table"
    InlineTable(_) -> "Table"
  }
}

fn push_key(result: Result(t, GetError), key: String) -> Result(t, GetError) {
  case result {
    Ok(t) -> Ok(t)
    Error(NotFound(path)) -> Error(NotFound([key, ..path]))
    Error(WrongType(path, expected, got)) ->
      Error(WrongType([key, ..path], expected, got))
  }
}

pub fn parse(input: String) -> Result(Dict(String, Toml), ParseError) {
  let input = string.to_graphemes(input)
  let input = drop_comments(input, [], False)
  let input = skip_whitespace(input)
  use toml, input <- do(parse_table(input, dict.new()))
  case parse_tables(input, toml) {
    Ok(toml) -> Ok(reverse_arrays_of_tables_table(toml))
    Error(e) -> Error(e)
  }
}

fn parse_tables(
  input: Tokens,
  toml: Dict(String, Toml),
) -> Result(Dict(String, Toml), ParseError) {
  case input {
    ["[", "[", ..input] -> {
      case parse_array_of_tables(input) {
        Error(e) -> Error(e)
        Ok(#(#(key, table), input)) -> {
          case insert(toml, key, ArrayOfTables([table])) {
            Ok(toml) -> parse_tables(input, toml)
            Error(e) -> Error(e)
          }
        }
      }
    }
    ["[", ..input] -> {
      case parse_table_and_header(input) {
        Error(e) -> Error(e)
        Ok(#(#(key, table), input)) -> {
          case insert(toml, key, Table(table)) {
            Ok(toml) -> parse_tables(input, toml)
            Error(e) -> Error(e)
          }
        }
      }
    }
    [g, ..] -> Error(Unexpected(g, "["))
    [] -> Ok(toml)
  }
}

fn parse_array_of_tables(
  input: Tokens,
) -> Parsed(#(List(String), Dict(String, Toml))) {
  let input = skip_line_whitespace(input)
  use key, input <- do(parse_key(input, []))
  use input <- expect(input, "]")
  use input <- expect(input, "]")
  use table, input <- do(parse_table(input, dict.new()))
  Ok(#(#(key, table), input))
}

fn parse_table_header(input: Tokens) -> Parsed(List(String)) {
  let input = skip_line_whitespace(input)
  use key, input <- do(parse_key(input, []))
  use input <- expect(input, "]")
  let input = skip_line_whitespace(input)
  use input <- expect_end_of_line(input)
  Ok(#(key, input))
}

fn parse_table_and_header(
  input: Tokens,
) -> Parsed(#(List(String), Dict(String, Toml))) {
  use key, input <- do(parse_table_header(input))
  use table, input <- do(parse_table(input, dict.new()))
  Ok(#(#(key, table), input))
}

fn parse_table(
  input: Tokens,
  toml: Dict(String, Toml),
) -> Parsed(Dict(String, Toml)) {
  let input = skip_whitespace(input)
  case input {
    ["[", ..] | [] -> Ok(#(toml, input))
    _ ->
      case parse_key_value(input, toml) {
        Ok(#(toml, input)) ->
          case skip_line_whitespace(input) {
            [] -> Ok(#(toml, []))
            ["\n", ..in] | ["\r\n", ..in] -> parse_table(in, toml)
            [g, ..] -> Error(Unexpected(g, "\n"))
          }
        e -> e
      }
  }
}

fn parse_key_value(
  input: Tokens,
  toml: Dict(String, Toml),
) -> Parsed(Dict(String, Toml)) {
  use key, input <- do(parse_key(input, []))
  let input = skip_line_whitespace(input)
  use input <- expect(input, "=")
  let input = skip_line_whitespace(input)
  use value, input <- do(parse_value(input))
  case insert(toml, key, value) {
    Ok(toml) -> Ok(#(toml, input))
    Error(e) -> Error(e)
  }
}

fn insert(
  table: Dict(String, Toml),
  key: List(String),
  value: Toml,
) -> Result(Dict(String, Toml), ParseError) {
  case insert_loop(table, key, value) {
    Ok(table) -> Ok(table)
    Error(path) -> Error(KeyAlreadyInUse(path))
  }
}

fn insert_loop(
  table: Dict(String, Toml),
  key: List(String),
  value: Toml,
) -> Result(Dict(String, Toml), List(String)) {
  case key {
    [] -> panic as "unreachable"
    [k] -> {
      case dict.get(table, k) {
        Error(Nil) -> Ok(dict.insert(table, k, value))
        Ok(old) -> merge(table, k, old, value)
      }
    }
    [k, ..key] -> {
      case dict.get(table, k) {
        Error(Nil) -> {
          case insert_loop(dict.new(), key, value) {
            Ok(inner) -> Ok(dict.insert(table, k, Table(inner)))
            Error(path) -> Error([k, ..path])
          }
        }
        Ok(ArrayOfTables([inner, ..rest])) -> {
          case insert_loop(inner, key, value) {
            Ok(inner) ->
              Ok(dict.insert(table, k, ArrayOfTables([inner, ..rest])))
            Error(path) -> Error([k, ..path])
          }
        }
        Ok(Table(inner)) -> {
          case insert_loop(inner, key, value) {
            Ok(inner) -> Ok(dict.insert(table, k, Table(inner)))
            Error(path) -> Error([k, ..path])
          }
        }
        Ok(_) -> Error([k])
      }
    }
  }
}

fn merge(
  table: Dict(String, Toml),
  key: String,
  old: Toml,
  new: Toml,
) -> Result(Dict(String, Toml), List(String)) {
  case old, new {
    // When both are arrays of tables then they are merged together
    ArrayOfTables(tables), ArrayOfTables(new) ->
      Ok(dict.insert(table, key, ArrayOfTables(list.append(new, tables))))

    _, _ -> Error([key])
  }
}

fn expect_end_of_line(input: Tokens, next: fn(Tokens) -> Parsed(a)) -> Parsed(a) {
  case input {
    ["\n", ..input] -> next(input)
    ["\r\n", ..input] -> next(input)
    [g, ..] -> Error(Unexpected(g, "\n"))
    [] -> Error(Unexpected("EOF", "\n"))
  }
}

fn parse_value(input) -> Parsed(Toml) {
  case input {
    ["t", "r", "u", "e", ..input] -> Ok(#(Bool(True), input))
    ["f", "a", "l", "s", "e", ..input] -> Ok(#(Bool(False), input))

    ["n", "a", "n", ..input] -> Ok(#(Nan(Positive), input))
    ["+", "n", "a", "n", ..input] -> Ok(#(Nan(Positive), input))
    ["-", "n", "a", "n", ..input] -> Ok(#(Nan(Negative), input))

    ["i", "n", "f", ..input] -> Ok(#(Infinity(Positive), input))
    ["+", "i", "n", "f", ..input] -> Ok(#(Infinity(Positive), input))
    ["-", "i", "n", "f", ..input] -> Ok(#(Infinity(Negative), input))

    ["[", ..input] -> parse_array(input, [])
    ["{", ..input] -> parse_inline_table(input, dict.new())

    ["0", "x", ..input] -> parse_hex(input, 0, Positive)
    ["+", "0", "x", ..input] -> parse_hex(input, 0, Positive)
    ["-", "0", "x", ..input] -> parse_hex(input, 0, Negative)

    ["0", "o", ..input] -> parse_octal(input, 0, Positive)
    ["+", "0", "o", ..input] -> parse_octal(input, 0, Positive)
    ["-", "0", "o", ..input] -> parse_octal(input, 0, Negative)

    ["0", "b", ..input] -> parse_binary(input, 0, Positive)
    ["+", "0", "b", ..input] -> parse_binary(input, 0, Positive)
    ["-", "0", "b", ..input] -> parse_binary(input, 0, Negative)

    ["+", ..input] -> parse_number(input, 0, Positive)
    ["-", ..input] -> parse_number(input, 0, Negative)
    ["0", ..]
    | ["1", ..]
    | ["2", ..]
    | ["3", ..]
    | ["4", ..]
    | ["5", ..]
    | ["6", ..]
    | ["7", ..]
    | ["8", ..]
    | ["9", ..] -> parse_number(input, 0, Positive)

    ["\"", "\"", "\"", ..input] -> parse_multi_line_string(input, "")
    ["\"", ..input] -> parse_string(input, "")

    ["'", "'", "'", ..input] -> parse_multi_line_literal_string(input, "")
    ["'", ..input] -> parse_literal_string(input, "")

    [g, ..] -> Error(Unexpected(g, "value"))
    [] -> Error(Unexpected("EOF", "value"))
  }
}

fn parse_key(input: Tokens, segments: List(String)) -> Parsed(List(String)) {
  use segment, input <- do(parse_key_segment(input))
  let segments = [segment, ..segments]
  let input = skip_line_whitespace(input)

  case input {
    [".", ..input] -> parse_key(input, segments)
    _ -> Ok(#(list.reverse(segments), input))
  }
}

fn parse_key_segment(input: Tokens) -> Parsed(String) {
  let input = skip_line_whitespace(input)
  case input {
    ["=", ..] -> Error(Unexpected("=", "Key"))
    ["\n", ..] -> Error(Unexpected("\n", "Key"))
    ["\r\n", ..] -> Error(Unexpected("\r\n", "Key"))
    ["[", ..] -> Error(Unexpected("[", "Key"))
    ["\"", ..input] -> parse_key_quoted(input, "\"", "")
    ["'", ..input] -> parse_key_quoted(input, "'", "")
    _ -> parse_key_bare(input, "")
  }
}

fn parse_key_quoted(
  input: Tokens,
  close: String,
  name: String,
) -> Parsed(String) {
  case input {
    [g, ..input] if g == close -> Ok(#(name, input))
    [g, ..input] -> parse_key_quoted(input, close, name <> g)
    [] -> Error(Unexpected("EOF", close))
  }
}

fn parse_key_bare(input: Tokens, name: String) -> Parsed(String) {
  case input {
    [" ", ..input] if name != "" -> Ok(#(name, input))
    ["=", ..] if name != "" -> Ok(#(name, input))
    [".", ..] if name != "" -> Ok(#(name, input))
    ["]", ..] if name != "" -> Ok(#(name, input))
    [",", ..] if name != "" -> Error(Unexpected(",", "="))
    ["\n", ..] if name != "" -> Error(Unexpected("\n", "="))
    ["\r\n", ..] if name != "" -> Error(Unexpected("\r\n", "="))
    ["\n", ..] -> Error(Unexpected("\n", "key"))
    ["\r\n", ..] -> Error(Unexpected("\r\n", "key"))
    ["]", ..] -> Error(Unexpected("]", "key"))
    [",", ..] -> Error(Unexpected(",", "key"))
    [g, ..input] -> parse_key_bare(input, name <> g)
    [] -> Error(Unexpected("EOF", "key"))
  }
}

fn skip_line_whitespace(input: Tokens) -> Tokens {
  list.drop_while(input, fn(g) { g == " " || g == "\t" })
}

fn skip_whitespace(input: Tokens) -> Tokens {
  case input {
    [" ", ..input] -> skip_whitespace(input)
    ["\t", ..input] -> skip_whitespace(input)
    ["\n", ..input] -> skip_whitespace(input)
    ["\r\n", ..input] -> skip_whitespace(input)
    input -> input
  }
}

fn drop_comments(input: Tokens, acc: Tokens, in_string: Bool) -> Tokens {
  case input {
    ["\\", "\"", ..input] if in_string ->
      drop_comments(input, ["\"", "\\", ..acc], in_string)
    ["\"", ..input] -> drop_comments(input, ["\"", ..acc], !in_string)
    ["#", ..input] if in_string -> drop_comments(input, ["#", ..acc], in_string)
    ["#", ..input] if !in_string ->
      input
      |> list.drop_while(fn(g) { g != "\n" })
      |> drop_comments(acc, in_string)
    [g, ..input] -> drop_comments(input, [g, ..acc], in_string)
    [] -> list.reverse(acc)
  }
}

fn do(
  result: Result(#(a, Tokens), ParseError),
  next: fn(a, Tokens) -> Result(b, ParseError),
) -> Result(b, ParseError) {
  case result {
    Ok(#(a, input)) -> next(a, input)
    Error(e) -> Error(e)
  }
}

fn expect(
  input: Tokens,
  expected: String,
  next: fn(Tokens) -> Parsed(a),
) -> Parsed(a) {
  case input {
    [g, ..input] if g == expected -> next(input)
    [g, ..] -> Error(Unexpected(g, expected))
    [] -> Error(Unexpected("EOF", expected))
  }
}

fn parse_inline_table(
  input: Tokens,
  properties: Dict(String, Toml),
) -> Parsed(Toml) {
  let input = skip_whitespace(input)
  case input {
    ["}", ..input] -> Ok(#(InlineTable(properties), input))
    _ ->
      case parse_inline_table_property(input, properties) {
        Ok(#(properties, input)) -> {
          let input = skip_whitespace(input)
          case input {
            ["}", ..input] -> Ok(#(InlineTable(properties), input))
            [",", ..input] -> {
              let input = skip_whitespace(input)
              parse_inline_table(input, properties)
            }
            [g, ..] -> Error(Unexpected(g, "}"))
            [] -> Error(Unexpected("EOF", "}"))
          }
        }
        Error(e) -> Error(e)
      }
  }
}

fn parse_inline_table_property(
  input: Tokens,
  properties: Dict(String, Toml),
) -> Parsed(Dict(String, Toml)) {
  let input = skip_whitespace(input)
  use key, input <- do(parse_key(input, []))
  let input = skip_line_whitespace(input)
  use input <- expect(input, "=")
  let input = skip_line_whitespace(input)
  use value, input <- do(parse_value(input))
  case insert(properties, key, value) {
    Ok(properties) -> Ok(#(properties, input))
    Error(e) -> Error(e)
  }
}

fn parse_array(input: Tokens, elements: List(Toml)) -> Parsed(Toml) {
  let input = skip_whitespace(input)
  case input {
    ["]", ..input] -> Ok(#(Array(list.reverse(elements)), input))
    _ -> {
      use element, input <- do(parse_value(input))
      let elements = [element, ..elements]
      let input = skip_whitespace(input)
      case input {
        ["]", ..input] -> Ok(#(Array(list.reverse(elements)), input))
        [",", ..input] -> {
          let input = skip_whitespace(input)
          parse_array(input, elements)
        }
        [g, ..] -> Error(Unexpected(g, "]"))
        [] -> Error(Unexpected("EOF", "]"))
      }
    }
  }
}

fn parse_hex(input: Tokens, number: Int, sign: Sign) -> Parsed(Toml) {
  case input {
    ["_", ..input] -> parse_hex(input, number, sign)
    ["0", ..input] -> parse_hex(input, number * 16 + 0, sign)
    ["1", ..input] -> parse_hex(input, number * 16 + 1, sign)
    ["2", ..input] -> parse_hex(input, number * 16 + 2, sign)
    ["3", ..input] -> parse_hex(input, number * 16 + 3, sign)
    ["4", ..input] -> parse_hex(input, number * 16 + 4, sign)
    ["5", ..input] -> parse_hex(input, number * 16 + 5, sign)
    ["6", ..input] -> parse_hex(input, number * 16 + 6, sign)
    ["7", ..input] -> parse_hex(input, number * 16 + 7, sign)
    ["8", ..input] -> parse_hex(input, number * 16 + 8, sign)
    ["9", ..input] -> parse_hex(input, number * 16 + 9, sign)
    ["a", ..input] -> parse_hex(input, number * 16 + 10, sign)
    ["b", ..input] -> parse_hex(input, number * 16 + 11, sign)
    ["c", ..input] -> parse_hex(input, number * 16 + 12, sign)
    ["d", ..input] -> parse_hex(input, number * 16 + 13, sign)
    ["e", ..input] -> parse_hex(input, number * 16 + 14, sign)
    ["f", ..input] -> parse_hex(input, number * 16 + 15, sign)
    ["A", ..input] -> parse_hex(input, number * 16 + 10, sign)
    ["B", ..input] -> parse_hex(input, number * 16 + 11, sign)
    ["C", ..input] -> parse_hex(input, number * 16 + 12, sign)
    ["D", ..input] -> parse_hex(input, number * 16 + 13, sign)
    ["E", ..input] -> parse_hex(input, number * 16 + 14, sign)
    ["F", ..input] -> parse_hex(input, number * 16 + 15, sign)

    // Anything else and the number is terminated
    input -> {
      let number = case sign {
        Positive -> number
        Negative -> -number
      }
      Ok(#(Int(number), input))
    }
  }
}

fn parse_octal(input: Tokens, number: Int, sign: Sign) -> Parsed(Toml) {
  case input {
    ["_", ..input] -> parse_octal(input, number, sign)
    ["0", ..input] -> parse_octal(input, number * 8 + 0, sign)
    ["1", ..input] -> parse_octal(input, number * 8 + 1, sign)
    ["2", ..input] -> parse_octal(input, number * 8 + 2, sign)
    ["3", ..input] -> parse_octal(input, number * 8 + 3, sign)
    ["4", ..input] -> parse_octal(input, number * 8 + 4, sign)
    ["5", ..input] -> parse_octal(input, number * 8 + 5, sign)
    ["6", ..input] -> parse_octal(input, number * 8 + 6, sign)
    ["7", ..input] -> parse_octal(input, number * 8 + 7, sign)

    // Anything else and the number is terminated
    input -> {
      let number = case sign {
        Positive -> number
        Negative -> -number
      }
      Ok(#(Int(number), input))
    }
  }
}

fn parse_binary(input: Tokens, number: Int, sign: Sign) -> Parsed(Toml) {
  case input {
    ["_", ..input] -> parse_binary(input, number, sign)
    ["0", ..input] -> parse_binary(input, number * 2 + 0, sign)
    ["1", ..input] -> parse_binary(input, number * 2 + 1, sign)

    // Anything else and the number is terminated
    input -> {
      let number = case sign {
        Positive -> number
        Negative -> -number
      }
      Ok(#(Int(number), input))
    }
  }
}

fn parse_number(input: Tokens, number: Int, sign: Sign) -> Parsed(Toml) {
  case input {
    ["_", ..input] -> parse_number(input, number, sign)
    ["0", ..input] -> parse_number(input, number * 10 + 0, sign)
    ["1", ..input] -> parse_number(input, number * 10 + 1, sign)
    ["2", ..input] -> parse_number(input, number * 10 + 2, sign)
    ["3", ..input] -> parse_number(input, number * 10 + 3, sign)
    ["4", ..input] -> parse_number(input, number * 10 + 4, sign)
    ["5", ..input] -> parse_number(input, number * 10 + 5, sign)
    ["6", ..input] -> parse_number(input, number * 10 + 6, sign)
    ["7", ..input] -> parse_number(input, number * 10 + 7, sign)
    ["8", ..input] -> parse_number(input, number * 10 + 8, sign)
    ["9", ..input] -> parse_number(input, number * 10 + 9, sign)

    ["-", ..input] -> parse_date(input, number)
    [":", ..input] if number < 24 -> parse_time_minute(input, number)

    [".", ..input] -> parse_float(input, int.to_float(number), sign, 0.1)

    ["e", "+", ..input] ->
      parse_exponent(input, int.to_float(number), sign, 0, Positive)
    ["e", "-", ..input] ->
      parse_exponent(input, int.to_float(number), sign, 0, Negative)
    ["e", ..input] ->
      parse_exponent(input, int.to_float(number), sign, 0, Positive)
    ["E", "+", ..input] ->
      parse_exponent(input, int.to_float(number), sign, 0, Positive)
    ["E", "-", ..input] ->
      parse_exponent(input, int.to_float(number), sign, 0, Negative)
    ["E", ..input] ->
      parse_exponent(input, int.to_float(number), sign, 0, Positive)

    // Anything else and the number is terminated
    input -> {
      let number = case sign {
        Positive -> number
        Negative -> -number
      }
      Ok(#(Int(number), input))
    }
  }
}

fn parse_exponent(
  input: Tokens,
  n: Float,
  n_sign: Sign,
  ex: Int,
  ex_sign: Sign,
) -> Parsed(Toml) {
  case input {
    ["_", ..input] -> parse_exponent(input, n, n_sign, ex, ex_sign)
    ["0", ..input] -> parse_exponent(input, n, n_sign, ex * 10, ex_sign)
    ["1", ..input] -> parse_exponent(input, n, n_sign, ex * 10 + 1, ex_sign)
    ["2", ..input] -> parse_exponent(input, n, n_sign, ex * 10 + 2, ex_sign)
    ["3", ..input] -> parse_exponent(input, n, n_sign, ex * 10 + 3, ex_sign)
    ["4", ..input] -> parse_exponent(input, n, n_sign, ex * 10 + 4, ex_sign)
    ["5", ..input] -> parse_exponent(input, n, n_sign, ex * 10 + 5, ex_sign)
    ["6", ..input] -> parse_exponent(input, n, n_sign, ex * 10 + 6, ex_sign)
    ["7", ..input] -> parse_exponent(input, n, n_sign, ex * 10 + 7, ex_sign)
    ["8", ..input] -> parse_exponent(input, n, n_sign, ex * 10 + 8, ex_sign)
    ["9", ..input] -> parse_exponent(input, n, n_sign, ex * 10 + 9, ex_sign)

    // Anything else and the number is terminated
    input -> {
      let number = case n_sign {
        Positive -> n
        Negative -> n *. -1.0
      }
      let exponent =
        int.to_float(case ex_sign {
          Positive -> ex
          Negative -> -ex
        })
      let multiplier = case float.power(10.0, exponent) {
        Ok(multiplier) -> multiplier
        Error(_) -> 1.0
      }
      Ok(#(Float(number *. multiplier), input))
    }
  }
}

fn parse_float(
  input: Tokens,
  number: Float,
  sign: Sign,
  unit: Float,
) -> Parsed(Toml) {
  case input {
    ["_", ..input] -> parse_float(input, number, sign, unit)
    ["0", ..input] -> parse_float(input, number, sign, unit *. 0.1)
    ["1", ..input] ->
      parse_float(input, number +. 1.0 *. unit, sign, unit *. 0.1)
    ["2", ..input] ->
      parse_float(input, number +. 2.0 *. unit, sign, unit *. 0.1)
    ["3", ..input] ->
      parse_float(input, number +. 3.0 *. unit, sign, unit *. 0.1)
    ["4", ..input] ->
      parse_float(input, number +. 4.0 *. unit, sign, unit *. 0.1)
    ["5", ..input] ->
      parse_float(input, number +. 5.0 *. unit, sign, unit *. 0.1)
    ["6", ..input] ->
      parse_float(input, number +. 6.0 *. unit, sign, unit *. 0.1)
    ["7", ..input] ->
      parse_float(input, number +. 7.0 *. unit, sign, unit *. 0.1)
    ["8", ..input] ->
      parse_float(input, number +. 8.0 *. unit, sign, unit *. 0.1)
    ["9", ..input] ->
      parse_float(input, number +. 9.0 *. unit, sign, unit *. 0.1)

    ["e", "+", ..input] -> parse_exponent(input, number, sign, 0, Positive)
    ["e", "-", ..input] -> parse_exponent(input, number, sign, 0, Negative)
    ["e", ..input] -> parse_exponent(input, number, sign, 0, Positive)
    ["E", "+", ..input] -> parse_exponent(input, number, sign, 0, Positive)
    ["E", "-", ..input] -> parse_exponent(input, number, sign, 0, Negative)
    ["E", ..input] -> parse_exponent(input, number, sign, 0, Positive)

    // Anything else and the number is terminated
    input -> {
      let number = case sign {
        Positive -> number
        Negative -> number *. -1.0
      }
      Ok(#(Float(number), input))
    }
  }
}

fn parse_string(input: Tokens, string: String) -> Parsed(Toml) {
  case input {
    ["\"", ..input] -> Ok(#(String(string), input))
    ["\\", "t", ..input] -> parse_string(input, string <> "\t")
    ["\\", "e", ..input] -> parse_string(input, string <> "\u{001b}")
    ["\\", "b", ..input] -> parse_string(input, string <> "\u{0008}")
    ["\\", "n", ..input] -> parse_string(input, string <> "\n")
    ["\\", "r", ..input] -> parse_string(input, string <> "\r")
    ["\\", "f", ..input] -> parse_string(input, string <> "\f")
    ["\\", "\"", ..input] -> parse_string(input, string <> "\"")
    ["\\", "\\", ..input] -> parse_string(input, string <> "\\")
    [] -> Error(Unexpected("EOF", "\""))
    ["\n", ..] -> Error(Unexpected("\n", "\""))
    ["\r\n", ..] -> Error(Unexpected("\r\n", "\""))
    [g, ..input] -> parse_string(input, string <> g)
  }
}

fn parse_multi_line_string(input: Tokens, string: String) -> Parsed(Toml) {
  case input {
    ["\"", "\"", "\"", ..input] -> Ok(#(String(string), input))
    ["\\", "\n", ..input] ->
      parse_multi_line_string(skip_whitespace(input), string)
    ["\\", "\r\n", ..input] ->
      parse_multi_line_string(skip_whitespace(input), string)
    ["\r\n", ..input] if string == "" -> parse_multi_line_string(input, string)
    ["\n", ..input] if string == "" -> parse_multi_line_string(input, string)
    ["\r\n", ..input] if string == "" -> parse_multi_line_string(input, string)
    ["\\", "t", ..input] -> parse_multi_line_string(input, string <> "\t")
    ["\\", "n", ..input] -> parse_multi_line_string(input, string <> "\n")
    ["\\", "r", ..input] -> parse_multi_line_string(input, string <> "\r")
    ["\\", "\"", ..input] -> parse_multi_line_string(input, string <> "\"")
    ["\\", "\\", ..input] -> parse_multi_line_string(input, string <> "\\")
    [] -> Error(Unexpected("EOF", "\""))
    [g, ..input] -> parse_multi_line_string(input, string <> g)
  }
}

fn parse_multi_line_literal_string(
  input: Tokens,
  string: String,
) -> Parsed(Toml) {
  case input {
    [] -> Error(Unexpected("EOF", "\""))
    ["'", "'", "'", "'", ..] -> Error(Unexpected("''''", "'''"))
    ["'", "'", "'", ..input] -> Ok(#(String(string), input))
    ["\n", ..input] if string == "" ->
      parse_multi_line_literal_string(input, string)
    ["\r\n", ..input] if string == "" ->
      parse_multi_line_literal_string(input, string)
    [g, ..input] -> parse_multi_line_literal_string(input, string <> g)
  }
}

fn parse_literal_string(input: Tokens, string: String) -> Parsed(Toml) {
  case input {
    [] -> Error(Unexpected("EOF", "\""))
    ["\n", ..] -> Error(Unexpected("\n", "'"))
    ["\r\n", ..] -> Error(Unexpected("\r\n", "'"))
    ["'", ..input] -> Ok(#(String(string), input))
    [g, ..input] -> parse_literal_string(input, string <> g)
  }
}

fn reverse_arrays_of_tables(toml: Toml) -> Toml {
  case toml {
    ArrayOfTables(tables) ->
      ArrayOfTables(reverse_arrays_of_tables_array(tables, []))

    Table(table) -> Table(reverse_arrays_of_tables_table(table))

    _ -> toml
  }
}

fn reverse_arrays_of_tables_table(
  table: Dict(String, Toml),
) -> Dict(String, Toml) {
  dict.map_values(table, fn(_, v) { reverse_arrays_of_tables(v) })
}

fn reverse_arrays_of_tables_array(
  array: List(Dict(String, Toml)),
  acc: List(Dict(String, Toml)),
) -> List(Dict(String, Toml)) {
  case array {
    [] -> acc
    [first, ..rest] -> {
      let first = reverse_arrays_of_tables_table(first)
      reverse_arrays_of_tables_array(rest, [first, ..acc])
    }
  }
}

fn parse_time_minute(input: Tokens, hours: Int) -> Parsed(Toml) {
  use minutes, input <- do(parse_number_under_60(input, "minutes"))
  use #(seconds, ns), input <- do(parse_time_s_ns(input))
  let time = calendar.TimeOfDay(hours, minutes, seconds, ns)
  Ok(#(Time(time), input))
}

fn parse_hour_minute(input: Tokens) -> Parsed(#(Int, Int)) {
  use hours, input <- do(case input {
    ["0", "0", ":", ..input] -> Ok(#(0, input))
    ["0", "1", ":", ..input] -> Ok(#(1, input))
    ["0", "2", ":", ..input] -> Ok(#(2, input))
    ["0", "3", ":", ..input] -> Ok(#(3, input))
    ["0", "4", ":", ..input] -> Ok(#(4, input))
    ["0", "5", ":", ..input] -> Ok(#(5, input))
    ["0", "6", ":", ..input] -> Ok(#(6, input))
    ["0", "7", ":", ..input] -> Ok(#(7, input))
    ["0", "8", ":", ..input] -> Ok(#(8, input))
    ["0", "9", ":", ..input] -> Ok(#(9, input))
    ["1", "0", ":", ..input] -> Ok(#(10, input))
    ["1", "1", ":", ..input] -> Ok(#(11, input))
    ["1", "2", ":", ..input] -> Ok(#(12, input))
    ["1", "3", ":", ..input] -> Ok(#(13, input))
    ["1", "4", ":", ..input] -> Ok(#(14, input))
    ["1", "5", ":", ..input] -> Ok(#(15, input))
    ["1", "6", ":", ..input] -> Ok(#(16, input))
    ["1", "7", ":", ..input] -> Ok(#(17, input))
    ["1", "8", ":", ..input] -> Ok(#(18, input))
    ["1", "9", ":", ..input] -> Ok(#(19, input))
    ["2", "0", ":", ..input] -> Ok(#(20, input))
    ["2", "1", ":", ..input] -> Ok(#(21, input))
    ["2", "2", ":", ..input] -> Ok(#(22, input))
    ["2", "3", ":", ..input] -> Ok(#(23, input))
    [g, ..] -> Error(Unexpected(g, "time"))
    [] -> Error(Unexpected("EOF", "time"))
  })

  use minutes, input <- do(parse_number_under_60(input, "minutes"))
  Ok(#(#(hours, minutes), input))
}

fn parse_time_value(input: Tokens) -> Parsed(calendar.TimeOfDay) {
  use #(hours, minutes), input <- do(parse_hour_minute(input))
  use #(seconds, ns), input <- do(parse_time_s_ns(input))
  let time = calendar.TimeOfDay(hours, minutes, seconds, ns)
  Ok(#(time, input))
}

fn parse_time_s_ns(input: Tokens) -> Parsed(#(Int, Int)) {
  case input {
    [":", ..input] -> {
      use seconds, input <- do(parse_number_under_60(input, "seconds"))
      case input {
        [".", ..input] -> parse_time_ns(input, seconds, 0, 0)
        _ -> Ok(#(#(seconds, 0), input))
      }
    }

    _ -> Ok(#(#(0, 0), input))
  }
}

fn parse_time_ns(
  input: Tokens,
  seconds: Int,
  ns: Int,
  digits_count: Int,
) -> Parsed(#(Int, Int)) {
  case input {
    ["0", ..input] if digits_count < 9 ->
      parse_time_ns(input, seconds, ns * 10 + 0, digits_count + 1)
    ["1", ..input] if digits_count < 9 ->
      parse_time_ns(input, seconds, ns * 10 + 1, digits_count + 1)
    ["2", ..input] if digits_count < 9 ->
      parse_time_ns(input, seconds, ns * 10 + 2, digits_count + 1)
    ["3", ..input] if digits_count < 9 ->
      parse_time_ns(input, seconds, ns * 10 + 3, digits_count + 1)
    ["4", ..input] if digits_count < 9 ->
      parse_time_ns(input, seconds, ns * 10 + 4, digits_count + 1)
    ["5", ..input] if digits_count < 9 ->
      parse_time_ns(input, seconds, ns * 10 + 5, digits_count + 1)
    ["6", ..input] if digits_count < 9 ->
      parse_time_ns(input, seconds, ns * 10 + 6, digits_count + 1)
    ["7", ..input] if digits_count < 9 ->
      parse_time_ns(input, seconds, ns * 10 + 7, digits_count + 1)
    ["8", ..input] if digits_count < 9 ->
      parse_time_ns(input, seconds, ns * 10 + 8, digits_count + 1)
    ["9", ..input] if digits_count < 9 ->
      parse_time_ns(input, seconds, ns * 10 + 9, digits_count + 1)

    // Anything else and the number is terminated
    _ -> {
      let exponent = int.to_float(9 - digits_count)
      let assert Ok(multiplier) = float.power(10.0, exponent)
      Ok(#(#(seconds, ns * float.truncate(multiplier)), input))
    }
  }
}

fn parse_number_under_60(input: Tokens, expected: String) -> Parsed(Int) {
  case input {
    ["0", "0", ..input] -> Ok(#(0, input))
    ["0", "1", ..input] -> Ok(#(1, input))
    ["0", "2", ..input] -> Ok(#(2, input))
    ["0", "3", ..input] -> Ok(#(3, input))
    ["0", "4", ..input] -> Ok(#(4, input))
    ["0", "5", ..input] -> Ok(#(5, input))
    ["0", "6", ..input] -> Ok(#(6, input))
    ["0", "7", ..input] -> Ok(#(7, input))
    ["0", "8", ..input] -> Ok(#(8, input))
    ["0", "9", ..input] -> Ok(#(9, input))
    ["1", "0", ..input] -> Ok(#(10, input))
    ["1", "1", ..input] -> Ok(#(11, input))
    ["1", "2", ..input] -> Ok(#(12, input))
    ["1", "3", ..input] -> Ok(#(13, input))
    ["1", "4", ..input] -> Ok(#(14, input))
    ["1", "5", ..input] -> Ok(#(15, input))
    ["1", "6", ..input] -> Ok(#(16, input))
    ["1", "7", ..input] -> Ok(#(17, input))
    ["1", "8", ..input] -> Ok(#(18, input))
    ["1", "9", ..input] -> Ok(#(19, input))
    ["2", "0", ..input] -> Ok(#(20, input))
    ["2", "1", ..input] -> Ok(#(21, input))
    ["2", "2", ..input] -> Ok(#(22, input))
    ["2", "3", ..input] -> Ok(#(23, input))
    ["2", "4", ..input] -> Ok(#(24, input))
    ["2", "5", ..input] -> Ok(#(25, input))
    ["2", "6", ..input] -> Ok(#(26, input))
    ["2", "7", ..input] -> Ok(#(27, input))
    ["2", "8", ..input] -> Ok(#(28, input))
    ["2", "9", ..input] -> Ok(#(29, input))
    ["3", "0", ..input] -> Ok(#(30, input))
    ["3", "1", ..input] -> Ok(#(31, input))
    ["3", "2", ..input] -> Ok(#(32, input))
    ["3", "3", ..input] -> Ok(#(33, input))
    ["3", "4", ..input] -> Ok(#(34, input))
    ["3", "5", ..input] -> Ok(#(35, input))
    ["3", "6", ..input] -> Ok(#(36, input))
    ["3", "7", ..input] -> Ok(#(37, input))
    ["3", "8", ..input] -> Ok(#(38, input))
    ["3", "9", ..input] -> Ok(#(39, input))
    ["4", "0", ..input] -> Ok(#(40, input))
    ["4", "1", ..input] -> Ok(#(41, input))
    ["4", "2", ..input] -> Ok(#(42, input))
    ["4", "3", ..input] -> Ok(#(43, input))
    ["4", "4", ..input] -> Ok(#(44, input))
    ["4", "5", ..input] -> Ok(#(45, input))
    ["4", "6", ..input] -> Ok(#(46, input))
    ["4", "7", ..input] -> Ok(#(47, input))
    ["4", "8", ..input] -> Ok(#(48, input))
    ["4", "9", ..input] -> Ok(#(49, input))
    ["5", "0", ..input] -> Ok(#(50, input))
    ["5", "1", ..input] -> Ok(#(51, input))
    ["5", "2", ..input] -> Ok(#(52, input))
    ["5", "3", ..input] -> Ok(#(53, input))
    ["5", "4", ..input] -> Ok(#(54, input))
    ["5", "5", ..input] -> Ok(#(55, input))
    ["5", "6", ..input] -> Ok(#(56, input))
    ["5", "7", ..input] -> Ok(#(57, input))
    ["5", "8", ..input] -> Ok(#(58, input))
    ["5", "9", ..input] -> Ok(#(59, input))

    [g, ..] -> Error(Unexpected(g, expected))
    [] -> Error(Unexpected("EOF", expected))
  }
}

fn parse_date(input: Tokens, year: Int) -> Parsed(Toml) {
  case input {
    ["0", "1", "-", ..input] -> parse_date_day(input, year, calendar.January)
    ["0", "2", "-", ..input] -> parse_date_day(input, year, calendar.February)
    ["0", "3", "-", ..input] -> parse_date_day(input, year, calendar.March)
    ["0", "4", "-", ..input] -> parse_date_day(input, year, calendar.April)
    ["0", "5", "-", ..input] -> parse_date_day(input, year, calendar.May)
    ["0", "6", "-", ..input] -> parse_date_day(input, year, calendar.June)
    ["0", "7", "-", ..input] -> parse_date_day(input, year, calendar.July)
    ["0", "8", "-", ..input] -> parse_date_day(input, year, calendar.August)
    ["0", "9", "-", ..input] -> parse_date_day(input, year, calendar.September)
    ["1", "0", "-", ..input] -> parse_date_day(input, year, calendar.October)
    ["1", "1", "-", ..input] -> parse_date_day(input, year, calendar.November)
    ["1", "2", "-", ..input] -> parse_date_day(input, year, calendar.December)

    [g, ..] -> Error(Unexpected(g, "date month"))
    [] -> Error(Unexpected("EOF", "date month"))
  }
}

fn parse_date_day(
  input: Tokens,
  year: Int,
  month: calendar.Month,
) -> Parsed(Toml) {
  case input {
    ["0", "1", ..input] -> parse_date_end(input, year, month, 1)
    ["0", "2", ..input] -> parse_date_end(input, year, month, 2)
    ["0", "3", ..input] -> parse_date_end(input, year, month, 3)
    ["0", "4", ..input] -> parse_date_end(input, year, month, 4)
    ["0", "5", ..input] -> parse_date_end(input, year, month, 5)
    ["0", "6", ..input] -> parse_date_end(input, year, month, 6)
    ["0", "7", ..input] -> parse_date_end(input, year, month, 7)
    ["0", "8", ..input] -> parse_date_end(input, year, month, 8)
    ["0", "9", ..input] -> parse_date_end(input, year, month, 9)
    ["1", "0", ..input] -> parse_date_end(input, year, month, 10)
    ["1", "1", ..input] -> parse_date_end(input, year, month, 11)
    ["1", "2", ..input] -> parse_date_end(input, year, month, 12)
    ["1", "3", ..input] -> parse_date_end(input, year, month, 13)
    ["1", "4", ..input] -> parse_date_end(input, year, month, 14)
    ["1", "5", ..input] -> parse_date_end(input, year, month, 15)
    ["1", "6", ..input] -> parse_date_end(input, year, month, 16)
    ["1", "7", ..input] -> parse_date_end(input, year, month, 17)
    ["1", "8", ..input] -> parse_date_end(input, year, month, 18)
    ["1", "9", ..input] -> parse_date_end(input, year, month, 19)
    ["2", "0", ..input] -> parse_date_end(input, year, month, 20)
    ["2", "1", ..input] -> parse_date_end(input, year, month, 21)
    ["2", "2", ..input] -> parse_date_end(input, year, month, 22)
    ["2", "3", ..input] -> parse_date_end(input, year, month, 23)
    ["2", "4", ..input] -> parse_date_end(input, year, month, 24)
    ["2", "5", ..input] -> parse_date_end(input, year, month, 25)
    ["2", "6", ..input] -> parse_date_end(input, year, month, 26)
    ["2", "7", ..input] -> parse_date_end(input, year, month, 27)
    ["2", "8", ..input] -> parse_date_end(input, year, month, 28)
    ["2", "9", ..input] -> parse_date_end(input, year, month, 29)
    ["3", "0", ..input] -> parse_date_end(input, year, month, 30)
    ["3", "1", ..input] -> parse_date_end(input, year, month, 31)

    [g, ..] -> Error(Unexpected(g, "date day"))
    [] -> Error(Unexpected("EOF", "date day"))
  }
}

fn parse_date_end(
  input: Tokens,
  year: Int,
  month: calendar.Month,
  day: Int,
) -> Parsed(Toml) {
  let date = calendar.Date(year, month, day)
  case input {
    [" ", ..input] | ["T", ..input] -> {
      use time, input <- do(parse_time_value(input))
      use offset, input <- do(parse_offset(input))
      Ok(#(DateTime(date, time, offset), input))
    }

    _ -> Ok(#(Date(date), input))
  }
}

fn parse_offset(input: Tokens) -> Parsed(Offset) {
  case input {
    ["Z", ..input] -> Ok(#(Offset(calendar.utc_offset), input))
    ["+", ..input] -> parse_offset_hours(input, Positive)
    ["-", ..input] -> parse_offset_hours(input, Negative)
    _ -> Ok(#(Local, input))
  }
}

fn parse_offset_hours(input: Tokens, sign: Sign) -> Parsed(Offset) {
  use #(hours, minutes), input <- do(parse_hour_minute(input))
  let duration = case sign {
    Positive -> duration.add(duration.hours(hours), duration.minutes(minutes))
    Negative -> duration.add(duration.hours(-hours), duration.minutes(-minutes))
  }
  Ok(#(Offset(duration), input))
}

/// Get a int from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_int(Int(1))
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// as_int(Float(1.4))
/// // -> Error(WrongType([], "Int", "Float"))
/// ```
pub fn as_int(toml: Toml) -> Result(Int, GetError) {
  case toml {
    Int(f) -> Ok(f)
    other -> Error(WrongType([], "Int", classify(other)))
  }
}

/// Get a float from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_float(Float(1.5))
/// // -> Ok(1.5)
/// ```
///
/// ```gleam
/// as_float(Int(1))
/// // -> Error(WrongType([], "Float", "Int"))
/// ```
pub fn as_float(toml: Toml) -> Result(Float, GetError) {
  case toml {
    Float(f) -> Ok(f)
    other -> Error(WrongType([], "Float", classify(other)))
  }
}

/// Get a bool from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_bool(Bool(true))
/// // -> Ok(true)
/// ```
///
/// ```gleam
/// as_bool(Int(1))
/// // -> Error(WrongType([], "Bool", "Int"))
/// ```
pub fn as_bool(toml: Toml) -> Result(Bool, GetError) {
  case toml {
    Bool(b) -> Ok(b)
    other -> Error(WrongType([], "Bool", classify(other)))
  }
}

/// Get a string from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_string(String("hello"))
/// // -> Ok("hello")
/// ```
///
/// ```gleam
/// as_string(Int(1))
/// // -> Error(WrongType([], "String", "Int"))
/// ```
pub fn as_string(toml: Toml) -> Result(String, GetError) {
  case toml {
    String(s) -> Ok(s)
    other -> Error(WrongType([], "String", classify(other)))
  }
}

/// Get a date from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_date(Date(date))
/// // -> Ok(date)
/// ```
///
/// ```gleam
/// as_date(Int(1))
/// // -> Error(WrongType([], "Date", "Int"))
/// ```
pub fn as_date(toml: Toml) -> Result(calendar.Date, GetError) {
  case toml {
    Date(d) -> Ok(d)
    other -> Error(WrongType([], "Date", classify(other)))
  }
}

/// Get a time from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_time_of_day(Time(time))
/// // -> Ok(time)
/// ```
///
/// ```gleam
/// as_time_of_day(Int(1))
/// // -> Error(WrongType([], "Time", "Int"))
/// ```
pub fn as_time_of_day(toml: Toml) -> Result(calendar.TimeOfDay, GetError) {
  case toml {
    Time(t) -> Ok(t)
    other -> Error(WrongType([], "Time", classify(other)))
  }
}

/// Get an unambiguous time from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_timestamp(Int(1))
/// // -> Error(WrongType([], "DateTime with offset", "Int"))
/// ```
pub fn as_timestamp(toml: Toml) -> Result(timestamp.Timestamp, GetError) {
  case toml {
    DateTime(date:, time:, offset: Offset(offset)) ->
      Ok(timestamp.from_calendar(date, time, offset))

    other -> Error(WrongType([], "DateTime with offset", classify(other)))
  }
}

/// Get a datetime from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_calendar_time(DateTime(datetime))
/// // -> Ok(datetime)
/// ```
///
/// ```gleam
/// as_calendar_time(Int(1))
/// // -> Error(WrongType([], "DateTime", "Int"))
/// ```
pub fn as_calendar_time(
  toml: Toml,
) -> Result(#(calendar.Date, calendar.TimeOfDay, Offset), GetError) {
  case toml {
    DateTime(d, t, o) -> Ok(#(d, t, o))
    other -> Error(WrongType([], "DateTime", classify(other)))
  }
}

/// Get an array from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_array(Array([]))
/// // -> Ok([])
/// ```
///
/// ```gleam
/// as_array(Int(1))
/// // -> Error(WrongType([], "Array", "Int"))
/// ```
pub fn as_array(toml: Toml) -> Result(List(Toml), GetError) {
  case toml {
    Array(arr) -> Ok(arr)
    other -> Error(WrongType([], "Array", classify(other)))
  }
}

/// Get a table from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_table(Table(dict.new()))
/// // -> Ok(dict.new())
/// ```
///
/// ```gleam
/// as_table(Int(1))
/// // -> Error(WrongType([], "Table", "Int"))
/// ```
pub fn as_table(toml: Toml) -> Result(Dict(String, Toml), GetError) {
  case toml {
    Table(tbl) -> Ok(tbl)
    InlineTable(tbl) -> Ok(tbl)
    other -> Error(WrongType([], "Table", classify(other)))
  }
}

/// Get a number (int or float) from a TOML document.
///
/// ## Examples
///
/// ```gleam
/// as_number(Int(1))
/// // -> Ok(NumberInt(1))
/// ```
///
/// ```gleam
/// as_number(Float(1.5))
/// // -> Ok(NumberFloat(1.5))
/// ```
///
/// ```gleam
/// as_number(Bool(true))
/// // -> Error(WrongType([], "Number", "Bool"))
/// ```
pub fn as_number(toml: Toml) -> Result(Number, GetError) {
  case toml {
    Int(x) -> Ok(NumberInt(x))
    Float(x) -> Ok(NumberFloat(x))
    Nan(x) -> Ok(NumberNan(x))
    Infinity(x) -> Ok(NumberInfinity(x))
    other -> Error(WrongType([], "Number", classify(other)))
  }
}
