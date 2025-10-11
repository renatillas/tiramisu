import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $gleam from "./gleam.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "./gleam.mjs";

export class Snag extends $CustomType {
  constructor(issue, cause) {
    super();
    this.issue = issue;
    this.cause = cause;
  }
}

/**
 * Create a new `Snag` with the given issue text.
 *
 * See also the `error` function for creating a `Snag` wrapped in a `Result`.
 *
 * # Example
 *
 * ```gleam
 * > new("Not enough credit")
 * > |> line_print
 * "error: Not enough credit"
 * ```
 */
export function new$(issue) {
  return new Snag(issue, toList([]));
}

/**
 * Create a new `Snag` wrapped in a `Result` with the given issue text.
 *
 * # Example
 *
 * ```gleam
 * > error("Not enough credit")
 * Error(new("Not enough credit"))
 * ```
 */
export function error(issue) {
  return new Error(new$(issue));
}

/**
 * Add additional contextual information to a `Snag`.
 *
 * See also the `context` function for adding contextual information to a `Snag`
 * wrapped in a `Result`.
 *
 * # Example
 *
 * ```gleam
 * > new("Not enough credit")
 * > |> layer("Unable to make purchase")
 * > |> line_print
 * "error: Unable to make purchase <- Not enough credit"
 * ```
 */
export function layer(snag, issue) {
  return new Snag(issue, listPrepend(snag.issue, snag.cause));
}

/**
 * Add additional contextual information to a `Snag` wrapped in a `Result`.
 *
 * # Example
 *
 * ```gleam
 * > error("Not enough credit")
 * > |> context("Unable to make purchase")
 * > |> result.map_error(line_print)
 * Error("error: Unable to make purchase <- Not enough credit")
 * ```
 */
export function context(result, issue) {
  if (result instanceof Ok) {
    return result;
  } else {
    let snag = result[0];
    return new Error(layer(snag, issue));
  }
}

/**
 * Maps the error type in a `Result` to a `Snag` given a describing function.
 * The describing function should produce a human friendly string
 * reprensentation of the error.
 * 
 * # Example
 *
 * ```gleam
 * > my_app.read_file("api_key.txt")
 * > |> snag.map_error(my_app.describe_error)
 * > |> snag.context("Could not load API key")
 * > |> snag.line_print
 * "error: Could not load API key <- File is locked"
 * ```
 */
export function map_error(result, describer) {
  if (result instanceof Ok) {
    return result;
  } else {
    let b = result[0];
    let _pipe = describer(b);
    return error(_pipe);
  }
}

function pretty_print_cause(cause) {
  let _pipe = cause;
  let _pipe$1 = $list.index_map(
    _pipe,
    (line, index) => {
      return $string.concat(
        toList(["  ", $int.to_string(index), ": ", line, "\n"]),
      );
    },
  );
  return $string.concat(_pipe$1);
}

/**
 * Turn a snag into a multi-line string, optimised for readability.
 *
 * # Example
 *
 * ```gleam
 * > new("Not enough credit")
 * > |> layer("Unable to make purchase")
 * > |> layer("Character creation failed")
 * > |> pretty_print
 * "error: Character creation failed
 *
 * cause:
 *   0: Unable to make purchase
 *   1: Not enough credit
 * "
 * ```
 */
export function pretty_print(snag) {
  let output = ("error: " + snag.issue) + "\n";
  let $ = snag.cause;
  if ($ instanceof $Empty) {
    return output;
  } else {
    let cause = $;
    return (output + "\ncause:\n") + pretty_print_cause(cause);
  }
}

/**
 * Turn a snag into a single-line string, optimised for compactness. This may be
 * useful for logging snags.
 *
 * # Example
 *
 * ```gleam
 * > new("Not enough credit")
 * > |> layer("Unable to make purchase")
 * > |> layer("Character creation failed")
 * > |> line_print
 * "error: Character creation failed <- Unable to make purchase <- Not enough credit"
 * ```
 */
export function line_print(snag) {
  let _pipe = listPrepend($string.append("error: ", snag.issue), snag.cause);
  return $string.join(_pipe, " <- ");
}
