import gleam
import gleam/int
import gleam/list
import gleam/string

/// A Snag is a boilerplate-free error type that can be used to track why an
/// error happened, though does not store as much detail on specific errors as a
/// custom error type would.
///
/// It is useful in code where it must either pass or fail, and when it fails we
/// want good debugging information to print to the user. i.e. Command line
/// tools, data processing pipelines, etc.
///
/// If it not suited to code where the application needs to make a decision about
/// what to do in the event of an error, such as whether to give up or to try
/// again. i.e. Libraries, web application backends, API clients, etc.
/// In these situations it is recommended to create a custom type for your errors
/// as it can be pattern matched on and have any additional detail added as
/// fields.
pub type Snag {
  Snag(issue: String, cause: List(String))
}

/// A concise alias for a `Result` that uses a `Snag` as the error value.
pub type Result(t) =
  gleam.Result(t, Snag)

/// Create a new `Snag` with the given issue text.
///
/// See also the `error` function for creating a `Snag` wrapped in a `Result`.
///
/// # Example
///
/// ```gleam
/// > new("Not enough credit")
/// > |> line_print
/// "error: Not enough credit"
/// ```
pub fn new(issue: String) -> Snag {
  Snag(issue: issue, cause: [])
}

/// Create a new `Snag` wrapped in a `Result` with the given issue text.
///
/// # Example
///
/// ```gleam
/// > error("Not enough credit")
/// Error(new("Not enough credit"))
/// ```
pub fn error(issue: String) -> Result(success) {
  Error(new(issue))
}

/// Add additional contextual information to a `Snag`.
///
/// See also the `context` function for adding contextual information to a `Snag`
/// wrapped in a `Result`.
///
/// # Example
///
/// ```gleam
/// > new("Not enough credit")
/// > |> layer("Unable to make purchase")
/// > |> line_print
/// "error: Unable to make purchase <- Not enough credit"
/// ```
pub fn layer(snag: Snag, issue: String) -> Snag {
  Snag(issue: issue, cause: [snag.issue, ..snag.cause])
}

/// Add additional contextual information to a `Snag` wrapped in a `Result`.
///
/// # Example
///
/// ```gleam
/// > error("Not enough credit")
/// > |> context("Unable to make purchase")
/// > |> result.map_error(line_print)
/// Error("error: Unable to make purchase <- Not enough credit")
/// ```
pub fn context(result: Result(success), issue: String) -> Result(success) {
  case result {
    Ok(_) -> result
    Error(snag) -> Error(layer(snag, issue))
  }
}

/// Maps the error type in a `Result` to a `Snag` given a describing function.
/// The describing function should produce a human friendly string
/// reprensentation of the error.
/// 
/// # Example
///
/// ```gleam
/// > my_app.read_file("api_key.txt")
/// > |> snag.map_error(my_app.describe_error)
/// > |> snag.context("Could not load API key")
/// > |> snag.line_print
/// "error: Could not load API key <- File is locked"
/// ```
pub fn map_error(
  result: gleam.Result(a, b),
  with describer: fn(b) -> String,
) -> Result(a) {
  case result {
    Ok(a) -> Ok(a)
    Error(b) -> describer(b) |> error
  }
}

/// Turn a snag into a multi-line string, optimised for readability.
///
/// # Example
///
/// ```gleam
/// > new("Not enough credit")
/// > |> layer("Unable to make purchase")
/// > |> layer("Character creation failed")
/// > |> pretty_print
/// "error: Character creation failed
///
/// cause:
///   0: Unable to make purchase
///   1: Not enough credit
/// "
/// ```
pub fn pretty_print(snag: Snag) -> String {
  let output = "error: " <> snag.issue <> "\n"

  case snag.cause {
    [] -> output
    cause -> output <> "\ncause:\n" <> pretty_print_cause(cause)
  }
}

fn pretty_print_cause(cause) {
  cause
  |> list.index_map(fn(line, index) {
    string.concat(["  ", int.to_string(index), ": ", line, "\n"])
  })
  |> string.concat
}

/// Turn a snag into a single-line string, optimised for compactness. This may be
/// useful for logging snags.
///
/// # Example
///
/// ```gleam
/// > new("Not enough credit")
/// > |> layer("Unable to make purchase")
/// > |> layer("Character creation failed")
/// > |> line_print
/// "error: Character creation failed <- Unable to make purchase <- Not enough credit"
/// ```
pub fn line_print(snag: Snag) -> String {
  [string.append("error: ", snag.issue), ..snag.cause]
  |> string.join(" <- ")
}
