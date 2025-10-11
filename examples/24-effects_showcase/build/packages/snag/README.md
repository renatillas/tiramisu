# Snag

A Snag is a boilerplate-free ad-hoc error type.

Use `Result(value, Snag)` (or the `snag.Result(value)` alias) in functions
that may fail.

A low level message like "Unexpected status 401" or "No such file or
directory" can be confusing or difficult to debug, so use the `snag.context`
function to add extra contextual information.

```gleam
import gleam/io
import gleam/result
import my_app.{User}
import snag.{type Result}

pub fn log_in(user_id: Int) -> Result(User) {
  use api_key <- result.try(
    my_app.read_file("api_key.txt")
    |> snag.context("Could not load API key")
  )

  use session_token <- result.try(
    user_id
    |> my_app.create_session(api_key)
    |> snag.context("Session creation failed")
  )

  Ok(session_token)
}

pub fn main() {
  case log_in(42) {
    Ok(session) -> io.println("Logged in!")
    Error(snag) -> {
      io.print(snag.pretty_print(snag))
      my_app.exit(1)
    }
  }
}
```

In this code when an error occurs within the `create_session` function an
error message like this is printed using the added contextual information:

```text
error: Session creation failed

cause:
  0: Unable to exchange token with authentication service
  1: Service authentication failed
  2: Unexpected HTTP status 401
```

## When should I use Snag?

Snag is useful in code where it must either pass or fail, and when it fails we
want good debugging information to print to the user. i.e. Command line
tools, data processing pipelines, etc. Here Snag provides a convenient way to
create errors with a reasonable amount of debugging information, without the
boilerplate of a custom error type.

It is not suited to code where the application needs to make a decision about
what to do in the event of an error, such as whether to give up or to try
again. i.e. Libraries, web application backends, API clients, etc. In these
situations it is recommended to create a custom type for your errors as it
can be pattern matched on and have any additional detail added as fields.

## Installation

Add `snag` to your Gleam project

```
gleam add snag
```

## Prior art

This library is inspired by the following projects:

- Rust's [`anyhow`](https://github.com/dtolnay/anyhow) and
  [`std::error::Error`](https://doc.rust-lang.org/std/error/trait.Error.html)
- Go's [`error`](https://golang.org/pkg/errors/).
