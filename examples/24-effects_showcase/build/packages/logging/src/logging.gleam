pub type LogLevel {
  Emergency
  Alert
  Critical
  Error
  Warning
  Notice
  Info
  Debug
}

type DoNotLeak

/// Configure the default Erlang logger handler with a pretty Gleam output
/// format, and sets the logging level to `Info`.
///
/// ## Interaction with Elixir
///
/// Elixir's built-in `logger` application removes Erlang's default logger
/// handler and replaces it with its own code, so if you have an Elixir package
/// in your project then this code will not be able to configure the logger as
/// it could normally.
///
@external(erlang, "logging_ffi", "configure")
pub fn configure() -> Nil

/// Log a message to the Erlang logger at the given log level.
///
pub fn log(level: LogLevel, message: String) -> Nil {
  erlang_log(level, message)
  Nil
}

@external(erlang, "logger", "log")
fn erlang_log(level: LogLevel, message: String) -> DoNotLeak

@external(erlang, "logger", "set_primary_config")
fn set_primary_config_level(key: Key, level: LogLevel) -> DoNotLeak

/// Change the log visibility level to be output.
///
pub fn set_level(level: LogLevel) -> Nil {
  set_primary_config_level(Level, level)
  Nil
}

type Key {
  Level
}
