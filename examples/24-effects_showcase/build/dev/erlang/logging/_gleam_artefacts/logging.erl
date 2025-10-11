-module(logging).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/logging.gleam").
-export([configure/0, log/2, set_level/1]).
-export_type([log_level/0, do_not_leak/0, key/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type log_level() :: emergency |
    alert |
    critical |
    error |
    warning |
    notice |
    info |
    debug.

-type do_not_leak() :: any().

-type key() :: level.

-file("src/logging.gleam", 25).
?DOC(
    " Configure the default Erlang logger handler with a pretty Gleam output\n"
    " format, and sets the logging level to `Info`.\n"
    "\n"
    " ## Interaction with Elixir\n"
    "\n"
    " Elixir's built-in `logger` application removes Erlang's default logger\n"
    " handler and replaces it with its own code, so if you have an Elixir package\n"
    " in your project then this code will not be able to configure the logger as\n"
    " it could normally.\n"
).
-spec configure() -> nil.
configure() ->
    logging_ffi:configure().

-file("src/logging.gleam", 29).
?DOC(" Log a message to the Erlang logger at the given log level.\n").
-spec log(log_level(), binary()) -> nil.
log(Level, Message) ->
    logger:log(Level, Message),
    nil.

-file("src/logging.gleam", 42).
?DOC(" Change the log visibility level to be output.\n").
-spec set_level(log_level()) -> nil.
set_level(Level) ->
    logger:set_primary_config(level, Level),
    nil.
