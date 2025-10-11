-module(logging).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([configure/0, log/2, set_level/1]).
-export_type([log_level/0, do_not_leak/0, key/0]).

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

-spec configure() -> nil.
configure() ->
    logging_ffi:configure().

-spec log(log_level(), binary()) -> nil.
log(Level, Message) ->
    logger:log(Level, Message),
    nil.

-spec set_level(log_level()) -> nil.
set_level(Level) ->
    logger:set_primary_config(level, Level),
    nil.
