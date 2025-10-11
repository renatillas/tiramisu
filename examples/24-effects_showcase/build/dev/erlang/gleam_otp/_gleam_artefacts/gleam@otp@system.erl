-module(gleam@otp@system).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/otp/system.gleam").
-export([debug_state/1, get_state/1, suspend/1, resume/1]).
-export_type([mode/0, debug_option/0, debug_state/0, status_info/0, system_message/0, do_not_leak/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type mode() :: running | suspended.

-type debug_option() :: no_debug.

-type debug_state() :: any().

-type status_info() :: {status_info,
        gleam@erlang@atom:atom_(),
        gleam@erlang@process:pid_(),
        mode(),
        debug_state(),
        gleam@dynamic:dynamic_()}.

-type system_message() :: {resume, fun(() -> nil)} |
    {suspend, fun(() -> nil)} |
    {get_state, fun((gleam@dynamic:dynamic_()) -> nil)} |
    {get_status, fun((status_info()) -> nil)}.

-type do_not_leak() :: any().

-file("src/gleam/otp/system.gleam", 19).
-spec debug_state(list(debug_option())) -> debug_state().
debug_state(A) ->
    sys:debug_options(A).

-file("src/gleam/otp/system.gleam", 65).
?DOC(
    " Get the state of a given OTP compatible process. This function is only\n"
    " intended for debugging.\n"
    "\n"
    " Requires Erlang/OTP 26.1 or newer, as the underlying interface changed\n"
    " in [OTP-18633][1] from a literal type to a result type.\n"
    "\n"
    " For more information see the [Erlang documentation][2].\n"
    "\n"
    " [1]: https://www.erlang.org/patches/otp-26.1#stdlib-5.1\n"
    " [2]: https://erlang.org/doc/man/sys.html#get_state-1\n"
).
-spec get_state(gleam@erlang@process:pid_()) -> gleam@dynamic:dynamic_().
get_state(From) ->
    sys:get_state(From).

-file("src/gleam/otp/system.gleam", 77).
?DOC(
    " Request an OTP compatible process to suspend, causing it to only handle\n"
    " system messages.\n"
    "\n"
    " For more information see the [Erlang documentation][1].\n"
    "\n"
    " [1]: https://erlang.org/doc/man/sys.html#suspend-1\n"
).
-spec suspend(gleam@erlang@process:pid_()) -> nil.
suspend(Pid) ->
    sys:suspend(Pid),
    nil.

-file("src/gleam/otp/system.gleam", 92).
?DOC(
    " Request a suspended OTP compatible process to result, causing it to handle\n"
    " all messages rather than only system messages.\n"
    "\n"
    " For more information see the [Erlang documentation][1].\n"
    "\n"
    " [1]: https://erlang.org/doc/man/sys.html#resume-1\n"
).
-spec resume(gleam@erlang@process:pid_()) -> nil.
resume(Pid) ->
    sys:resume(Pid),
    nil.
