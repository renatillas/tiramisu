-module(exception).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/exception.gleam").
-export([rescue/1, defer/2, on_crash/2]).
-export_type([exception/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type exception() :: {errored, gleam@dynamic:dynamic_()} |
    {thrown, gleam@dynamic:dynamic_()} |
    {exited, gleam@dynamic:dynamic_()}.

-file("src/exception.gleam", 29).
?DOC(
    " This function will catch any crash and convert it into a result rather than\n"
    " crashing the process.\n"
    "\n"
    " You should ideally never use this function! Exceptions are not flow control\n"
    " in Gleam, a result type should be used instead. This function is only if you\n"
    " need to perform some cleanup when a crash occurs, and then you should favour\n"
    " `defer` if possible.\n"
).
-spec rescue(fun(() -> EHB)) -> {ok, EHB} | {error, exception()}.
rescue(Body) ->
    exception_ffi:rescue(Body).

-file("src/exception.gleam", 51).
?DOC(
    " This function will run a cleanup function after the given body function, even\n"
    " if the body function crashes.\n"
    "\n"
    " You should ideally never use this function! Exceptions are not flow control\n"
    " in Gleam, a result type should be used instead. This function is only if you\n"
    " need to perform some cleanup when a crash occurs.\n"
    "\n"
    " # Examples\n"
    " \n"
    " ```gleam\n"
    " pub fn run_with_lock(f: fn() -> a) -> a {\n"
    "   let lock = acquire()\n"
    "   use <- defer(fn() { release(lock) })\n"
    "   f()\n"
    " }\n"
    " ```\n"
).
-spec defer(fun(() -> any()), fun(() -> EHF)) -> EHF.
defer(Cleanup, Body) ->
    exception_ffi:defer(Cleanup, Body).

-file("src/exception.gleam", 62).
?DOC(
    " This function will run a cleanup function after the given body function,\n"
    " but only if the body function crashes.\n"
    "\n"
    " You should ideally never use this function! Exceptions are not flow control\n"
    " in Gleam, a result type should be used instead. This function is only if you\n"
    " need to perform some cleanup when a crash occurs.\n"
).
-spec on_crash(fun(() -> any()), fun(() -> EHH)) -> EHH.
on_crash(Cleanup, Body) ->
    exception_ffi:on_crash(Cleanup, Body).
