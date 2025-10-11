-module(lustre_dev_tools@system).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/system.gleam").
-export([detect_os/0, detect_arch/0, is_alpine/0, run/1, find/1, cwd/0, exit/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre_dev_tools/system.gleam", 4).
?DOC(false).
-spec detect_os() -> binary().
detect_os() ->
    system_ffi:detect_os().

-file("src/lustre_dev_tools/system.gleam", 9).
?DOC(false).
-spec detect_arch() -> binary().
detect_arch() ->
    system_ffi:detect_arch().

-file("src/lustre_dev_tools/system.gleam", 14).
?DOC(false).
-spec is_alpine() -> boolean().
is_alpine() ->
    system_ffi:is_alpine().

-file("src/lustre_dev_tools/system.gleam", 20).
?DOC(false).
-spec run(binary()) -> {ok, binary()} | {error, binary()}.
run(Command) ->
    system_ffi:run(Command).

-file("src/lustre_dev_tools/system.gleam", 23).
?DOC(false).
-spec find(binary()) -> {ok, binary()} | {error, nil}.
find(Executable) ->
    system_ffi:find(Executable).

-file("src/lustre_dev_tools/system.gleam", 28).
?DOC(false).
-spec cwd() -> binary().
cwd() ->
    system_ffi:cwd().

-file("src/lustre_dev_tools/system.gleam", 33).
?DOC(false).
-spec exit(integer()) -> nil.
exit(Code) ->
    system_ffi:exit(Code).
