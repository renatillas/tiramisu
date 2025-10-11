-module(lustre@internals@constants).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/internals/constants.gleam").
-export([empty_dict/0, empty_set/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre/internals/constants.gleam", 22).
?DOC(false).
-spec empty_dict() -> gleam@dict:dict(any(), any()).
empty_dict() ->
    gleam@dict:new().

-file("src/lustre/internals/constants.gleam", 26).
?DOC(false).
-spec empty_set() -> gleam@set:set(any()).
empty_set() ->
    gleam@set:new().
