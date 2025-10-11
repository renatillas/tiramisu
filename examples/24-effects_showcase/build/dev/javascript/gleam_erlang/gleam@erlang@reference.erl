-module(gleam@erlang@reference).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/erlang/reference.gleam").
-export([new/0]).
-export_type([reference_/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type reference_() :: any().

-file("src/gleam/erlang/reference.gleam", 15).
?DOC(" Create a new unique reference.\n").
-spec new() -> reference_().
new() ->
    erlang:make_ref().
