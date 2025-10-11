-module(vec@internal).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vec/internal.gleam").
-export([cos/1, acos/1, sin/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/vec/internal.gleam", 13).
?DOC(false).
-spec cos(float()) -> float().
cos(X) ->
    math:cos(X).

-file("src/vec/internal.gleam", 23).
?DOC(false).
-spec acos(float()) -> {ok, float()} | {error, nil}.
acos(X) ->
    case (X >= -1.0) andalso (X =< 1.0) of
        true ->
            {ok, math:acos(X)};

        false ->
            {error, nil}
    end.

-file("src/vec/internal.gleam", 7).
?DOC(false).
-spec sin(float()) -> float().
sin(X) ->
    cos((3.1415926535897932 / 2.0) - X).
