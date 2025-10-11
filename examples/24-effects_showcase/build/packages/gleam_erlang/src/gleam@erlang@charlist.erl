-module(gleam@erlang@charlist).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/erlang/charlist.gleam").
-export([to_string/1, from_string/1]).
-export_type([charlist/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " A charlist is a list of integers where all the integers are valid code\n"
    " points.\n"
    "\n"
    " In practice, you will not come across them often, except perhaps when\n"
    " interfacing with Erlang, in particular when using older libraries that do\n"
    " not accept binaries as arguments.\n"
).

-type charlist() :: any().

-file("src/gleam/erlang/charlist.gleam", 18).
?DOC(
    " Convert a charlist to a string using Erlang's\n"
    " `unicode:characters_to_binary`.\n"
).
-spec to_string(charlist()) -> binary().
to_string(A) ->
    unicode:characters_to_binary(A).

-file("src/gleam/erlang/charlist.gleam", 24).
?DOC(
    " Convert a string to a charlist using Erlang's\n"
    " `unicode:characters_to_list`.\n"
).
-spec from_string(binary()) -> charlist().
from_string(A) ->
    unicode:characters_to_list(A).
