-module(lustre@internals@json_object_builder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/internals/json_object_builder.gleam").
-export([new/0, json/3, tagged/1, build/1, string/3, int/3, bool/3, list/4, object/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre/internals/json_object_builder.gleam", 13).
?DOC(false).
-spec new() -> list({binary(), gleam@json:json()}).
new() ->
    [].

-file("src/lustre/internals/json_object_builder.gleam", 25).
?DOC(false).
-spec json(list({binary(), gleam@json:json()}), binary(), gleam@json:json()) -> list({binary(),
    gleam@json:json()}).
json(Entries, Key, Value) ->
    [{Key, Value} | Entries].

-file("src/lustre/internals/json_object_builder.gleam", 17).
?DOC(false).
-spec tagged(integer()) -> list({binary(), gleam@json:json()}).
tagged(Kind) ->
    [{<<"kind"/utf8>>, gleam@json:int(Kind)}].

-file("src/lustre/internals/json_object_builder.gleam", 21).
?DOC(false).
-spec build(list({binary(), gleam@json:json()})) -> gleam@json:json().
build(Entries) ->
    gleam@json:object(Entries).

-file("src/lustre/internals/json_object_builder.gleam", 29).
?DOC(false).
-spec string(list({binary(), gleam@json:json()}), binary(), binary()) -> list({binary(),
    gleam@json:json()}).
string(Entries, Key, Value) ->
    case Value /= <<""/utf8>> of
        true ->
            [{Key, gleam@json:string(Value)} | Entries];

        false ->
            Entries
    end.

-file("src/lustre/internals/json_object_builder.gleam", 36).
?DOC(false).
-spec int(list({binary(), gleam@json:json()}), binary(), integer()) -> list({binary(),
    gleam@json:json()}).
int(Entries, Key, Value) ->
    case Value /= 0 of
        true ->
            [{Key, gleam@json:int(Value)} | Entries];

        false ->
            Entries
    end.

-file("src/lustre/internals/json_object_builder.gleam", 43).
?DOC(false).
-spec bool(list({binary(), gleam@json:json()}), binary(), boolean()) -> list({binary(),
    gleam@json:json()}).
bool(Entries, Key, Value) ->
    case Value of
        true ->
            [{Key, gleam@json:int(1)} | Entries];

        false ->
            Entries
    end.

-file("src/lustre/internals/json_object_builder.gleam", 50).
?DOC(false).
-spec list(
    list({binary(), gleam@json:json()}),
    binary(),
    list(MSQ),
    fun((MSQ) -> gleam@json:json())
) -> list({binary(), gleam@json:json()}).
list(Entries, Key, Values, To_json) ->
    case Values of
        [] ->
            Entries;

        _ ->
            [{Key, gleam@json:array(Values, To_json)} | Entries]
    end.

-file("src/lustre/internals/json_object_builder.gleam", 62).
?DOC(false).
-spec object(
    list({binary(), gleam@json:json()}),
    binary(),
    list({binary(), gleam@json:json()})
) -> list({binary(), gleam@json:json()}).
object(Entries, Key, Nested) ->
    case Nested of
        [] ->
            Entries;

        _ ->
            [{Key, gleam@json:object(Nested)} | Entries]
    end.
