-module(vec@vec2).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vec/vec2.gleam").
-export([splat/1, from_tuple/1, to_tuple/1, to_list/1, x/1, y/1, replace_x/2, replace_y/2, map_x/2, map_y/2, swap/1, map/2, map2/3, result/1]).
-export_type([vec2/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type vec2(DTW) :: {vec2, DTW, DTW}.

-file("src/vec/vec2.gleam", 17).
?DOC(
    " Creates a vector with all elements set to a value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " splat(12)\n"
    " // -> Vec2(12, 12)\n"
    " ```\n"
).
-spec splat(DTY) -> vec2(DTY).
splat(Value) ->
    {vec2, Value, Value}.

-file("src/vec/vec2.gleam", 30).
?DOC(
    " Converts a tuple of the contained elements into a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " #(12, -34) |> from_tuple()\n"
    " // -> Vec2(12, -34)\n"
    " ```\n"
).
-spec from_tuple({DUA, DUA}) -> vec2(DUA).
from_tuple(Tuple) ->
    {vec2, erlang:element(1, Tuple), erlang:element(2, Tuple)}.

-file("src/vec/vec2.gleam", 43).
?DOC(
    " Converts the vector into a tuple of the contained elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> to_tuple()\n"
    " // -> #(12, -34)\n"
    " ```\n"
).
-spec to_tuple(vec2(DUC)) -> {DUC, DUC}.
to_tuple(Vector) ->
    {erlang:element(2, Vector), erlang:element(3, Vector)}.

-file("src/vec/vec2.gleam", 56).
?DOC(
    " Converts the vector into a list of the contained elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> to_list()\n"
    " // -> [12, -34]\n"
    " ```\n"
).
-spec to_list(vec2(DUE)) -> list(DUE).
to_list(Vector) ->
    [erlang:element(2, Vector), erlang:element(3, Vector)].

-file("src/vec/vec2.gleam", 69).
?DOC(
    " Returns the x element in a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> x()\n"
    " // -> 12\n"
    " ```\n"
).
-spec x(vec2(DUH)) -> DUH.
x(Vector) ->
    erlang:element(2, Vector).

-file("src/vec/vec2.gleam", 82).
?DOC(
    " Returns the y element in a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> y()\n"
    " // -> -34\n"
    " ```\n"
).
-spec y(vec2(DUJ)) -> DUJ.
y(Vector) ->
    erlang:element(3, Vector).

-file("src/vec/vec2.gleam", 95).
?DOC(
    " Returns a new vector with the x element replace with `value`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> replace_x(777)\n"
    " // -> Vec2(777, -34)\n"
    " ```\n"
).
-spec replace_x(vec2(DUL), DUL) -> vec2(DUL).
replace_x(Vector, Value) ->
    {vec2, Value, erlang:element(3, Vector)}.

-file("src/vec/vec2.gleam", 108).
?DOC(
    " Returns a new vector with the y element replace with `value`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> replace_y(777)\n"
    " // -> Vec2(12, 777)\n"
    " ```\n"
).
-spec replace_y(vec2(DUO), DUO) -> vec2(DUO).
replace_y(Vector, Value) ->
    {vec2, erlang:element(2, Vector), Value}.

-file("src/vec/vec2.gleam", 121).
?DOC(
    " Returns a new vector with the x element having had `with` applied to it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> map_x(fn(n) { n * 2 })\n"
    " // -> Vec2(24, -34)\n"
    " ```\n"
).
-spec map_x(vec2(DUR), fun((DUR) -> DUR)) -> vec2(DUR).
map_x(Vector, Fun) ->
    {vec2, Fun(erlang:element(2, Vector)), erlang:element(3, Vector)}.

-file("src/vec/vec2.gleam", 134).
?DOC(
    " Returns a new vector with the y element having had `with` applied to it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> map_y(fn(n) { n * 2 })\n"
    " // -> Vec2(12, -68)\n"
    " ```\n"
).
-spec map_y(vec2(DUU), fun((DUU) -> DUU)) -> vec2(DUU).
map_y(Vector, Fun) ->
    {vec2, erlang:element(2, Vector), Fun(erlang:element(3, Vector))}.

-file("src/vec/vec2.gleam", 147).
?DOC(
    " Returns a new vector with the x and y elements swaped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> swap()\n"
    " // -> Vec2(-34, 12)\n"
    " ```\n"
).
-spec swap(vec2(DUX)) -> vec2(DUX).
swap(Vector) ->
    {vec2, erlang:element(3, Vector), erlang:element(2, Vector)}.

-file("src/vec/vec2.gleam", 161).
?DOC(
    " Returns a new vector containing only the elements of the first vector after\n"
    " the function has been applied to each one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> map(fn(x) { x * 2 })\n"
    " // -> Vec2(24, -68)\n"
    " ```\n"
).
-spec map(vec2(DVA), fun((DVA) -> DVC)) -> vec2(DVC).
map(Vector, Fun) ->
    {vec2, Fun(erlang:element(2, Vector)), Fun(erlang:element(3, Vector))}.

-file("src/vec/vec2.gleam", 174).
?DOC(
    " Combines two vectors into a single vector using the given function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> map2(Vec2(420, 69), fn(x, y) { x + y })\n"
    " // -> Vec2(432, 35)\n"
    " ```\n"
).
-spec map2(vec2(DVE), vec2(DVG), fun((DVE, DVG) -> DVI)) -> vec2(DVI).
map2(A, B, Fun) ->
    {vec2,
        Fun(erlang:element(2, A), erlang:element(2, B)),
        Fun(erlang:element(3, A), erlang:element(3, B))}.

-file("src/vec/vec2.gleam", 194).
?DOC(
    " Combines a vector of results into a single result. If all elements in the\n"
    " vector are `Ok` then returns an `Ok` holding the vector of values. If any\n"
    " element is `Error` then returns the first error.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(Ok(12), Ok(-34)) |> result()\n"
    " // -> Ok(Vec2(12, -34))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec2(Ok(12), Error(\"foo\")) |> result()\n"
    " // -> Error(\"foo\")\n"
    " ```\n"
).
-spec result(vec2({ok, DVK} | {error, DVL})) -> {ok, vec2(DVK)} | {error, DVL}.
result(Vector) ->
    case Vector of
        {vec2, {ok, X}, {ok, Y}} ->
            {ok, {vec2, X, Y}};

        {vec2, {error, Error}, _} ->
            {error, Error};

        {vec2, _, {error, Error@1}} ->
            {error, Error@1}
    end.
