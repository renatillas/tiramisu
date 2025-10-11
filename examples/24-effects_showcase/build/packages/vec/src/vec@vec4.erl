-module(vec@vec4).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vec/vec4.gleam").
-export([splat/1, from_tuple/1, to_tuple/1, to_list/1, x/1, y/1, z/1, w/1, replace_x/2, replace_y/2, replace_z/2, replace_w/2, map_x/2, map_y/2, map_z/2, map_w/2, swap_xy/1, swap_xz/1, swap_xw/1, swap_yz/1, swap_yw/1, swap_zw/1, map/2, map2/3, result/1]).
-export_type([vec4/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type vec4(FIA) :: {vec4, FIA, FIA, FIA, FIA}.

-file("src/vec/vec4.gleam", 17).
?DOC(
    " Creates a vector with all elements set to a value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " splat(12)\n"
    " // -> Vec4(12, 12, 12, 12)\n"
    " ```\n"
).
-spec splat(FIC) -> vec4(FIC).
splat(Value) ->
    {vec4, Value, Value, Value, Value}.

-file("src/vec/vec4.gleam", 30).
?DOC(
    " Converts a tuple of the contained elements into a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " #(12, -34, 420, 69) |> from_tuple()\n"
    " // -> Vec4(12, -34, 420, 69)\n"
    " ```\n"
).
-spec from_tuple({FIE, FIE, FIE, FIE}) -> vec4(FIE).
from_tuple(Tuple) ->
    {vec4,
        erlang:element(1, Tuple),
        erlang:element(2, Tuple),
        erlang:element(3, Tuple),
        erlang:element(4, Tuple)}.

-file("src/vec/vec4.gleam", 43).
?DOC(
    " Converts the vector into a tuple of the contained elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> to_tuple()\n"
    " // -> #(12, -34, 420, 69)\n"
    " ```\n"
).
-spec to_tuple(vec4(FIG)) -> {FIG, FIG, FIG, FIG}.
to_tuple(Vector) ->
    {erlang:element(2, Vector),
        erlang:element(3, Vector),
        erlang:element(4, Vector),
        erlang:element(5, Vector)}.

-file("src/vec/vec4.gleam", 56).
?DOC(
    " Converts the vector into a list of the contained elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> to_list()\n"
    " // -> [12, -34, 420, 69]\n"
    " ```\n"
).
-spec to_list(vec4(FII)) -> list(FII).
to_list(Vector) ->
    [erlang:element(2, Vector),
        erlang:element(3, Vector),
        erlang:element(4, Vector),
        erlang:element(5, Vector)].

-file("src/vec/vec4.gleam", 69).
?DOC(
    " Returns the x element in a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> x()\n"
    " // -> 12\n"
    " ```\n"
).
-spec x(vec4(FIL)) -> FIL.
x(Vector) ->
    erlang:element(2, Vector).

-file("src/vec/vec4.gleam", 82).
?DOC(
    " Returns the y element in a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> y()\n"
    " // -> -34\n"
    " ```\n"
).
-spec y(vec4(FIN)) -> FIN.
y(Vector) ->
    erlang:element(3, Vector).

-file("src/vec/vec4.gleam", 95).
?DOC(
    " Returns the z element in a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> z()\n"
    " // -> 420\n"
    " ```\n"
).
-spec z(vec4(FIP)) -> FIP.
z(Vector) ->
    erlang:element(4, Vector).

-file("src/vec/vec4.gleam", 108).
?DOC(
    " Returns the w element in a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> w()\n"
    " // -> 69\n"
    " ```\n"
).
-spec w(vec4(FIR)) -> FIR.
w(Vector) ->
    erlang:element(5, Vector).

-file("src/vec/vec4.gleam", 121).
?DOC(
    " Returns a new vector with the x element replace with `value`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> replace_x(777)\n"
    " // -> Vec4(777, -34, 420, 69)\n"
    " ```\n"
).
-spec replace_x(vec4(FIT), FIT) -> vec4(FIT).
replace_x(Vector, Value) ->
    {vec4,
        Value,
        erlang:element(3, Vector),
        erlang:element(4, Vector),
        erlang:element(5, Vector)}.

-file("src/vec/vec4.gleam", 134).
?DOC(
    " Returns a new vector with the y element replace with `value`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> replace_y(777)\n"
    " // -> Vec4(12, 777, 420, 69)\n"
    " ```\n"
).
-spec replace_y(vec4(FIW), FIW) -> vec4(FIW).
replace_y(Vector, Value) ->
    {vec4,
        erlang:element(2, Vector),
        Value,
        erlang:element(4, Vector),
        erlang:element(5, Vector)}.

-file("src/vec/vec4.gleam", 147).
?DOC(
    " Returns a new vector with the z element replace with `value`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> replace_z(777)\n"
    " // -> Vec4(12, -34, 777, 69)\n"
    " ```\n"
).
-spec replace_z(vec4(FIZ), FIZ) -> vec4(FIZ).
replace_z(Vector, Value) ->
    {vec4,
        erlang:element(2, Vector),
        erlang:element(3, Vector),
        Value,
        erlang:element(5, Vector)}.

-file("src/vec/vec4.gleam", 160).
?DOC(
    " Returns a new vector with the w element replace with `value`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> replace_w(777)\n"
    " // -> Vec4(12, -34, 420, 777)\n"
    " ```\n"
).
-spec replace_w(vec4(FJC), FJC) -> vec4(FJC).
replace_w(Vector, Value) ->
    {vec4,
        erlang:element(2, Vector),
        erlang:element(3, Vector),
        erlang:element(4, Vector),
        Value}.

-file("src/vec/vec4.gleam", 173).
?DOC(
    " Returns a new vector with the x element having had `with` applied to it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> map_x(fn(n) { n * 2 })\n"
    " // -> Vec4(24, -34, 420, 69)\n"
    " ```\n"
).
-spec map_x(vec4(FJF), fun((FJF) -> FJF)) -> vec4(FJF).
map_x(Vector, Fun) ->
    {vec4,
        Fun(erlang:element(2, Vector)),
        erlang:element(3, Vector),
        erlang:element(4, Vector),
        erlang:element(5, Vector)}.

-file("src/vec/vec4.gleam", 186).
?DOC(
    " Returns a new vector with the y element having had `with` applied to it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> map_y(fn(n) { n * 2 })\n"
    " // -> Vec4(12, -68, 420, 69)\n"
    " ```\n"
).
-spec map_y(vec4(FJI), fun((FJI) -> FJI)) -> vec4(FJI).
map_y(Vector, Fun) ->
    {vec4,
        erlang:element(2, Vector),
        Fun(erlang:element(3, Vector)),
        erlang:element(4, Vector),
        erlang:element(5, Vector)}.

-file("src/vec/vec4.gleam", 199).
?DOC(
    " Returns a new vector with the z element having had `with` applied to it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> map_z(fn(n) { n * 2 })\n"
    " // -> Vec4(12, -34, 840, 69)\n"
    " ```\n"
).
-spec map_z(vec4(FJL), fun((FJL) -> FJL)) -> vec4(FJL).
map_z(Vector, Fun) ->
    {vec4,
        erlang:element(2, Vector),
        erlang:element(3, Vector),
        Fun(erlang:element(4, Vector)),
        erlang:element(5, Vector)}.

-file("src/vec/vec4.gleam", 212).
?DOC(
    " Returns a new vector with the w element having had `with` applied to it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> map_w(fn(n) { n * 2 })\n"
    " // -> Vec4(12, -34, 420, 138)\n"
    " ```\n"
).
-spec map_w(vec4(FJO), fun((FJO) -> FJO)) -> vec4(FJO).
map_w(Vector, Fun) ->
    {vec4,
        erlang:element(2, Vector),
        erlang:element(3, Vector),
        erlang:element(4, Vector),
        Fun(erlang:element(5, Vector))}.

-file("src/vec/vec4.gleam", 225).
?DOC(
    " Returns a new vector with the x and y elements swaped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> swap_xy()\n"
    " // -> Vec4(-34, 12, 420, 69)\n"
    " ```\n"
).
-spec swap_xy(vec4(FJR)) -> vec4(FJR).
swap_xy(Vector) ->
    {vec4,
        erlang:element(3, Vector),
        erlang:element(2, Vector),
        erlang:element(4, Vector),
        erlang:element(5, Vector)}.

-file("src/vec/vec4.gleam", 238).
?DOC(
    " Returns a new vector with the x and z elements swaped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> swap_xz()\n"
    " // -> Vec4(420, -34, 12, 69)\n"
    " ```\n"
).
-spec swap_xz(vec4(FJU)) -> vec4(FJU).
swap_xz(Vector) ->
    {vec4,
        erlang:element(4, Vector),
        erlang:element(3, Vector),
        erlang:element(2, Vector),
        erlang:element(5, Vector)}.

-file("src/vec/vec4.gleam", 251).
?DOC(
    " Returns a new vector with the x and w elements swaped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> swap_xw()\n"
    " // -> Vec4(69, -34, 420, 12)\n"
    " ```\n"
).
-spec swap_xw(vec4(FJX)) -> vec4(FJX).
swap_xw(Vector) ->
    {vec4,
        erlang:element(5, Vector),
        erlang:element(3, Vector),
        erlang:element(4, Vector),
        erlang:element(2, Vector)}.

-file("src/vec/vec4.gleam", 264).
?DOC(
    " Returns a new vector with the y and z elements swaped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> swap_yz()\n"
    " // -> Vec4(12, 420, -34, 69)\n"
    " ```\n"
).
-spec swap_yz(vec4(FKA)) -> vec4(FKA).
swap_yz(Vector) ->
    {vec4,
        erlang:element(2, Vector),
        erlang:element(4, Vector),
        erlang:element(3, Vector),
        erlang:element(5, Vector)}.

-file("src/vec/vec4.gleam", 277).
?DOC(
    " Returns a new vector with the y and w elements swaped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> swap_yw()\n"
    " // -> Vec4(12, 69, 420, -34)\n"
    " ```\n"
).
-spec swap_yw(vec4(FKD)) -> vec4(FKD).
swap_yw(Vector) ->
    {vec4,
        erlang:element(2, Vector),
        erlang:element(5, Vector),
        erlang:element(4, Vector),
        erlang:element(3, Vector)}.

-file("src/vec/vec4.gleam", 290).
?DOC(
    " Returns a new vector with the z and w elements swaped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> swap_zw()\n"
    " // -> Vec4(12, -34, 69, 420)\n"
    " ```\n"
).
-spec swap_zw(vec4(FKG)) -> vec4(FKG).
swap_zw(Vector) ->
    {vec4,
        erlang:element(2, Vector),
        erlang:element(3, Vector),
        erlang:element(5, Vector),
        erlang:element(4, Vector)}.

-file("src/vec/vec4.gleam", 304).
?DOC(
    " Returns a new vector containing only the elements of the first vector after\n"
    " the function has been applied to each one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> map(fn(x) { x * 2 })\n"
    " // -> Vec4(24, -68, 840, 138)\n"
    " ```\n"
).
-spec map(vec4(FKJ), fun((FKJ) -> FKL)) -> vec4(FKL).
map(Vector, Fun) ->
    {vec4,
        Fun(erlang:element(2, Vector)),
        Fun(erlang:element(3, Vector)),
        Fun(erlang:element(4, Vector)),
        Fun(erlang:element(5, Vector))}.

-file("src/vec/vec4.gleam", 317).
?DOC(
    " Combines two vectors into a single vector using the given function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> map2(Vec4(1, 2, 3, 4), fn(x, y) { x * y })\n"
    " // -> Vec4(12, -68, 1260, 276)\n"
    " ```\n"
).
-spec map2(vec4(FKN), vec4(FKP), fun((FKN, FKP) -> FKR)) -> vec4(FKR).
map2(A, B, Fun) ->
    {vec4,
        Fun(erlang:element(2, A), erlang:element(2, B)),
        Fun(erlang:element(3, A), erlang:element(3, B)),
        Fun(erlang:element(4, A), erlang:element(4, B)),
        Fun(erlang:element(5, A), erlang:element(5, B))}.

-file("src/vec/vec4.gleam", 337).
?DOC(
    " Combines a vector of results into a single result. If all elements in the\n"
    " vector are `Ok` then returns an `Ok` holding the vector of values. If any\n"
    " element is `Error` then returns the first error.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(Ok(12), Ok(-34), Ok(420), Ok(69)) |> result()\n"
    " // -> Ok(Vec4(12, -34, 420, 69))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec4(Ok(12), Error(\"foo\"), Ok(420), Error(\"bar\")) |> result()\n"
    " // -> Error(\"foo\")\n"
    " ```\n"
).
-spec result(vec4({ok, FKT} | {error, FKU})) -> {ok, vec4(FKT)} | {error, FKU}.
result(Vector) ->
    case Vector of
        {vec4, {ok, X}, {ok, Y}, {ok, Z}, {ok, W}} ->
            {ok, {vec4, X, Y, Z, W}};

        {vec4, {error, Error}, _, _, _} ->
            {error, Error};

        {vec4, _, {error, Error@1}, _, _} ->
            {error, Error@1};

        {vec4, _, _, {error, Error@2}, _} ->
            {error, Error@2};

        {vec4, _, _, _, {error, Error@3}} ->
            {error, Error@3}
    end.
