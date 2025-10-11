-module(vec@vec3).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vec/vec3.gleam").
-export([splat/1, from_tuple/1, to_tuple/1, to_list/1, x/1, y/1, z/1, replace_x/2, replace_y/2, replace_z/2, map_x/2, map_y/2, map_z/2, swap_xy/1, swap_xz/1, swap_yz/1, map/2, map2/3, result/1]).
-export_type([vec3/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type vec3(EMI) :: {vec3, EMI, EMI, EMI}.

-file("src/vec/vec3.gleam", 17).
?DOC(
    " Creates a vector with all elements set to a value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " splat(12)\n"
    " // -> Vec3(12, 12, 12)\n"
    " ```\n"
).
-spec splat(EMK) -> vec3(EMK).
splat(Value) ->
    {vec3, Value, Value, Value}.

-file("src/vec/vec3.gleam", 30).
?DOC(
    " Converts a tuple of the contained elements into a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " #(12, -34, 420) |> from_tuple()\n"
    " // -> Vec3(12, -34, 420)\n"
    " ```\n"
).
-spec from_tuple({EMM, EMM, EMM}) -> vec3(EMM).
from_tuple(Tuple) ->
    {vec3,
        erlang:element(1, Tuple),
        erlang:element(2, Tuple),
        erlang:element(3, Tuple)}.

-file("src/vec/vec3.gleam", 43).
?DOC(
    " Converts the vector into a tuple of the contained elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> to_tuple()\n"
    " // -> #(12, -34, 420)\n"
    " ```\n"
).
-spec to_tuple(vec3(EMO)) -> {EMO, EMO, EMO}.
to_tuple(Vector) ->
    {erlang:element(2, Vector),
        erlang:element(3, Vector),
        erlang:element(4, Vector)}.

-file("src/vec/vec3.gleam", 56).
?DOC(
    " Converts the vector into a list of the contained elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> to_list()\n"
    " // -> [12, -34, 420]\n"
    " ```\n"
).
-spec to_list(vec3(EMQ)) -> list(EMQ).
to_list(Vector) ->
    [erlang:element(2, Vector),
        erlang:element(3, Vector),
        erlang:element(4, Vector)].

-file("src/vec/vec3.gleam", 69).
?DOC(
    " Returns the x element in a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> x()\n"
    " // -> 12\n"
    " ```\n"
).
-spec x(vec3(EMT)) -> EMT.
x(Vector) ->
    erlang:element(2, Vector).

-file("src/vec/vec3.gleam", 82).
?DOC(
    " Returns the y element in a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> y()\n"
    " // -> -34\n"
    " ```\n"
).
-spec y(vec3(EMV)) -> EMV.
y(Vector) ->
    erlang:element(3, Vector).

-file("src/vec/vec3.gleam", 95).
?DOC(
    " Returns the z element in a vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> z()\n"
    " // -> 420\n"
    " ```\n"
).
-spec z(vec3(EMX)) -> EMX.
z(Vector) ->
    erlang:element(4, Vector).

-file("src/vec/vec3.gleam", 108).
?DOC(
    " Returns a new vector with the x element replace with `value`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> replace_x(777)\n"
    " // -> Vec3(777, -34, 420)\n"
    " ```\n"
).
-spec replace_x(vec3(EMZ), EMZ) -> vec3(EMZ).
replace_x(Vector, Value) ->
    {vec3, Value, erlang:element(3, Vector), erlang:element(4, Vector)}.

-file("src/vec/vec3.gleam", 121).
?DOC(
    " Returns a new vector with the y element replace with `value`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> replace_y(777)\n"
    " // -> Vec3(12, 777, 420)\n"
    " ```\n"
).
-spec replace_y(vec3(ENC), ENC) -> vec3(ENC).
replace_y(Vector, Value) ->
    {vec3, erlang:element(2, Vector), Value, erlang:element(4, Vector)}.

-file("src/vec/vec3.gleam", 134).
?DOC(
    " Returns a new vector with the z element replace with `value`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> replace_z(777)\n"
    " // -> Vec3(12, -34, 777)\n"
    " ```\n"
).
-spec replace_z(vec3(ENF), ENF) -> vec3(ENF).
replace_z(Vector, Value) ->
    {vec3, erlang:element(2, Vector), erlang:element(3, Vector), Value}.

-file("src/vec/vec3.gleam", 147).
?DOC(
    " Returns a new vector with the x element having had `with` applied to it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> map_x(fn(n) { n * 2 })\n"
    " // -> Vec3(24, -34, 420)\n"
    " ```\n"
).
-spec map_x(vec3(ENI), fun((ENI) -> ENI)) -> vec3(ENI).
map_x(Vector, Fun) ->
    {vec3,
        Fun(erlang:element(2, Vector)),
        erlang:element(3, Vector),
        erlang:element(4, Vector)}.

-file("src/vec/vec3.gleam", 160).
?DOC(
    " Returns a new vector with the y element having had `with` applied to it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> map_y(fn(n) { n * 2 })\n"
    " // -> Vec3(12, -68, 420)\n"
    " ```\n"
).
-spec map_y(vec3(ENL), fun((ENL) -> ENL)) -> vec3(ENL).
map_y(Vector, Fun) ->
    {vec3,
        erlang:element(2, Vector),
        Fun(erlang:element(3, Vector)),
        erlang:element(4, Vector)}.

-file("src/vec/vec3.gleam", 173).
?DOC(
    " Returns a new vector with the z element having had `with` applied to it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> map_z(fn(n) { n * 2 })\n"
    " // -> Vec3(12, -34, 840)\n"
    " ```\n"
).
-spec map_z(vec3(ENO), fun((ENO) -> ENO)) -> vec3(ENO).
map_z(Vector, Fun) ->
    {vec3,
        erlang:element(2, Vector),
        erlang:element(3, Vector),
        Fun(erlang:element(4, Vector))}.

-file("src/vec/vec3.gleam", 186).
?DOC(
    " Returns a new vector with the x and y elements swaped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> swap_xy()\n"
    " // -> Vec3(-34, 12, 420)\n"
    " ```\n"
).
-spec swap_xy(vec3(ENR)) -> vec3(ENR).
swap_xy(Vector) ->
    {vec3,
        erlang:element(3, Vector),
        erlang:element(2, Vector),
        erlang:element(4, Vector)}.

-file("src/vec/vec3.gleam", 199).
?DOC(
    " Returns a new vector with the x and z elements swaped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> swap_xz()\n"
    " // -> Vec3(420, -34, 12)\n"
    " ```\n"
).
-spec swap_xz(vec3(ENU)) -> vec3(ENU).
swap_xz(Vector) ->
    {vec3,
        erlang:element(4, Vector),
        erlang:element(3, Vector),
        erlang:element(2, Vector)}.

-file("src/vec/vec3.gleam", 212).
?DOC(
    " Returns a new vector with the y and z elements swaped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> swap_yz()\n"
    " // -> Vec3(12, 420, -34)\n"
    " ```\n"
).
-spec swap_yz(vec3(ENX)) -> vec3(ENX).
swap_yz(Vector) ->
    {vec3,
        erlang:element(2, Vector),
        erlang:element(4, Vector),
        erlang:element(3, Vector)}.

-file("src/vec/vec3.gleam", 226).
?DOC(
    " Returns a new vector containing only the elements of the first vector after\n"
    " the function has been applied to each one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> map(fn(x) { x * 2 })\n"
    " // -> Vec3(24, -68, 840)\n"
    " ```\n"
).
-spec map(vec3(EOA), fun((EOA) -> EOC)) -> vec3(EOC).
map(Vector, Fun) ->
    {vec3,
        Fun(erlang:element(2, Vector)),
        Fun(erlang:element(3, Vector)),
        Fun(erlang:element(4, Vector))}.

-file("src/vec/vec3.gleam", 239).
?DOC(
    " Combines two vectors into a single vector using the given function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> map2(Vec3(1, 2, 3), fn(x, y) { x * y })\n"
    " // -> Vec3(12, -68, 1260)\n"
    " ```\n"
).
-spec map2(vec3(EOE), vec3(EOG), fun((EOE, EOG) -> EOI)) -> vec3(EOI).
map2(A, B, Fun) ->
    {vec3,
        Fun(erlang:element(2, A), erlang:element(2, B)),
        Fun(erlang:element(3, A), erlang:element(3, B)),
        Fun(erlang:element(4, A), erlang:element(4, B))}.

-file("src/vec/vec3.gleam", 259).
?DOC(
    " Combines a vector of results into a single result. If all elements in the\n"
    " vector are `Ok` then returns an `Ok` holding the vector of values. If any\n"
    " element is `Error` then returns the first error.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(Ok(12), Ok(-34), Ok(420)) |> result()\n"
    " // -> Ok(Vec3(12, -34, 420))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec3(Ok(12), Error(\"foo\"), Error(\"bar\")) |> result()\n"
    " // -> Error(\"foo\")\n"
    " ```\n"
).
-spec result(vec3({ok, EOK} | {error, EOL})) -> {ok, vec3(EOK)} | {error, EOL}.
result(Vector) ->
    case Vector of
        {vec3, {ok, X}, {ok, Y}, {ok, Z}} ->
            {ok, {vec3, X, Y, Z}};

        {vec3, {error, Error}, _, _} ->
            {error, Error};

        {vec3, _, {error, Error@1}, _} ->
            {error, Error@1};

        {vec3, _, _, {error, Error@2}} ->
            {error, Error@2}
    end.
