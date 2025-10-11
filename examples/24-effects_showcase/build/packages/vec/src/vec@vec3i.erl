-module(vec@vec3i).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vec/vec3i.gleam").
-export([clamp/3, min/2, max/2, absolute_value/1, to_vec3f/1, negate/1, remainder/2, modulo/2, divide/2, floor_divide/2, add/2, sum/1, multiply/2, product/1, subtract/2, length_squared/1, length/1, compare_length/2, distance_squared/2, distance/2, compare_distance/3, scale/2, cross/2, dot/2, project/2, slide/2, reflect/2, mirror/2, anchor_position/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/vec/vec3i.gleam", 27).
?DOC(
    " Returns a new vector with all components clamped between a lower and upper\n"
    " bound.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> clamp(\n"
    "   Vec3(10, 21, -54),\n"
    "   Vec3(14, 18, 323),\n"
    " )\n"
    " // -> Vec3(12, 21, 323)\n"
    " ```\n"
).
-spec clamp(
    vec@vec3:vec3(integer()),
    vec@vec3:vec3(integer()),
    vec@vec3:vec3(integer())
) -> vec@vec3:vec3(integer()).
clamp(Vector, Start_bound, Stop_bound) ->
    {vec3,
        gleam@int:clamp(
            erlang:element(2, Vector),
            erlang:element(2, Start_bound),
            erlang:element(2, Stop_bound)
        ),
        gleam@int:clamp(
            erlang:element(3, Vector),
            erlang:element(3, Start_bound),
            erlang:element(3, Stop_bound)
        ),
        gleam@int:clamp(
            erlang:element(4, Vector),
            erlang:element(4, Start_bound),
            erlang:element(4, Stop_bound)
        )}.

-file("src/vec/vec3i.gleam", 48).
?DOC(
    " Compares two vectors, returning the smaller of the two.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " min(Vec3(12, -34, 420), Vec3(10, 21, -54))\n"
    " // -> Vec3(10, -34, -54)\n"
    " ```\n"
).
-spec min(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
min(A, B) ->
    _pipe = A,
    vec@vec3:map2(_pipe, B, fun gleam@int:min/2).

-file("src/vec/vec3i.gleam", 61).
?DOC(
    " Compares two vectors, returning the larger of the two.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " max(Vec3(12, -34, 420), Vec3(14, -93, 323))\n"
    " // -> Vec3(14, -34, 420)\n"
    " ```\n"
).
-spec max(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
max(A, B) ->
    _pipe = A,
    vec@vec3:map2(_pipe, B, fun gleam@int:max/2).

-file("src/vec/vec3i.gleam", 74).
?DOC(
    " Returns a new vector with all elements in absolute values.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> absolute_value()\n"
    " // -> Vec3(12, 34, 420)\n"
    " ```\n"
).
-spec absolute_value(vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
absolute_value(Vector) ->
    _pipe = Vector,
    vec@vec3:map(_pipe, fun gleam@int:absolute_value/1).

-file("src/vec/vec3i.gleam", 87).
?DOC(
    " Takes an int vector and returns its value as a float vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> to_vec3f()\n"
    " // -> Vec3(12.0, -34.0, 420.0)\n"
    " ```\n"
).
-spec to_vec3f(vec@vec3:vec3(integer())) -> vec@vec3:vec3(float()).
to_vec3f(Vector) ->
    _pipe = Vector,
    vec@vec3:map(_pipe, fun erlang:float/1).

-file("src/vec/vec3i.gleam", 100).
?DOC(
    " Returns a new vector with all elements negated.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> negate()\n"
    " // -> Vec3(-12, 34, -420)\n"
    " ```\n"
).
-spec negate(vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
negate(Vector) ->
    _pipe = Vector,
    vec@vec3:map(_pipe, fun gleam@int:negate/1).

-file("src/vec/vec3i.gleam", 155).
?DOC(
    " Computes the remainder of an integer vector division of inputs as a\n"
    " `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(13, -13, 13) |> remainder(Vec3(3, 3, -3))\n"
    " // -> Ok(Vec3(1, -1, 1))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> remainder(Vec3(0, 1, 2))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec remainder(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> {ok,
        vec@vec3:vec3(integer())} |
    {error, nil}.
remainder(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec3:map2(_pipe, Divisor, fun gleam@int:remainder/2),
    vec@vec3:result(_pipe@1).

-file("src/vec/vec3i.gleam", 171).
?DOC(
    " Returns the modulo of the inputs as a `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(13, -13, 13) |> modulo(Vec3(3, 3, -3))\n"
    " // -> Ok(Vec3(1, 2, -2))\n"
    " ```\n"
).
-spec modulo(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> {ok,
        vec@vec3:vec3(integer())} |
    {error, nil}.
modulo(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec3:map2(_pipe, Divisor, fun gleam@int:modulo/2),
    vec@vec3:result(_pipe@1).

-file("src/vec/vec3i.gleam", 192).
?DOC(
    " Returns division of the inputs as a `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> divide(Vec3(2, 5, 4))\n"
    " // -> Ok(Vec3(6, -6, 105))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> divide(Vec3(0, 5, 4))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec divide(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> {ok,
        vec@vec3:vec3(integer())} |
    {error, nil}.
divide(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec3:map2(_pipe, Divisor, fun gleam@int:divide/2),
    vec@vec3:result(_pipe@1).

-file("src/vec/vec3i.gleam", 214).
?DOC(
    " Performs a *floored* integer vector division, which means that the result\n"
    " will always be rounded towards negative infinity.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> floor_divide(Vec3(2, 5, 4))\n"
    " // -> Ok(Vec3(6, -7, 105))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> floor_divide(Vec3(0, 5, 4))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec floor_divide(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> {ok,
        vec@vec3:vec3(integer())} |
    {error, nil}.
floor_divide(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec3:map2(_pipe, Divisor, fun gleam@int:floor_divide/2),
    vec@vec3:result(_pipe@1).

-file("src/vec/vec3i.gleam", 230).
?DOC(
    " Adds two vectors together.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> add(Vec3(21, 45, -20))\n"
    " // -> Vec3(33, 11, 400)\n"
    " ```\n"
).
-spec add(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
add(A, B) ->
    _pipe = A,
    vec@vec3:map2(_pipe, B, fun gleam@int:add/2).

-file("src/vec/vec3i.gleam", 118).
?DOC(
    " Sums a list of vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\n"
    "   Vec3(12, -34, 420),\n"
    "   Vec3(21, 45, -20),\n"
    "   Vec3(33, 0, -200),\n"
    " ]\n"
    " |> sum()\n"
    " // -> Vec3(66, 11, 200)\n"
    " ```\n"
).
-spec sum(list(vec@vec3:vec3(integer()))) -> vec@vec3:vec3(integer()).
sum(Vectors) ->
    _pipe = Vectors,
    gleam@list:fold(_pipe, vec@vec3:splat(0), fun add/2).

-file("src/vec/vec3i.gleam", 243).
?DOC(
    " Multiplies two vectors together.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> multiply(Vec3(2, -3, 0))\n"
    " // -> Vec3(24, 102, 0)\n"
    " ```\n"
).
-spec multiply(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
multiply(A, B) ->
    _pipe = A,
    vec@vec3:map2(_pipe, B, fun gleam@int:multiply/2).

-file("src/vec/vec3i.gleam", 136).
?DOC(
    " Multiplies a list of vectors and returns the product.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\n"
    "   Vec3(12, -34, 420),\n"
    "   Vec3(21, -10, 999),\n"
    "   Vec3(32, 20, 0),\n"
    " ]\n"
    " |> product()\n"
    " // -> Vec3(8064, 6800, 0)\n"
    " ```\n"
).
-spec product(list(vec@vec3:vec3(integer()))) -> vec@vec3:vec3(integer()).
product(Vectors) ->
    _pipe = Vectors,
    gleam@list:fold(_pipe, vec@vec3:splat(1), fun multiply/2).

-file("src/vec/vec3i.gleam", 256).
?DOC(
    " Subtracts one vector from another.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> subtract(Vec3(7, -45, 20))\n"
    " // -> Vec3(5, 11, 400)\n"
    " ```\n"
).
-spec subtract(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
subtract(A, B) ->
    _pipe = A,
    vec@vec3:map2(_pipe, B, fun gleam@int:subtract/2).

-file("src/vec/vec3i.gleam", 269).
?DOC(
    " Returns the squared length (squared magnitude) of the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> length_squared()\n"
    " // -> 177_700\n"
    " ```\n"
).
-spec length_squared(vec@vec3:vec3(integer())) -> integer().
length_squared(Vector) ->
    _pipe = Vector,
    _pipe@1 = vec@vec3:to_list(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun(Element) -> Element * Element end),
    gleam@int:sum(_pipe@2).

-file("src/vec/vec3i.gleam", 285).
?DOC(
    " Returns the length (magnitude) of the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> length()\n"
    " // -> 421.54\n"
    " ```\n"
).
-spec length(vec@vec3:vec3(integer())) -> float().
length(Vector) ->
    Length@1 = case begin
        _pipe = Vector,
        _pipe@1 = length_squared(_pipe),
        gleam@int:square_root(_pipe@1)
    end of
        {ok, Length} -> Length;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vec/vec3i"/utf8>>,
                        function => <<"length"/utf8>>,
                        line => 286,
                        value => _assert_fail,
                        start => 5891,
                        'end' => 5962,
                        pattern_start => 5902,
                        pattern_end => 5912})
    end,
    Length@1.

-file("src/vec/vec3i.gleam", 301).
?DOC(
    " Compares two vector's lengths, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare_length(Vec3(12, -34, 420), Vec3(2, 3, 4))\n"
    " // -> Gt\n"
    " ```\n"
).
-spec compare_length(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> gleam@order:order().
compare_length(A, B) ->
    gleam@int:compare(
        begin
            _pipe = A,
            length_squared(_pipe)
        end,
        begin
            _pipe@1 = B,
            length_squared(_pipe@1)
        end
    ).

-file("src/vec/vec3i.gleam", 314).
?DOC(
    " Returns the squared distance between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> distance_squared(Vec3(2, 3, 4))\n"
    " // -> 174_525\n"
    " ```\n"
).
-spec distance_squared(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> integer().
distance_squared(A, B) ->
    _pipe = A,
    _pipe@1 = vec@vec3:map2(_pipe, B, fun gleam@int:subtract/2),
    length_squared(_pipe@1).

-file("src/vec/vec3i.gleam", 327).
?DOC(
    " Returns the distance between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> distance(Vec3(2, 3, 4))\n"
    " // -> 417.76\n"
    " ```\n"
).
-spec distance(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> float().
distance(A, B) ->
    Distance@1 = case begin
        _pipe = distance_squared(A, B),
        gleam@int:square_root(_pipe)
    end of
        {ok, Distance} -> Distance;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vec/vec3i"/utf8>>,
                        function => <<"distance"/utf8>>,
                        line => 328,
                        value => _assert_fail,
                        start => 6872,
                        'end' => 6941,
                        pattern_start => 6883,
                        pattern_end => 6895})
    end,
    Distance@1.

-file("src/vec/vec3i.gleam", 343).
?DOC(
    " Compares two vector's distances to a vector, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare_distance(Vec3(12, -34, 420), Vec3(2, 3, 4), Vec3(-25, 67, 194))\n"
    " // -> Gt\n"
    " ```\n"
).
-spec compare_distance(
    vec@vec3:vec3(integer()),
    vec@vec3:vec3(integer()),
    vec@vec3:vec3(integer())
) -> gleam@order:order().
compare_distance(A, B, Vector) ->
    gleam@int:compare(
        begin
            _pipe = A,
            distance_squared(_pipe, Vector)
        end,
        begin
            _pipe@1 = B,
            distance_squared(_pipe@1, Vector)
        end
    ).

-file("src/vec/vec3i.gleam", 360).
?DOC(
    " Returns a new vector containing the elements multiplies by `scalar`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> scale(2)\n"
    " // -> Vec3(24, -68, 840)\n"
    " ```\n"
).
-spec scale(vec@vec3:vec3(integer()), integer()) -> vec@vec3:vec3(integer()).
scale(Vector, Scalar) ->
    _pipe = Vector,
    vec@vec3:map(
        _pipe,
        fun(_capture) -> gleam@int:multiply(_capture, Scalar) end
    ).

-file("src/vec/vec3i.gleam", 373).
?DOC(
    " Returns the cross product of two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> cross(Vec3(2, 3, 4))\n"
    " // -> Vec3(-1396, 792, 104)\n"
    " ```\n"
).
-spec cross(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
cross(A, B) ->
    {vec3,
        (erlang:element(3, A) * erlang:element(4, B)) - (erlang:element(4, A) * erlang:element(
            3,
            B
        )),
        (erlang:element(4, A) * erlang:element(2, B)) - (erlang:element(2, A) * erlang:element(
            4,
            B
        )),
        (erlang:element(2, A) * erlang:element(3, B)) - (erlang:element(3, A) * erlang:element(
            2,
            B
        ))}.

-file("src/vec/vec3i.gleam", 386).
?DOC(
    " Returns the dot product of two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> dot(Vec3(2, 3, 4))\n"
    " // -> 1602\n"
    " ```\n"
).
-spec dot(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> integer().
dot(A, B) ->
    _pipe = A,
    _pipe@1 = multiply(_pipe, B),
    _pipe@2 = vec@vec3:to_list(_pipe@1),
    gleam@int:sum(_pipe@2).

-file("src/vec/vec3i.gleam", 399).
?DOC(
    " Returns the projection of a vector on another vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> project(Vec3(2, 3, 4))\n"
    " // -> Vec3(110, 165, 220)\n"
    " ```\n"
).
-spec project(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
project(A, B) ->
    _pipe = B,
    scale(_pipe, case dot(B, B) of
            0 -> 0;
            Gleam@denominator -> dot(A, B) div Gleam@denominator
        end).

-file("src/vec/vec3i.gleam", 413).
?DOC(
    " Returns a new vector resulting from sliding this vector along a plane\n"
    " defined by the given normal vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> slide(Vec3(2, 3, 4))\n"
    " // -> Vec3(-98, -199, 200)\n"
    " ```\n"
).
-spec slide(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
slide(A, B) ->
    _pipe = A,
    subtract(
        _pipe,
        begin
            _pipe@1 = A,
            project(_pipe@1, B)
        end
    ).

-file("src/vec/vec3i.gleam", 427).
?DOC(
    " Returns the reflection of a vector through a plane defined by the given\n"
    " normal vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> reflect(Vec3(2, 3, 4))\n"
    " // -> Vec3(208, 364, 20)\n"
    " ```\n"
).
-spec reflect(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
reflect(Vector, Normal) ->
    _pipe = Vector,
    _pipe@1 = project(_pipe, Normal),
    _pipe@2 = scale(_pipe@1, 2),
    subtract(_pipe@2, Vector).

-file("src/vec/vec3i.gleam", 441).
?DOC(
    " Returns the mirror of a vector through a plane defined by the given normal\n"
    " vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420) |> mirror(Vec3(2, 3, 4))\n"
    " // -> Vec3(-208, -364, -20)\n"
    " ```\n"
).
-spec mirror(vec@vec3:vec3(integer()), vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()).
mirror(Vector, Normal) ->
    _pipe = Vector,
    _pipe@1 = reflect(_pipe, Normal),
    negate(_pipe@1).

-file("src/vec/vec3i.gleam", 455).
?DOC(
    " Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(12, -34, 420)\n"
    " |> anchor_position(Vec3(20, 40, 0), scale(_, 2))\n"
    " // -> Vec3(4, -108, 840)\n"
    " ```\n"
).
-spec anchor_position(
    vec@vec3:vec3(integer()),
    vec@vec3:vec3(integer()),
    fun((vec@vec3:vec3(integer())) -> vec@vec3:vec3(integer()))
) -> vec@vec3:vec3(integer()).
anchor_position(Vector, Position, Fun) ->
    _pipe = Vector,
    _pipe@1 = subtract(_pipe, Position),
    _pipe@2 = Fun(_pipe@1),
    add(_pipe@2, Position).
