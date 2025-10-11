-module(vec@vec4i).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vec/vec4i.gleam").
-export([clamp/3, min/2, max/2, absolute_value/1, to_vec4f/1, negate/1, remainder/2, modulo/2, divide/2, floor_divide/2, add/2, sum/1, multiply/2, product/1, subtract/2, length_squared/1, length/1, compare_length/2, distance_squared/2, distance/2, compare_distance/3, scale/2, dot/2, project/2, slide/2, reflect/2, mirror/2, anchor_position/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/vec/vec4i.gleam", 27).
?DOC(
    " Returns a new vector with all components clamped between a lower and upper\n"
    " bound.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> clamp(\n"
    "   Vec4(10, 21, -54, 75),\n"
    "   Vec4(14, 18, 323, 91),\n"
    " )\n"
    " // -> Vec4(12, 21, 323, 75)\n"
    " ```\n"
).
-spec clamp(
    vec@vec4:vec4(integer()),
    vec@vec4:vec4(integer()),
    vec@vec4:vec4(integer())
) -> vec@vec4:vec4(integer()).
clamp(Vector, Start_bound, Stop_bound) ->
    {vec4,
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
        ),
        gleam@int:clamp(
            erlang:element(5, Vector),
            erlang:element(5, Start_bound),
            erlang:element(5, Stop_bound)
        )}.

-file("src/vec/vec4i.gleam", 49).
?DOC(
    " Compares two vectors, returning the smaller of the two.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " min(Vec4(12, -34, 420, 69), Vec4(10, 21, -54, 75))\n"
    " // -> Vec4(10, -34, -54, 69)\n"
    " ```\n"
).
-spec min(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
min(A, B) ->
    _pipe = A,
    vec@vec4:map2(_pipe, B, fun gleam@int:min/2).

-file("src/vec/vec4i.gleam", 62).
?DOC(
    " Compares two vectors, returning the larger of the two.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " max(Vec4(12, -34, 420, 69), Vec4(14, -93, 323, 91))\n"
    " // -> Vec4(14, -34, 420, 91)\n"
    " ```\n"
).
-spec max(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
max(A, B) ->
    _pipe = A,
    vec@vec4:map2(_pipe, B, fun gleam@int:max/2).

-file("src/vec/vec4i.gleam", 75).
?DOC(
    " Returns a new vector with all elements in absolute values.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> absolute_value()\n"
    " // -> Vec4(12, 34, 420, 69)\n"
    " ```\n"
).
-spec absolute_value(vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
absolute_value(Vector) ->
    _pipe = Vector,
    vec@vec4:map(_pipe, fun gleam@int:absolute_value/1).

-file("src/vec/vec4i.gleam", 88).
?DOC(
    " Takes an int vector and returns its value as a int vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> to_vec4f()\n"
    " // -> Vec4(12.0, -34.0, 420.0, 69.0)\n"
    " ```\n"
).
-spec to_vec4f(vec@vec4:vec4(integer())) -> vec@vec4:vec4(float()).
to_vec4f(Vector) ->
    _pipe = Vector,
    vec@vec4:map(_pipe, fun erlang:float/1).

-file("src/vec/vec4i.gleam", 101).
?DOC(
    " Returns a new vector with all elements negated.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> negate()\n"
    " // -> Vec4(-12, 34, -420, -69)\n"
    " ```\n"
).
-spec negate(vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
negate(Vector) ->
    _pipe = Vector,
    vec@vec4:map(_pipe, fun gleam@int:negate/1).

-file("src/vec/vec4i.gleam", 156).
?DOC(
    " Computes the remainder of an integer vector division of inputs as a\n"
    " `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(13, -13, 13, -13) |> remainder(Vec4(3, 3, -3, -3))\n"
    " // -> Ok(Vec4(1, -1, 1, -1))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> remainder(Vec4(0, 1, 2, 3))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec remainder(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> {ok,
        vec@vec4:vec4(integer())} |
    {error, nil}.
remainder(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec4:map2(_pipe, Divisor, fun gleam@int:remainder/2),
    vec@vec4:result(_pipe@1).

-file("src/vec/vec4i.gleam", 172).
?DOC(
    " Returns the modulo of the inputs as a `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(13, -13, 13, -13) |> modulo(Vec4(3, 3, -3, -3))\n"
    " // -> Ok(Vec4(1, 2, -2, -1))\n"
    " ```\n"
).
-spec modulo(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> {ok,
        vec@vec4:vec4(integer())} |
    {error, nil}.
modulo(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec4:map2(_pipe, Divisor, fun gleam@int:modulo/2),
    vec@vec4:result(_pipe@1).

-file("src/vec/vec4i.gleam", 193).
?DOC(
    " Returns division of the inputs as a `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> divide(Vec4(2, 5, 4, 1))\n"
    " // -> Ok(Vec4(6, -6, 105, 69))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> divide(Vec4(0, 5, 4, 1))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec divide(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> {ok,
        vec@vec4:vec4(integer())} |
    {error, nil}.
divide(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec4:map2(_pipe, Divisor, fun gleam@int:divide/2),
    vec@vec4:result(_pipe@1).

-file("src/vec/vec4i.gleam", 215).
?DOC(
    " Performs a *floored* integer vector division, which means that the result\n"
    " will always be rounded towards negative infinity.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> floor_divide(Vec4(2, 5, 4, 1))\n"
    " // -> Ok(Vec4(6, -7, 105, 69))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> floor_divide(Vec4(0, 5, 4, 1))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec floor_divide(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> {ok,
        vec@vec4:vec4(integer())} |
    {error, nil}.
floor_divide(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec4:map2(_pipe, Divisor, fun gleam@int:floor_divide/2),
    vec@vec4:result(_pipe@1).

-file("src/vec/vec4i.gleam", 231).
?DOC(
    " Adds two vectors together.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> add(Vec4(21, 45, -20, -9))\n"
    " // -> Vec4(33, 11, 400, 60)\n"
    " ```\n"
).
-spec add(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
add(A, B) ->
    _pipe = A,
    vec@vec4:map2(_pipe, B, fun gleam@int:add/2).

-file("src/vec/vec4i.gleam", 119).
?DOC(
    " Sums a list of vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\n"
    "   Vec4(12, -34, 420, 69),\n"
    "   Vec4(21, 45, -20, 9),\n"
    "   Vec4(33, 0, -200, 3),\n"
    " ]\n"
    " |> sum()\n"
    " // -> Vec4(66, 11, 200, 81)\n"
    " ```\n"
).
-spec sum(list(vec@vec4:vec4(integer()))) -> vec@vec4:vec4(integer()).
sum(Vectors) ->
    _pipe = Vectors,
    gleam@list:fold(_pipe, vec@vec4:splat(0), fun add/2).

-file("src/vec/vec4i.gleam", 244).
?DOC(
    " Multiplies two vectors together.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> multiply(Vec4(2, -3, 0, 1))\n"
    " // -> Vec4(24, 102, 0, 69)\n"
    " ```\n"
).
-spec multiply(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
multiply(A, B) ->
    _pipe = A,
    vec@vec4:map2(_pipe, B, fun gleam@int:multiply/2).

-file("src/vec/vec4i.gleam", 137).
?DOC(
    " Multiplies a list of vectors and returns the product.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\n"
    "   Vec4(12, -34, 420, 69),\n"
    "   Vec4(21, -10, 999, 20),\n"
    "   Vec4(32, 20, 0, 5),\n"
    " ]\n"
    " |> product()\n"
    " // -> Vec4(8064, 6800, 0, 6900)\n"
    " ```\n"
).
-spec product(list(vec@vec4:vec4(integer()))) -> vec@vec4:vec4(integer()).
product(Vectors) ->
    _pipe = Vectors,
    gleam@list:fold(_pipe, vec@vec4:splat(1), fun multiply/2).

-file("src/vec/vec4i.gleam", 257).
?DOC(
    " Subtracts one vector from another.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> subtract(Vec4(7, -45, 20, 32))\n"
    " // -> Vec4(5, 11, 400, 37)\n"
    " ```\n"
).
-spec subtract(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
subtract(A, B) ->
    _pipe = A,
    vec@vec4:map2(_pipe, B, fun gleam@int:subtract/2).

-file("src/vec/vec4i.gleam", 270).
?DOC(
    " Returns the squared length (squared magnitude) of the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> length_squared()\n"
    " // -> 182_461\n"
    " ```\n"
).
-spec length_squared(vec@vec4:vec4(integer())) -> integer().
length_squared(Vector) ->
    _pipe = Vector,
    _pipe@1 = vec@vec4:to_list(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun(Element) -> Element * Element end),
    gleam@int:sum(_pipe@2).

-file("src/vec/vec4i.gleam", 286).
?DOC(
    " Returns the length (magnitude) of the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> length()\n"
    " // -> 427.15\n"
    " ```\n"
).
-spec length(vec@vec4:vec4(integer())) -> float().
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
                        module => <<"vec/vec4i"/utf8>>,
                        function => <<"length"/utf8>>,
                        line => 287,
                        value => _assert_fail,
                        start => 6159,
                        'end' => 6230,
                        pattern_start => 6170,
                        pattern_end => 6180})
    end,
    Length@1.

-file("src/vec/vec4i.gleam", 302).
?DOC(
    " Compares two vector's lengths, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare_length(Vec4(12, -34, 420, 69), Vec4(2, 3, 4, 5))\n"
    " // -> Gt\n"
    " ```\n"
).
-spec compare_length(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> gleam@order:order().
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

-file("src/vec/vec4i.gleam", 315).
?DOC(
    " Returns the squared distance between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> distance_squared(Vec4(2, 3, 4, 5))\n"
    " // -> 178_621\n"
    " ```\n"
).
-spec distance_squared(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> integer().
distance_squared(A, B) ->
    _pipe = A,
    _pipe@1 = vec@vec4:map2(_pipe, B, fun gleam@int:subtract/2),
    length_squared(_pipe@1).

-file("src/vec/vec4i.gleam", 328).
?DOC(
    " Returns the distance between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> distance(Vec4(2, 3, 4, 5))\n"
    " // -> 422.64\n"
    " ```\n"
).
-spec distance(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> float().
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
                        module => <<"vec/vec4i"/utf8>>,
                        function => <<"distance"/utf8>>,
                        line => 329,
                        value => _assert_fail,
                        start => 7161,
                        'end' => 7230,
                        pattern_start => 7172,
                        pattern_end => 7184})
    end,
    Distance@1.

-file("src/vec/vec4i.gleam", 348).
?DOC(
    " Compares two vector's distances to a vector, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare_distance(\n"
    "   Vec4(12, -34, 420, 69),\n"
    "   Vec4(2, 3, 4, 5),\n"
    "   Vec4(-25, 67, 194, 0),\n"
    " )\n"
    " // -> Gt\n"
    " ```\n"
).
-spec compare_distance(
    vec@vec4:vec4(integer()),
    vec@vec4:vec4(integer()),
    vec@vec4:vec4(integer())
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

-file("src/vec/vec4i.gleam", 365).
?DOC(
    " Returns a new vector containing the elements multiplies by `scalar`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> scale(2)\n"
    " // -> Vec4(24, -68, 840, 138)\n"
    " ```\n"
).
-spec scale(vec@vec4:vec4(integer()), integer()) -> vec@vec4:vec4(integer()).
scale(Vector, Scalar) ->
    _pipe = Vector,
    vec@vec4:map(
        _pipe,
        fun(_capture) -> gleam@int:multiply(_capture, Scalar) end
    ).

-file("src/vec/vec4i.gleam", 378).
?DOC(
    " Returns the dot product of two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> dot(Vec4(2, 3, 4, 5))\n"
    " // -> 1947\n"
    " ```\n"
).
-spec dot(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> integer().
dot(A, B) ->
    _pipe = A,
    _pipe@1 = multiply(_pipe, B),
    _pipe@2 = vec@vec4:to_list(_pipe@1),
    gleam@int:sum(_pipe@2).

-file("src/vec/vec4i.gleam", 391).
?DOC(
    " Returns the projection of a vector on another vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> project(Vec4(2, 3, 4, 5))\n"
    " // -> Vec4(72, 108, 144, 180)\n"
    " ```\n"
).
-spec project(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
project(A, B) ->
    _pipe = B,
    scale(_pipe, case dot(B, B) of
            0 -> 0;
            Gleam@denominator -> dot(A, B) div Gleam@denominator
        end).

-file("src/vec/vec4i.gleam", 405).
?DOC(
    " Returns a new vector resulting from sliding this vector along a plane\n"
    " defined by the given normal vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> slide(Vec4(2, 3, 4, 5))\n"
    " // -> Vec4(-60, -142, 276, -111)\n"
    " ```\n"
).
-spec slide(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
slide(A, B) ->
    _pipe = A,
    subtract(
        _pipe,
        begin
            _pipe@1 = A,
            project(_pipe@1, B)
        end
    ).

-file("src/vec/vec4i.gleam", 419).
?DOC(
    " Returns the reflection of a vector through a plane defined by the given\n"
    " normal vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> reflect(Vec4(2, 3, 4, 5))\n"
    " // -> Vec4(132, 250, -132, 291)\n"
    " ```\n"
).
-spec reflect(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
reflect(Vector, Normal) ->
    _pipe = Vector,
    _pipe@1 = project(_pipe, Normal),
    _pipe@2 = scale(_pipe@1, 2),
    subtract(_pipe@2, Vector).

-file("src/vec/vec4i.gleam", 433).
?DOC(
    " Returns the mirror of a vector through a plane defined by the given normal\n"
    " vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69) |> mirror(Vec4(2, 3, 4, 5))\n"
    " // -> Vec4(-132, -250, 132, -291)\n"
    " ```\n"
).
-spec mirror(vec@vec4:vec4(integer()), vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()).
mirror(Vector, Normal) ->
    _pipe = Vector,
    _pipe@1 = reflect(_pipe, Normal),
    negate(_pipe@1).

-file("src/vec/vec4i.gleam", 447).
?DOC(
    " Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(12, -34, 420, 69)\n"
    " |> anchor_position(Vec4(2, 3, 4, 5), scale(_, 2))\n"
    " // -> Vec4(22, -71, 836, 133)\n"
    " ```\n"
).
-spec anchor_position(
    vec@vec4:vec4(integer()),
    vec@vec4:vec4(integer()),
    fun((vec@vec4:vec4(integer())) -> vec@vec4:vec4(integer()))
) -> vec@vec4:vec4(integer()).
anchor_position(Vector, Position, Fun) ->
    _pipe = Vector,
    _pipe@1 = subtract(_pipe, Position),
    _pipe@2 = Fun(_pipe@1),
    add(_pipe@2, Position).
