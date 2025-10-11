-module(vec@vec2i).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vec/vec2i.gleam").
-export([clamp/3, min/2, max/2, absolute_value/1, to_vec2f/1, negate/1, remainder/2, modulo/2, divide/2, floor_divide/2, add/2, sum/1, multiply/2, product/1, subtract/2, length_squared/1, length/1, compare_length/2, distance_squared/2, distance/2, compare_distance/3, scale/2, cross/2, dot/2, project/2, slide/2, reflect/2, mirror/2, rotate/2, anchor_position/3, anchor_rotation/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/vec/vec2i.gleam", 24).
?DOC(
    " Returns a new vector with all components clamped between a lower and upper\n"
    " bound.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> clamp(Vec2(10, 21), Vec2(14, 18))\n"
    " // -> Vec2(12, 21)\n"
    " ```\n"
).
-spec clamp(
    vec@vec2:vec2(integer()),
    vec@vec2:vec2(integer()),
    vec@vec2:vec2(integer())
) -> vec@vec2:vec2(integer()).
clamp(Vector, Start_bound, Stop_bound) ->
    {vec2,
        gleam@int:clamp(
            erlang:element(2, Vector),
            erlang:element(2, Start_bound),
            erlang:element(2, Stop_bound)
        ),
        gleam@int:clamp(
            erlang:element(3, Vector),
            erlang:element(3, Start_bound),
            erlang:element(3, Stop_bound)
        )}.

-file("src/vec/vec2i.gleam", 44).
?DOC(
    " Compares two vectors, returning the smaller of the two.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " min(Vec2(12, -34), Vec2(10, 21))\n"
    " // -> Vec2(10, -34)\n"
    " ```\n"
).
-spec min(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
min(A, B) ->
    _pipe = A,
    vec@vec2:map2(_pipe, B, fun gleam@int:min/2).

-file("src/vec/vec2i.gleam", 57).
?DOC(
    " Compares two vectors, returning the larger of the two.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " max(Vec2(12, -34), Vec2(14, -93))\n"
    " // -> Vec2(14, -34)\n"
    " ```\n"
).
-spec max(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
max(A, B) ->
    _pipe = A,
    vec@vec2:map2(_pipe, B, fun gleam@int:max/2).

-file("src/vec/vec2i.gleam", 70).
?DOC(
    " Returns a new vector with all elements in absolute values.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> absolute_value()\n"
    " // -> Vec2(12, 34)\n"
    " ```\n"
).
-spec absolute_value(vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
absolute_value(Vector) ->
    _pipe = Vector,
    vec@vec2:map(_pipe, fun gleam@int:absolute_value/1).

-file("src/vec/vec2i.gleam", 83).
?DOC(
    " Takes an int vector and returns its value as a float vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> to_vec2f()\n"
    " // -> Vec2(12.0, -34.0)\n"
    " ```\n"
).
-spec to_vec2f(vec@vec2:vec2(integer())) -> vec@vec2:vec2(float()).
to_vec2f(Vector) ->
    _pipe = Vector,
    vec@vec2:map(_pipe, fun erlang:float/1).

-file("src/vec/vec2i.gleam", 96).
?DOC(
    " Returns a new vector with all elements negated.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> negate()\n"
    " // -> Vec2(-12, 34)\n"
    " ```\n"
).
-spec negate(vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
negate(Vector) ->
    _pipe = Vector,
    vec@vec2:map(_pipe, fun gleam@int:negate/1).

-file("src/vec/vec2i.gleam", 151).
?DOC(
    " Computes the remainder of an integer vector division of inputs as a\n"
    " `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(13, -13) |> remainder(Vec2(3, 3))\n"
    " // -> Ok(Vec2(1, -1))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> remainder(Vec2(0, 1))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec remainder(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> {ok,
        vec@vec2:vec2(integer())} |
    {error, nil}.
remainder(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec2:map2(_pipe, Divisor, fun gleam@int:remainder/2),
    vec@vec2:result(_pipe@1).

-file("src/vec/vec2i.gleam", 172).
?DOC(
    " Returns the modulo of the inputs as a `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(13, 13) |> modulo(Vec2(3, -3))\n"
    " // -> Ok(Vec2(1, -2))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec2(-13, -13) |> modulo(Vec2(3, -3))\n"
    " // -> Ok(Vec2(2, -1))\n"
    " ```\n"
).
-spec modulo(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> {ok,
        vec@vec2:vec2(integer())} |
    {error, nil}.
modulo(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec2:map2(_pipe, Divisor, fun gleam@int:modulo/2),
    vec@vec2:result(_pipe@1).

-file("src/vec/vec2i.gleam", 193).
?DOC(
    " Returns division of the inputs as a `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> divide(Vec2(2, 5))\n"
    " // -> Ok(Vec2(6, -6))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> divide(Vec2(0, 5))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec divide(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> {ok,
        vec@vec2:vec2(integer())} |
    {error, nil}.
divide(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec2:map2(_pipe, Divisor, fun gleam@int:divide/2),
    vec@vec2:result(_pipe@1).

-file("src/vec/vec2i.gleam", 215).
?DOC(
    " Performs a *floored* integer vector division, which means that the result\n"
    " will always be rounded towards negative infinity.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> floor_divide(Vec2(2, 5))\n"
    " // -> Ok(Vec2(6, -7))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> floor_divide(Vec2(0, 5))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec floor_divide(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> {ok,
        vec@vec2:vec2(integer())} |
    {error, nil}.
floor_divide(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec2:map2(_pipe, Divisor, fun gleam@int:floor_divide/2),
    vec@vec2:result(_pipe@1).

-file("src/vec/vec2i.gleam", 231).
?DOC(
    " Adds two vectors together.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> add(Vec2(21, 45))\n"
    " // -> Vec2(33, 11)\n"
    " ```\n"
).
-spec add(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
add(A, B) ->
    _pipe = A,
    vec@vec2:map2(_pipe, B, fun gleam@int:add/2).

-file("src/vec/vec2i.gleam", 114).
?DOC(
    " Sums a list of vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\n"
    "   Vec2(12, -34),\n"
    "   Vec2(21, 45),\n"
    "   Vec2(33, 0),\n"
    " ]\n"
    " |> sum()\n"
    " // -> Vec2(66, 11)\n"
    " ```\n"
).
-spec sum(list(vec@vec2:vec2(integer()))) -> vec@vec2:vec2(integer()).
sum(Vectors) ->
    _pipe = Vectors,
    gleam@list:fold(_pipe, vec@vec2:splat(0), fun add/2).

-file("src/vec/vec2i.gleam", 244).
?DOC(
    " Multiplies two vectors together.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> multiply(Vec2(2, -3))\n"
    " // -> Vec2(24, 102)\n"
    " ```\n"
).
-spec multiply(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
multiply(A, B) ->
    _pipe = A,
    vec@vec2:map2(_pipe, B, fun gleam@int:multiply/2).

-file("src/vec/vec2i.gleam", 132).
?DOC(
    " Multiplies a list of vectors and returns the product.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\n"
    "   Vec2(12, -34),\n"
    "   Vec2(21, -10),\n"
    "   Vec2(32, 20),\n"
    " ]\n"
    " |> product()\n"
    " // -> Vec2(8064, 6800)\n"
    " ```\n"
).
-spec product(list(vec@vec2:vec2(integer()))) -> vec@vec2:vec2(integer()).
product(Vectors) ->
    _pipe = Vectors,
    gleam@list:fold(_pipe, vec@vec2:splat(1), fun multiply/2).

-file("src/vec/vec2i.gleam", 257).
?DOC(
    " Subtracts one vector from another.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> subtract(Vec2(7, -45))\n"
    " // -> Vec2(5, 11)\n"
    " ```\n"
).
-spec subtract(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
subtract(A, B) ->
    _pipe = A,
    vec@vec2:map2(_pipe, B, fun gleam@int:subtract/2).

-file("src/vec/vec2i.gleam", 270).
?DOC(
    " Returns the squared length (squared magnitude) of the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> length_squared()\n"
    " // -> 1300\n"
    " ```\n"
).
-spec length_squared(vec@vec2:vec2(integer())) -> integer().
length_squared(Vector) ->
    _pipe = Vector,
    _pipe@1 = vec@vec2:to_list(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun(Element) -> Element * Element end),
    gleam@int:sum(_pipe@2).

-file("src/vec/vec2i.gleam", 286).
?DOC(
    " Returns the length (magnitude) of the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> length()\n"
    " // -> 36.06\n"
    " ```\n"
).
-spec length(vec@vec2:vec2(integer())) -> float().
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
                        module => <<"vec/vec2i"/utf8>>,
                        function => <<"length"/utf8>>,
                        line => 287,
                        value => _assert_fail,
                        start => 5659,
                        'end' => 5730,
                        pattern_start => 5670,
                        pattern_end => 5680})
    end,
    Length@1.

-file("src/vec/vec2i.gleam", 302).
?DOC(
    " Compares two vector's lengths, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare_length(Vec2(12, -34), Vec2(2, 3))\n"
    " // -> Gt\n"
    " ```\n"
).
-spec compare_length(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> gleam@order:order().
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

-file("src/vec/vec2i.gleam", 315).
?DOC(
    " Returns the squared distance between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> distance_squared(Vec2(2, 3))\n"
    " // -> 1469\n"
    " ```\n"
).
-spec distance_squared(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> integer().
distance_squared(A, B) ->
    _pipe = A,
    _pipe@1 = vec@vec2:map2(_pipe, B, fun gleam@int:subtract/2),
    length_squared(_pipe@1).

-file("src/vec/vec2i.gleam", 328).
?DOC(
    " Returns the distance between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> distance(Vec2(2, 3))\n"
    " // -> 38.33\n"
    " ```\n"
).
-spec distance(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> float().
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
                        module => <<"vec/vec2i"/utf8>>,
                        function => <<"distance"/utf8>>,
                        line => 329,
                        value => _assert_fail,
                        start => 6612,
                        'end' => 6681,
                        pattern_start => 6623,
                        pattern_end => 6635})
    end,
    Distance@1.

-file("src/vec/vec2i.gleam", 344).
?DOC(
    " Compares two vector's distances to a vector, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare_distance(Vec2(12, -34), Vec2(2, 3), Vec2(-25, 67))\n"
    " // -> Gt\n"
    " ```\n"
).
-spec compare_distance(
    vec@vec2:vec2(integer()),
    vec@vec2:vec2(integer()),
    vec@vec2:vec2(integer())
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

-file("src/vec/vec2i.gleam", 361).
?DOC(
    " Returns a new vector containing the elements multiplies by `scalar`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> scale(2)\n"
    " // -> Vec2(24, -68)\n"
    " ```\n"
).
-spec scale(vec@vec2:vec2(integer()), integer()) -> vec@vec2:vec2(integer()).
scale(Vector, Scalar) ->
    _pipe = Vector,
    vec@vec2:map(
        _pipe,
        fun(_capture) -> gleam@int:multiply(_capture, Scalar) end
    ).

-file("src/vec/vec2i.gleam", 374).
?DOC(
    " Returns the cross product of two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> cross(Vec2(2, 3))\n"
    " // -> 104\n"
    " ```\n"
).
-spec cross(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> integer().
cross(A, B) ->
    (erlang:element(2, A) * erlang:element(3, B)) - (erlang:element(2, B) * erlang:element(
        3,
        A
    )).

-file("src/vec/vec2i.gleam", 387).
?DOC(
    " Returns the dot product of two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> dot(Vec2(2, 3))\n"
    " // -> -78\n"
    " ```\n"
).
-spec dot(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> integer().
dot(A, B) ->
    _pipe = A,
    _pipe@1 = multiply(_pipe, B),
    _pipe@2 = vec@vec2:to_list(_pipe@1),
    gleam@int:sum(_pipe@2).

-file("src/vec/vec2i.gleam", 400).
?DOC(
    " Returns the projection of a vector on another vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> project(Vec2(2, 3))\n"
    " // -> Vec2(-12, -18)\n"
    " ```\n"
).
-spec project(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
project(A, B) ->
    _pipe = B,
    scale(_pipe, case dot(B, B) of
            0 -> 0;
            Gleam@denominator -> dot(A, B) div Gleam@denominator
        end).

-file("src/vec/vec2i.gleam", 414).
?DOC(
    " Returns a new vector resulting from sliding this vector along a plane\n"
    " defined by the given normal vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> slide(Vec2(2, 3))\n"
    " // -> -16\n"
    " ```\n"
).
-spec slide(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
slide(A, B) ->
    _pipe = A,
    subtract(
        _pipe,
        begin
            _pipe@1 = A,
            project(_pipe@1, B)
        end
    ).

-file("src/vec/vec2i.gleam", 428).
?DOC(
    " Returns the reflection of a vector through a plane defined by the given\n"
    " normal vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> reflect(Vec2(2, 3))\n"
    " // -> Vec2(-36, -2)\n"
    " ```\n"
).
-spec reflect(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
reflect(Vector, Normal) ->
    _pipe = Vector,
    _pipe@1 = project(_pipe, Normal),
    _pipe@2 = scale(_pipe@1, 2),
    subtract(_pipe@2, Vector).

-file("src/vec/vec2i.gleam", 442).
?DOC(
    " Returns the mirror of a vector through a plane defined by the given normal\n"
    " vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> mirror(Vec2(2, 3))\n"
    " // -> Vec2(36, 2)\n"
    " ```\n"
).
-spec mirror(vec@vec2:vec2(integer()), vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()).
mirror(Vector, Normal) ->
    _pipe = Vector,
    _pipe@1 = reflect(_pipe, Normal),
    negate(_pipe@1).

-file("src/vec/vec2i.gleam", 455).
?DOC(
    " Rotate a vector by an angle (in 90 degree steps).\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34) |> rotate(1)\n"
    " // -> Vec2(34, 12)\n"
    " ```\n"
).
-spec rotate(vec@vec2:vec2(integer()), integer()) -> vec@vec2:vec2(integer()).
rotate(Vector, Angle) ->
    case begin
        _pipe = Angle,
        gleam@int:modulo(_pipe, 4)
    end of
        {ok, 0} ->
            Vector;

        {ok, 1} ->
            {vec2, - erlang:element(3, Vector), erlang:element(2, Vector)};

        {ok, 2} ->
            {vec2, - erlang:element(2, Vector), - erlang:element(3, Vector)};

        {ok, 3} ->
            {vec2, erlang:element(3, Vector), - erlang:element(2, Vector)};

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"`panic` expression evaluated."/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"vec/vec2i"/utf8>>,
                    function => <<"rotate"/utf8>>,
                    line => 461})
    end.

-file("src/vec/vec2i.gleam", 475).
?DOC(
    " Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34)\n"
    " |> anchor_position(Vec2(10, 21), rotate(_, 1))\n"
    " // -> Vec2(65, 23)\n"
    " ```\n"
).
-spec anchor_position(
    vec@vec2:vec2(integer()),
    vec@vec2:vec2(integer()),
    fun((vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()))
) -> vec@vec2:vec2(integer()).
anchor_position(Vector, Position, Fun) ->
    _pipe = Vector,
    _pipe@1 = subtract(_pipe, Position),
    _pipe@2 = Fun(_pipe@1),
    add(_pipe@2, Position).

-file("src/vec/vec2i.gleam", 493).
?DOC(
    " Return the equivalent of `vector |> rotate(-angle) |> fun() |> rotate(angle)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec2(12, -34)\n"
    " |> anchor_rotation(1, add(_, Vec2(6, 9)))\n"
    " // -> Vec2(3, -28)\n"
    " ```\n"
).
-spec anchor_rotation(
    vec@vec2:vec2(integer()),
    integer(),
    fun((vec@vec2:vec2(integer())) -> vec@vec2:vec2(integer()))
) -> vec@vec2:vec2(integer()).
anchor_rotation(Vector, Angle, Fun) ->
    _pipe = Vector,
    _pipe@1 = rotate(_pipe, - Angle),
    _pipe@2 = Fun(_pipe@1),
    rotate(_pipe@2, Angle).
