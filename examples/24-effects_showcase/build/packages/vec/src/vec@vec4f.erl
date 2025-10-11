-module(vec@vec4f).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vec/vec4f.gleam").
-export([clamp/3, loosely_equals/3, min/2, max/2, ceiling/1, floor/1, round/1, truncate/1, to_precision/2, absolute_value/1, negate/1, modulo/2, divide/2, add/2, sum/1, multiply/2, product/1, subtract/2, length_squared/1, length/1, compare_length/2, loosely_compare_length/3, distance_squared/2, distance/2, compare_distance/3, loosely_compare_distance/4, scale/2, normalize/1, direction/2, dot/2, project/2, slide/2, reflect/2, mirror/2, angle/2, anchor_position/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/vec/vec4f.gleam", 28).
?DOC(
    " Returns a new vector with all components clamped between a lower and upper\n"
    " bound.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> clamp(\n"
    "   Vec4(1.0, 2.1, -5.4, 7.5),\n"
    "   Vec4(1.4, 18.2, 32.3, 9.1),\n"
    " )\n"
    " // -> Vec4(1.2, 2.1, 32.3, 7.5)\n"
    " ```\n"
).
-spec clamp(
    vec@vec4:vec4(float()),
    vec@vec4:vec4(float()),
    vec@vec4:vec4(float())
) -> vec@vec4:vec4(float()).
clamp(Vector, Start_bound, Stop_bound) ->
    {vec4,
        gleam@float:clamp(
            erlang:element(2, Vector),
            erlang:element(2, Start_bound),
            erlang:element(2, Stop_bound)
        ),
        gleam@float:clamp(
            erlang:element(3, Vector),
            erlang:element(3, Start_bound),
            erlang:element(3, Stop_bound)
        ),
        gleam@float:clamp(
            erlang:element(4, Vector),
            erlang:element(4, Start_bound),
            erlang:element(4, Stop_bound)
        ),
        gleam@float:clamp(
            erlang:element(5, Vector),
            erlang:element(5, Start_bound),
            erlang:element(5, Stop_bound)
        )}.

-file("src/vec/vec4f.gleam", 51).
?DOC(
    " Checks for equality of two vectors within a tolerance, returning an `Bool`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69)\n"
    " |> loosely_equals(Vec4(1.25, -3.43, 42.0001, 0.6999), tolerating: 0.1)\n"
    " // -> True\n"
    " ```\n"
).
-spec loosely_equals(vec@vec4:vec4(float()), vec@vec4:vec4(float()), float()) -> boolean().
loosely_equals(A, B, Tolerance) ->
    case begin
        _pipe = A,
        vec@vec4:map2(
            _pipe,
            B,
            fun(A@1, B@1) -> gleam@float:loosely_equals(A@1, B@1, Tolerance) end
        )
    end of
        {vec4, true, true, true, true} ->
            true;

        _ ->
            false
    end.

-file("src/vec/vec4f.gleam", 71).
?DOC(
    " Compares two vectors, returning the smaller of the two.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " min(Vec4(1.2, -3.4, 42.0, 0.69), Vec4(1.0, 2.1, -5.4, 7.5))\n"
    " // -> Vec4(1.0, -3.4, -5.4, 0.69)\n"
    " ```\n"
).
-spec min(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
min(A, B) ->
    _pipe = A,
    vec@vec4:map2(_pipe, B, fun gleam@float:min/2).

-file("src/vec/vec4f.gleam", 84).
?DOC(
    " Compares two vectors, returning the larger of the two.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " max(Vec4(1.2, -3.4, 42.0, 0.69), Vec4(1.4, -9.3, 32.3, 9.1))\n"
    " // -> Vec4(1.4, -3.4, 42.0, 9.1)\n"
    " ```\n"
).
-spec max(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
max(A, B) ->
    _pipe = A,
    vec@vec4:map2(_pipe, B, fun gleam@float:max/2).

-file("src/vec/vec4f.gleam", 98).
?DOC(
    " Returns a new vector with all elements rounded to the next highest whole\n"
    " number as a `Float`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.6, 42.0, 0.5) |> ceiling()\n"
    " // -> Vec4(2.0, -3.0, 42.0, 1.0)\n"
    " ```\n"
).
-spec ceiling(vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
ceiling(Vector) ->
    _pipe = Vector,
    vec@vec4:map(_pipe, fun math:ceil/1).

-file("src/vec/vec4f.gleam", 112).
?DOC(
    " Returns a new vector with all elements rounded to the next lowest whole\n"
    " number as an `Float`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.6, 42.0, 0.5) |> floor()\n"
    " // -> Vec4(1.0, -4.0, 42.0, 0.0)\n"
    " ```\n"
).
-spec floor(vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
floor(Vector) ->
    _pipe = Vector,
    vec@vec4:map(_pipe, fun math:floor/1).

-file("src/vec/vec4f.gleam", 126).
?DOC(
    " Returns a new vector with all elements rounded to the nearest whole number\n"
    " as an `Int`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.6, 42.0, 0.5) |> round()\n"
    " // -> Vec4(1, -4, 42, 1)\n"
    " ```\n"
).
-spec round(vec@vec4:vec4(float())) -> vec@vec4:vec4(integer()).
round(Vector) ->
    _pipe = Vector,
    vec@vec4:map(_pipe, fun erlang:round/1).

-file("src/vec/vec4f.gleam", 139).
?DOC(
    " Returns a new vector with all elements truncated as an `Int`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2323232827383238, -3.656565, 42.0, 0.5) |> truncate()\n"
    " // -> Vec4(1, -3, 42, 0)\n"
    " ```\n"
).
-spec truncate(vec@vec4:vec4(float())) -> vec@vec4:vec4(integer()).
truncate(Vector) ->
    _pipe = Vector,
    vec@vec4:map(_pipe, fun erlang:trunc/1).

-file("src/vec/vec4f.gleam", 157).
?DOC(
    " Returns a new vector with all elements converted to a given precision.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(2.43434348473, -3.656565, 42.0, 0.5) |> to_precision(2)\n"
    " // -> Vec4(2.43, -3.66, 42.0, 0.5)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec4(547_890.453444, -3.656565, 42.0, 0.5) |> to_precision(-3)\n"
    " // -> Vec4(548_000.0, 0.0, 0.0, 0.0)\n"
    " ```\n"
).
-spec to_precision(vec@vec4:vec4(float()), integer()) -> vec@vec4:vec4(float()).
to_precision(Vector, Precision) ->
    _pipe = Vector,
    vec@vec4:map(
        _pipe,
        fun(_capture) -> gleam@float:to_precision(_capture, Precision) end
    ).

-file("src/vec/vec4f.gleam", 170).
?DOC(
    " Returns a new vector with all elements in absolute values.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> absolute_value()\n"
    " // -> Vec4(1.2, 3.4, 42.0, 0.69)\n"
    " ```\n"
).
-spec absolute_value(vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
absolute_value(Vector) ->
    _pipe = Vector,
    vec@vec4:map(_pipe, fun gleam@float:absolute_value/1).

-file("src/vec/vec4f.gleam", 183).
?DOC(
    " Returns a new vector with all elements negated.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> negate()\n"
    " // -> Vec4(-1.2, 3.4, -42.0, -0.69)\n"
    " ```\n"
).
-spec negate(vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
negate(Vector) ->
    _pipe = Vector,
    vec@vec4:map(_pipe, fun gleam@float:negate/1).

-file("src/vec/vec4f.gleam", 232).
?DOC(
    " Returns the modulo of the inputs as a `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(13.3, -13.3, 13.3, -13.3) |> modulo(Vec4(3.3, 3.3, -3.3, -3.3))\n"
    " // -> Ok(Vec4(0.1, 3.2, -3.2, -0.1))\n"
    " ```\n"
).
-spec modulo(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> {ok,
        vec@vec4:vec4(float())} |
    {error, nil}.
modulo(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec4:map2(_pipe, Divisor, fun gleam@float:modulo/2),
    vec@vec4:result(_pipe@1).

-file("src/vec/vec4f.gleam", 253).
?DOC(
    " Returns division of the inputs as a `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> divide(Vec4(2.0, 0.5, 4.0, 1.0))\n"
    " // -> Ok(Vec4(0.6, -6.8, 10.5, 0.69))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> divide(Vec4(0.0, 0.5, 4.0, 1.0))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec divide(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> {ok,
        vec@vec4:vec4(float())} |
    {error, nil}.
divide(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec4:map2(_pipe, Divisor, fun gleam@float:divide/2),
    vec@vec4:result(_pipe@1).

-file("src/vec/vec4f.gleam", 269).
?DOC(
    " Adds two vectors together.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> add(Vec4(2.1, 4.5, -2.0, 9.01))\n"
    " // -> Vec4(3.3, 1.1, 40.0, 9.7)\n"
    " ```\n"
).
-spec add(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
add(A, B) ->
    _pipe = A,
    vec@vec4:map2(_pipe, B, fun gleam@float:add/2).

-file("src/vec/vec4f.gleam", 201).
?DOC(
    " Sums a list of vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\n"
    "   Vec4(1.2, -3.4, 42.0, 0.69),\n"
    "   Vec4(2.1, 4.5, -2.0, 9.01),\n"
    "   Vec4(3.3, 0.0, -20.0, 0.3),\n"
    " ]\n"
    " |> sum()\n"
    " // -> Vec4(6.6, 1.1, 20.0, 10.0)\n"
    " ```\n"
).
-spec sum(list(vec@vec4:vec4(float()))) -> vec@vec4:vec4(float()).
sum(Vectors) ->
    _pipe = Vectors,
    gleam@list:fold(_pipe, vec@vec4:splat(+0.0), fun add/2).

-file("src/vec/vec4f.gleam", 282).
?DOC(
    " Multiplies two vectors together.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> multiply(Vec4(2.1, -1.0, 0.0, 1.0))\n"
    " // -> Vec4(2.52, 3.4, 0.0, 0.69)\n"
    " ```\n"
).
-spec multiply(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
multiply(A, B) ->
    _pipe = A,
    vec@vec4:map2(_pipe, B, fun gleam@float:multiply/2).

-file("src/vec/vec4f.gleam", 219).
?DOC(
    " Multiplies a list of vectors and returns the product.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\n"
    "   Vec4(1.2, -3.4, 42.0, 0.69),\n"
    "   Vec4(2.1, -1.0, 999.9, 2.0),\n"
    "   Vec4(3.2, 2.0, 0.0, 0.5),\n"
    " ]\n"
    " |> product()\n"
    " // -> Vec4(8.064, 6.8, 0.0, 0.69)\n"
    " ```\n"
).
-spec product(list(vec@vec4:vec4(float()))) -> vec@vec4:vec4(float()).
product(Vectors) ->
    _pipe = Vectors,
    gleam@list:fold(_pipe, vec@vec4:splat(1.0), fun multiply/2).

-file("src/vec/vec4f.gleam", 295).
?DOC(
    " Subtracts one vector from another.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> subtract(Vec4(0.7, -4.5, 2.0, 1.39))\n"
    " // -> Vec4(0.5, 1.1, 40.0, -0.7)\n"
    " ```\n"
).
-spec subtract(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
subtract(A, B) ->
    _pipe = A,
    vec@vec4:map2(_pipe, B, fun gleam@float:subtract/2).

-file("src/vec/vec4f.gleam", 308).
?DOC(
    " Returns the squared length (squared magnitude) of the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> length_squared()\n"
    " // -> 1777.47\n"
    " ```\n"
).
-spec length_squared(vec@vec4:vec4(float())) -> float().
length_squared(Vector) ->
    _pipe = Vector,
    _pipe@1 = vec@vec4:to_list(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun(Element) -> Element * Element end),
    gleam@float:sum(_pipe@2).

-file("src/vec/vec4f.gleam", 324).
?DOC(
    " Returns the length (magnitude) of the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> length()\n"
    " // -> 42.16\n"
    " ```\n"
).
-spec length(vec@vec4:vec4(float())) -> float().
length(Vector) ->
    Length@1 = case begin
        _pipe = Vector,
        _pipe@1 = length_squared(_pipe),
        gleam@float:square_root(_pipe@1)
    end of
        {ok, Length} -> Length;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vec/vec4f"/utf8>>,
                        function => <<"length"/utf8>>,
                        line => 325,
                        value => _assert_fail,
                        start => 7431,
                        'end' => 7504,
                        pattern_start => 7442,
                        pattern_end => 7452})
    end,
    Length@1.

-file("src/vec/vec4f.gleam", 343).
?DOC(
    " Compares two vector's lengths, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare_length(\n"
    "   Vec4(1.2, -3.4, 42.0, 0.69),\n"
    "   Vec4(1.0, 2.1, 3.2, 4.3),\n"
    " )\n"
    " // -> Gt\n"
    " ```\n"
).
-spec compare_length(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> gleam@order:order().
compare_length(A, B) ->
    gleam@float:compare(
        begin
            _pipe = A,
            length_squared(_pipe)
        end,
        begin
            _pipe@1 = B,
            length_squared(_pipe@1)
        end
    ).

-file("src/vec/vec4f.gleam", 361).
?DOC(
    " Compares two vector's lengths within a tolerance, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " loosely_compare_length(\n"
    "   Vec4(1.2, -3.4, 42.0, 0.69),\n"
    "   Vec4(-1.25, 3.43, -42.0001, -0.6999),\n"
    "   tolerating: 0.5,\n"
    " )\n"
    " // -> Eq\n"
    " ```\n"
).
-spec loosely_compare_length(
    vec@vec4:vec4(float()),
    vec@vec4:vec4(float()),
    float()
) -> gleam@order:order().
loosely_compare_length(A, B, Tolerance) ->
    gleam@float:loosely_compare(
        begin
            _pipe = A,
            length_squared(_pipe)
        end,
        begin
            _pipe@1 = B,
            length_squared(_pipe@1)
        end,
        Tolerance
    ).

-file("src/vec/vec4f.gleam", 378).
?DOC(
    " Returns the squared distance between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> distance_squared(Vec4(1.0, 2.1, 3.2, 4.3))\n"
    " // -> 1548.76\n"
    " ```\n"
).
-spec distance_squared(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> float().
distance_squared(A, B) ->
    _pipe = A,
    _pipe@1 = vec@vec4:map2(_pipe, B, fun gleam@float:subtract/2),
    length_squared(_pipe@1).

-file("src/vec/vec4f.gleam", 391).
?DOC(
    " Returns the distance between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> distance(Vec4(1.0, 2.1, 3.2, 4.3))\n"
    " // -> 39.35\n"
    " ```\n"
).
-spec distance(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> float().
distance(A, B) ->
    Distance@1 = case begin
        _pipe = distance_squared(A, B),
        gleam@float:square_root(_pipe)
    end of
        {ok, Distance} -> Distance;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vec/vec4f"/utf8>>,
                        function => <<"distance"/utf8>>,
                        line => 392,
                        value => _assert_fail,
                        start => 9047,
                        'end' => 9118,
                        pattern_start => 9058,
                        pattern_end => 9070})
    end,
    Distance@1.

-file("src/vec/vec4f.gleam", 411).
?DOC(
    " Compares two vector's distances to a vector, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare_distance(\n"
    "   Vec4(1.2, -3.4, 42.0, 0.69),\n"
    "   Vec4(1.0, 2.1, 3.2, 4.3),\n"
    "   Vec4(-2.5, 6.7, 19.4, 0.0),\n"
    " )\n"
    " // -> Gt\n"
    " ```\n"
).
-spec compare_distance(
    vec@vec4:vec4(float()),
    vec@vec4:vec4(float()),
    vec@vec4:vec4(float())
) -> gleam@order:order().
compare_distance(A, B, Vector) ->
    gleam@float:compare(
        begin
            _pipe = A,
            distance_squared(_pipe, Vector)
        end,
        begin
            _pipe@1 = B,
            distance_squared(_pipe@1, Vector)
        end
    ).

-file("src/vec/vec4f.gleam", 435).
?DOC(
    " Compares two vector's distances to a vector within a tolerance, returning\n"
    " an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " loosely_compare_distance(\n"
    "   Vec4(1.2, -3.4, 42.0, 0.69),\n"
    "   Vec4(1.25, -3.43, 42.0001, 0.6999),\n"
    "   Vec4(-2.5, 6.7, 19.4, 0.0),\n"
    "   tolerating: 1.0,\n"
    " )\n"
    " // -> Eq\n"
    " ```\n"
).
-spec loosely_compare_distance(
    vec@vec4:vec4(float()),
    vec@vec4:vec4(float()),
    vec@vec4:vec4(float()),
    float()
) -> gleam@order:order().
loosely_compare_distance(A, B, Vector, Tolerance) ->
    gleam@float:loosely_compare(
        begin
            _pipe = A,
            distance_squared(_pipe, Vector)
        end,
        begin
            _pipe@1 = B,
            distance_squared(_pipe@1, Vector)
        end,
        Tolerance
    ).

-file("src/vec/vec4f.gleam", 457).
?DOC(
    " Returns a new vector containing the elements multiplies by `scalar`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> scale(2.5)\n"
    " // -> Vec4(3.0, -8.5, 105.0, 1.72)\n"
    " ```\n"
).
-spec scale(vec@vec4:vec4(float()), float()) -> vec@vec4:vec4(float()).
scale(Vector, Scalar) ->
    _pipe = Vector,
    vec@vec4:map(
        _pipe,
        fun(_capture) -> gleam@float:multiply(_capture, Scalar) end
    ).

-file("src/vec/vec4f.gleam", 470).
?DOC(
    " Normalize the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> normalize()\n"
    " // -> Vec4(0.03, -0.08, 1.0, 0.02)\n"
    " ```\n"
).
-spec normalize(vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
normalize(Vector) ->
    _pipe = Vector,
    scale(
        _pipe,
        case begin
            _pipe@1 = Vector,
            length(_pipe@1)
        end of
            +0.0 -> +0.0;
            -0.0 -> -0.0;
            Gleam@denominator -> 1.0 / Gleam@denominator
        end
    ).

-file("src/vec/vec4f.gleam", 483).
?DOC(
    " Returns a normalized vector pointing from `a` to `b`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> direction(Vec4(1.0, 2.1, 3.2, 4.3))\n"
    " // -> Vec4(-0.0, 0.14, -0.99, 0.092)\n"
    " ```\n"
).
-spec direction(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
direction(A, B) ->
    _pipe = B,
    _pipe@1 = subtract(_pipe, A),
    normalize(_pipe@1).

-file("src/vec/vec4f.gleam", 496).
?DOC(
    " Returns the dot product of two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> dot(Vec4(1.0, 2.1, 3.2, 4.3))\n"
    " // -> 131.43\n"
    " ```\n"
).
-spec dot(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> float().
dot(A, B) ->
    _pipe = A,
    _pipe@1 = multiply(_pipe, B),
    _pipe@2 = vec@vec4:to_list(_pipe@1),
    gleam@float:sum(_pipe@2).

-file("src/vec/vec4f.gleam", 509).
?DOC(
    " Returns the projection of a vector on another vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> project(Vec4(1.0, 2.1, 3.2, 4.3))\n"
    " // -> Vec4(3.85, 8.08, 12.32, 16.55)\n"
    " ```\n"
).
-spec project(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
project(A, B) ->
    _pipe = B,
    scale(_pipe, case dot(B, B) of
            +0.0 -> +0.0;
            -0.0 -> -0.0;
            Gleam@denominator -> dot(A, B) / Gleam@denominator
        end).

-file("src/vec/vec4f.gleam", 523).
?DOC(
    " Returns a new vector resulting from sliding this vector along a plane\n"
    " defined by the given normal vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> slide(Vec4(1.0, 2.1, 3.2, 4.3))\n"
    " // -> Vec4(-2.65, -11.48, 29.68, -15.86)\n"
    " ```\n"
).
-spec slide(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
slide(A, B) ->
    _pipe = A,
    subtract(
        _pipe,
        begin
            _pipe@1 = A,
            project(_pipe@1, B)
        end
    ).

-file("src/vec/vec4f.gleam", 537).
?DOC(
    " Returns the reflection of a vector through a plane defined by the given\n"
    " normal vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> reflect(Vec4(1.0, 2.1, 3.2, 4.3))\n"
    " // -> Vec4(6.5, 19.57, -17.36, 32.42)\n"
    " ```\n"
).
-spec reflect(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
reflect(Vector, Normal) ->
    _pipe = Vector,
    _pipe@1 = project(_pipe, Normal),
    _pipe@2 = scale(_pipe@1, 2.0),
    subtract(_pipe@2, Vector).

-file("src/vec/vec4f.gleam", 551).
?DOC(
    " Returns the mirror of a vector through a plane defined by the given normal\n"
    " vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> mirror(Vec4(1.0, 2.1, 3.2, 4.3))\n"
    " // -> Vec4(-6.5, -19.57, 17.36, -32.42)\n"
    " ```\n"
).
-spec mirror(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> vec@vec4:vec4(float()).
mirror(Vector, Normal) ->
    _pipe = Vector,
    _pipe@1 = reflect(_pipe, Normal),
    negate(_pipe@1).

-file("src/vec/vec4f.gleam", 564).
?DOC(
    " Returns the angle (in radians) between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69) |> angle(Vec4(1.0, 2.1, 3.2, 4.3))\n"
    " // -> 1.0\n"
    " ```\n"
).
-spec angle(vec@vec4:vec4(float()), vec@vec4:vec4(float())) -> float().
angle(A, B) ->
    Angle@1 = case begin
        _pipe = dot(normalize(A), normalize(B)),
        _pipe@1 = gleam@float:clamp(_pipe, -1.0, 1.0),
        vec@internal:acos(_pipe@1)
    end of
        {ok, Angle} -> Angle;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vec/vec4f"/utf8>>,
                        function => <<"angle"/utf8>>,
                        line => 565,
                        value => _assert_fail,
                        start => 13231,
                        'end' => 13334,
                        pattern_start => 13242,
                        pattern_end => 13251})
    end,
    Angle@1.

-file("src/vec/vec4f.gleam", 581).
?DOC(
    " Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec4(1.2, -3.4, 42.0, 0.69)\n"
    " |> anchor_position(Vec4(1.0, 2.1, 3.2, 4.3), scale(_, 2.0))\n"
    " // -> Vec4(1.4, -8.9, 80.8, -2.92)\n"
    " ```\n"
).
-spec anchor_position(
    vec@vec4:vec4(float()),
    vec@vec4:vec4(float()),
    fun((vec@vec4:vec4(float())) -> vec@vec4:vec4(float()))
) -> vec@vec4:vec4(float()).
anchor_position(Vector, Position, Fun) ->
    _pipe = Vector,
    _pipe@1 = subtract(_pipe, Position),
    _pipe@2 = Fun(_pipe@1),
    add(_pipe@2, Position).
