-module(vec@vec3f).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vec/vec3f.gleam").
-export([clamp/3, loosely_equals/3, min/2, max/2, ceiling/1, floor/1, round/1, truncate/1, to_precision/2, absolute_value/1, negate/1, modulo/2, divide/2, add/2, sum/1, multiply/2, product/1, subtract/2, length_squared/1, length/1, compare_length/2, loosely_compare_length/3, distance_squared/2, distance/2, compare_distance/3, loosely_compare_distance/4, scale/2, normalize/1, direction/2, cross/2, dot/2, project/2, slide/2, reflect/2, mirror/2, angle/2, rotate/3, anchor_position/3, anchor_rotation/4]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/vec/vec3f.gleam", 28).
?DOC(
    " Returns a new vector with all components clamped between a lower and upper\n"
    " bound.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> clamp(\n"
    "   Vec3(1.0, 2.1, -5.4),\n"
    "   Vec3(1.4, 18.2, 32.3),\n"
    " )\n"
    " // -> Vec3(1.2, 2.1, 32.3)\n"
    " ```\n"
).
-spec clamp(
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float())
) -> vec@vec3:vec3(float()).
clamp(Vector, Start_bound, Stop_bound) ->
    {vec3,
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
        )}.

-file("src/vec/vec3f.gleam", 50).
?DOC(
    " Checks for equality of two vectors within a tolerance, returning an `Bool`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0)\n"
    " |> loosely_equals(Vec3(1.25, -3.43, 42.0001), tolerating: 0.1)\n"
    " // -> True\n"
    " ```\n"
).
-spec loosely_equals(vec@vec3:vec3(float()), vec@vec3:vec3(float()), float()) -> boolean().
loosely_equals(A, B, Tolerance) ->
    case begin
        _pipe = A,
        vec@vec3:map2(
            _pipe,
            B,
            fun(A@1, B@1) -> gleam@float:loosely_equals(A@1, B@1, Tolerance) end
        )
    end of
        {vec3, true, true, true} ->
            true;

        _ ->
            false
    end.

-file("src/vec/vec3f.gleam", 70).
?DOC(
    " Compares two vectors, returning the smaller of the two.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " min(Vec3(1.2, -3.4, 42.0), Vec3(1.0, 2.1, -5.4))\n"
    " // -> Vec3(1.0, -3.4, -5.4)\n"
    " ```\n"
).
-spec min(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
min(A, B) ->
    _pipe = A,
    vec@vec3:map2(_pipe, B, fun gleam@float:min/2).

-file("src/vec/vec3f.gleam", 83).
?DOC(
    " Compares two vectors, returning the larger of the two.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " max(Vec3(1.2, -3.4, 42.0), Vec3(1.4, -9.3, 32.3))\n"
    " // -> Vec3(1.4, -3.4, 42.0)\n"
    " ```\n"
).
-spec max(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
max(A, B) ->
    _pipe = A,
    vec@vec3:map2(_pipe, B, fun gleam@float:max/2).

-file("src/vec/vec3f.gleam", 97).
?DOC(
    " Returns a new vector with all elements rounded to the next highest whole\n"
    " number as a `Float`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.6, 42.0) |> ceiling()\n"
    " // -> Vec3(2.0, -3.0, 42.0)\n"
    " ```\n"
).
-spec ceiling(vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
ceiling(Vector) ->
    _pipe = Vector,
    vec@vec3:map(_pipe, fun math:ceil/1).

-file("src/vec/vec3f.gleam", 111).
?DOC(
    " Returns a new vector with all elements rounded to the next lowest whole\n"
    " number as an `Float`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.6, 42.0) |> floor()\n"
    " // -> Vec3(1.0, -4.0, 42.0)\n"
    " ```\n"
).
-spec floor(vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
floor(Vector) ->
    _pipe = Vector,
    vec@vec3:map(_pipe, fun math:floor/1).

-file("src/vec/vec3f.gleam", 125).
?DOC(
    " Returns a new vector with all elements rounded to the nearest whole number\n"
    " as an `Int`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.6, 42.0) |> round()\n"
    " // -> Vec3(1, -4, 42)\n"
    " ```\n"
).
-spec round(vec@vec3:vec3(float())) -> vec@vec3:vec3(integer()).
round(Vector) ->
    _pipe = Vector,
    vec@vec3:map(_pipe, fun erlang:round/1).

-file("src/vec/vec3f.gleam", 138).
?DOC(
    " Returns a new vector with all elements truncated as an `Int`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2323232827383238, -3.656565, 42.0) |> truncate()\n"
    " // -> Vec3(1, -3, 42)\n"
    " ```\n"
).
-spec truncate(vec@vec3:vec3(float())) -> vec@vec3:vec3(integer()).
truncate(Vector) ->
    _pipe = Vector,
    vec@vec3:map(_pipe, fun erlang:trunc/1).

-file("src/vec/vec3f.gleam", 156).
?DOC(
    " Returns a new vector with all elements converted to a given precision.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(2.43434348473, -3.656565, 42.0) |> to_precision(2)\n"
    " // -> Vec3(2.43, -3.66, 42.0)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec3(547_890.453444, -3.656565, 42.0) |> to_precision(-3)\n"
    " // -> Vec3(548_000.0, 0.0, 0.0)\n"
    " ```\n"
).
-spec to_precision(vec@vec3:vec3(float()), integer()) -> vec@vec3:vec3(float()).
to_precision(Vector, Precision) ->
    _pipe = Vector,
    vec@vec3:map(
        _pipe,
        fun(_capture) -> gleam@float:to_precision(_capture, Precision) end
    ).

-file("src/vec/vec3f.gleam", 169).
?DOC(
    " Returns a new vector with all elements in absolute values.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> absolute_value()\n"
    " // -> Vec3(1.2, 3.4, 42.0)\n"
    " ```\n"
).
-spec absolute_value(vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
absolute_value(Vector) ->
    _pipe = Vector,
    vec@vec3:map(_pipe, fun gleam@float:absolute_value/1).

-file("src/vec/vec3f.gleam", 182).
?DOC(
    " Returns a new vector with all elements negated.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> negate()\n"
    " // -> Vec3(-1.2, 3.4, -42.0)\n"
    " ```\n"
).
-spec negate(vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
negate(Vector) ->
    _pipe = Vector,
    vec@vec3:map(_pipe, fun gleam@float:negate/1).

-file("src/vec/vec3f.gleam", 231).
?DOC(
    " Returns the modulo of the inputs as a `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(13.3, -13.3, 13.3) |> modulo(Vec3(3.3, 3.3, -3.3))\n"
    " // -> Ok(Vec3(0.1, 3.2, -3.2))\n"
    " ```\n"
).
-spec modulo(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> {ok,
        vec@vec3:vec3(float())} |
    {error, nil}.
modulo(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec3:map2(_pipe, Divisor, fun gleam@float:modulo/2),
    vec@vec3:result(_pipe@1).

-file("src/vec/vec3f.gleam", 252).
?DOC(
    " Returns division of the inputs as a `Result`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> divide(Vec3(2.0, 0.5, 4.0))\n"
    " // -> Ok(Vec3(0.6, -6.8, 10.5))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> divide(Vec3(0.0, 0.5, 4.0))\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec divide(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> {ok,
        vec@vec3:vec3(float())} |
    {error, nil}.
divide(Dividend, Divisor) ->
    _pipe = Dividend,
    _pipe@1 = vec@vec3:map2(_pipe, Divisor, fun gleam@float:divide/2),
    vec@vec3:result(_pipe@1).

-file("src/vec/vec3f.gleam", 268).
?DOC(
    " Adds two vectors together.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> add(Vec3(2.1, 4.5, -2.0))\n"
    " // -> Vec3(3.3, 1.1, 40.0)\n"
    " ```\n"
).
-spec add(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
add(A, B) ->
    _pipe = A,
    vec@vec3:map2(_pipe, B, fun gleam@float:add/2).

-file("src/vec/vec3f.gleam", 200).
?DOC(
    " Sums a list of vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\n"
    "   Vec3(1.2, -3.4, 42.0),\n"
    "   Vec3(2.1, 4.5, -2.0),\n"
    "   Vec3(3.3, 0.0, -20.0),\n"
    " ]\n"
    " |> sum()\n"
    " // -> Vec3(6.6, 1.1, 20.0)\n"
    " ```\n"
).
-spec sum(list(vec@vec3:vec3(float()))) -> vec@vec3:vec3(float()).
sum(Vectors) ->
    _pipe = Vectors,
    gleam@list:fold(_pipe, vec@vec3:splat(+0.0), fun add/2).

-file("src/vec/vec3f.gleam", 281).
?DOC(
    " Multiplies two vectors together.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> multiply(Vec3(2.1, -1.0, 0.0))\n"
    " // -> Vec3(2.52, 3.4, 0.0)\n"
    " ```\n"
).
-spec multiply(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
multiply(A, B) ->
    _pipe = A,
    vec@vec3:map2(_pipe, B, fun gleam@float:multiply/2).

-file("src/vec/vec3f.gleam", 218).
?DOC(
    " Multiplies a list of vectors and returns the product.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\n"
    "   Vec3(1.2, -3.4, 42.0),\n"
    "   Vec3(2.1, -1.0, 999.9),\n"
    "   Vec3(3.2, 2.0, 0.0),\n"
    " ]\n"
    " |> product()\n"
    " // -> Vec3(8.064, 6.8, 0.0)\n"
    " ```\n"
).
-spec product(list(vec@vec3:vec3(float()))) -> vec@vec3:vec3(float()).
product(Vectors) ->
    _pipe = Vectors,
    gleam@list:fold(_pipe, vec@vec3:splat(1.0), fun multiply/2).

-file("src/vec/vec3f.gleam", 294).
?DOC(
    " Subtracts one vector from another.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> subtract(Vec3(0.7, -4.5, 2.0))\n"
    " // -> Vec3(0.5, 1.1, 40.0)\n"
    " ```\n"
).
-spec subtract(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
subtract(A, B) ->
    _pipe = A,
    vec@vec3:map2(_pipe, B, fun gleam@float:subtract/2).

-file("src/vec/vec3f.gleam", 307).
?DOC(
    " Returns the squared length (squared magnitude) of the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> length_squared()\n"
    " // -> 1777.0\n"
    " ```\n"
).
-spec length_squared(vec@vec3:vec3(float())) -> float().
length_squared(Vector) ->
    _pipe = Vector,
    _pipe@1 = vec@vec3:to_list(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun(Element) -> Element * Element end),
    gleam@float:sum(_pipe@2).

-file("src/vec/vec3f.gleam", 323).
?DOC(
    " Returns the length (magnitude) of the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> length()\n"
    " // -> 42.15\n"
    " ```\n"
).
-spec length(vec@vec3:vec3(float())) -> float().
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
                        module => <<"vec/vec3f"/utf8>>,
                        function => <<"length"/utf8>>,
                        line => 324,
                        value => _assert_fail,
                        start => 7053,
                        'end' => 7126,
                        pattern_start => 7064,
                        pattern_end => 7074})
    end,
    Length@1.

-file("src/vec/vec3f.gleam", 342).
?DOC(
    " Compares two vector's lengths, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare_length(\n"
    "   Vec3(1.2, -3.4, 42.0),\n"
    "   Vec3(1.0, 2.1, 3.2),\n"
    " )\n"
    " // -> Gt\n"
    " ```\n"
).
-spec compare_length(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> gleam@order:order().
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

-file("src/vec/vec3f.gleam", 360).
?DOC(
    " Compares two vector's lengths within a tolerance, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " loosely_compare_length(\n"
    "   Vec3(1.2, -3.4, 42.0),\n"
    "   Vec3(-1.25, 3.43, -42.0001),\n"
    "   tolerating: 0.5,\n"
    " )\n"
    " // -> Eq\n"
    " ```\n"
).
-spec loosely_compare_length(
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
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

-file("src/vec/vec3f.gleam", 377).
?DOC(
    " Returns the squared distance between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> distance_squared(Vec3(1.0, 2.1, 3.2))\n"
    " // -> 1535.73\n"
    " ```\n"
).
-spec distance_squared(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> float().
distance_squared(A, B) ->
    _pipe = A,
    _pipe@1 = vec@vec3:map2(_pipe, B, fun gleam@float:subtract/2),
    length_squared(_pipe@1).

-file("src/vec/vec3f.gleam", 390).
?DOC(
    " Returns the distance between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> distance(Vec3(1.0, 2.1, 3.2))\n"
    " // -> 39.19\n"
    " ```\n"
).
-spec distance(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> float().
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
                        module => <<"vec/vec3f"/utf8>>,
                        function => <<"distance"/utf8>>,
                        line => 391,
                        value => _assert_fail,
                        start => 8626,
                        'end' => 8697,
                        pattern_start => 8637,
                        pattern_end => 8649})
    end,
    Distance@1.

-file("src/vec/vec3f.gleam", 410).
?DOC(
    " Compares two vector's distances to a vector, returning an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare_distance(\n"
    "   Vec3(1.2, -3.4, 42.0),\n"
    "   Vec3(1.0, 2.1, 3.2),\n"
    "   Vec3(-2.5, 6.7, 19.4),\n"
    " )\n"
    " // -> Gt\n"
    " ```\n"
).
-spec compare_distance(
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float())
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

-file("src/vec/vec3f.gleam", 434).
?DOC(
    " Compares two vector's distances to a vector within a tolerance, returning\n"
    " an `Order`:\n"
    " `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " loosely_compare_distance(\n"
    "   Vec3(1.2, -3.4, 42.0),\n"
    "   Vec3(1.25, -3.43, 42.0001),\n"
    "   Vec3(-2.5, 6.7, 19.4),\n"
    "   tolerating: 1.0,\n"
    " )\n"
    " // -> Eq\n"
    " ```\n"
).
-spec loosely_compare_distance(
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
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

-file("src/vec/vec3f.gleam", 456).
?DOC(
    " Returns a new vector containing the elements multiplies by `scalar`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> scale(2.5)\n"
    " // -> Vec3(3.0, -8.5, 105.0)\n"
    " ```\n"
).
-spec scale(vec@vec3:vec3(float()), float()) -> vec@vec3:vec3(float()).
scale(Vector, Scalar) ->
    _pipe = Vector,
    vec@vec3:map(
        _pipe,
        fun(_capture) -> gleam@float:multiply(_capture, Scalar) end
    ).

-file("src/vec/vec3f.gleam", 469).
?DOC(
    " Normalize the vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> normalize()\n"
    " // -> Vec3(0.03, -0.08, 1.0)\n"
    " ```\n"
).
-spec normalize(vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
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

-file("src/vec/vec3f.gleam", 482).
?DOC(
    " Returns a normalized vector pointing from `a` to `b`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> direction(Vec3(1.0, 2.1, 3.2))\n"
    " // -> Vec3(-0.01, 0.14, -1.0)\n"
    " ```\n"
).
-spec direction(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
direction(A, B) ->
    _pipe = B,
    _pipe@1 = subtract(_pipe, A),
    normalize(_pipe@1).

-file("src/vec/vec3f.gleam", 495).
?DOC(
    " Returns the cross product of two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> dot(Vec3(1.0, 2.1, 3.2))\n"
    " // -> Vec3(-99.08, 38.16, 5.92)\n"
    " ```\n"
).
-spec cross(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
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

-file("src/vec/vec3f.gleam", 512).
?DOC(
    " Returns the dot product of two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> dot(Vec3(1.0, 2.1, 3.2))\n"
    " // -> 128.46\n"
    " ```\n"
).
-spec dot(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> float().
dot(A, B) ->
    _pipe = A,
    _pipe@1 = multiply(_pipe, B),
    _pipe@2 = vec@vec3:to_list(_pipe@1),
    gleam@float:sum(_pipe@2).

-file("src/vec/vec3f.gleam", 525).
?DOC(
    " Returns the projection of a vector on another vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> project(Vec3(1.0, 2.1, 3.2))\n"
    " // -> Vec3(8.21, 17.24, 26.27)\n"
    " ```\n"
).
-spec project(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
project(A, B) ->
    _pipe = B,
    scale(_pipe, case dot(B, B) of
            +0.0 -> +0.0;
            -0.0 -> -0.0;
            Gleam@denominator -> dot(A, B) / Gleam@denominator
        end).

-file("src/vec/vec3f.gleam", 539).
?DOC(
    " Returns a new vector resulting from sliding this vector along a plane\n"
    " defined by the given normal vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> slide(Vec3(1.0, 2.1, 3.2))\n"
    " // -> Vec3(-7.01, -20.64, 15.73)\n"
    " ```\n"
).
-spec slide(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
slide(A, B) ->
    _pipe = A,
    subtract(
        _pipe,
        begin
            _pipe@1 = A,
            project(_pipe@1, B)
        end
    ).

-file("src/vec/vec3f.gleam", 553).
?DOC(
    " Returns the reflection of a vector through a plane defined by the given\n"
    " normal vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> reflect(Vec3(1.0, 2.1, 3.2))\n"
    " // -> Vec3(15.22, 37.87, 10.53)\n"
    " ```\n"
).
-spec reflect(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
reflect(Vector, Normal) ->
    _pipe = Vector,
    _pipe@1 = project(_pipe, Normal),
    _pipe@2 = scale(_pipe@1, 2.0),
    subtract(_pipe@2, Vector).

-file("src/vec/vec3f.gleam", 567).
?DOC(
    " Returns the mirror of a vector through a plane defined by the given normal\n"
    " vector.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> mirror(Vec3(1.0, 2.1, 3.2))\n"
    " // -> Vec3(-15.22, -37.87, -10.53)\n"
    " ```\n"
).
-spec mirror(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> vec@vec3:vec3(float()).
mirror(Vector, Normal) ->
    _pipe = Vector,
    _pipe@1 = reflect(_pipe, Normal),
    negate(_pipe@1).

-file("src/vec/vec3f.gleam", 580).
?DOC(
    " Returns the angle (in radians) between two vectors.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0) |> angle(Vec3(1.0, 2.1, 3.2))\n"
    " // -> 0.69\n"
    " ```\n"
).
-spec angle(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> float().
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
                        module => <<"vec/vec3f"/utf8>>,
                        function => <<"angle"/utf8>>,
                        line => 581,
                        value => _assert_fail,
                        start => 12995,
                        'end' => 13098,
                        pattern_start => 13006,
                        pattern_end => 13015})
    end,
    Angle@1.

-file("src/vec/vec3f.gleam", 597).
?DOC(
    " Rotate a vector around a given axis by an angle (in radians).\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0)\n"
    " |> rotate(around: Vec3(1.0, 2.1, 3.2), by: maths.pi() *. 0.25)\n"
    " // -> Vec3(20.96, -4.18, 36.33)\n"
    " ```\n"
).
-spec rotate(vec@vec3:vec3(float()), vec@vec3:vec3(float()), float()) -> vec@vec3:vec3(float()).
rotate(Vector, Axis, Angle) ->
    Axis@1 = begin
        _pipe = Axis,
        normalize(_pipe)
    end,
    Cos_angle = vec@internal:cos(Angle),
    Sin_angle = vec@internal:sin(Angle),
    {vec3,
        ((erlang:element(2, Vector) * (Cos_angle + ((erlang:element(2, Axis@1) * erlang:element(
            2,
            Axis@1
        ))
        * (1.0 - Cos_angle))))
        + (erlang:element(3, Vector) * (((erlang:element(2, Axis@1) * erlang:element(
            3,
            Axis@1
        ))
        * (1.0 - Cos_angle))
        - (erlang:element(4, Axis@1) * Sin_angle))))
        + (erlang:element(4, Vector) * (((erlang:element(2, Axis@1) * erlang:element(
            4,
            Axis@1
        ))
        * (1.0 - Cos_angle))
        + (erlang:element(3, Axis@1) * Sin_angle))),
        ((erlang:element(2, Vector) * (((erlang:element(3, Axis@1) * erlang:element(
            2,
            Axis@1
        ))
        * (1.0 - Cos_angle))
        + (erlang:element(4, Axis@1) * Sin_angle)))
        + (erlang:element(3, Vector) * (Cos_angle + ((erlang:element(3, Axis@1)
        * erlang:element(3, Axis@1))
        * (1.0 - Cos_angle)))))
        + (erlang:element(4, Vector) * (((erlang:element(3, Axis@1) * erlang:element(
            4,
            Axis@1
        ))
        * (1.0 - Cos_angle))
        - (erlang:element(2, Axis@1) * Sin_angle))),
        ((erlang:element(2, Vector) * (((erlang:element(4, Axis@1) * erlang:element(
            2,
            Axis@1
        ))
        * (1.0 - Cos_angle))
        - (erlang:element(3, Axis@1) * Sin_angle)))
        + (erlang:element(3, Vector) * (((erlang:element(4, Axis@1) * erlang:element(
            3,
            Axis@1
        ))
        * (1.0 - Cos_angle))
        + (erlang:element(2, Axis@1) * Sin_angle))))
        + (erlang:element(4, Vector) * (Cos_angle + ((erlang:element(4, Axis@1)
        * erlang:element(4, Axis@1))
        * (1.0 - Cos_angle))))}.

-file("src/vec/vec3f.gleam", 642).
?DOC(
    " Return the equivalent of `vector |> subtract(position) |> fun() |> add(position)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0)\n"
    " |> anchor_position(\n"
    "   Vec3(2.0, 4.0, 0.0),\n"
    "   rotate(_, Vec3(6.0, 9.0, 1.0), maths.pi() *. 0.25),\n"
    " )\n"
    " // -> Vec3(26.08, -18.35, 27.2)\n"
    " ```\n"
).
-spec anchor_position(
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
    fun((vec@vec3:vec3(float())) -> vec@vec3:vec3(float()))
) -> vec@vec3:vec3(float()).
anchor_position(Vector, Position, Fun) ->
    _pipe = Vector,
    _pipe@1 = subtract(_pipe, Position),
    _pipe@2 = Fun(_pipe@1),
    add(_pipe@2, Position).

-file("src/vec/vec3f.gleam", 664).
?DOC(
    " Return the equivalent of `vector |> rotate(axis, float.negate(angle)) |> fun() |> rotate(axis, angle)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " Vec3(1.2, -3.4, 42.0)\n"
    " |> anchor_rotation(\n"
    "   Vec3(6.0, 9.0, 1.0),\n"
    "   maths.pi() *. 0.25,\n"
    "   add(_, Vec3(2.0, 4.0, 0.0)),\n"
    " )\n"
    " // -> Vec3(3.07, 0.63, 42.51)\n"
    " ```\n"
).
-spec anchor_rotation(
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
    float(),
    fun((vec@vec3:vec3(float())) -> vec@vec3:vec3(float()))
) -> vec@vec3:vec3(float()).
anchor_rotation(Vector, Axis, Angle, Fun) ->
    _pipe = Vector,
    _pipe@1 = rotate(_pipe, Axis, gleam@float:negate(Angle)),
    _pipe@2 = Fun(_pipe@1),
    rotate(_pipe@2, Axis, Angle).
