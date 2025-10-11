-module(tiramisu@animation).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/animation.gleam").
-export([ease/2, tween/5, update_tween/2, get_tween_value/1, is_tween_complete/1, tween_float/4, tween_vec3/4, tween_transform/4, reset_tween/1, reverse_tween/1]).
-export_type([easing/0, tween/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Animation system - tweens and easing functions for smooth interpolation.\n"
    "\n"
    " Provides easing functions and tween helpers for animating values over time.\n"
    "\n"
    " ## Quick Example\n"
    "\n"
    " ```gleam\n"
    " import tiramisu/animation\n"
    "\n"
    " // Create a tween from 0 to 100 over 2 seconds\n"
    " let tween = animation.tween_float(\n"
    "   from: 0.0,\n"
    "   to: 100.0,\n"
    "   duration: 2.0,\n"
    "   easing: animation.EaseOutQuad,\n"
    " )\n"
    "\n"
    " // Update in game loop\n"
    " let tween = animation.update_tween(tween, ctx.delta_time)\n"
    " let current_value = animation.tween_value(tween)\n"
    " let done = animation.is_tween_complete(tween)\n"
    " ```\n"
).

-type easing() :: linear |
    ease_in_quad |
    ease_out_quad |
    ease_in_out_quad |
    ease_in_cubic |
    ease_out_cubic |
    ease_in_out_cubic |
    ease_in_sine |
    ease_out_sine |
    ease_in_out_sine.

-type tween(AUOZ) :: {tween,
        AUOZ,
        AUOZ,
        float(),
        float(),
        easing(),
        fun((AUOZ, AUOZ, float()) -> AUOZ)}.

-file("src/tiramisu/animation.gleam", 47).
?DOC(" Apply easing function to a value t in [0, 1]\n").
-spec ease(easing(), float()) -> float().
ease(Easing, T) ->
    T@1 = gleam@float:clamp(T, +0.0, 1.0),
    case Easing of
        linear ->
            T@1;

        ease_in_quad ->
            T@1 * T@1;

        ease_out_quad ->
            T@1 * (2.0 - T@1);

        ease_in_out_quad ->
            case T@1 < 0.5 of
                true ->
                    (2.0 * T@1) * T@1;

                false ->
                    T_adj = T@1 - 1.0,
                    -1.0 * (((2.0 * T_adj) * T_adj) - 1.0)
            end;

        ease_in_cubic ->
            (T@1 * T@1) * T@1;

        ease_out_cubic ->
            T_adj@1 = T@1 - 1.0,
            ((T_adj@1 * T_adj@1) * T_adj@1) + 1.0;

        ease_in_out_cubic ->
            case T@1 < 0.5 of
                true ->
                    ((4.0 * T@1) * T@1) * T@1;

                false ->
                    T_adj@2 = (2.0 * T@1) - 2.0,
                    (((T_adj@2 * T_adj@2) * T_adj@2) + 2.0) / 2.0
            end;

        ease_in_sine ->
            Angle = T@1 * 1.5707963267948966,
            1.0 - gleam_community@maths:cos(Angle);

        ease_out_sine ->
            Angle@1 = T@1 * 1.5707963267948966,
            gleam_community@maths:sin(Angle@1);

        ease_in_out_sine ->
            Angle@2 = 3.141592653589793 * T@1,
            (1.0 - gleam_community@maths:cos(Angle@2)) / 2.0
    end.

-file("src/tiramisu/animation.gleam", 104).
?DOC(" Create a new tween\n").
-spec tween(AUPA, AUPA, float(), easing(), fun((AUPA, AUPA, float()) -> AUPA)) -> tween(AUPA).
tween(Start, End, Duration, Easing, Lerp_fn) ->
    {tween, Start, End, Duration, +0.0, Easing, Lerp_fn}.

-file("src/tiramisu/animation.gleam", 122).
?DOC(" Update a tween with delta time\n").
-spec update_tween(tween(AUPC), float()) -> tween(AUPC).
update_tween(Tween, Delta) ->
    {tween,
        erlang:element(2, Tween),
        erlang:element(3, Tween),
        erlang:element(4, Tween),
        erlang:element(5, Tween) + Delta,
        erlang:element(6, Tween),
        erlang:element(7, Tween)}.

-file("src/tiramisu/animation.gleam", 127).
?DOC(" Get the current value of a tween\n").
-spec get_tween_value(tween(AUPF)) -> AUPF.
get_tween_value(Tween) ->
    T = case erlang:element(5, Tween) >= erlang:element(4, Tween) of
        true ->
            1.0;

        false ->
            case erlang:element(4, Tween) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:element(5, Tween) / Gleam@denominator
            end
    end,
    Eased_t = ease(erlang:element(6, Tween), T),
    (erlang:element(7, Tween))(
        erlang:element(2, Tween),
        erlang:element(3, Tween),
        Eased_t
    ).

-file("src/tiramisu/animation.gleam", 138).
?DOC(" Check if a tween is complete\n").
-spec is_tween_complete(tween(any())) -> boolean().
is_tween_complete(Tween) ->
    erlang:element(5, Tween) >= erlang:element(4, Tween).

-file("src/tiramisu/animation.gleam", 145).
?DOC(" Tween a Float value\n").
-spec tween_float(float(), float(), float(), easing()) -> tween(float()).
tween_float(Start, End, Duration, Easing) ->
    tween(Start, End, Duration, Easing, fun(A, B, T) -> A + ((B - A) * T) end).

-file("src/tiramisu/animation.gleam", 154).
-spec tween_vec3(
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
    float(),
    easing()
) -> tween(vec@vec3:vec3(float())).
tween_vec3(Start, End, Duration, Easing) ->
    tween(
        Start,
        End,
        Duration,
        Easing,
        fun(A, B, T) ->
            {vec3,
                erlang:element(2, A) + ((erlang:element(2, B) - erlang:element(
                    2,
                    A
                ))
                * T),
                erlang:element(3, A) + ((erlang:element(3, B) - erlang:element(
                    3,
                    A
                ))
                * T),
                erlang:element(4, A) + ((erlang:element(4, B) - erlang:element(
                    4,
                    A
                ))
                * T)}
        end
    ).

-file("src/tiramisu/animation.gleam", 169).
-spec tween_transform(
    tiramisu@transform:transform(),
    tiramisu@transform:transform(),
    float(),
    easing()
) -> tween(tiramisu@transform:transform()).
tween_transform(Start, End, Duration, Easing) ->
    tween(Start, End, Duration, Easing, fun tiramisu@transform:lerp/3).

-file("src/tiramisu/animation.gleam", 179).
?DOC(" Reset a tween to start\n").
-spec reset_tween(tween(AUPP)) -> tween(AUPP).
reset_tween(Tween) ->
    {tween,
        erlang:element(2, Tween),
        erlang:element(3, Tween),
        erlang:element(4, Tween),
        +0.0,
        erlang:element(6, Tween),
        erlang:element(7, Tween)}.

-file("src/tiramisu/animation.gleam", 184).
?DOC(" Reverse a tween (swap start and end)\n").
-spec reverse_tween(tween(AUPS)) -> tween(AUPS).
reverse_tween(Tween) ->
    {tween,
        erlang:element(3, Tween),
        erlang:element(2, Tween),
        erlang:element(4, Tween),
        erlang:element(5, Tween),
        erlang:element(6, Tween),
        erlang:element(7, Tween)}.
