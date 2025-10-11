-module(tiramisu@particle_emitter).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/particle_emitter.gleam").
-export([new/0, rate/2, lifetime/2, velocity/2, velocity_variance/2, size/2, size_variance/2, color/2, fade_to/2, gravity/2, max_particles/2, build/1, get_emit_rate/1, get_max_particles/1, get_velocity/1, get_velocity_variance/1, get_size/1, get_size_variance/1, get_lifetime/1, get_lifetime_variance/1, get_start_color/1, get_end_color/1, get_gravity_scale/1]).
-export_type([particle_emitter/0, particle_error/0, particle_emitter_builder/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque particle_emitter() :: {particle_emitter,
        float(),
        float(),
        vec@vec3:vec3(float()),
        vec@vec3:vec3(float()),
        float(),
        float(),
        integer(),
        gleam@option:option(integer()),
        float(),
        integer()}.

-type particle_error() :: {negative_rate, float()} |
    {negative_lifetime, float()} |
    {negative_size, float()} |
    {negative_size_variance, float()} |
    {out_of_bounds_color, integer()} |
    {out_of_bounds_color_end, integer()} |
    {negative_max_particles, integer()}.

-opaque particle_emitter_builder() :: {particle_emitter_builder,
        float(),
        float(),
        vec@vec3:vec3(float()),
        vec@vec3:vec3(float()),
        float(),
        float(),
        integer(),
        gleam@option:option(integer()),
        float(),
        integer()}.

-file("src/tiramisu/particle_emitter.gleam", 53).
-spec particle_emitter(
    float(),
    float(),
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
    float(),
    float(),
    integer(),
    gleam@option:option(integer()),
    float(),
    integer()
) -> {ok, particle_emitter()} | {error, particle_error()}.
particle_emitter(
    Rate,
    Lifetime,
    Velocity,
    Velocity_variance,
    Size,
    Size_variance,
    Color,
    Color_end,
    Gravity_scale,
    Max_particles
) ->
    gleam@bool:guard(
        Rate =< +0.0,
        {error, {negative_rate, Rate}},
        fun() ->
            gleam@bool:guard(
                Lifetime =< +0.0,
                {error, {negative_lifetime, Lifetime}},
                fun() ->
                    gleam@bool:guard(
                        Size =< +0.0,
                        {error, {negative_size, Size}},
                        fun() ->
                            gleam@bool:guard(
                                Size_variance < +0.0,
                                {error, {negative_size_variance, Size_variance}},
                                fun() ->
                                    gleam@bool:guard(
                                        (Color < 16#000000) orelse (Color > 16#ffffff),
                                        {error, {out_of_bounds_color, Color}},
                                        fun() ->
                                            gleam@bool:guard(case Color_end of
                                                    {some, C} ->
                                                        (C < 16#000000) orelse (C
                                                        > 16#ffffff);

                                                    none ->
                                                        false
                                                end, {error,
                                                    {out_of_bounds_color_end,
                                                        case Color_end of
                                                            {some, C@1} ->
                                                                C@1;

                                                            none ->
                                                                0
                                                        end}}, fun() ->
                                                    gleam@bool:guard(
                                                        Max_particles =< 0,
                                                        {error,
                                                            {negative_max_particles,
                                                                Max_particles}},
                                                        fun() ->
                                                            {ok,
                                                                {particle_emitter,
                                                                    Rate,
                                                                    Lifetime,
                                                                    Velocity,
                                                                    Velocity_variance,
                                                                    Size,
                                                                    Size_variance,
                                                                    Color,
                                                                    Color_end,
                                                                    Gravity_scale,
                                                                    Max_particles}}
                                                        end
                                                    )
                                                end)
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/particle_emitter.gleam", 152).
?DOC(
    " Create a new particle emitter builder with default values.\n"
    "\n"
    " Defaults:\n"
    " - rate: 50.0 particles/sec\n"
    " - lifetime: 1.0 seconds\n"
    " - velocity: upward (0, 2, 0)\n"
    " - velocity_variance: (1, 1, 1)\n"
    " - size: 0.1\n"
    " - size_variance: 0.05\n"
    " - color: white (0xffffff)\n"
    " - color_end: None\n"
    " - gravity_scale: 1.0\n"
    " - max_particles: 1000\n"
).
-spec new() -> particle_emitter_builder().
new() ->
    {particle_emitter_builder,
        50.0,
        1.0,
        {vec3, +0.0, 2.0, +0.0},
        {vec3, 1.0, 1.0, 1.0},
        0.1,
        0.05,
        16#ffffff,
        none,
        1.0,
        1000}.

-file("src/tiramisu/particle_emitter.gleam", 168).
?DOC(" Set the emission rate (particles per second).\n").
-spec rate(particle_emitter_builder(), float()) -> particle_emitter_builder().
rate(Builder, Rate) ->
    {particle_emitter_builder,
        Rate,
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/particle_emitter.gleam", 176).
?DOC(" Set how long particles live (in seconds).\n").
-spec lifetime(particle_emitter_builder(), float()) -> particle_emitter_builder().
lifetime(Builder, Lifetime) ->
    {particle_emitter_builder,
        erlang:element(2, Builder),
        Lifetime,
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/particle_emitter.gleam", 184).
?DOC(" Set the base velocity for new particles.\n").
-spec velocity(particle_emitter_builder(), vec@vec3:vec3(float())) -> particle_emitter_builder().
velocity(Builder, Velocity) ->
    {particle_emitter_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        Velocity,
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/particle_emitter.gleam", 192).
?DOC(" Set random variance added to velocity (per axis).\n").
-spec velocity_variance(particle_emitter_builder(), vec@vec3:vec3(float())) -> particle_emitter_builder().
velocity_variance(Builder, Variance) ->
    {particle_emitter_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        Variance,
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/particle_emitter.gleam", 200).
?DOC(" Set the base particle size.\n").
-spec size(particle_emitter_builder(), float()) -> particle_emitter_builder().
size(Builder, Size) ->
    {particle_emitter_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        Size,
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/particle_emitter.gleam", 208).
?DOC(" Set random variance added to size.\n").
-spec size_variance(particle_emitter_builder(), float()) -> particle_emitter_builder().
size_variance(Builder, Variance) ->
    {particle_emitter_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        Variance,
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/particle_emitter.gleam", 216).
?DOC(" Set the start color for particles.\n").
-spec color(particle_emitter_builder(), integer()) -> particle_emitter_builder().
color(Builder, Color) ->
    {particle_emitter_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        Color,
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/particle_emitter.gleam", 224).
?DOC(" Set the end color for particles (for fading effect).\n").
-spec fade_to(particle_emitter_builder(), integer()) -> particle_emitter_builder().
fade_to(Builder, Color) ->
    {particle_emitter_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        {some, Color},
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/particle_emitter.gleam", 232).
?DOC(" Set gravity multiplier (1.0 = normal, 0.0 = no gravity).\n").
-spec gravity(particle_emitter_builder(), float()) -> particle_emitter_builder().
gravity(Builder, Scale) ->
    {particle_emitter_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        Scale,
        erlang:element(11, Builder)}.

-file("src/tiramisu/particle_emitter.gleam", 240).
?DOC(" Set maximum number of active particles.\n").
-spec max_particles(particle_emitter_builder(), integer()) -> particle_emitter_builder().
max_particles(Builder, Max) ->
    {particle_emitter_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        Max}.

-file("src/tiramisu/particle_emitter.gleam", 248).
?DOC(" Build the particle emitter from the builder (validates parameters).\n").
-spec build(particle_emitter_builder()) -> {ok, particle_emitter()} |
    {error, particle_error()}.
build(Builder) ->
    particle_emitter(
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)
    ).

-file("src/tiramisu/particle_emitter.gleam", 268).
?DOC(false).
-spec get_emit_rate(particle_emitter()) -> integer().
get_emit_rate(Emitter) ->
    _pipe = erlang:element(2, Emitter),
    erlang:round(_pipe).

-file("src/tiramisu/particle_emitter.gleam", 273).
?DOC(false).
-spec get_max_particles(particle_emitter()) -> integer().
get_max_particles(Emitter) ->
    erlang:element(11, Emitter).

-file("src/tiramisu/particle_emitter.gleam", 278).
?DOC(false).
-spec get_velocity(particle_emitter()) -> vec@vec3:vec3(float()).
get_velocity(Emitter) ->
    erlang:element(4, Emitter).

-file("src/tiramisu/particle_emitter.gleam", 283).
?DOC(false).
-spec get_velocity_variance(particle_emitter()) -> vec@vec3:vec3(float()).
get_velocity_variance(Emitter) ->
    erlang:element(5, Emitter).

-file("src/tiramisu/particle_emitter.gleam", 288).
?DOC(false).
-spec get_size(particle_emitter()) -> float().
get_size(Emitter) ->
    erlang:element(6, Emitter).

-file("src/tiramisu/particle_emitter.gleam", 293).
?DOC(false).
-spec get_size_variance(particle_emitter()) -> float().
get_size_variance(Emitter) ->
    erlang:element(7, Emitter).

-file("src/tiramisu/particle_emitter.gleam", 298).
?DOC(false).
-spec get_lifetime(particle_emitter()) -> float().
get_lifetime(Emitter) ->
    erlang:element(3, Emitter).

-file("src/tiramisu/particle_emitter.gleam", 303).
?DOC(false).
-spec get_lifetime_variance(particle_emitter()) -> float().
get_lifetime_variance(Emitter) ->
    erlang:element(3, Emitter) * 0.2.

-file("src/tiramisu/particle_emitter.gleam", 308).
?DOC(false).
-spec get_start_color(particle_emitter()) -> integer().
get_start_color(Emitter) ->
    erlang:element(8, Emitter).

-file("src/tiramisu/particle_emitter.gleam", 313).
?DOC(false).
-spec get_end_color(particle_emitter()) -> integer().
get_end_color(Emitter) ->
    case erlang:element(9, Emitter) of
        {some, C} ->
            C;

        none ->
            erlang:element(8, Emitter)
    end.

-file("src/tiramisu/particle_emitter.gleam", 321).
?DOC(false).
-spec get_gravity_scale(particle_emitter()) -> float().
get_gravity_scale(Emitter) ->
    erlang:element(10, Emitter).
