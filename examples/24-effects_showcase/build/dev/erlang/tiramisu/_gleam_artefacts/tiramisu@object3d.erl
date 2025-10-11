-module(tiramisu@object3d).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/object3d.gleam").
-export([new_animation/1, set_loop/2, set_speed/2, set_weight/2]).
-export_type([object3_d/0, animation_clip/0, loop_mode/0, animation/0, animation_playback/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type object3_d() :: any().

-type animation_clip() :: any().

-type loop_mode() :: loop_once | loop_repeat.

-type animation() :: {animation,
        animation_clip(),
        loop_mode(),
        float(),
        float()}.

-type animation_playback() :: {single_animation, animation()} |
    {blended_animations, animation(), animation(), float()}.

-file("src/tiramisu/object3d.gleam", 27).
?DOC(" Create an animation from a clip with default settings (loop repeat, normal speed, full weight)\n").
-spec new_animation(animation_clip()) -> animation().
new_animation(Clip) ->
    {animation, Clip, loop_repeat, 1.0, 1.0}.

-file("src/tiramisu/object3d.gleam", 32).
?DOC(" Set the loop mode\n").
-spec set_loop(animation(), loop_mode()) -> animation().
set_loop(Anim, Mode) ->
    {animation,
        erlang:element(2, Anim),
        Mode,
        erlang:element(4, Anim),
        erlang:element(5, Anim)}.

-file("src/tiramisu/object3d.gleam", 37).
?DOC(" Set the animation speed (1.0 = normal, 2.0 = double speed, 0.5 = half speed)\n").
-spec set_speed(animation(), float()) -> animation().
set_speed(Anim, Speed) ->
    {animation,
        erlang:element(2, Anim),
        erlang:element(3, Anim),
        Speed,
        erlang:element(5, Anim)}.

-file("src/tiramisu/object3d.gleam", 42).
?DOC(" Set the animation weight (0.0 to 1.0, for blending animations)\n").
-spec set_weight(animation(), float()) -> animation().
set_weight(Anim, Weight) ->
    {animation,
        erlang:element(2, Anim),
        erlang:element(3, Anim),
        erlang:element(4, Anim),
        Weight}.
