-module(tiramisu@effect).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/effect.gleam").
-export([none/0, from/1, batch/1, map/2, run/2]).
-export_type([effect/1, easing/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Effect system for managing side effects in Tiramisu.\n"
    "\n"
    " Effects represent side effects as immutable data, following The Elm Architecture.\n"
    " Your `update` function returns effects that the runtime executes for you.\n"
    "\n"
    " ## Quick Example\n"
    "\n"
    " ```gleam\n"
    " import tiramisu/effect\n"
    "\n"
    " type Msg {\n"
    "   Tick\n"
    "   PlayerMoved(Vec3(Float))\n"
    " }\n"
    "\n"
    " fn update(model: Model, msg: Msg, ctx: Context) {\n"
    "   case msg {\n"
    "     Tick -> #(\n"
    "       update_physics(model),\n"
    "       effect.batch([\n"
    "         effect.tick(Tick),  // Request next frame\n"
    "         effect.from(fn(dispatch) {\n"
    "           // Custom side effect\n"
    "           log_position(model.player_pos)\n"
    "           dispatch(PlayerMoved(model.player_pos))\n"
    "         }),\n"
    "       ]),\n"
    "     )\n"
    "     PlayerMoved(_) -> #(model, effect.none())\n"
    "   }\n"
    " }\n"
    " ```\n"
).

-opaque effect(DIB) :: {effect, fun((fun((DIB) -> nil)) -> nil)}.

-type easing() :: linear |
    ease_in_quad |
    ease_out_quad |
    ease_in_out_quad |
    ease_in_cubic |
    ease_out_cubic |
    ease_in_out_cubic.

-file("src/tiramisu/effect.gleam", 61).
?DOC(
    " Create an effect that performs no side effects.\n"
    "\n"
    " Use when you want to update state without triggering any effects.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " fn update(model, msg, ctx) {\n"
    "   case msg {\n"
    "     Idle -> #(model, effect.none())\n"
    "   }\n"
    " }\n"
    " ```\n"
).
-spec none() -> effect(any()).
none() ->
    {effect, fun(_) -> nil end}.

-file("src/tiramisu/effect.gleam", 77).
?DOC(
    " Create a custom effect from a function.\n"
    "\n"
    " The function receives a `dispatch` callback to send messages back to your `update` function.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " effect.from(fn(dispatch) {\n"
    "   log(\"Player score: \" <> int.to_string(score))\n"
    "   dispatch(ScoreLogged)\n"
    " })\n"
    " ```\n"
).
-spec from(fun((fun((DIE) -> nil)) -> nil)) -> effect(DIE).
from(Effect) ->
    {effect, Effect}.

-file("src/tiramisu/effect.gleam", 94).
?DOC(
    " Batch multiple effects to run them together.\n"
    "\n"
    " All effects execute in order during the same frame.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " effect.batch([\n"
    "   effect.tick(NextFrame),\n"
    "   play_sound_effect(\"jump.wav\"),\n"
    "   update_scoreboard(score),\n"
    " ])\n"
    " ```\n"
).
-spec batch(list(effect(DIG))) -> effect(DIG).
batch(Effects) ->
    {effect, fun(Dispatch) -> _pipe = Effects,
            gleam@list:each(
                _pipe,
                fun(Effect) ->
                    {effect, Perform} = Effect,
                    Perform(Dispatch)
                end
            ) end}.

-file("src/tiramisu/effect.gleam", 114).
?DOC(
    " Map effect messages to a different type.\n"
    "\n"
    " Useful when composing effects from subcomponents.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let player_effect = player.update(player_model, player_msg)\n"
    " effect.map(player_effect, PlayerMsg)\n"
    " ```\n"
).
-spec map(effect(DIK), fun((DIK) -> DIM)) -> effect(DIM).
map(Effect, F) ->
    {effect,
        fun(Dispatch) ->
            {effect, Perform} = Effect,
            Perform(fun(Msg) -> Dispatch(F(Msg)) end)
        end}.

-file("src/tiramisu/effect.gleam", 139).
?DOC(false).
-spec run(effect(DIR), fun((DIR) -> nil)) -> nil.
run(Effect, Dispatch) ->
    {effect, Perform} = Effect,
    Perform(Dispatch).

-file("src/tiramisu/effect.gleam", 437).
-spec easing_to_string(easing()) -> binary().
easing_to_string(Easing) ->
    case Easing of
        linear ->
            <<"linear"/utf8>>;

        ease_in_quad ->
            <<"easeInQuad"/utf8>>;

        ease_out_quad ->
            <<"easeOutQuad"/utf8>>;

        ease_in_out_quad ->
            <<"easeInOutQuad"/utf8>>;

        ease_in_cubic ->
            <<"easeInCubic"/utf8>>;

        ease_out_cubic ->
            <<"easeOutCubic"/utf8>>;

        ease_in_out_cubic ->
            <<"easeInOutCubic"/utf8>>
    end.
