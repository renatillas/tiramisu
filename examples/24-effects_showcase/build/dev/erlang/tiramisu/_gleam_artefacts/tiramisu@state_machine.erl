-module(tiramisu@state_machine).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/state_machine.gleam").
-export([new/1, add_state/4, add_transition/5, set_default_blend/2, get_current_animation/1, transition_to/3, current_state/1, is_blending/1, blend_progress/1, update/3, get_state/2, state_ids/1, state_count/1, transition_count/1, to_playback/1]).
-export_type([condition/1, transition/2, state/1, state_machine_state/1, state_machine/2, animation_output/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type condition(AVVE) :: always |
    {after_duration, float()} |
    {custom, fun((AVVE) -> boolean())}.

-type transition(AVVF, AVVG) :: {transition,
        AVVF,
        AVVF,
        condition(AVVG),
        float()}.

-type state(AVVH) :: {state, AVVH, tiramisu@object3d:animation(), boolean()}.

-type state_machine_state(AVVI) :: {playing, AVVI, float()} |
    {blending, AVVI, AVVI, float(), float()}.

-opaque state_machine(AVVJ, AVVK) :: {state_machine,
        gleam@dict:dict(AVVJ, state(AVVJ)),
        list(transition(AVVJ, AVVK)),
        state_machine_state(AVVJ),
        float()}.

-type animation_output() :: none |
    {single, tiramisu@object3d:animation()} |
    {blend,
        tiramisu@object3d:animation(),
        tiramisu@object3d:animation(),
        float()}.

-file("src/tiramisu/state_machine.gleam", 55).
?DOC(" Create a new state machine with a starting state\n").
-spec new(AVVL) -> state_machine(AVVL, any()).
new(Initial_state) ->
    {state_machine, maps:new(), [], {playing, Initial_state, +0.0}, 0.2}.

-file("src/tiramisu/state_machine.gleam", 65).
?DOC(" Add a state to the state machine\n").
-spec add_state(
    state_machine(AVVP, AVVQ),
    AVVP,
    tiramisu@object3d:animation(),
    boolean()
) -> state_machine(AVVP, AVVQ).
add_state(Machine, Id, Animation, Looping) ->
    State = {state, Id, Animation, Looping},
    {state_machine,
        gleam@dict:insert(erlang:element(2, Machine), Id, State),
        erlang:element(3, Machine),
        erlang:element(4, Machine),
        erlang:element(5, Machine)}.

-file("src/tiramisu/state_machine.gleam", 76).
?DOC(" Add a transition between two states\n").
-spec add_transition(
    state_machine(AVVV, AVVW),
    AVVV,
    AVVV,
    condition(AVVW),
    float()
) -> state_machine(AVVV, AVVW).
add_transition(Machine, From, To, Condition, Blend_duration) ->
    Transition = {transition, From, To, Condition, Blend_duration},
    {state_machine,
        erlang:element(2, Machine),
        [Transition | erlang:element(3, Machine)],
        erlang:element(4, Machine),
        erlang:element(5, Machine)}.

-file("src/tiramisu/state_machine.gleam", 88).
?DOC(" Set the default blend duration for transitions\n").
-spec set_default_blend(state_machine(AVWC, AVWD), float()) -> state_machine(AVWC, AVWD).
set_default_blend(Machine, Duration) ->
    {state_machine,
        erlang:element(2, Machine),
        erlang:element(3, Machine),
        erlang:element(4, Machine),
        Duration}.

-file("src/tiramisu/state_machine.gleam", 154).
?DOC(
    " Get the current animation(s) to play\n"
    " Returns a single animation or blend information\n"
).
-spec get_current_animation(state_machine(any(), any())) -> animation_output().
get_current_animation(Machine) ->
    case erlang:element(4, Machine) of
        {playing, State_id, _} ->
            case gleam_stdlib:map_get(erlang:element(2, Machine), State_id) of
                {ok, State} ->
                    {single, erlang:element(3, State)};

                {error, _} ->
                    none
            end;

        {blending, From, To, Progress, Duration} ->
            case {gleam_stdlib:map_get(erlang:element(2, Machine), From),
                gleam_stdlib:map_get(erlang:element(2, Machine), To)} of
                {{ok, From_state}, {ok, To_state}} ->
                    Blend_factor = case Duration of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> Progress / Gleam@denominator
                    end,
                    {blend,
                        erlang:element(3, From_state),
                        erlang:element(3, To_state),
                        Blend_factor};

                {_, _} ->
                    none
            end
    end.

-file("src/tiramisu/state_machine.gleam", 192).
?DOC(" Manually trigger a transition to a specific state\n").
-spec transition_to(
    state_machine(AVWS, AVWT),
    AVWS,
    gleam@option:option(float())
) -> state_machine(AVWS, AVWT).
transition_to(Machine, Target, Blend_duration) ->
    Blend = gleam@option:unwrap(Blend_duration, erlang:element(5, Machine)),
    case erlang:element(4, Machine) of
        {playing, From, _} ->
            {state_machine,
                erlang:element(2, Machine),
                erlang:element(3, Machine),
                {blending, From, Target, +0.0, Blend},
                erlang:element(5, Machine)};

        {blending, _, Current_to, _, _} ->
            {state_machine,
                erlang:element(2, Machine),
                erlang:element(3, Machine),
                {blending, Current_to, Target, +0.0, Blend},
                erlang:element(5, Machine)}
    end.

-file("src/tiramisu/state_machine.gleam", 211).
?DOC(" Get the current state ID\n").
-spec current_state(state_machine(AVWZ, any())) -> AVWZ.
current_state(Machine) ->
    case erlang:element(4, Machine) of
        {playing, State_id, _} ->
            State_id;

        {blending, _, To, _, _} ->
            To
    end.

-file("src/tiramisu/state_machine.gleam", 219).
?DOC(" Check if currently blending\n").
-spec is_blending(state_machine(any(), any())) -> boolean().
is_blending(Machine) ->
    case erlang:element(4, Machine) of
        {playing, _, _} ->
            false;

        {blending, _, _, _, _} ->
            true
    end.

-file("src/tiramisu/state_machine.gleam", 227).
?DOC(" Get blend progress (0.0 to 1.0) if blending\n").
-spec blend_progress(state_machine(any(), any())) -> gleam@option:option(float()).
blend_progress(Machine) ->
    case erlang:element(4, Machine) of
        {playing, _, _} ->
            none;

        {blending, _, _, Progress, Duration} ->
            {some, case Duration of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> Progress / Gleam@denominator
                end}
    end.

-file("src/tiramisu/state_machine.gleam", 250).
?DOC(" Check if a condition is met\n").
-spec check_condition(condition(AVXU), AVXU) -> boolean().
check_condition(Condition, Context) ->
    case Condition of
        always ->
            true;

        {after_duration, _} ->
            false;

        {custom, Check} ->
            Check(Context)
    end.

-file("src/tiramisu/state_machine.gleam", 237).
?DOC(" Find a valid transition from the current state\n").
-spec find_valid_transition(state_machine(AVXM, AVXN), AVXM, AVXN) -> {ok,
        transition(AVXM, AVXN)} |
    {error, nil}.
find_valid_transition(Machine, From_state, Context) ->
    _pipe = erlang:element(3, Machine),
    gleam@list:find(
        _pipe,
        fun(Transition) ->
            (erlang:element(2, Transition) =:= From_state) andalso check_condition(
                erlang:element(4, Transition),
                Context
            )
        end
    ).

-file("src/tiramisu/state_machine.gleam", 97).
?DOC(
    " Update the state machine (should be called every frame)\n"
    " Returns updated state machine and whether a transition occurred\n"
).
-spec update(state_machine(AVWI, AVWJ), AVWJ, float()) -> {state_machine(AVWI, AVWJ),
    boolean()}.
update(Machine, Context, Delta_time) ->
    case erlang:element(4, Machine) of
        {playing, State_id, Elapsed} ->
            New_elapsed = Elapsed + Delta_time,
            case find_valid_transition(Machine, State_id, Context) of
                {ok, Transition} ->
                    New_current = {blending,
                        State_id,
                        erlang:element(3, Transition),
                        +0.0,
                        erlang:element(5, Transition)},
                    {{state_machine,
                            erlang:element(2, Machine),
                            erlang:element(3, Machine),
                            New_current,
                            erlang:element(5, Machine)},
                        true};

                {error, _} ->
                    {{state_machine,
                            erlang:element(2, Machine),
                            erlang:element(3, Machine),
                            {playing, State_id, New_elapsed},
                            erlang:element(5, Machine)},
                        false}
            end;

        {blending, From, To, Progress, Duration} ->
            New_progress = Progress + Delta_time,
            case New_progress >= Duration of
                true ->
                    {{state_machine,
                            erlang:element(2, Machine),
                            erlang:element(3, Machine),
                            {playing, To, +0.0},
                            erlang:element(5, Machine)},
                        true};

                false ->
                    {{state_machine,
                            erlang:element(2, Machine),
                            erlang:element(3, Machine),
                            {blending, From, To, New_progress, Duration},
                            erlang:element(5, Machine)},
                        false}
            end
    end.

-file("src/tiramisu/state_machine.gleam", 260).
?DOC(" Get a state by ID\n").
-spec get_state(state_machine(AVXW, any()), AVXW) -> {ok, state(AVXW)} |
    {error, nil}.
get_state(Machine, Id) ->
    gleam_stdlib:map_get(erlang:element(2, Machine), Id).

-file("src/tiramisu/state_machine.gleam", 268).
?DOC(" Get all state IDs\n").
-spec state_ids(state_machine(AVYD, any())) -> list(AVYD).
state_ids(Machine) ->
    maps:keys(erlang:element(2, Machine)).

-file("src/tiramisu/state_machine.gleam", 273).
?DOC(" Get the number of states\n").
-spec state_count(state_machine(any(), any())) -> integer().
state_count(Machine) ->
    maps:size(erlang:element(2, Machine)).

-file("src/tiramisu/state_machine.gleam", 278).
?DOC(" Get the number of transitions\n").
-spec transition_count(state_machine(any(), any())) -> integer().
transition_count(Machine) ->
    erlang:length(erlang:element(3, Machine)).

-file("src/tiramisu/state_machine.gleam", 283).
?DOC(" Convert AnimationOutput to AnimationPlayback for use with Model3D\n").
-spec to_playback(animation_output()) -> gleam@option:option(tiramisu@object3d:animation_playback()).
to_playback(Output) ->
    case Output of
        none ->
            none;

        {single, Anim} ->
            {some, {single_animation, Anim}};

        {blend, From, To, Factor} ->
            {some, {blended_animations, From, To, Factor}}
    end.
