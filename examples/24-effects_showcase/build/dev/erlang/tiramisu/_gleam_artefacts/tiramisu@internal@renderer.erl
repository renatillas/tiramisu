-module(tiramisu@internal@renderer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/internal/renderer.gleam").
-export([get_renderer/1, get_scene/1, set_physics_world/2, get_physics_world/1, set_audio_manager/2, get_cameras_with_viewports/1]).
-export_type([scene/0, three_camera/0, web_g_l_renderer/0, dom_element/0, dimensions/0, renderer_options/0, renderer_state/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type scene() :: any().

-type three_camera() :: any().

-type web_g_l_renderer() :: any().

-type dom_element() :: any().

-type dimensions() :: {dimensions, float(), float()}.

-type renderer_options() :: {renderer_options,
        boolean(),
        boolean(),
        gleam@option:option(dimensions())}.

-type renderer_state(DHM) :: {renderer_state,
        web_g_l_renderer(),
        scene(),
        tiramisu@internal@object_cache:cache_state(),
        gleam@option:option(tiramisu@physics:physics_world(DHM)),
        gleam@dynamic:dynamic_()}.

-file("src/tiramisu/internal/renderer.gleam", 437).
?DOC(false).
-spec get_renderer(renderer_state(any())) -> web_g_l_renderer().
get_renderer(State) ->
    erlang:element(2, State).

-file("src/tiramisu/internal/renderer.gleam", 442).
?DOC(false).
-spec get_scene(renderer_state(any())) -> scene().
get_scene(State) ->
    erlang:element(3, State).

-file("src/tiramisu/internal/renderer.gleam", 447).
?DOC(false).
-spec set_physics_world(
    renderer_state(DID),
    gleam@option:option(tiramisu@physics:physics_world(DID))
) -> renderer_state(DID).
set_physics_world(State, World) ->
    {renderer_state,
        erlang:element(2, State),
        erlang:element(3, State),
        erlang:element(4, State),
        World,
        erlang:element(6, State)}.

-file("src/tiramisu/internal/renderer.gleam", 455).
?DOC(false).
-spec get_physics_world(renderer_state(DII)) -> gleam@option:option(tiramisu@physics:physics_world(DII)).
get_physics_world(State) ->
    erlang:element(5, State).

-file("src/tiramisu/internal/renderer.gleam", 462).
?DOC(false).
-spec set_audio_manager(renderer_state(DIM), gleam@dynamic:dynamic_()) -> renderer_state(DIM).
set_audio_manager(State, Audio_manager) ->
    {renderer_state,
        erlang:element(2, State),
        erlang:element(3, State),
        erlang:element(4, State),
        erlang:element(5, State),
        Audio_manager}.

-file("src/tiramisu/internal/renderer.gleam", 1412).
?DOC(false).
-spec dynamic_to_transform(
    gleam@dynamic:dynamic_(),
    gleam@dynamic:dynamic_(),
    gleam@dynamic:dynamic_()
) -> tiramisu@transform:transform().
dynamic_to_transform(Position, Rotation, Scale) ->
    _ = Position,
    _ = Rotation,
    _ = Scale,
    {transform,
        {vec3, +0.0, +0.0, +0.0},
        {vec3, +0.0, +0.0, +0.0},
        {vec3, 1.0, 1.0, 1.0}}.

-file("src/tiramisu/internal/renderer.gleam", 1798).
?DOC(false).
-spec get_cameras_with_viewports(renderer_state(any())) -> list({tiramisu@object3d:object3_d(),
    tiramisu@internal@object_cache:viewport()}).
get_cameras_with_viewports(State) ->
    _pipe = tiramisu@internal@object_cache:get_cameras_with_viewports(
        erlang:element(4, State)
    ),
    gleam@list:map(
        _pipe,
        fun(Entry) ->
            {Camera_obj, Viewport} = Entry,
            {tiramisu@internal@object_cache:unwrap_object(Camera_obj), Viewport}
        end
    ).
