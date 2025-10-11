-module(tiramisu@internal@object_cache).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/internal/object_cache.gleam").
-export([init/0, get_all_objects/1, get_all_mixers/1, get_cameras_with_viewports/1, get_all_particle_systems/1, clear/1, unwrap_object/1, unwrap_mixer/1, unwrap_action/1, unwrap_particle_system/1, wrap_object/1, wrap_mixer/1, wrap_action/1, wrap_particle_system/1]).
-export_type([three_object/0, animation_mixer/0, animation_action/0, animation_actions/0, particle_system/0, viewport/0, cache_state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-opaque three_object() :: {three_object, tiramisu@object3d:object3_d()}.

-opaque animation_mixer() :: {animation_mixer, gleam@dynamic:dynamic_()}.

-opaque animation_action() :: {animation_action, gleam@dynamic:dynamic_()}.

-type animation_actions() :: {single_action, animation_action()} |
    {blended_actions, animation_action(), animation_action()}.

-opaque particle_system() :: {particle_system, gleam@dynamic:dynamic_()}.

-type viewport() :: {viewport, float(), float(), float(), float()}.

-type cache_state() :: {cache_state,
        gleam@dict:dict(binary(), three_object()),
        gleam@dict:dict(binary(), animation_mixer()),
        gleam@dict:dict(binary(), animation_actions()),
        gleam@dict:dict(binary(), viewport()),
        gleam@dict:dict(binary(), particle_system())}.

-file("src/tiramisu/internal/object_cache.gleam", 71).
?DOC(false).
-spec init() -> cache_state().
init() ->
    {cache_state, maps:new(), maps:new(), maps:new(), maps:new(), maps:new()}.

-file("src/tiramisu/internal/object_cache.gleam", 104).
?DOC(false).
-spec get_all_objects(cache_state()) -> list({binary(), three_object()}).
get_all_objects(Cache) ->
    maps:to_list(erlang:element(2, Cache)).

-file("src/tiramisu/internal/object_cache.gleam", 131).
?DOC(false).
-spec get_all_mixers(cache_state()) -> list({binary(), animation_mixer()}).
get_all_mixers(Cache) ->
    maps:to_list(erlang:element(3, Cache)).

-file("src/tiramisu/internal/object_cache.gleam", 187).
?DOC(false).
-spec get_cameras_with_viewports(cache_state()) -> list({three_object(),
    viewport()}).
get_cameras_with_viewports(Cache) ->
    _pipe = maps:to_list(erlang:element(5, Cache)),
    gleam@list:filter_map(
        _pipe,
        fun(Entry) ->
            {Id, Viewport} = Entry,
            case gleam_stdlib:map_get(erlang:element(2, Cache), Id) of
                {ok, Camera_obj} ->
                    {ok, {Camera_obj, Viewport}};

                {error, _} ->
                    {error, nil}
            end
        end
    ).

-file("src/tiramisu/internal/object_cache.gleam", 230).
?DOC(false).
-spec get_all_particle_systems(cache_state()) -> list({binary(),
    particle_system()}).
get_all_particle_systems(Cache) ->
    maps:to_list(erlang:element(6, Cache)).

-file("src/tiramisu/internal/object_cache.gleam", 252).
?DOC(false).
-spec clear(cache_state()) -> cache_state().
clear(_) ->
    init().

-file("src/tiramisu/internal/object_cache.gleam", 262).
?DOC(false).
-spec unwrap_object(three_object()) -> tiramisu@object3d:object3_d().
unwrap_object(Object) ->
    erlang:element(2, Object).

-file("src/tiramisu/internal/object_cache.gleam", 268).
?DOC(false).
-spec unwrap_mixer(animation_mixer()) -> gleam@dynamic:dynamic_().
unwrap_mixer(Mixer) ->
    erlang:element(2, Mixer).

-file("src/tiramisu/internal/object_cache.gleam", 274).
?DOC(false).
-spec unwrap_action(animation_action()) -> gleam@dynamic:dynamic_().
unwrap_action(Action) ->
    erlang:element(2, Action).

-file("src/tiramisu/internal/object_cache.gleam", 280).
?DOC(false).
-spec unwrap_particle_system(particle_system()) -> gleam@dynamic:dynamic_().
unwrap_particle_system(System) ->
    erlang:element(2, System).

-file("src/tiramisu/internal/object_cache.gleam", 286).
?DOC(false).
-spec wrap_object(tiramisu@object3d:object3_d()) -> three_object().
wrap_object(Object) ->
    {three_object, Object}.

-file("src/tiramisu/internal/object_cache.gleam", 292).
?DOC(false).
-spec wrap_mixer(gleam@dynamic:dynamic_()) -> animation_mixer().
wrap_mixer(Mixer) ->
    {animation_mixer, Mixer}.

-file("src/tiramisu/internal/object_cache.gleam", 298).
?DOC(false).
-spec wrap_action(gleam@dynamic:dynamic_()) -> animation_action().
wrap_action(Action) ->
    {animation_action, Action}.

-file("src/tiramisu/internal/object_cache.gleam", 304).
?DOC(false).
-spec wrap_particle_system(gleam@dynamic:dynamic_()) -> particle_system().
wrap_particle_system(Data) ->
    {particle_system, Data}.
