-module(tiramisu@internal@camera_manager).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/internal/camera_manager.gleam").
-export([init/0, set_active/2, get_active/1, unwrap_camera/1]).
-export_type([three_camera/0, camera_state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-opaque three_camera() :: {three_camera, gleam@dynamic:dynamic_()}.

-type camera_state() :: {camera_state, gleam@option:option(three_camera())}.

-file("src/tiramisu/internal/camera_manager.gleam", 23).
?DOC(false).
-spec init() -> camera_state().
init() ->
    {camera_state, none}.

-file("src/tiramisu/internal/camera_manager.gleam", 28).
?DOC(false).
-spec set_active(camera_state(), three_camera()) -> camera_state().
set_active(_, Camera) ->
    {camera_state, {some, Camera}}.

-file("src/tiramisu/internal/camera_manager.gleam", 33).
?DOC(false).
-spec get_active(camera_state()) -> gleam@option:option(three_camera()).
get_active(State) ->
    erlang:element(2, State).

-file("src/tiramisu/internal/camera_manager.gleam", 39).
?DOC(false).
-spec unwrap_camera(three_camera()) -> gleam@dynamic:dynamic_().
unwrap_camera(Camera) ->
    erlang:element(2, Camera).
