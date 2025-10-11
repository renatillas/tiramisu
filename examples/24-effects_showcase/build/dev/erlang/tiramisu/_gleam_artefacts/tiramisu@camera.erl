-module(tiramisu@camera).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/camera.gleam").
-export([perspective/3, orthographic/6, camera_2d/2, camera_2d_screen_space/2, camera_2d_with_bounds/4, get_projection/1]).
-export_type([camera/0, camera_projection/0, camera_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Camera module - define viewpoints and projections for rendering.\n"
    "\n"
    " Cameras determine how your 3D scene is viewed. Use perspective cameras for 3D games\n"
    " and orthographic cameras for 2D games or UI elements.\n"
    "\n"
    " ## Quick Example\n"
    "\n"
    " ```gleam\n"
    " import tiramisu/camera\n"
    "\n"
    " // 3D perspective camera\n"
    " let assert Ok(cam_3d) = camera.perspective(fov: 75.0, aspect: 16.0 /. 9.0, near: 0.1, far: 1000.0)\n"
    "\n"
    " // 2D orthographic camera\n"
    " let cam_2d = camera.camera_2d(width: 800, height: 600)\n"
    " ```\n"
).

-opaque camera() :: {camera, camera_projection()}.

-type camera_projection() :: {perspective, float(), float(), float(), float()} |
    {orthographic, float(), float(), float(), float(), float(), float()}.

-type camera_error() :: {invalid_field_of_view, float()} |
    {invalid_aspect_ratio, float()} |
    {invalid_near_plane, float()} |
    {invalid_far_plane, float()} |
    {near_far_conflict, float(), float()}.

-file("src/tiramisu/camera.gleam", 75).
?DOC(
    " Create a perspective camera (for 3D games).\n"
    "\n"
    " Objects further away appear smaller, like in real life.\n"
    " The aspect ratio is automatically calculated from the viewport or renderer dimensions at render time.\n"
    "\n"
    " ## Parameters\n"
    " - `field_of_view`: Vertical FOV in degrees (typically 60-90)\n"
    " - `near`: Near clipping plane (objects closer are not rendered)\n"
    " - `far`: Far clipping plane (objects further are not rendered)\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(cam) = camera.perspective(\n"
    "   field_of_view: 75.0,\n"
    "   near: 0.1,\n"
    "   far: 1000.0,\n"
    " )\n"
    " ```\n"
).
-spec perspective(float(), float(), float()) -> {ok, camera()} |
    {error, camera_error()}.
perspective(Fov, Near, Far) ->
    gleam@bool:guard(
        (Fov =< +0.0) orelse (Fov >= 180.0),
        {error, {invalid_field_of_view, Fov}},
        fun() ->
            gleam@bool:guard(
                Near =< +0.0,
                {error, {invalid_near_plane, Near}},
                fun() ->
                    gleam@bool:guard(
                        Far =< +0.0,
                        {error, {invalid_far_plane, Far}},
                        fun() ->
                            gleam@bool:guard(
                                Near >= Far,
                                {error, {near_far_conflict, Near, Far}},
                                fun() ->
                                    {ok,
                                        {camera,
                                            {perspective, Fov, 1.0, Near, Far}}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/camera.gleam", 108).
?DOC(
    " Create an orthographic camera (for 2D games or isometric views).\n"
    "\n"
    " No perspective distortion - objects are the same size regardless of distance.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let cam = camera.orthographic(\n"
    "   left: -400.0, right: 400.0,\n"
    "   top: 300.0, bottom: -300.0,\n"
    "   near: 0.1, far: 1000.0,\n"
    " )\n"
    " ```\n"
).
-spec orthographic(float(), float(), float(), float(), float(), float()) -> camera().
orthographic(Left, Right, Top, Bottom, Near, Far) ->
    {camera, {orthographic, Left, Right, Top, Bottom, Near, Far}}.

-file("src/tiramisu/camera.gleam", 144).
?DOC(
    " Create a 2D camera centered at origin with world coordinates.\n"
    "\n"
    " Useful for 2D games where (0,0) is the center of the screen.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let cam = camera.camera_2d(width: 800, height: 600)\n"
    " scene.Camera(\n"
    "   id: \"main_camera\",\n"
    "   camera: cam,\n"
    "   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),\n"
    "   look_at: option.None,\n"
    "   active: True,\n"
    "   viewport: option.None,\n"
    " )\n"
    " // (0, 0) is screen center, positive Y is up\n"
    " ```\n"
).
-spec camera_2d(integer(), integer()) -> camera().
camera_2d(Width, Height) ->
    W = erlang:float(Width),
    H = erlang:float(Height),
    Half_w = W / 2.0,
    Half_h = H / 2.0,
    orthographic(+0.0 - Half_w, Half_w, Half_h, +0.0 - Half_h, 0.1, 1000.0).

-file("src/tiramisu/camera.gleam", 178).
?DOC(
    " Create a 2D camera with screen-space coordinates (top-left origin).\n"
    "\n"
    " Useful for UI or pixel-perfect 2D games where (0,0) is top-left corner.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let cam = camera.camera_2d_screen_space(width: 800, height: 600)\n"
    " scene.Camera(\n"
    "   id: \"ui_camera\",\n"
    "   camera: cam,\n"
    "   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),\n"
    "   look_at: option.None,\n"
    "   active: True,\n"
    "   viewport: option.None,\n"
    " )\n"
    " // (0, 0) is top-left, positive Y is down (like CSS)\n"
    " ```\n"
).
-spec camera_2d_screen_space(integer(), integer()) -> camera().
camera_2d_screen_space(Width, Height) ->
    W = erlang:float(Width),
    H = erlang:float(Height),
    orthographic(+0.0, W, +0.0, +0.0 - H, 0.1, 1000.0).

-file("src/tiramisu/camera.gleam", 210).
?DOC(
    " Create a 2D camera with custom bounds.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let cam = camera.camera_2d_with_bounds(\n"
    "   left: -100.0, right: 100.0,\n"
    "   top: 75.0, bottom: -75.0,\n"
    " )\n"
    " scene.Camera(\n"
    "   id: \"game_camera\",\n"
    "   camera: cam,\n"
    "   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),\n"
    "   look_at: option.None,\n"
    "   active: True,\n"
    "   viewport: option.None,\n"
    " )\n"
    " ```\n"
).
-spec camera_2d_with_bounds(float(), float(), float(), float()) -> camera().
camera_2d_with_bounds(Left, Right, Top, Bottom) ->
    orthographic(Left, Right, Top, Bottom, 0.1, 1000.0).

-file("src/tiramisu/camera.gleam", 230).
?DOC(false).
-spec get_projection(camera()) -> camera_projection().
get_projection(Camera) ->
    erlang:element(2, Camera).
