-module(tiramisu@transform).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/transform.gleam").
-export([at/1, with_position/2, with_rotation/2, with_scale/2, lerp/3, compose/2, look_at/2, translate/2, rotate_by/2, scale_by/2, scale_uniform/2, rotate_y/2, rotate_x/2, rotate_z/2]).
-export_type([transform/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Transform module - position, rotation, and scale for 3D objects.\n"
    "\n"
    " Transforms define where objects are in 3D space, how they're rotated, and their size.\n"
    " All transforms are immutable - functions return new transforms instead of modifying existing ones.\n"
    "\n"
    " ## Quick Example\n"
    "\n"
    " ```gleam\n"
    " import tiramisu/transform\n"
    " import vec/vec3\n"
    "\n"
    " let player_transform = transform.identity\n"
    "   |> transform.set_position(vec3.Vec3(0.0, 1.0, 0.0))\n"
    "   |> transform.set_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // 90 degrees in radians\n"
    "   |> transform.set_scale(vec3.Vec3(1.0, 2.0, 1.0))  // Tall player\n"
    " ```\n"
).

-type transform() :: {transform,
        vec@vec3:vec3(float()),
        vec@vec3:vec3(float()),
        vec@vec3:vec3(float())}.

-file("src/tiramisu/transform.gleam", 58).
?DOC(
    " Create a transform at a specific position with default rotation and scale.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let t = transform.at(position: vec3.Vec3(5.0, 0.0, -3.0))\n"
    " // Object positioned at (5, 0, -3)\n"
    " ```\n"
).
-spec at(vec@vec3:vec3(float())) -> transform().
at(Position) ->
    {transform, Position, {vec3, +0.0, +0.0, +0.0}, {vec3, 1.0, 1.0, 1.0}}.

-file("src/tiramisu/transform.gleam", 70).
?DOC(
    " Update the position of a transform.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let moved = transform.identity\n"
    "   |> transform.set_position(vec3.Vec3(1.0, 2.0, 3.0))\n"
    " ```\n"
).
-spec with_position(transform(), vec@vec3:vec3(float())) -> transform().
with_position(Transform, Position) ->
    {transform,
        Position,
        erlang:element(3, Transform),
        erlang:element(4, Transform)}.

-file("src/tiramisu/transform.gleam", 85).
?DOC(
    " Update the rotation of a transform (Euler angles in radians).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let rotated = transform.identity\n"
    "   |> transform.set_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // 90° turn around Y axis\n"
    " ```\n"
).
-spec with_rotation(transform(), vec@vec3:vec3(float())) -> transform().
with_rotation(Transform, Rotation) ->
    {transform,
        erlang:element(2, Transform),
        Rotation,
        erlang:element(4, Transform)}.

-file("src/tiramisu/transform.gleam", 100).
?DOC(
    " Update the scale of a transform.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let scaled = transform.identity\n"
    "   |> transform.set_scale(vec3.Vec3(2.0, 1.0, 2.0))  // Wide and deep, normal height\n"
    " ```\n"
).
-spec with_scale(transform(), vec@vec3:vec3(float())) -> transform().
with_scale(Transform, Scale) ->
    {transform,
        erlang:element(2, Transform),
        erlang:element(3, Transform),
        Scale}.

-file("src/tiramisu/transform.gleam", 127).
-spec lerp_vec(vec@vec3:vec3(float()), vec@vec3:vec3(float()), float()) -> vec@vec3:vec3(float()).
lerp_vec(A, B, T) ->
    {vec3,
        erlang:element(2, A) + ((erlang:element(2, B) - erlang:element(2, A)) * T),
        erlang:element(3, A) + ((erlang:element(3, B) - erlang:element(3, A)) * T),
        erlang:element(4, A) + ((erlang:element(4, B) - erlang:element(4, A)) * T)}.

-file("src/tiramisu/transform.gleam", 119).
?DOC(
    " Linearly interpolate between two transforms.\n"
    "\n"
    " Useful for smooth animations and transitions. Parameter `t` should be between 0.0 and 1.0:\n"
    " - `t = 0.0` returns `from`\n"
    " - `t = 1.0` returns `to`\n"
    " - `t = 0.5` returns halfway between\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let start = transform.at(vec3.Vec3(0.0, 0.0, 0.0))\n"
    " let end = transform.at(vec3.Vec3(10.0, 0.0, 0.0))\n"
    " let halfway = transform.lerp(start, to: end, with: 0.5)\n"
    " // position: (5.0, 0.0, 0.0)\n"
    " ```\n"
).
-spec lerp(transform(), transform(), float()) -> transform().
lerp(From, To, T) ->
    {transform,
        lerp_vec(erlang:element(2, From), erlang:element(2, To), T),
        lerp_vec(erlang:element(3, From), erlang:element(3, To), T),
        lerp_vec(erlang:element(4, From), erlang:element(4, To), T)}.

-file("src/tiramisu/transform.gleam", 149).
?DOC(
    " Compose two transforms (apply second transform after first).\n"
    "\n"
    " Useful for relative transformations. Note: This is a simplified composition\n"
    " that adds positions/rotations and multiplies scales. For proper hierarchical\n"
    " transforms, use scene `Group` nodes instead.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let base = transform.at(vec3.Vec3(5.0, 0.0, 0.0))\n"
    " let offset = transform.at(vec3.Vec3(0.0, 2.0, 0.0))\n"
    " let combined = transform.compose(base, offset)\n"
    " // position: (5.0, 2.0, 0.0)\n"
    " ```\n"
).
-spec compose(transform(), transform()) -> transform().
compose(First, Second) ->
    {transform,
        vec@vec3f:add(erlang:element(2, First), erlang:element(2, Second)),
        vec@vec3f:add(erlang:element(3, First), erlang:element(3, Second)),
        {vec3,
            erlang:element(2, erlang:element(4, First)) * erlang:element(
                2,
                erlang:element(4, Second)
            ),
            erlang:element(3, erlang:element(4, First)) * erlang:element(
                3,
                erlang:element(4, Second)
            ),
            erlang:element(4, erlang:element(4, First)) * erlang:element(
                4,
                erlang:element(4, Second)
            )}}.

-file("src/tiramisu/transform.gleam", 179).
?DOC(
    " Create a transform that looks at a target position from a source position.\n"
    "\n"
    " Calculates the rotation needed to point from `from` towards `to`.\n"
    " Uses proper Euler angle conversion with atan2 for stable results.\n"
    "\n"
    " Returns rotation in radians (pitch, yaw, roll) where:\n"
    " - **Pitch (X)**: rotation around X axis (looking up/down)\n"
    " - **Yaw (Y)**: rotation around Y axis (turning left/right)\n"
    " - **Roll (Z)**: rotation around Z axis (typically 0 for look-at)\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let camera_pos = vec3.Vec3(0.0, 5.0, 10.0)\n"
    " let target_pos = vec3.Vec3(0.0, 0.0, 0.0)\n"
    " let look_transform = transform.look_at(from: camera_pos, to: target_pos)\n"
    " // Camera now faces the origin\n"
    " ```\n"
).
-spec look_at(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> transform().
look_at(From, To) ->
    Direction = vec@vec3f:subtract(To, From),
    Direction@1 = case vec@vec3f:length(Direction) < 0.0001 of
        true ->
            {vec3, +0.0, +0.0, 1.0};

        false ->
            vec@vec3f:normalize(Direction)
    end,
    Horizontal_distance = begin
        _pipe = gleam@float:square_root(
            (erlang:element(2, Direction@1) * erlang:element(2, Direction@1)) + (erlang:element(
                4,
                Direction@1
            )
            * erlang:element(4, Direction@1))
        ),
        (fun(Result) -> case Result of
                {ok, Val} ->
                    Val;

                {error, _} ->
                    +0.0
            end end)(_pipe)
    end,
    Pitch = gleam_community@maths:atan2(
        erlang:element(3, Direction@1),
        Horizontal_distance
    ),
    Yaw = gleam_community@maths:atan2(
        erlang:element(2, Direction@1),
        erlang:element(4, Direction@1)
    ),
    Roll = +0.0,
    {transform, From, {vec3, Pitch, Yaw, Roll}, {vec3, 1.0, 1.0, 1.0}}.

-file("src/tiramisu/transform.gleam", 233).
?DOC(
    " Move a transform by adding to its current position (relative movement).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let t = transform.at(vec3.Vec3(5.0, 0.0, 0.0))\n"
    "   |> transform.translate_by(vec3.Vec3(2.0, 1.0, 0.0))\n"
    " // position: (7.0, 1.0, 0.0)\n"
    " ```\n"
).
-spec translate(transform(), vec@vec3:vec3(float())) -> transform().
translate(Transform, Offset) ->
    {transform,
        vec@vec3f:add(erlang:element(2, Transform), Offset),
        erlang:element(3, Transform),
        erlang:element(4, Transform)}.

-file("src/tiramisu/transform.gleam", 247).
?DOC(
    " Rotate a transform by adding to its current rotation (relative rotation).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let t = transform.identity\n"
    "   |> transform.rotate_by(vec3.Vec3(0.0, 1.57, 0.0))  // Turn 90° right\n"
    "   |> transform.rotate_by(vec3.Vec3(0.0, 1.57, 0.0))  // Turn another 90° right\n"
    " // rotation: (0.0, 3.14, 0.0) - now facing backward\n"
    " ```\n"
).
-spec rotate_by(transform(), vec@vec3:vec3(float())) -> transform().
rotate_by(Transform, Rotation) ->
    {transform,
        erlang:element(2, Transform),
        vec@vec3f:add(erlang:element(3, Transform), Rotation),
        erlang:element(4, Transform)}.

-file("src/tiramisu/transform.gleam", 261).
?DOC(
    " Scale a transform by multiplying its current scale (relative scaling).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let t = transform.identity\n"
    "   |> transform.scale_by(vec3.Vec3(2.0, 1.0, 2.0))\n"
    "   |> transform.scale_by(vec3.Vec3(2.0, 1.0, 1.0))\n"
    " // scale: (4.0, 1.0, 2.0)\n"
    " ```\n"
).
-spec scale_by(transform(), vec@vec3:vec3(float())) -> transform().
scale_by(Transform, Scale_factor) ->
    {transform,
        erlang:element(2, Transform),
        erlang:element(3, Transform),
        {vec3,
            erlang:element(2, erlang:element(4, Transform)) * erlang:element(
                2,
                Scale_factor
            ),
            erlang:element(3, erlang:element(4, Transform)) * erlang:element(
                3,
                Scale_factor
            ),
            erlang:element(4, erlang:element(4, Transform)) * erlang:element(
                4,
                Scale_factor
            )}}.

-file("src/tiramisu/transform.gleam", 284).
?DOC(
    " Set uniform scale on all axes (width = height = depth).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let t = transform.identity\n"
    "   |> transform.scale_uniform(2.0)\n"
    " // scale: (2.0, 2.0, 2.0) - twice as big in all dimensions\n"
    " ```\n"
).
-spec scale_uniform(transform(), float()) -> transform().
scale_uniform(Transform, Scale) ->
    {transform,
        erlang:element(2, Transform),
        erlang:element(3, Transform),
        {vec3, Scale, Scale, Scale}}.

-file("src/tiramisu/transform.gleam", 296).
?DOC(
    " Rotate around the Y axis (yaw/turn left-right).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let t = transform.identity\n"
    "   |> transform.rotate_y(1.57)  // Turn 90° right\n"
    " ```\n"
).
-spec rotate_y(transform(), float()) -> transform().
rotate_y(Transform, Angle) ->
    {transform,
        erlang:element(2, Transform),
        {vec3,
            erlang:element(2, erlang:element(3, Transform)),
            erlang:element(3, erlang:element(3, Transform)) + Angle,
            erlang:element(4, erlang:element(3, Transform))},
        erlang:element(4, Transform)}.

-file("src/tiramisu/transform.gleam", 315).
?DOC(
    " Rotate around the X axis (pitch/look up-down).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let t = transform.identity\n"
    "   |> transform.rotate_x(0.5)  // Look up slightly\n"
    " ```\n"
).
-spec rotate_x(transform(), float()) -> transform().
rotate_x(Transform, Angle) ->
    {transform,
        erlang:element(2, Transform),
        {vec3,
            erlang:element(2, erlang:element(3, Transform)) + Angle,
            erlang:element(3, erlang:element(3, Transform)),
            erlang:element(4, erlang:element(3, Transform))},
        erlang:element(4, Transform)}.

-file("src/tiramisu/transform.gleam", 334).
?DOC(
    " Rotate around the Z axis (roll/tilt left-right).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let t = transform.identity\n"
    "   |> transform.rotate_z(0.3)  // Tilt right\n"
    " ```\n"
).
-spec rotate_z(transform(), float()) -> transform().
rotate_z(Transform, Angle) ->
    {transform,
        erlang:element(2, Transform),
        {vec3,
            erlang:element(2, erlang:element(3, Transform)),
            erlang:element(3, erlang:element(3, Transform)),
            erlang:element(4, erlang:element(3, Transform)) + Angle},
        erlang:element(4, Transform)}.
