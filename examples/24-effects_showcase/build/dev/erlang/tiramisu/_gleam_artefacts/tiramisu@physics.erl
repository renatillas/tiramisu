-module(tiramisu@physics).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/physics.gleam").
-export([new_rigid_body/1, with_collider/2, with_mass/2, with_restitution/2, with_friction/2, with_linear_damping/2, with_angular_damping/2, with_body_ccd_enabled/1, with_lock_translation_x/1, with_lock_translation_y/1, with_lock_translation_z/1, with_lock_rotation_x/1, with_lock_rotation_y/1, with_lock_rotation_z/1, with_collision_groups/3, build/1, apply_force/3, apply_impulse/3, set_velocity/3, set_angular_velocity/3, apply_torque/3, apply_torque_impulse/3, raycast_all/4, get_collision_events/1, id_to_string/2, collision_groups_to_bitmask/1]).
-export_type([rapier_world/0, rapier_event_queue/0, rapier_rigid_body/0, quaternion/0, physics_world/1, physics_command/1, body/0, collider_shape/0, axis_lock/0, collision_groups/0, rigid_body/0, world_config/1, raycast_hit/1, collision_event/0, rigid_body_builder/1, with_collider/0, without_collider/0, rapier_ray/0, rapier_ray_hit/0, rapier_body_desc/0, rapier_collider_desc/0, rapier_collider/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Physics module using Rapier physics engine\n"
    "\n"
    " Provides declarative physics simulation following the same immutable,\n"
    " diff/patch pattern as the rest of Tiramisu.\n"
    "\n"
    " Physics bodies are declared alongside scene nodes, and the physics world\n"
    " is managed as part of the game's Model state.\n"
).

-type rapier_world() :: any().

-type rapier_event_queue() :: any().

-type rapier_rigid_body() :: any().

-type quaternion() :: {quaternion, float(), float(), float(), float()}.

-opaque physics_world(ARIE) :: {physics_world,
        rapier_world(),
        rapier_event_queue(),
        gleam@dict:dict(ARIE, rigid_body()),
        gleam@dict:dict(binary(), rapier_rigid_body()),
        list(physics_command(ARIE)),
        gleam@dict:dict(integer(), binary()),
        list(collision_event()),
        structures@bimap:bi_map(ARIE, binary())}.

-type physics_command(ARIF) :: {apply_force, ARIF, vec@vec3:vec3(float())} |
    {apply_impulse, ARIF, vec@vec3:vec3(float())} |
    {set_velocity, ARIF, vec@vec3:vec3(float())} |
    {set_angular_velocity, ARIF, vec@vec3:vec3(float())} |
    {apply_torque, ARIF, vec@vec3:vec3(float())} |
    {apply_torque_impulse, ARIF, vec@vec3:vec3(float())}.

-type body() :: dynamic | kinematic | fixed.

-type collider_shape() :: {box, float(), float(), float()} |
    {sphere, float()} |
    {capsule, float(), float()} |
    {cylinder, float(), float()}.

-type axis_lock() :: {axis_lock,
        boolean(),
        boolean(),
        boolean(),
        boolean(),
        boolean(),
        boolean()}.

-type collision_groups() :: {collision_groups, list(integer()), list(integer())}.

-type rigid_body() :: {rigid_body,
        body(),
        gleam@option:option(float()),
        float(),
        float(),
        float(),
        float(),
        collider_shape(),
        boolean(),
        axis_lock(),
        gleam@option:option(collision_groups())}.

-type world_config(ARIG) :: {world_config,
        vec@vec3:vec3(float()),
        list({ARIG, binary()})}.

-type raycast_hit(ARIH) :: {raycast_hit,
        ARIH,
        vec@vec3:vec3(float()),
        vec@vec3:vec3(float()),
        float()}.

-type collision_event() :: {collision_started, binary(), binary()} |
    {collision_ended, binary(), binary()}.

-opaque rigid_body_builder(ARII) :: {rigid_body_builder,
        body(),
        gleam@option:option(collider_shape()),
        gleam@option:option(float()),
        float(),
        float(),
        float(),
        float(),
        boolean(),
        axis_lock(),
        gleam@option:option(collision_groups())} |
    {gleam_phantom, ARII}.

-type with_collider() :: any().

-type without_collider() :: any().

-type rapier_ray() :: any().

-type rapier_ray_hit() :: any().

-type rapier_body_desc() :: any().

-type rapier_collider_desc() :: any().

-type rapier_collider() :: any().

-file("src/tiramisu/physics.gleam", 218).
-spec create_bimap(list({ARIM, binary()})) -> structures@bimap:bi_map(ARIM, binary()).
create_bimap(Correspondances) ->
    gleam@list:fold(
        Correspondances,
        structures@bimap:new(),
        fun(Acc, Item) ->
            {Body, String} = Item,
            structures@bimap:insert(Acc, Body, String)
        end
    ).

-file("src/tiramisu/physics.gleam", 251).
?DOC(
    " Create a new rigid body builder\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let body = physics.new_rigid_body(physics.Dynamic)\n"
    "   |> physics.body_collider(physics.Box(2.0, 2.0, 2.0))\n"
    "   |> physics.body_mass(5.0)\n"
    "   |> physics.build_body()\n"
    " ```\n"
).
-spec new_rigid_body(body()) -> rigid_body_builder(without_collider()).
new_rigid_body(Body_type) ->
    {rigid_body_builder,
        Body_type,
        none,
        none,
        0.3,
        0.5,
        +0.0,
        +0.0,
        false,
        {axis_lock, false, false, false, false, false, false},
        none}.

-file("src/tiramisu/physics.gleam", 274).
?DOC(" Set the collider shape for the rigid body\n").
-spec with_collider(rigid_body_builder(any()), collider_shape()) -> rigid_body_builder(with_collider()).
with_collider(Builder, Collider) ->
    {rigid_body_builder,
        erlang:element(2, Builder),
        {some, Collider},
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 282).
?DOC(" Set the mass for the rigid body\n").
-spec with_mass(rigid_body_builder(any()), float()) -> rigid_body_builder(any()).
with_mass(Builder, Mass) ->
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        {some, Mass},
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 290).
?DOC(" Set the restitution (bounciness) for the rigid body\n").
-spec with_restitution(rigid_body_builder(any()), float()) -> rigid_body_builder(any()).
with_restitution(Builder, Restitution) ->
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        Restitution,
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 298).
?DOC(" Set the friction for the rigid body\n").
-spec with_friction(rigid_body_builder(any()), float()) -> rigid_body_builder(any()).
with_friction(Builder, Friction) ->
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        Friction,
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 306).
?DOC(" Set the linear damping for the rigid body\n").
-spec with_linear_damping(rigid_body_builder(any()), float()) -> rigid_body_builder(any()).
with_linear_damping(Builder, Damping) ->
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        Damping,
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 314).
?DOC(" Set the angular damping for the rigid body\n").
-spec with_angular_damping(rigid_body_builder(any()), float()) -> rigid_body_builder(any()).
with_angular_damping(Builder, Damping) ->
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        Damping,
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 322).
?DOC(" Enable continuous collision detection for the rigid body\n").
-spec with_body_ccd_enabled(rigid_body_builder(any())) -> rigid_body_builder(any()).
with_body_ccd_enabled(Builder) ->
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        true,
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 329).
?DOC(" Lock translation on the X axis\n").
-spec with_lock_translation_x(rigid_body_builder(any())) -> rigid_body_builder(any()).
with_lock_translation_x(Builder) ->
    Locks = erlang:element(10, Builder),
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        {axis_lock,
            true,
            erlang:element(3, Locks),
            erlang:element(4, Locks),
            erlang:element(5, Locks),
            erlang:element(6, Locks),
            erlang:element(7, Locks)},
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 340).
?DOC(" Lock translation on the Y axis\n").
-spec with_lock_translation_y(rigid_body_builder(any())) -> rigid_body_builder(any()).
with_lock_translation_y(Builder) ->
    Locks = erlang:element(10, Builder),
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        {axis_lock,
            erlang:element(2, Locks),
            true,
            erlang:element(4, Locks),
            erlang:element(5, Locks),
            erlang:element(6, Locks),
            erlang:element(7, Locks)},
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 351).
?DOC(" Lock translation on the Z axis\n").
-spec with_lock_translation_z(rigid_body_builder(any())) -> rigid_body_builder(any()).
with_lock_translation_z(Builder) ->
    Locks = erlang:element(10, Builder),
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        {axis_lock,
            erlang:element(2, Locks),
            erlang:element(3, Locks),
            true,
            erlang:element(5, Locks),
            erlang:element(6, Locks),
            erlang:element(7, Locks)},
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 362).
?DOC(" Lock rotation on the X axis (pitch)\n").
-spec with_lock_rotation_x(rigid_body_builder(any())) -> rigid_body_builder(any()).
with_lock_rotation_x(Builder) ->
    Locks = erlang:element(10, Builder),
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        {axis_lock,
            erlang:element(2, Locks),
            erlang:element(3, Locks),
            erlang:element(4, Locks),
            true,
            erlang:element(6, Locks),
            erlang:element(7, Locks)},
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 371).
?DOC(" Lock rotation on the Y axis (yaw)\n").
-spec with_lock_rotation_y(rigid_body_builder(any())) -> rigid_body_builder(any()).
with_lock_rotation_y(Builder) ->
    Locks = erlang:element(10, Builder),
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        {axis_lock,
            erlang:element(2, Locks),
            erlang:element(3, Locks),
            erlang:element(4, Locks),
            erlang:element(5, Locks),
            true,
            erlang:element(7, Locks)},
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 380).
?DOC(" Lock rotation on the Z axis (roll)\n").
-spec with_lock_rotation_z(rigid_body_builder(any())) -> rigid_body_builder(any()).
with_lock_rotation_z(Builder) ->
    Locks = erlang:element(10, Builder),
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        {axis_lock,
            erlang:element(2, Locks),
            erlang:element(3, Locks),
            erlang:element(4, Locks),
            erlang:element(5, Locks),
            erlang:element(6, Locks),
            true},
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 402).
?DOC(
    " Set collision groups for filtering which objects can collide\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " // Player belongs to layer 0, collides with enemies (1) and ground (2)\n"
    " let body = physics.new_rigid_body(physics.Dynamic)\n"
    "   |> physics.body_collider(physics.Capsule(1.0, 0.5))\n"
    "   |> physics.body_collision_groups(\n"
    "     membership: [0],\n"
    "     filter: [1, 2]\n"
    "   )\n"
    "   |> physics.build_body()\n"
    " ```\n"
).
-spec with_collision_groups(
    rigid_body_builder(any()),
    list(integer()),
    list(integer())
) -> rigid_body_builder(any()).
with_collision_groups(Builder, Membership, Filter) ->
    {rigid_body_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        {some, {collision_groups, Membership, Filter}}}.

-file("src/tiramisu/physics.gleam", 423).
?DOC(
    " Build the final rigid body from the builder\n"
    "\n"
    " Returns an error if no collider was set.\n"
).
-spec build(rigid_body_builder(with_collider())) -> rigid_body().
build(Builder) ->
    Collider@1 = case erlang:element(3, Builder) of
        {some, Collider} -> Collider;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"tiramisu/physics"/utf8>>,
                        function => <<"build"/utf8>>,
                        line => 424,
                        value => _assert_fail,
                        start => 11719,
                        'end' => 11770,
                        pattern_start => 11730,
                        pattern_end => 11751})
    end,
    {rigid_body,
        erlang:element(2, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        Collider@1,
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/physics.gleam", 631).
?DOC(
    " Queue a force to be applied to a rigid body during the next physics step.\n"
    " Returns updated world with the command queued.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let world = physics.apply_force(world, \"player\", vec3.Vec3(0.0, 100.0, 0.0))\n"
    " let world = physics.step(world, ctx.delta_time)  // Force is applied here\n"
    " ```\n"
).
-spec apply_force(physics_world(ARLT), ARLT, vec@vec3:vec3(float())) -> physics_world(ARLT).
apply_force(World, Id, Force) ->
    Command = {apply_force, Id, Force},
    {physics_world,
        erlang:element(2, World),
        erlang:element(3, World),
        erlang:element(4, World),
        erlang:element(5, World),
        [Command | erlang:element(6, World)],
        erlang:element(7, World),
        erlang:element(8, World),
        erlang:element(9, World)}.

-file("src/tiramisu/physics.gleam", 652).
?DOC(
    " Queue an impulse to be applied to a rigid body during the next physics step.\n"
    " Returns updated world with the command queued.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " // Jump\n"
    " let world = physics.apply_impulse(world, \"player\", vec3.Vec3(0.0, 10.0, 0.0))\n"
    " ```\n"
).
-spec apply_impulse(physics_world(ARLX), ARLX, vec@vec3:vec3(float())) -> physics_world(ARLX).
apply_impulse(World, Id, Impulse) ->
    Command = {apply_impulse, Id, Impulse},
    {physics_world,
        erlang:element(2, World),
        erlang:element(3, World),
        erlang:element(4, World),
        erlang:element(5, World),
        [Command | erlang:element(6, World)],
        erlang:element(7, World),
        erlang:element(8, World),
        erlang:element(9, World)}.

-file("src/tiramisu/physics.gleam", 666).
?DOC(
    " Queue a velocity change for a rigid body during the next physics step.\n"
    " Returns updated world with the command queued.\n"
).
-spec set_velocity(physics_world(ARMB), ARMB, vec@vec3:vec3(float())) -> physics_world(ARMB).
set_velocity(World, Id, Velocity) ->
    Command = {set_velocity, Id, Velocity},
    {physics_world,
        erlang:element(2, World),
        erlang:element(3, World),
        erlang:element(4, World),
        erlang:element(5, World),
        [Command | erlang:element(6, World)],
        erlang:element(7, World),
        erlang:element(8, World),
        erlang:element(9, World)}.

-file("src/tiramisu/physics.gleam", 691).
?DOC(
    " Queue an angular velocity change for a rigid body during the next physics step.\n"
    " Returns updated world with the command queued.\n"
).
-spec set_angular_velocity(physics_world(ARMK), ARMK, vec@vec3:vec3(float())) -> physics_world(ARMK).
set_angular_velocity(World, Id, Velocity) ->
    Command = {set_angular_velocity, Id, Velocity},
    {physics_world,
        erlang:element(2, World),
        erlang:element(3, World),
        erlang:element(4, World),
        erlang:element(5, World),
        [Command | erlang:element(6, World)],
        erlang:element(7, World),
        erlang:element(8, World),
        erlang:element(9, World)}.

-file("src/tiramisu/physics.gleam", 716).
?DOC(
    " Queue a torque to be applied to a rigid body during the next physics step.\n"
    " Returns updated world with the command queued.\n"
).
-spec apply_torque(physics_world(ARMT), ARMT, vec@vec3:vec3(float())) -> physics_world(ARMT).
apply_torque(World, Id, Torque) ->
    Command = {apply_torque, Id, Torque},
    {physics_world,
        erlang:element(2, World),
        erlang:element(3, World),
        erlang:element(4, World),
        erlang:element(5, World),
        [Command | erlang:element(6, World)],
        erlang:element(7, World),
        erlang:element(8, World),
        erlang:element(9, World)}.

-file("src/tiramisu/physics.gleam", 730).
?DOC(
    " Queue a torque impulse to be applied to a rigid body during the next physics step.\n"
    " Returns updated world with the command queued.\n"
).
-spec apply_torque_impulse(physics_world(ARMX), ARMX, vec@vec3:vec3(float())) -> physics_world(ARMX).
apply_torque_impulse(World, Id, Impulse) ->
    Command = {apply_torque_impulse, Id, Impulse},
    {physics_world,
        erlang:element(2, World),
        erlang:element(3, World),
        erlang:element(4, World),
        erlang:element(5, World),
        [Command | erlang:element(6, World)],
        erlang:element(7, World),
        erlang:element(8, World),
        erlang:element(9, World)}.

-file("src/tiramisu/physics.gleam", 831).
?DOC(
    " Cast a ray and return all hits along the ray\n"
    "\n"
    " Returns hits sorted by distance (closest first).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " // Check what's in front of the player\n"
    " let hits = physics.raycast_all(world, origin, direction, max_distance: 100.0)\n"
    "\n"
    " // Find first enemy hit\n"
    " let enemy_hit = list.find(hits, fn(hit) {\n"
    "   string.starts_with(hit.body_id, \"enemy_\")\n"
    " })\n"
    " ```\n"
).
-spec raycast_all(
    physics_world(ARNI),
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
    float()
) -> list(raycast_hit(ARNI)).
raycast_all(_, _, _, _) ->
    [].

-file("src/tiramisu/physics.gleam", 860).
?DOC(
    " Get all collision events that occurred during the last physics step.\n"
    "\n"
    " Events are automatically collected when `step()` is called and stored in the world.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let physics_world = physics.step(physics_world)\n"
    " let collision_events = physics.get_collision_events(physics_world)\n"
    "\n"
    " list.each(collision_events, fn(event) {\n"
    "   case event {\n"
    "     physics.CollisionStarted(a, b) ->\n"
    "       io.println(a <> \" started colliding with \" <> b)\n"
    "     physics.CollisionEnded(a, b) ->\n"
    "       io.println(a <> \" ended colliding with \" <> b)\n"
    "   }\n"
    " })\n"
    " ```\n"
).
-spec get_collision_events(physics_world(any())) -> list(collision_event()).
get_collision_events(World) ->
    erlang:element(8, World).

-file("src/tiramisu/physics.gleam", 869).
?DOC(false).
-spec id_to_string(physics_world(ARNR), ARNR) -> {ok, binary()} | {error, nil}.
id_to_string(World, Id) ->
    structures@bimap:get(erlang:element(9, World), Id).

-file("src/tiramisu/physics.gleam", 939).
?DOC(
    " Convert list of collision layer indices (0-15) to a 16-bit bitmask\n"
    "\n"
    " For example, [0, 2, 3] becomes 0b0000000000001101 = 0x000D\n"
).
-spec layers_to_bitmask(list(integer())) -> integer().
layers_to_bitmask(Layers) ->
    gleam@list:fold(
        Layers,
        0,
        fun(Mask, Layer) -> case (Layer >= 0) andalso (Layer =< 15) of
                true ->
                    Bit = erlang:'bsl'(1, Layer),
                    erlang:'bor'(Mask, Bit);

                false ->
                    Mask
            end end
    ).

-file("src/tiramisu/physics.gleam", 957).
?DOC(
    " Pack membership and filter bitmasks into a single 32-bit value\n"
    "\n"
    " Rapier format: 16 upper bits = membership, 16 lower bits = filter\n"
    " For example: membership=0x000D, filter=0x0004 -> 0x000D0004\n"
).
-spec pack_collision_groups(integer(), integer()) -> integer().
pack_collision_groups(Membership, Filter) ->
    Membership_shifted = erlang:'bsl'(Membership, 16),
    erlang:'bor'(Membership_shifted, Filter).

-file("src/tiramisu/physics.gleam", 964).
?DOC(false).
-spec collision_groups_to_bitmask(collision_groups()) -> integer().
collision_groups_to_bitmask(Groups) ->
    Membership_mask = layers_to_bitmask(erlang:element(2, Groups)),
    Filter_mask = layers_to_bitmask(erlang:element(3, Groups)),
    pack_collision_groups(Membership_mask, Filter_mask).
