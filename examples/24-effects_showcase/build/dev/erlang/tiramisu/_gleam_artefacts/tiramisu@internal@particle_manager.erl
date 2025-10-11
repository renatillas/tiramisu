-module(tiramisu@internal@particle_manager).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/internal/particle_manager.gleam").
-export([set_active/2, get_points_object/1]).
-export_type([particle/0, particle_system_state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type particle() :: {particle,
        vec@vec3:vec3(float()),
        vec@vec3:vec3(float()),
        float(),
        float(),
        float()}.

-type particle_system_state() :: {particle_system_state,
        tiramisu@particle_emitter:particle_emitter(),
        tiramisu@transform:transform(),
        boolean(),
        list(particle()),
        float(),
        gleam@dynamic:dynamic_(),
        gleam@dynamic:dynamic_(),
        gleam@dynamic:dynamic_()}.

-file("src/tiramisu/internal/particle_manager.gleam", 141).
?DOC(false).
-spec init_particle(
    tiramisu@particle_emitter:particle_emitter(),
    tiramisu@transform:transform()
) -> particle().
init_particle(Emitter, Transform) ->
    Pos = erlang:element(2, Transform),
    Base_velocity = tiramisu@particle_emitter:get_velocity(Emitter),
    Velocity_variance = tiramisu@particle_emitter:get_velocity_variance(Emitter),
    Base_size = tiramisu@particle_emitter:get_size(Emitter),
    Size_variance = tiramisu@particle_emitter:get_size_variance(Emitter),
    Base_lifetime = tiramisu@particle_emitter:get_lifetime(Emitter),
    Lifetime_variance = tiramisu@particle_emitter:get_lifetime_variance(Emitter),
    Vx = erlang:element(2, Base_velocity) + (((rand:uniform() * 2.0) - 1.0) * erlang:element(
        2,
        Velocity_variance
    )),
    Vy = erlang:element(3, Base_velocity) + (((rand:uniform() * 2.0) - 1.0) * erlang:element(
        3,
        Velocity_variance
    )),
    Vz = erlang:element(4, Base_velocity) + (((rand:uniform() * 2.0) - 1.0) * erlang:element(
        4,
        Velocity_variance
    )),
    Size = Base_size + (rand:uniform() * Size_variance),
    Lifetime = Base_lifetime + (rand:uniform() * Lifetime_variance),
    {particle, Pos, {vec3, Vx, Vy, Vz}, +0.0, Lifetime, Size}.

-file("src/tiramisu/internal/particle_manager.gleam", 107).
?DOC(false).
-spec spawn_particle(particle_system_state()) -> particle_system_state().
spawn_particle(State) ->
    Emitter = erlang:element(2, State),
    Max_particles = tiramisu@particle_emitter:get_max_particles(Emitter),
    Particle_count = erlang:length(erlang:element(5, State)),
    case Particle_count >= Max_particles of
        true ->
            case erlang:element(5, State) of
                [] ->
                    State;

                [_ | Rest] ->
                    New_particle = init_particle(
                        Emitter,
                        erlang:element(3, State)
                    ),
                    {particle_system_state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        lists:append(Rest, [New_particle]),
                        erlang:element(6, State),
                        erlang:element(7, State),
                        erlang:element(8, State),
                        erlang:element(9, State)}
            end;

        false ->
            New_particle@1 = init_particle(Emitter, erlang:element(3, State)),
            {particle_system_state,
                erlang:element(2, State),
                erlang:element(3, State),
                erlang:element(4, State),
                lists:append(erlang:element(5, State), [New_particle@1]),
                erlang:element(6, State),
                erlang:element(7, State),
                erlang:element(8, State),
                erlang:element(9, State)}
    end.

-file("src/tiramisu/internal/particle_manager.gleam", 210).
?DOC(false).
-spec handle_spawning(particle_system_state(), float()) -> particle_system_state().
handle_spawning(State, Delta_time) ->
    New_time = erlang:element(6, State) + Delta_time,
    Emit_rate = tiramisu@particle_emitter:get_emit_rate(
        erlang:element(2, State)
    ),
    Spawn_interval = case erlang:float(Emit_rate) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> 1.0 / Gleam@denominator
    end,
    case New_time >= Spawn_interval of
        true ->
            Spawned_state = spawn_particle(State),
            {particle_system_state,
                erlang:element(2, Spawned_state),
                erlang:element(3, Spawned_state),
                erlang:element(4, Spawned_state),
                erlang:element(5, Spawned_state),
                +0.0,
                erlang:element(7, Spawned_state),
                erlang:element(8, Spawned_state),
                erlang:element(9, Spawned_state)};

        false ->
            {particle_system_state,
                erlang:element(2, State),
                erlang:element(3, State),
                erlang:element(4, State),
                erlang:element(5, State),
                New_time,
                erlang:element(7, State),
                erlang:element(8, State),
                erlang:element(9, State)}
    end.

-file("src/tiramisu/internal/particle_manager.gleam", 243).
?DOC(false).
-spec update_single_particle(particle(), float(), float()) -> particle().
update_single_particle(Particle, Delta_time, Gravity_scale) ->
    New_life = erlang:element(4, Particle) + Delta_time,
    Gravity_force = -9.81 * Gravity_scale,
    New_vy = erlang:element(3, erlang:element(3, Particle)) + (Gravity_force * Delta_time),
    New_velocity = begin
        _record = erlang:element(3, Particle),
        {vec3, erlang:element(2, _record), New_vy, erlang:element(4, _record)}
    end,
    New_x = erlang:element(2, erlang:element(2, Particle)) + (erlang:element(
        2,
        New_velocity
    )
    * Delta_time),
    New_y = erlang:element(3, erlang:element(2, Particle)) + (erlang:element(
        3,
        New_velocity
    )
    * Delta_time),
    New_z = erlang:element(4, erlang:element(2, Particle)) + (erlang:element(
        4,
        New_velocity
    )
    * Delta_time),
    New_position = {vec3, New_x, New_y, New_z},
    {particle,
        New_position,
        New_velocity,
        New_life,
        erlang:element(5, Particle),
        erlang:element(6, Particle)}.

-file("src/tiramisu/internal/particle_manager.gleam", 232).
?DOC(false).
-spec update_particle_list(list(particle()), float(), float()) -> list(particle()).
update_particle_list(Particles, Delta_time, Gravity_scale) ->
    gleam@list:map(
        Particles,
        fun(Particle) ->
            update_single_particle(Particle, Delta_time, Gravity_scale)
        end
    ).

-file("src/tiramisu/internal/particle_manager.gleam", 273).
?DOC(false).
-spec remove_dead_particles(list(particle())) -> list(particle()).
remove_dead_particles(Particles) ->
    gleam@list:filter(
        Particles,
        fun(Particle) ->
            erlang:element(4, Particle) < erlang:element(5, Particle)
        end
    ).

-file("src/tiramisu/internal/particle_manager.gleam", 379).
?DOC(false).
-spec set_active(particle_system_state(), boolean()) -> particle_system_state().
set_active(State, Active) ->
    {particle_system_state,
        erlang:element(2, State),
        erlang:element(3, State),
        Active,
        erlang:element(5, State),
        erlang:element(6, State),
        erlang:element(7, State),
        erlang:element(8, State),
        erlang:element(9, State)}.

-file("src/tiramisu/internal/particle_manager.gleam", 404).
?DOC(false).
-spec get_points_object(particle_system_state()) -> gleam@dynamic:dynamic_().
get_points_object(State) ->
    erlang:element(7, State).
