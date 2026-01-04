import gleam/float
import gleam/list
import gleam/option
import gleam/time/duration
import tiramisu
import tiramisu/effect
import tiramisu/geometry
import tiramisu/input
import tiramisu/material
import tiramisu/scene
import tiramisu/simulate
import tiramisu/transform
import vec/vec2.{Vec2}
import vec/vec3

// ============================================================================
// Test Game for Simulation - Input-Driven Game
// ============================================================================

pub type Model {
  Model(
    x: Float,
    y: Float,
    velocity_x: Float,
    velocity_y: Float,
    is_jumping: Bool,
    score: Int,
    combo: Int,
    game_over: Bool,
  )
}

type Msg {
  // Direct messages
  Tick
  ScorePoint
  AddCombo
  ResetCombo
  GameOver
  Restart
  // Input-triggered messages
  StartJump
  Land
  // Batched effect messages
  PlaySound(String)
  ShowParticle(String)
}

const move_speed = 5.0

const jump_velocity = 15.0

const gravity = 0.8

fn game_init(_ctx: tiramisu.Context) {
  #(
    Model(
      x: 0.0,
      y: 0.0,
      velocity_x: 0.0,
      velocity_y: 0.0,
      is_jumping: False,
      score: 0,
      combo: 0,
      game_over: False,
    ),
    effect.none(),
    option.None,
  )
}

fn game_update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(a)) {
  case msg {
    Tick -> {
      // Process input every tick
      let delta_seconds =
        ctx.delta_time
        |> duration.to_seconds()

      // Movement based on input
      let velocity_x = case
        input.is_key_pressed(ctx.input, input.KeyD),
        input.is_key_pressed(ctx.input, input.KeyA)
      {
        True, False -> move_speed
        False, True -> float.negate(move_speed)
        _, _ -> 0.0
      }

      // Jump on space press
      let #(velocity_y, is_jumping, jump_effect) = case
        input.is_key_just_pressed(ctx.input, input.Space) && !model.is_jumping
      {
        True -> #(jump_velocity, True, effect.dispatch(StartJump))
        False -> #(model.velocity_y, model.is_jumping, effect.none())
      }

      // Apply gravity
      let velocity_y = velocity_y -. gravity

      // Update position
      let new_x = model.x +. velocity_x *. delta_seconds *. 60.0
      let new_y = model.y +. velocity_y *. delta_seconds *. 60.0

      // Ground collision
      let #(new_y, velocity_y, is_jumping, land_effect) = case new_y <. 0.0 {
        True -> #(0.0, 0.0, False, case model.is_jumping {
          True -> effect.dispatch(Land)
          False -> effect.none()
        })
        False -> #(new_y, velocity_y, is_jumping, effect.none())
      }

      // Score on click
      let #(score, combo, click_effect) = case
        input.is_left_button_just_pressed(ctx.input)
      {
        True -> #(
          model.score + 1 + model.combo,
          model.combo + 1,
          effect.batch([
            effect.dispatch(ScorePoint),
            effect.dispatch(AddCombo),
          ]),
        )
        False -> #(model.score, model.combo, effect.none())
      }

      #(
        Model(
          ..model,
          x: new_x,
          y: new_y,
          velocity_x: velocity_x,
          velocity_y: velocity_y,
          is_jumping: is_jumping,
          score: score,
          combo: combo,
        ),
        effect.batch([jump_effect, land_effect, click_effect]),
        option.None,
      )
    }

    ScorePoint -> #(model, effect.dispatch(PlaySound("score")), option.None)

    AddCombo -> #(model, effect.dispatch(ShowParticle("combo")), option.None)

    ResetCombo -> #(Model(..model, combo: 0), effect.none(), option.None)

    StartJump -> #(model, effect.dispatch(PlaySound("jump")), option.None)

    Land -> #(
      model,
      effect.batch([
        effect.dispatch(PlaySound("land")),
        effect.dispatch(ShowParticle("dust")),
      ]),
      option.None,
    )

    PlaySound(_name) -> #(model, effect.none(), option.None)

    ShowParticle(_name) -> #(model, effect.none(), option.None)

    GameOver -> #(Model(..model, game_over: True), effect.none(), option.None)

    Restart -> #(
      Model(
        x: 0.0,
        y: 0.0,
        velocity_x: 0.0,
        velocity_y: 0.0,
        is_jumping: False,
        score: 0,
        combo: 0,
        game_over: False,
      ),
      effect.none(),
      option.None,
    )
  }
}

fn game_view(model: Model, _ctx: tiramisu.Context) -> scene.Node {
  // Create a scene that reflects model state
  let player_transform =
    transform.at(vec3.Vec3(model.x, model.y, 0.0))
    |> transform.with_scale(vec3.Vec3(1.0, 1.0, 1.0))

  let assert Ok(player_geometry) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(player_material) =
    material.new()
    |> material.with_color(case model.is_jumping {
      True -> 0x00FF00
      // Green when jumping
      False -> 0x0000FF
      // Blue when grounded
    })
    |> material.build()

  let player =
    scene.mesh(
      id: "player",
      transform: player_transform,
      geometry: player_geometry,
      material: player_material,
      physics: option.None,
    )

  // Game over overlay
  let children = case model.game_over {
    True -> [
      player,
      scene.empty(
        id: "game_over_overlay",
        transform: transform.identity,
        children: [],
      ),
    ]
    False -> [player]
  }

  scene.empty(id: "root", transform: transform.identity, children: children)
}

// ============================================================================
// Helper to create simulation
// ============================================================================

fn create_sim() {
  simulate.start(
    init: game_init,
    update: game_update,
    view: game_view,
    canvas_size: Vec2(800.0, 600.0),
  )
}

// ============================================================================
// Simulation Lifecycle Tests
// ============================================================================

pub fn start_simulation_test() {
  let simulation = create_sim()

  let assert Model(
    x: 0.0,
    y: 0.0,
    score: 0,
    combo: 0,
    is_jumping: False,
    game_over: False,
    velocity_x: 0.0,
    velocity_y: 0.0,
  ) = simulate.model(simulation)
}

pub fn initial_frame_count_is_zero_test() {
  let simulation = create_sim()
  assert simulate.frame_count(simulation) == 0
}

pub fn initial_total_time_is_zero_test() {
  let sim = create_sim()
  assert simulate.total_time(sim) == duration.nanoseconds(0)
}

// ============================================================================
// Input-Driven Behavior Tests
// ============================================================================

pub fn move_right_with_d_key_test() {
  // Press D key and tick
  let simulation =
    create_sim()
    |> simulate.with_key_pressed(input.KeyD)
    |> simulate.dispatch_now(Tick)

  assert Model(
      x: 0.0,
      y: 0.0,
      score: 0,
      combo: 0,
      is_jumping: False,
      game_over: False,
      velocity_x: move_speed,
      velocity_y: 0.0 -. gravity,
    )
    == simulate.model(simulation)
}

pub fn no_movement_when_no_keys_pressed_test() {
  let simulation = create_sim()
  let simulation = simulate.dispatch_now(simulation, Tick)

  assert Model(
      x: 0.0,
      y: 0.0,
      velocity_x: 0.0,
      velocity_y: 0.0 -. gravity,
      is_jumping: False,
      score: 0,
      combo: 0,
      game_over: False,
    )
    == simulate.model(simulation)
}

pub fn jump_on_space_press_test() {
  let simulation =
    create_sim()
    |> simulate.with_key_just_pressed(input.Space)
    |> simulate.dispatch_now(Tick)

  assert Model(
      x: 0.0,
      y: 0.0,
      velocity_x: 0.0,
      velocity_y: jump_velocity -. gravity,
      is_jumping: True,
      score: 0,
      combo: 0,
      game_over: False,
    )
    == simulate.model(simulation)
}

pub fn cannot_double_jump_test() {
  // First jump
  let simulation =
    create_sim()
    |> simulate.with_key_just_pressed(input.Space)
    |> simulate.dispatch_now(Tick)

  let Model(velocity_y: first_jump_velocity, ..) = simulate.model(simulation)
  assert first_jump_velocity == jump_velocity -. gravity

  // Clear input and advance frame, try to jump again while in air
  let simulation =
    simulation
    |> simulate.frame(delta: duration.milliseconds(16))
    |> simulate.with_key_just_pressed(input.Space)
    |> simulate.dispatch_now(Tick)

  let assert Model(is_jumping: True, velocity_y: second_velocity, ..) =
    simulate.model(simulation)
  // Velocity should be less than initial jump velocity (gravity applied twice more)
  assert second_velocity <. first_jump_velocity
}

pub fn click_increases_score_test() {
  // First click: score = 0 + 1 + 0 (combo) = 1
  let simulation =
    create_sim()
    |> simulate.with_left_button_just_pressed()
    |> simulate.dispatch_now(Tick)

  let assert Model(score: 1, combo: 1, ..) = simulate.model(simulation)
}

pub fn combo_increases_score_multiplier_test() {
  // First click
  let simulation =
    create_sim()
    |> simulate.with_left_button_just_pressed()
    |> simulate.dispatch_now(Tick)

  // Clear click state and click again
  // Second click: score = 1 + 1 + 1 (combo from first click) = 3
  let simulation =
    simulation
    |> simulate.frame(delta: duration.milliseconds(16))
    |> simulate.with_left_button_just_pressed()
    |> simulate.dispatch_now(Tick)

  let assert Model(score: 3, combo: 2, ..) = simulate.model(simulation)
}

// ============================================================================
// Effect Recording Tests
// ============================================================================

pub fn jump_dispatches_sound_effect_test() {
  let simulation =
    create_sim()
    |> simulate.with_key_just_pressed(input.Space)
    |> simulate.dispatch_now(Tick)

  let effects = simulate.effects(simulation)
  assert list.contains(effects, simulate.RecordedDispatch(StartJump))
}

pub fn click_dispatches_multiple_effects_test() {
  let simulation =
    create_sim()
    |> simulate.with_left_button_just_pressed()
    |> simulate.dispatch_now(Tick)

  let effects = simulate.effects(simulation)
  let messages = simulate.dispatched_messages(simulation)

  assert list.contains(messages, ScorePoint)
  assert list.contains(messages, AddCombo)
  assert list.contains(effects, simulate.RecordedDispatch(ScorePoint))
  assert list.contains(effects, simulate.RecordedDispatch(AddCombo))
}

pub fn landing_dispatches_sound_and_particle_test() {
  let simulation =
    create_sim()
    |> simulate.with_key_just_pressed(input.Space)
    |> simulate.dispatch_now(Tick)
    |> simulate.clear_effects()
    |> simulate.frames(count: 60, delta: duration.milliseconds(16))
    |> simulate.dispatch_now(Land)

  let messages = simulate.dispatched_messages(simulation)
  assert list.contains(messages, PlaySound("land"))
  assert list.contains(messages, ShowParticle("dust"))
}

pub fn has_effect_predicate_test() {
  let simulation =
    create_sim()
    |> simulate.with_key_just_pressed(input.Space)
    |> simulate.dispatch_now(Tick)

  assert simulate.has_effect(simulation, fn(e) {
      case e {
        simulate.RecordedDispatch(StartJump) -> True
        _ -> False
      }
    })
    == True

  assert simulate.has_effect(simulation, fn(e) {
      case e {
        simulate.RecordedDispatch(GameOver) -> True
        _ -> False
      }
    })
    == False
}

pub fn clear_effects_removes_all_test() {
  let simulation =
    create_sim()
    |> simulate.with_left_button_just_pressed()
    |> simulate.dispatch_now(Tick)

  assert simulate.effects(simulation) != []

  let simulation = simulate.clear_effects(simulation)
  assert simulate.effects(simulation) == []
}

// ============================================================================
// Frame Advancement Tests
// ============================================================================

pub fn frame_increments_count_test() {
  let simulation =
    create_sim()
    |> simulate.frame(delta: duration.milliseconds(16))
  assert simulate.frame_count(simulation) == 1

  let simulation = simulate.frame(simulation, delta: duration.milliseconds(16))
  assert simulate.frame_count(simulation) == 2

  let simulation = simulate.frame(simulation, delta: duration.milliseconds(16))
  assert simulate.frame_count(simulation) == 3
}

pub fn frames_advances_multiple_test() {
  let simulation =
    create_sim()
    |> simulate.frames(count: 10, delta: duration.milliseconds(16))
  assert simulate.frame_count(simulation) == 10

  let simulation =
    simulate.frames(simulation, count: 5, delta: duration.milliseconds(16))
  assert simulate.frame_count(simulation) == 15
}

pub fn total_time_accumulates_test() {
  let simulation =
    create_sim()
    |> simulate.frame(delta: duration.milliseconds(16))
  assert simulate.total_time(simulation) == duration.milliseconds(16)

  let simulation = simulate.frame(simulation, delta: duration.milliseconds(16))
  assert simulate.total_time(simulation) == duration.milliseconds(32)

  let simulation = simulate.frame(simulation, delta: duration.milliseconds(100))
  assert simulate.total_time(simulation) == duration.milliseconds(132)
}

pub fn queued_messages_processed_on_frame_test() {
  // Queue multiple messages
  let simulation =
    create_sim()
    |> simulate.dispatch(Restart)
    |> simulate.dispatch(GameOver)

  // Before frame, game_over should be False
  let assert Model(game_over: False, ..) = simulate.model(simulation)

  // After frame, messages are processed in order (Restart then GameOver)
  let simulation = simulate.frame(simulation, delta: duration.milliseconds(16))
  let assert Model(game_over: True, ..) = simulate.model(simulation)
}

// ============================================================================
// Input State Tests
// ============================================================================

pub fn key_pressed_persists_across_frames_test() {
  let simulation =
    create_sim()
    |> simulate.with_key_pressed(input.KeyW)
    |> simulate.frame(delta: duration.milliseconds(16))

  let state = simulate.input_state(simulation)
  assert input.is_key_pressed(state, input.KeyW) == True
}

pub fn key_just_pressed_clears_after_frame_test() {
  let simulation =
    create_sim()
    |> simulate.with_key_just_pressed(input.Space)

  // Before frame
  let state = simulate.input_state(simulation)
  assert input.is_key_just_pressed(state, input.Space) == True

  // After frame: just_pressed clears but still pressed
  let simulation = simulate.frame(simulation, delta: duration.milliseconds(16))
  let state = simulate.input_state(simulation)
  assert input.is_key_just_pressed(state, input.Space) == False
  assert input.is_key_pressed(state, input.Space) == True
}

pub fn key_released_sets_just_released_test() {
  let simulation =
    create_sim()
    |> simulate.with_key_pressed(input.KeyA)
    |> simulate.with_key_released(input.KeyA)

  let state = simulate.input_state(simulation)
  assert input.is_key_pressed(state, input.KeyA) == False
  assert input.is_key_just_released(state, input.KeyA) == True

  // After frame, just_released clears
  let simulation = simulate.frame(simulation, delta: duration.milliseconds(16))
  let state = simulate.input_state(simulation)
  assert input.is_key_just_released(state, input.KeyA) == False
}

pub fn mouse_position_test() {
  let simulation =
    create_sim()
    |> simulate.with_mouse_position(123.5, 456.7)

  let assert Vec2(123.5, 456.7) =
    simulation |> simulate.input_state |> input.mouse_position
}

pub fn mouse_delta_test() {
  let simulation =
    create_sim()
    |> simulate.with_mouse_delta(10.0, -5.0)

  let assert Vec2(10.0, -5.0) =
    simulation |> simulate.input_state |> input.mouse_delta

  // Delta clears after frame
  let simulation = simulate.frame(simulation, delta: duration.milliseconds(16))
  let assert Vec2(0.0, 0.0) =
    simulation |> simulate.input_state |> input.mouse_delta
}

pub fn left_button_just_pressed_clears_after_frame_test() {
  let simulation =
    create_sim()
    |> simulate.with_left_button_just_pressed()

  // Before frame
  let state = simulate.input_state(simulation)
  assert input.is_left_button_just_pressed(state) == True

  // After frame: just_pressed clears but still pressed
  let simulation = simulate.frame(simulation, delta: duration.milliseconds(16))
  let state = simulate.input_state(simulation)
  assert input.is_left_button_just_pressed(state) == False
  assert input.is_left_button_pressed(state) == True
}

pub fn right_button_pressed_test() {
  let simulation =
    create_sim()
    |> simulate.with_right_button_pressed()

  let state = simulate.input_state(simulation)
  assert input.is_right_button_pressed(state) == True
}

pub fn multiple_keys_pressed_test() {
  let simulation =
    create_sim()
    |> simulate.with_key_pressed(input.KeyW)
    |> simulate.with_key_pressed(input.KeyA)
    |> simulate.with_key_pressed(input.KeyS)
    |> simulate.with_key_pressed(input.KeyD)

  let state = simulate.input_state(simulation)
  assert input.is_key_pressed(state, input.KeyW) == True
  assert input.is_key_pressed(state, input.KeyA) == True
  assert input.is_key_pressed(state, input.KeyS) == True
  assert input.is_key_pressed(state, input.KeyD) == True
  assert input.is_key_pressed(state, input.Space) == False
}

// ============================================================================
// View Tests
// ============================================================================

pub fn view_reflects_jumping_state_test() {
  let simulation =
    create_sim()
    |> simulate.with_key_just_pressed(input.Space)
    |> simulate.dispatch_now(Tick)

  let assert Model(is_jumping: True, ..) = simulate.model(simulation)

  // Construct expected node structure
  let assert Ok(player_geometry) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(player_material) =
    material.new()
    |> material.with_color(0x00FF00)
    // Green when jumping
    |> material.build()

  let expected =
    scene.empty(id: "root", transform: transform.identity, children: [
      scene.mesh(
        id: "player",
        geometry: player_geometry,
        material: player_material,
        transform: transform.at(vec3.Vec3(0.0, 0.0, 0.0))
          |> transform.with_scale(vec3.Vec3(1.0, 1.0, 1.0)),
        physics: option.None,
      ),
    ])

  assert simulate.view(simulation) == expected
}

pub fn view_shows_game_over_overlay_test() {
  let simulation =
    create_sim()
    |> simulate.dispatch_now(GameOver)

  let assert Model(game_over: True, ..) = simulate.model(simulation)

  // Construct expected node structure with overlay
  let assert Ok(player_geometry) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
  let assert Ok(player_material) =
    material.new()
    |> material.with_color(0x0000FF)
    // Blue when grounded
    |> material.build()

  let expected =
    scene.empty(id: "root", transform: transform.identity, children: [
      scene.mesh(
        id: "player",
        geometry: player_geometry,
        material: player_material,
        transform: transform.at(vec3.Vec3(0.0, 0.0, 0.0))
          |> transform.with_scale(vec3.Vec3(1.0, 1.0, 1.0)),
        physics: option.None,
      ),
      scene.empty(
        id: "game_over_overlay",
        transform: transform.identity,
        children: [],
      ),
    ])

  assert simulate.view(simulation) == expected
}

// ============================================================================
// Complex Scenario Tests
// ============================================================================

pub fn full_gameplay_sequence_test() {
  // Frame 1: Move right and jump
  let simulation =
    create_sim()
    |> simulate.with_key_pressed(input.KeyD)
    |> simulate.with_key_just_pressed(input.Space)
    |> simulate.dispatch(Tick)
    |> simulate.frame(delta: duration.milliseconds(16))

  let assert Model(is_jumping: True, velocity_x: velocity_x, ..) =
    simulate.model(simulation)
  assert velocity_x == move_speed

  // Frame 2: Fall and click
  let simulation =
    simulation
    |> simulate.with_left_button_just_pressed()
    |> simulate.dispatch(Tick)
    |> simulate.frame(delta: duration.milliseconds(16))

  let assert Model(score: 1, combo: 1, ..) = simulate.model(simulation)

  // Continue falling with Tick messages until landed
  let simulation = dispatch_tick_for_n_frames(simulation, 100)

  let assert Model(is_jumping: False, y: 0.0, ..) = simulate.model(simulation)
}

fn dispatch_tick_for_n_frames(simulation, count) {
  case count {
    0 -> simulation
    _ -> {
      simulation
      |> simulate.dispatch(Tick)
      |> simulate.frame(delta: duration.milliseconds(16))
      |> dispatch_tick_for_n_frames(count - 1)
    }
  }
}

pub fn restart_resets_all_state_test() {
  // Build up some state
  let simulation =
    create_sim()
    |> simulate.with_left_button_just_pressed()
    |> simulate.dispatch_now(Tick)
    |> simulate.dispatch_now(GameOver)

  let assert Model(score: 1, game_over: True, ..) = simulate.model(simulation)

  // Restart
  let simulation = simulate.dispatch_now(simulation, Restart)

  let assert Model(
    x: 0.0,
    y: 0.0,
    score: 0,
    combo: 0,
    game_over: False,
    velocity_x: 0.0,
    velocity_y: 0.0,
    is_jumping: False,
  ) = simulate.model(simulation)
}
