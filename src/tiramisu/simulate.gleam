//// Testing utilities for Tiramisu games.
////
//// The simulate module allows you to test game logic without a browser,
//// similar to `lustre/dev/simulate`. It provides frame-by-frame game
//// simulation with input mocking and effect recording.
////
//// ## Basic Usage
////
//// ```gleam
//// import tiramisu/simulate
//// import tiramisu/input
//// import gleam/time/duration
////
//// pub fn player_moves_when_key_pressed_test() {
////   let sim = simulate.start(my_game.init, my_game.update, my_game.view)
////
////   // Simulate pressing W key
////   let sim = simulate.with_key_pressed(sim, input.KeyW)
////   let sim = simulate.frame(sim, delta: duration.milliseconds(16))
////
////   let model = simulate.model(sim)
////   assert model.player.position.y > 0.0
//// }
//// ```
////

import gleam/javascript/array
import gleam/list
import gleam/option.{type Option}
import gleam/set
import gleam/time/duration.{type Duration}
import tiramisu
import tiramisu/effect
import tiramisu/input
import tiramisu/physics
import tiramisu/scene
import tiramisu/transform
import vec/vec2.{type Vec2, Vec2}

// ============================================================================
// TYPES
// ============================================================================

/// Opaque simulation state that holds the game model, effects, and context
pub opaque type Simulation(model, msg) {
  Simulation(
    /// Current game model
    model: model,
    /// Update function
    update: fn(model, msg, tiramisu.Context) ->
      #(model, effect.Effect(msg), Option(physics.PhysicsWorld)),
    /// View function
    view: fn(model, tiramisu.Context) -> scene.Node,
    /// Current physics world (if enabled)
    physics_world: Option(physics.PhysicsWorld),
    /// Current input state
    input_state: input.InputState,
    /// Canvas size for context
    canvas_size: Vec2(Float),
    /// Headless renderer state
    renderer_state: scene.RendererState,
    /// Recorded effects (dispatched messages)
    recorded_effects: List(RecordedEffect(msg)),
    /// Pending messages to process
    pending_messages: List(msg),
    /// Frame counter
    frame_count: Int,
    /// Accumulated time
    total_time: Duration,
  )
}

/// Recorded effect for inspection
///
/// Effects in simulations are recorded but NOT executed, allowing you to
/// verify that your game logic produces the expected effects.
pub type RecordedEffect(msg) {
  /// A message was dispatched via effect.dispatch
  RecordedDispatch(msg: msg)
  /// A delayed message via effect.delay
  RecordedDelay(delay: Duration, msg: msg)
  /// An interval was created via effect.interval
  RecordedInterval(interval: Duration, msg: msg)
}

// ============================================================================
// SIMULATION LIFECYCLE
// ============================================================================

/// Start a simulation with a specific canvas size
///
/// ## Example
///
/// ```gleam
/// let sim = simulate.start(
///   init: game.init,
///   update: game.update,
///   view: game.view,
///   canvas_size: vec2.Vec2(1920.0, 1080.0),
/// )
/// ```
pub fn start(
  init init: fn(tiramisu.Context) ->
    #(model, effect.Effect(msg), Option(physics.PhysicsWorld)),
  update update: fn(model, msg, tiramisu.Context) ->
    #(model, effect.Effect(msg), Option(physics.PhysicsWorld)),
  view view: fn(model, tiramisu.Context) -> scene.Node,
  canvas_size canvas_size: Vec2(Float),
) -> Simulation(model, msg) {
  let Vec2(width, height) = canvas_size

  // Create headless renderer state
  let renderer_state = scene.new_headless_render_state(width, height)

  // Create initial context
  let initial_context =
    tiramisu.Context(
      delta_time: duration.nanoseconds(0),
      input: input.new(),
      canvas_size: canvas_size,
      physics_world: option.None,
      scene: scene.get_scene(renderer_state),
      renderer: scene.get_renderer(renderer_state),
    )

  // Run init function
  let #(initial_model, initial_effect, physics_world) = init(initial_context)

  // Record effects from init
  let recorded = record_effect(initial_effect, [])

  // Update renderer state with physics world if provided
  let renderer_state = case physics_world {
    option.Some(world) ->
      scene.set_physics_world(renderer_state, option.Some(world))
    option.None -> renderer_state
  }

  Simulation(
    model: initial_model,
    update: update,
    view: view,
    physics_world: physics_world,
    input_state: input.new(),
    canvas_size: canvas_size,
    renderer_state: renderer_state,
    recorded_effects: recorded,
    pending_messages: [],
    frame_count: 0,
    total_time: duration.nanoseconds(0),
  )
}

// ============================================================================
// FRAME ADVANCEMENT
// ============================================================================

/// Advance simulation by one frame with specified delta time
///
/// This processes all pending messages and updates the frame counter.
/// Physics stepping is NOT done automatically - if your game uses physics,
/// call `physics.step` in your update function.
///
/// ## Example
///
/// ```gleam
/// let sim = simulate.frame(sim, delta: duration.milliseconds(16))
/// ```
pub fn frame(
  sim: Simulation(model, msg),
  delta delta: Duration,
) -> Simulation(model, msg) {
  // Create context for this frame
  let context =
    tiramisu.Context(
      delta_time: delta,
      input: sim.input_state,
      canvas_size: sim.canvas_size,
      physics_world: sim.physics_world,
      scene: scene.get_scene(sim.renderer_state),
      renderer: scene.get_renderer(sim.renderer_state),
    )

  // Process all pending messages
  let #(new_model, new_effects, final_physics_world) =
    process_messages(
      sim.model,
      sim.pending_messages,
      context,
      sim.recorded_effects,
      sim.update,
    )

  // Update physics world from message processing
  let final_physics = case final_physics_world {
    option.Some(world) -> option.Some(world)
    option.None -> sim.physics_world
  }

  // Clear per-frame input state
  let new_input = clear_frame_input_state(sim.input_state)

  Simulation(
    ..sim,
    model: new_model,
    physics_world: final_physics,
    input_state: new_input,
    renderer_state: scene.set_physics_world(sim.renderer_state, final_physics),
    recorded_effects: new_effects,
    pending_messages: [],
    frame_count: sim.frame_count + 1,
    total_time: duration.add(sim.total_time, delta),
  )
}

/// Advance simulation by N frames with fixed delta time
///
/// ## Example
///
/// ```gleam
/// // Advance 60 frames at 16ms each (roughly 1 second)
/// let sim = simulate.frames(sim, count: 60, delta: duration.milliseconds(16))
/// ```
pub fn frames(
  sim: Simulation(model, msg),
  count count: Int,
  delta delta: Duration,
) -> Simulation(model, msg) {
  case count <= 0 {
    True -> sim
    False -> frames(frame(sim, delta: delta), count: count - 1, delta: delta)
  }
}

// ============================================================================
// MESSAGE DISPATCHING
// ============================================================================

/// Queue a message to be processed on the next frame
///
/// ## Example
///
/// ```gleam
/// let sim = simulate.dispatch(sim, Jump)
/// let sim = simulate.frame(sim, delta: duration.milliseconds(16))
/// ```
pub fn dispatch(sim: Simulation(model, msg), msg: msg) -> Simulation(model, msg) {
  Simulation(..sim, pending_messages: list.append(sim.pending_messages, [msg]))
}

/// Dispatch a message and immediately process it (within the same frame)
///
/// ## Example
///
/// ```gleam
/// let sim = simulate.dispatch_now(sim, StartGame)
/// let model = simulate.model(sim)
/// ```
pub fn dispatch_now(
  sim: Simulation(model, msg),
  msg: msg,
) -> Simulation(model, msg) {
  // Create context with current state
  let context =
    tiramisu.Context(
      delta_time: duration.nanoseconds(0),
      input: sim.input_state,
      canvas_size: sim.canvas_size,
      physics_world: sim.physics_world,
      scene: scene.get_scene(sim.renderer_state),
      renderer: scene.get_renderer(sim.renderer_state),
    )

  // Process the message immediately
  let #(new_model, new_effect, new_physics_world) =
    sim.update(sim.model, msg, context)

  // Record effects
  let new_recorded = record_effect(new_effect, sim.recorded_effects)

  // Update physics world if changed
  let final_physics = case new_physics_world {
    option.Some(world) -> option.Some(world)
    option.None -> sim.physics_world
  }

  Simulation(
    ..sim,
    model: new_model,
    physics_world: final_physics,
    renderer_state: scene.set_physics_world(sim.renderer_state, final_physics),
    recorded_effects: new_recorded,
  )
}

// ============================================================================
// STATE INSPECTION
// ============================================================================

/// Get the current model
pub fn model(sim: Simulation(model, msg)) -> model {
  sim.model
}

/// Get the current scene node (result of calling view)
pub fn view(sim: Simulation(model, msg)) -> scene.Node {
  let context =
    tiramisu.Context(
      delta_time: duration.nanoseconds(0),
      input: sim.input_state,
      canvas_size: sim.canvas_size,
      physics_world: sim.physics_world,
      scene: scene.get_scene(sim.renderer_state),
      renderer: scene.get_renderer(sim.renderer_state),
    )

  sim.view(sim.model, context)
}

/// Get all recorded effects
pub fn effects(sim: Simulation(model, msg)) -> List(RecordedEffect(msg)) {
  list.reverse(sim.recorded_effects)
}

/// Get the physics world (if physics is enabled)
pub fn physics_world(
  sim: Simulation(model, msg),
) -> Option(physics.PhysicsWorld) {
  sim.physics_world
}

/// Get current frame count
pub fn frame_count(sim: Simulation(model, msg)) -> Int {
  sim.frame_count
}

/// Get total simulation time
pub fn total_time(sim: Simulation(model, msg)) -> Duration {
  sim.total_time
}

/// Get the current input state
pub fn input_state(sim: Simulation(model, msg)) -> input.InputState {
  sim.input_state
}

// ============================================================================
// INPUT SIMULATION
// ============================================================================

/// Set a key as pressed (held down)
///
/// ## Example
///
/// ```gleam
/// let sim = simulate.with_key_pressed(sim, input.KeyW)
/// ```
pub fn with_key_pressed(
  sim: Simulation(model, msg),
  key: input.Key,
) -> Simulation(model, msg) {
  let key_code = key_to_code(key)
  let new_keyboard =
    input.build_keyboard_state(
      pressed: set.insert(input.get_pressed_keys(sim.input_state), key_code),
      just_pressed: input.get_just_pressed_keys(sim.input_state),
      just_released: input.get_just_released_keys(sim.input_state),
    )
  let new_input = rebuild_input_with_keyboard(sim.input_state, new_keyboard)
  Simulation(..sim, input_state: new_input)
}

/// Set a key as just pressed (for this frame only)
///
/// This sets the key as both pressed AND just_pressed.
///
/// ## Example
///
/// ```gleam
/// let sim = simulate.with_key_just_pressed(sim, input.Space)
/// let sim = simulate.frame(sim, delta: duration.milliseconds(16))
/// // After frame(), just_pressed is cleared but pressed remains
/// ```
pub fn with_key_just_pressed(
  sim: Simulation(model, msg),
  key: input.Key,
) -> Simulation(model, msg) {
  let key_code = key_to_code(key)
  let new_keyboard =
    input.build_keyboard_state(
      pressed: set.insert(input.get_pressed_keys(sim.input_state), key_code),
      just_pressed: set.insert(
        input.get_just_pressed_keys(sim.input_state),
        key_code,
      ),
      just_released: input.get_just_released_keys(sim.input_state),
    )
  let new_input = rebuild_input_with_keyboard(sim.input_state, new_keyboard)
  Simulation(..sim, input_state: new_input)
}

/// Release a key
///
/// This removes the key from pressed and sets it as just_released.
pub fn with_key_released(
  sim: Simulation(model, msg),
  key: input.Key,
) -> Simulation(model, msg) {
  let key_code = key_to_code(key)
  let new_keyboard =
    input.build_keyboard_state(
      pressed: set.delete(input.get_pressed_keys(sim.input_state), key_code),
      just_pressed: input.get_just_pressed_keys(sim.input_state),
      just_released: set.insert(
        input.get_just_released_keys(sim.input_state),
        key_code,
      ),
    )
  let new_input = rebuild_input_with_keyboard(sim.input_state, new_keyboard)
  Simulation(..sim, input_state: new_input)
}

/// Set mouse position
///
/// ## Example
///
/// ```gleam
/// let sim = simulate.with_mouse_position(sim, 400.0, 300.0)
/// ```
pub fn with_mouse_position(
  sim: Simulation(model, msg),
  x: Float,
  y: Float,
) -> Simulation(model, msg) {
  let left = input.get_left_button_state(sim.input_state)
  let middle = input.get_middle_button_state(sim.input_state)
  let right = input.get_right_button_state(sim.input_state)
  let new_mouse =
    input.build_mouse_state(
      x: x,
      y: y,
      delta_x: input.get_mouse_delta_x(sim.input_state),
      delta_y: input.get_mouse_delta_y(sim.input_state),
      wheel_delta: input.get_mouse_wheel_delta(sim.input_state),
      left_pressed: left.pressed,
      left_just_pressed: left.just_pressed,
      left_just_released: left.just_released,
      middle_pressed: middle.pressed,
      middle_just_pressed: middle.just_pressed,
      middle_just_released: middle.just_released,
      right_pressed: right.pressed,
      right_just_pressed: right.just_pressed,
      right_just_released: right.just_released,
    )
  let new_input = rebuild_input_with_mouse(sim.input_state, new_mouse)
  Simulation(..sim, input_state: new_input)
}

/// Set mouse delta (movement since last frame)
pub fn with_mouse_delta(
  sim: Simulation(model, msg),
  dx: Float,
  dy: Float,
) -> Simulation(model, msg) {
  let left = input.get_left_button_state(sim.input_state)
  let middle = input.get_middle_button_state(sim.input_state)
  let right = input.get_right_button_state(sim.input_state)
  let new_mouse =
    input.build_mouse_state(
      x: input.get_mouse_x(sim.input_state),
      y: input.get_mouse_y(sim.input_state),
      delta_x: dx,
      delta_y: dy,
      wheel_delta: input.get_mouse_wheel_delta(sim.input_state),
      left_pressed: left.pressed,
      left_just_pressed: left.just_pressed,
      left_just_released: left.just_released,
      middle_pressed: middle.pressed,
      middle_just_pressed: middle.just_pressed,
      middle_just_released: middle.just_released,
      right_pressed: right.pressed,
      right_just_pressed: right.just_pressed,
      right_just_released: right.just_released,
    )
  let new_input = rebuild_input_with_mouse(sim.input_state, new_mouse)
  Simulation(..sim, input_state: new_input)
}

/// Set left mouse button as pressed
pub fn with_left_button_pressed(
  sim: Simulation(model, msg),
) -> Simulation(model, msg) {
  let left = input.get_left_button_state(sim.input_state)
  let middle = input.get_middle_button_state(sim.input_state)
  let right = input.get_right_button_state(sim.input_state)
  let new_mouse =
    input.build_mouse_state(
      x: input.get_mouse_x(sim.input_state),
      y: input.get_mouse_y(sim.input_state),
      delta_x: input.get_mouse_delta_x(sim.input_state),
      delta_y: input.get_mouse_delta_y(sim.input_state),
      wheel_delta: input.get_mouse_wheel_delta(sim.input_state),
      left_pressed: True,
      left_just_pressed: left.just_pressed,
      left_just_released: left.just_released,
      middle_pressed: middle.pressed,
      middle_just_pressed: middle.just_pressed,
      middle_just_released: middle.just_released,
      right_pressed: right.pressed,
      right_just_pressed: right.just_pressed,
      right_just_released: right.just_released,
    )
  let new_input = rebuild_input_with_mouse(sim.input_state, new_mouse)
  Simulation(..sim, input_state: new_input)
}

/// Set left mouse button as just pressed (for this frame only)
pub fn with_left_button_just_pressed(
  sim: Simulation(model, msg),
) -> Simulation(model, msg) {
  let middle = input.get_middle_button_state(sim.input_state)
  let right = input.get_right_button_state(sim.input_state)
  let new_mouse =
    input.build_mouse_state(
      x: input.get_mouse_x(sim.input_state),
      y: input.get_mouse_y(sim.input_state),
      delta_x: input.get_mouse_delta_x(sim.input_state),
      delta_y: input.get_mouse_delta_y(sim.input_state),
      wheel_delta: input.get_mouse_wheel_delta(sim.input_state),
      left_pressed: True,
      left_just_pressed: True,
      left_just_released: False,
      middle_pressed: middle.pressed,
      middle_just_pressed: middle.just_pressed,
      middle_just_released: middle.just_released,
      right_pressed: right.pressed,
      right_just_pressed: right.just_pressed,
      right_just_released: right.just_released,
    )
  let new_input = rebuild_input_with_mouse(sim.input_state, new_mouse)
  Simulation(..sim, input_state: new_input)
}

/// Set right mouse button as pressed
pub fn with_right_button_pressed(
  sim: Simulation(model, msg),
) -> Simulation(model, msg) {
  let left = input.get_left_button_state(sim.input_state)
  let middle = input.get_middle_button_state(sim.input_state)
  let right = input.get_right_button_state(sim.input_state)
  let new_mouse =
    input.build_mouse_state(
      x: input.get_mouse_x(sim.input_state),
      y: input.get_mouse_y(sim.input_state),
      delta_x: input.get_mouse_delta_x(sim.input_state),
      delta_y: input.get_mouse_delta_y(sim.input_state),
      wheel_delta: input.get_mouse_wheel_delta(sim.input_state),
      left_pressed: left.pressed,
      left_just_pressed: left.just_pressed,
      left_just_released: left.just_released,
      middle_pressed: middle.pressed,
      middle_just_pressed: middle.just_pressed,
      middle_just_released: middle.just_released,
      right_pressed: True,
      right_just_pressed: right.just_pressed,
      right_just_released: right.just_released,
    )
  let new_input = rebuild_input_with_mouse(sim.input_state, new_mouse)
  Simulation(..sim, input_state: new_input)
}

/// Set full input state (for complex scenarios)
pub fn with_input(
  sim: Simulation(model, msg),
  input_state: input.InputState,
) -> Simulation(model, msg) {
  Simulation(..sim, input_state: input_state)
}

/// Clear per-frame input state (just_pressed, just_released, deltas)
///
/// This is automatically called by `frame()`, but you can call it manually
/// if needed.
pub fn clear_input(sim: Simulation(model, msg)) -> Simulation(model, msg) {
  Simulation(..sim, input_state: clear_frame_input_state(sim.input_state))
}

// ============================================================================
// PHYSICS HELPERS
// ============================================================================

/// Step physics explicitly
///
/// Physics is NOT stepped automatically by `frame()`. Use this helper
/// to step physics in your tests, or call `physics.step` in your game's
/// update function.
pub fn step_physics(
  sim: Simulation(model, msg),
  delta: Duration,
) -> Simulation(model, msg) {
  case sim.physics_world {
    option.Some(world) -> {
      let new_world = physics.step(world, delta)
      Simulation(
        ..sim,
        physics_world: option.Some(new_world),
        renderer_state: scene.set_physics_world(
          sim.renderer_state,
          option.Some(new_world),
        ),
      )
    }
    option.None -> sim
  }
}

/// Get transform of a physics body
pub fn get_body_transform(
  sim: Simulation(model, msg),
  id: String,
) -> Result(transform.Transform, Nil) {
  case sim.physics_world {
    option.Some(world) -> physics.get_transform(world, id)
    option.None -> Error(Nil)
  }
}

/// Get collision events from last physics step
pub fn get_collision_events(
  sim: Simulation(model, msg),
) -> List(physics.CollisionEvent) {
  case sim.physics_world {
    option.Some(world) -> physics.get_collision_events(world)
    option.None -> []
  }
}

// ============================================================================
// EFFECT HELPERS
// ============================================================================

/// Clear all recorded effects
pub fn clear_effects(sim: Simulation(model, msg)) -> Simulation(model, msg) {
  Simulation(..sim, recorded_effects: [])
}

/// Check if a specific effect was recorded
///
/// ## Example
///
/// ```gleam
/// let has_jump_sound = simulate.has_effect(sim, fn(e) {
///   case e {
///     simulate.RecordedDispatch(PlaySound("jump")) -> True
///     _ -> False
///   }
/// })
/// ```
pub fn has_effect(
  sim: Simulation(model, msg),
  predicate: fn(RecordedEffect(msg)) -> Bool,
) -> Bool {
  list.any(sim.recorded_effects, predicate)
}

/// Get all dispatched messages from effects
pub fn dispatched_messages(sim: Simulation(model, msg)) -> List(msg) {
  list.filter_map(sim.recorded_effects, fn(e) {
    case e {
      RecordedDispatch(msg) -> Ok(msg)
      _ -> Error(Nil)
    }
  })
}

// ============================================================================
// INTERNAL HELPERS
// ============================================================================

/// Process messages and record effects
fn process_messages(
  model: model,
  messages: List(msg),
  context: tiramisu.Context,
  recorded: List(RecordedEffect(msg)),
  update: fn(model, msg, tiramisu.Context) ->
    #(model, effect.Effect(msg), Option(physics.PhysicsWorld)),
) -> #(model, List(RecordedEffect(msg)), Option(physics.PhysicsWorld)) {
  case messages {
    [] -> #(model, recorded, context.physics_world)
    [msg, ..rest] -> {
      let #(new_model, new_effect, new_physics_world) =
        update(model, msg, context)

      // Record effects
      let new_recorded = record_effect(new_effect, recorded)

      // Update context with new physics world
      let new_context = case new_physics_world {
        option.Some(world) ->
          tiramisu.Context(..context, physics_world: option.Some(world))
        option.None -> context
      }

      process_messages(new_model, rest, new_context, new_recorded, update)
    }
  }
}

/// Record an effect (run it with a recording dispatch)
fn record_effect(
  eff: effect.Effect(msg),
  acc: List(RecordedEffect(msg)),
) -> List(RecordedEffect(msg)) {
  // Run the effect with a recording dispatch function
  // This captures dispatched messages without actually doing side effects
  let messages = capture_dispatched_messages(eff)

  // Add captured messages to recorded effects
  list.fold(messages, acc, fn(acc, msg) { [RecordedDispatch(msg), ..acc] })
}

/// Capture messages dispatched by an effect
fn capture_dispatched_messages(eff: effect.Effect(msg)) -> List(msg) {
  // We use a mutable list via FFI to capture messages
  let captured = array.from_list([])

  effect.run(eff, fn(msg) { push_to_mutable_list(captured, msg) })

  array.to_list(captured)
}

/// Clear per-frame input state
fn clear_frame_input_state(input_state: input.InputState) -> input.InputState {
  // Clear just_pressed, just_released, and deltas
  let keyboard =
    input.build_keyboard_state(
      pressed: input.get_pressed_keys(input_state),
      just_pressed: set.new(),
      just_released: set.new(),
    )

  let left = input.get_left_button_state(input_state)
  let middle = input.get_middle_button_state(input_state)
  let right = input.get_right_button_state(input_state)
  let new_mouse =
    input.build_mouse_state(
      x: input.get_mouse_x(input_state),
      y: input.get_mouse_y(input_state),
      delta_x: 0.0,
      delta_y: 0.0,
      wheel_delta: 0.0,
      left_pressed: left.pressed,
      left_just_pressed: False,
      left_just_released: False,
      middle_pressed: middle.pressed,
      middle_just_pressed: False,
      middle_just_released: False,
      right_pressed: right.pressed,
      right_just_pressed: False,
      right_just_released: False,
    )

  let touch =
    input.build_touch_state(
      active: input.get_active_touches(input_state),
      just_started: [],
      just_ended: [],
    )

  input.build_input_state(
    keyboard: keyboard,
    mouse: new_mouse,
    gamepads: input.get_gamepad_list(input_state),
    touch: touch,
  )
}

/// Rebuild input state with new keyboard
fn rebuild_input_with_keyboard(
  input_state: input.InputState,
  keyboard: input.KeyboardState,
) -> input.InputState {
  input.build_input_state(
    keyboard: keyboard,
    mouse: input.get_mouse_state(input_state),
    gamepads: input.get_gamepad_list(input_state),
    touch: input.get_touch_state(input_state),
  )
}

/// Rebuild input state with new mouse
fn rebuild_input_with_mouse(
  input_state: input.InputState,
  mouse: input.MouseState,
) -> input.InputState {
  input.build_input_state(
    keyboard: input.get_keyboard_state(input_state),
    mouse: mouse,
    gamepads: input.get_gamepad_list(input_state),
    touch: input.get_touch_state(input_state),
  )
}

// ============================================================================
// KEY CODE CONVERSION
// ============================================================================

fn key_to_code(key: input.Key) -> String {
  case key {
    // Letters
    input.KeyA -> "KeyA"
    input.KeyB -> "KeyB"
    input.KeyC -> "KeyC"
    input.KeyD -> "KeyD"
    input.KeyE -> "KeyE"
    input.KeyF -> "KeyF"
    input.KeyG -> "KeyG"
    input.KeyH -> "KeyH"
    input.KeyI -> "KeyI"
    input.KeyJ -> "KeyJ"
    input.KeyK -> "KeyK"
    input.KeyL -> "KeyL"
    input.KeyM -> "KeyM"
    input.KeyN -> "KeyN"
    input.KeyO -> "KeyO"
    input.KeyP -> "KeyP"
    input.KeyQ -> "KeyQ"
    input.KeyR -> "KeyR"
    input.KeyS -> "KeyS"
    input.KeyT -> "KeyT"
    input.KeyU -> "KeyU"
    input.KeyV -> "KeyV"
    input.KeyW -> "KeyW"
    input.KeyX -> "KeyX"
    input.KeyY -> "KeyY"
    input.KeyZ -> "KeyZ"
    // Numbers
    input.Digit0 -> "Digit0"
    input.Digit1 -> "Digit1"
    input.Digit2 -> "Digit2"
    input.Digit3 -> "Digit3"
    input.Digit4 -> "Digit4"
    input.Digit5 -> "Digit5"
    input.Digit6 -> "Digit6"
    input.Digit7 -> "Digit7"
    input.Digit8 -> "Digit8"
    input.Digit9 -> "Digit9"
    // Function keys
    input.F1 -> "F1"
    input.F2 -> "F2"
    input.F3 -> "F3"
    input.F4 -> "F4"
    input.F5 -> "F5"
    input.F6 -> "F6"
    input.F7 -> "F7"
    input.F8 -> "F8"
    input.F9 -> "F9"
    input.F10 -> "F10"
    input.F11 -> "F11"
    input.F12 -> "F12"
    // Special keys
    input.Space -> "Space"
    input.Enter -> "Enter"
    input.Escape -> "Escape"
    input.Tab -> "Tab"
    input.Backspace -> "Backspace"
    input.Delete -> "Delete"
    input.Insert -> "Insert"
    input.Home -> "Home"
    input.End -> "End"
    input.PageUp -> "PageUp"
    input.PageDown -> "PageDown"
    // Arrow keys
    input.ArrowUp -> "ArrowUp"
    input.ArrowDown -> "ArrowDown"
    input.ArrowLeft -> "ArrowLeft"
    input.ArrowRight -> "ArrowRight"
    // Modifiers
    input.ShiftLeft -> "ShiftLeft"
    input.ShiftRight -> "ShiftRight"
    input.ControlLeft -> "ControlLeft"
    input.ControlRight -> "ControlRight"
    input.AltLeft -> "AltLeft"
    input.AltRight -> "AltRight"
    input.MetaLeft -> "MetaLeft"
    input.MetaRight -> "MetaRight"
    // Symbols and punctuation
    input.Minus -> "Minus"
    input.Equal -> "Equal"
    input.BracketLeft -> "BracketLeft"
    input.BracketRight -> "BracketRight"
    input.Backslash -> "Backslash"
    input.Semicolon -> "Semicolon"
    input.Quote -> "Quote"
    input.Backquote -> "Backquote"
    input.Comma -> "Comma"
    input.Period -> "Period"
    input.Slash -> "Slash"
    // Other
    input.CapsLock -> "CapsLock"
    input.NumLock -> "NumLock"
    input.ScrollLock -> "ScrollLock"
    input.PrintScreen -> "PrintScreen"
    input.Pause -> "Pause"
    // Numpad
    input.Numpad0 -> "Numpad0"
    input.Numpad1 -> "Numpad1"
    input.Numpad2 -> "Numpad2"
    input.Numpad3 -> "Numpad3"
    input.Numpad4 -> "Numpad4"
    input.Numpad5 -> "Numpad5"
    input.Numpad6 -> "Numpad6"
    input.Numpad7 -> "Numpad7"
    input.Numpad8 -> "Numpad8"
    input.Numpad9 -> "Numpad9"
    input.NumpadAdd -> "NumpadAdd"
    input.NumpadSubtract -> "NumpadSubtract"
    input.NumpadMultiply -> "NumpadMultiply"
    input.NumpadDivide -> "NumpadDivide"
    input.NumpadDecimal -> "NumpadDecimal"
    input.NumpadEnter -> "NumpadEnter"
    // Media keys
    input.AudioVolumeDown -> "AudioVolumeDown"
    input.AudioVolumeMute -> "AudioVolumeMute"
    input.AudioVolumeUp -> "AudioVolumeUp"
    input.MediaPlayPause -> "MediaPlayPause"
    input.MediaStop -> "MediaStop"
    input.MediaTrackNext -> "MediaTrackNext"
    input.MediaTrackPrevious -> "MediaTrackPrevious"
    // Context menu
    input.ContextMenu -> "ContextMenu"
    // Custom key
    input.Custom(code) -> code
  }
}

// ============================================================================
// FFI HELPERS
// ============================================================================

@external(javascript, "./simulate.ffi.mjs", "pushToMutableList")
fn push_to_mutable_list(list: array.Array(b), value: b) -> Nil
