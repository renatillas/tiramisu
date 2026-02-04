//// Game context containing frame timing, input state, and canvas dimensions.
////
//// This module defines the Context type that is passed to your game through
//// tick subscriptions. The context contains all the dynamic state from the
//// game loop: delta time since last frame, current input state, canvas size,
//// and optional physics world reference.
////
//// ## Usage
////
//// ```gleam
//// import tiramisu/context.{type Context}
//// import tiramisu
////
//// type Msg {
////   Tick(Context)
//// }
////
//// fn init(_flags) -> #(Model, Effect(Msg)) {
////   #(Model(...), tiramisu.subscribe_to_ticks(Tick))
//// }
////
//// fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
////   case msg {
////     Tick(ctx) -> {
////       let delta = duration.to_seconds(ctx.delta_time)
////       let new_pos = model.pos +. delta *. model.velocity
////       #(Model(..model, pos: new_pos), effect.none())
////     }
////   }
//// }
//// ```

import gleam/option.{type Option}
import gleam/time/duration.{type Duration}
import tiramisu/input.{type InputState}
import tiramisu/physics.{type PhysicsWorld}
import vec/vec2.{type Vec2}

// TYPES -----------------------------------------------------------------------

/// Context contains all dynamic state from the game loop.
///
/// This is provided to your update function through tick subscriptions,
/// giving you access to frame timing, input, and canvas information.
pub type Context {
  Context(
    /// Time elapsed since the previous frame.
    /// Use `duration.to_seconds()` to convert to Float for physics calculations.
    delta_time: Duration,
    /// Current state of keyboard, mouse, touch, and gamepad input.
    input: InputState,
    /// Current canvas dimensions in pixels.
    canvas_size: Vec2(Float),
    /// Optional physics world reference (if physics is enabled).
    physics_world: Option(PhysicsWorld),
  )
}

// CONSTRUCTOR -----------------------------------------------------------------

/// Create a new context with the given values.
///
/// This is typically only used internally by the platform.
pub fn new(
  delta_time delta: Duration,
  input input_state: InputState,
  canvas_size size: Vec2(Float),
  physics_world world: Option(PhysicsWorld),
) -> Context {
  Context(
    delta_time: delta,
    input: input_state,
    canvas_size: size,
    physics_world: world,
  )
}
