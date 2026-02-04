//// Tiramisu - Game engine subscriptions and utilities.
////
//// This module provides the main subscription API for game loops, similar to
//// how OpenTUI provides keyboard subscriptions. Instead of passing context as
//// a parameter, your game subscribes to tick events that include the context.
////
//// ## Example
////
//// ```gleam
//// import lustre
//// import lustre/effect
//// import tiramisu
//// import tiramisu/context.{type Context}
//// import tiramisu/platform
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
////       let new_x = model.x +. delta *. model.velocity
////       #(Model(..model, x: new_x), effect.none())
////     }
////   }
//// }
////
//// pub fn main() {
////   let assert Ok(platform) = platform.platform("#game")
////   let app = lustre.application(init, update, view)
////   let assert Ok(_) = lustre.start(app, on: platform, with: Nil)
////   Nil
//// }
//// ```

// RE-EXPORTS ------------------------------------------------------------------

// Re-export commonly used types and modules
pub type Context =
  context.Context

// IMPORTS ---------------------------------------------------------------------

import lustre/effect.{type Effect}
import tiramisu/context

// SUBSCRIPTION EFFECTS --------------------------------------------------------

/// Subscribe to game tick events. Dispatches `handler(Context)` on every
/// animation frame (typically 60 FPS). Call this in your `init` function
/// to start receiving tick events with frame timing and input state.
///
/// The Context includes:
/// - `delta_time`: Duration since last frame
/// - `input`: Current keyboard/mouse/touch/gamepad state
/// - `canvas_size`: Current canvas dimensions
/// - `physics_world`: Optional physics world reference
///
/// ## Example
///
/// ```gleam
/// type Msg {
////   Tick(Context)
////   UserAction
//// }
////
//// fn init(_) -> #(Model, Effect(Msg)) {
////   #(Model(...), tiramisu.subscribe_to_ticks(Tick))
//// }
////
//// fn update(model, msg) {
////   case msg {
////     Tick(ctx) -> {
////       let delta_seconds = duration.to_seconds(ctx.delta_time)
////       let new_rotation = model.rotation +. delta_seconds
////       #(Model(..model, rotation: new_rotation), effect.none())
////     }
////     UserAction -> // ...
////   }
//// }
/// ```
pub fn subscribe_to_ticks(handler: fn(Context) -> msg) -> Effect(msg) {
  effect.from(do_subscribe_to_ticks(handler, _))
}

// PLATFORM INTEGRATION --------------------------------------------------------

/// Set the renderer and scene for the integrated render loop.
/// This is called internally by the platform - you don't need to call this.
@external(javascript, "./tiramisu.ffi.mjs", "setRendererAndScene")
pub fn set_renderer_and_scene(renderer: a, scene: b) -> Nil

// FFI -------------------------------------------------------------------------

@external(javascript, "./tiramisu.ffi.mjs", "subscribe_to_ticks")
fn do_subscribe_to_ticks(
  handler: fn(Context) -> msg,
  dispatch: fn(msg) -> Nil,
) -> Nil
