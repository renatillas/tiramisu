//// Tick subscription for animation loops.
////
//// This module provides a way to subscribe to the render loop's animation
//// frames, receiving timing and input information on each frame for smooth
//// animations and responsive game controls.
////
//// ## Scene IDs
////
//// There are three ways to specify which scene to subscribe to:
////
//// ### 1. Global subscription (recommended for single-renderer apps)
////
//// Use an empty string `""` to receive ticks from all scenes:
////
//// ```gleam
//// fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
////   #(Model(...), tick.subscribe("", Tick))
//// }
//// ```
////
//// ### 2. User-defined scene ID
////
//// Set a known scene ID on the renderer and use it directly:
////
//// ```gleam
//// // In your view:
//// renderer.renderer([renderer.scene_id("my-scene")], [...])
////
//// // In your init:
//// fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
////   #(Model(...), tick.subscribe("my-scene", Tick))
//// }
//// ```
////
//// ### 3. Dynamic scene ID via event
////
//// Listen for the scene-ready event and subscribe afterwards:
////
//// ```gleam
//// pub type Msg {
////   SceneReady(String)
////   Tick(tick.TickContext)
//// }
////
//// fn view(model: Model) {
////   renderer.renderer([renderer.on_scene_ready(SceneReady)], [...])
//// }
////
//// fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
////   case msg {
////     SceneReady(scene_id) -> {
////       #(Model(..model, scene_id: Some(scene_id)), tick.subscribe(scene_id, Tick))
////     }
////     Tick(ctx) -> {
////       let dt = duration.to_seconds(ctx.delta_time)
////       // Use dt for smooth animation...
////
////       // Access input state
////       let moving = input.is_key_pressed(ctx.input, input.KeyW)
////       let #(dx, dy) = input.mouse_delta(ctx.input)
////     }
////   }
//// }
//// ```

import gleam/time/duration.{type Duration}
import gleam/time/timestamp.{type Timestamp}
import lustre/effect.{type Effect}
import tiramisu/input.{type InputState}

// TYPES -----------------------------------------------------------------------

/// Context provided on each animation frame tick.
///
/// Contains timing and input information useful for smooth animations
/// and responsive game controls.
pub type TickContext {
  TickContext(
    /// Time elapsed since the last frame (typically ~16ms at 60fps).
    /// Use `duration.to_seconds()` to convert to a Float for animation math.
    delta_time: Duration,
    /// The timestamp when this frame was rendered.
    /// Useful for time-based effects or synchronized animations.
    timestamp: Timestamp,
    /// Current input state (keyboard, mouse, gamepad, touch).
    /// Use functions from `tiramisu/input` to query this state.
    input: InputState,
  )
}

// SUBSCRIPTIONS ---------------------------------------------------------------

/// Subscribe to animation frame ticks.
///
/// The handler function will be called on each animation frame with timing
/// information. Use the delta_time for smooth, frame-rate independent animation.
///
/// ## Parameters
///
/// - `scene_id`: The scene to subscribe to. Options:
///   - `""` (empty string): Receive ticks from ALL scenes (global handler)
///   - User-defined ID: Set via `renderer.scene_id("my-id")` attribute
///   - Dynamic ID: Received from `renderer.on_scene_ready` event
/// - `handler`: A function that receives a `TickContext` and returns your message type.
///
/// ## Examples
///
/// ```gleam
/// // Global subscription (all scenes)
/// tick.subscribe("", Tick)
///
/// // Specific scene (using user-defined ID)
/// tick.subscribe("my-scene", Tick)
/// ```
///
pub fn subscribe(
  scene_id: String,
  handler: fn(TickContext) -> msg,
) -> Effect(msg) {
  effect.from(fn(dispatch) {
    subscribe_to_ticks(scene_id, fn(ctx) { dispatch(handler(ctx)) })
  })
}

/// Unsubscribe from animation frame ticks.
///
/// Call this when your component unmounts or no longer needs tick updates.
///
pub fn unsubscribe(scene_id: String) -> Effect(msg) {
  effect.from(fn(_dispatch) { unsubscribe_from_ticks(scene_id) })
}

// POINTER LOCK ----------------------------------------------------------------

/// Request pointer lock on the renderer canvas.
///
/// This should be called in response to a user interaction (e.g., click).
/// When pointer lock is active, `input.mouse_delta()` will return relative
/// movement rather than absolute position, which is ideal for FPS-style
/// camera controls.
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
///   case msg {
///     StartGame -> #(model, tick.request_pointer_lock())
///     // ...
///   }
/// }
/// ```
///
pub fn request_pointer_lock() -> Effect(msg) {
  effect.from(fn(_dispatch) { request_pointer_lock_ffi() })
}

/// Exit pointer lock mode.
///
/// This will return the cursor to normal mode.
///
pub fn exit_pointer_lock() -> Effect(msg) {
  effect.from(fn(_dispatch) { exit_pointer_lock_ffi() })
}

// FFI -------------------------------------------------------------------------

@external(javascript, "./tick.ffi.mjs", "subscribeToTicks")
fn subscribe_to_ticks(
  scene_id: String,
  handler: fn(TickContext) -> Nil,
) -> Nil

@external(javascript, "./tick.ffi.mjs", "unsubscribeFromTicks")
fn unsubscribe_from_ticks(scene_id: String) -> Nil

@external(javascript, "./tick.ffi.mjs", "requestPointerLock")
fn request_pointer_lock_ffi() -> Nil

@external(javascript, "./tick.ffi.mjs", "exitPointerLock")
fn exit_pointer_lock_ffi() -> Nil
