import gleam/time/duration.{type Duration}
import gleam/time/timestamp.{type Timestamp}
import lustre/effect.{type Effect}

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
    let _key =
      subscribe_to_ticks(scene_id, fn(ctx) { dispatch(handler(ctx)) })
    Nil
  })
}

/// Unsubscribe from animation frame ticks.
///
/// Call this when your component unmounts or no longer needs tick updates.
///
pub fn unsubscribe(scene_id: String) -> Effect(msg) {
  effect.from(fn(_dispatch) { unsubscribe_from_ticks(scene_id) })
}

// FFI -------------------------------------------------------------------------

@external(javascript, "./tick.ffi.mjs", "subscribeToTicks")
fn subscribe_to_ticks(
  scene_id: String,
  handler: fn(TickContext) -> Nil,
) -> String

@external(javascript, "./tick.ffi.mjs", "unsubscribeFromTicks")
fn unsubscribe_from_ticks(scene_id: String) -> Nil
