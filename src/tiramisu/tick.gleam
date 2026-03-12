//// Frame tick events for Tiramisu scenes.
////
//// Ticks are emitted by the renderer loop and dispatched on the scene element.
//// This keeps frame timing inside the normal Lustre attribute/event model rather
//// than requiring a separate subscription API.

import gleam/dynamic/decode
import gleam/float
import gleam/time/duration
import gleam/time/timestamp
import lustre/attribute.{type Attribute}
import lustre/event

// TYPES -----------------------------------------------------------------------

/// Context provided on each animation frame tick.
///
/// Contains timing and input information useful for smooth animations
/// and responsive game controls.
pub type TickContext {
  TickContext(
    /// Time elapsed since the last frame (typically ~16ms at 60fps).
    /// Use `duration.to_seconds()` to convert to a Float for animation math.
    delta_time: duration.Duration,
    /// The timestamp when this frame was rendered.
    /// Useful for time-based effects or synchronized animations.
    timestamp: timestamp.Timestamp,
  )
}

// EVENTS ----------------------------------------------------------------------

/// Attach a per-frame tick handler to the current scene element.
pub fn on_tick(
  to_msg: fn(TickContext) -> msg,
) -> Attribute(msg) {
  event.on("tiramisu:tick", {
    use tick_context <- decode.field("detail", tick_context_decoder())
    decode.success(to_msg(tick_context))
  })
}

fn tick_context_decoder() -> decode.Decoder(TickContext) {
  use delta_ms <- decode.field("delta_ms", decode.float)
  use timestamp_ms <- decode.field("timestamp_ms", decode.int)

  let seconds = timestamp_ms / 1000
  let milliseconds = timestamp_ms - seconds * 1000
  let nanoseconds = milliseconds * 1000000

  TickContext(
    delta_time: duration.milliseconds(float.round(delta_ms)),
    timestamp: timestamp.from_unix_seconds_and_nanoseconds(seconds, nanoseconds),
  )
  |> decode.success
}
