//// Structural scene root for Tiramisu.
////
//// A scene lives inside a `tiramisu.renderer` and owns the scene-node subtree
//// that will be parsed and reconciled into Three.js objects.
////
//// Scene roots are structural rather than imperative: application code declares
//// the scene subtree, and Tiramisu reconciles it into the runtime.

import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/string
import gleam/time/duration
import gleam/time/timestamp
import lustre/attribute.{type Attribute}
import lustre/element.{type Element}
import lustre/event

// EVENTS ----------------------------------------------------------------------

/// The custom element tag used for scene roots.
@internal
pub const tag = "tiramisu-scene"

@internal
pub fn observed_attributes() -> List(String) {
  ["background", "background-color-space", "fog"]
}

/// Context provided on each animation frame tick.
///
/// Contains timing and input information useful for smooth animations
/// and responsive game controls.
pub type Tick {
  Tick(
    /// Time elapsed since the last frame (typically ~16ms at 60fps).
    /// Use `duration.to_seconds()` to convert to a Float for animation math.
    delta_time: duration.Duration,
    /// The timestamp when this frame was rendered.
    /// Useful for time-based effects or synchronized animations.
    timestamp: timestamp.Timestamp,
  )
}

/// Create a `tiramisu-scene` element.
pub fn scene(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag, [attribute.id(id), ..attributes], children)
}

/// Set the background color for the scene (as hex int).
pub fn background_color(hex: Int) -> Attribute(msg) {
  attribute.attribute("background", int.to_base16(hex))
}

/// Clear any scene background.
pub fn clear_background() -> Attribute(msg) {
  attribute.attribute("background", "none")
}

/// Set the background to a flat 2D texture.
pub fn background_texture(url: String) -> Attribute(msg) {
  attribute.attribute("background", "texture:" <> url)
}

/// Set the background to an equirectangular panorama.
pub fn background_equirectangular(url: String) -> Attribute(msg) {
  attribute.attribute("background", "equirectangular:" <> url)
}

/// Set the background to a cubemap skybox.
pub fn background_cube(
  positive_x px: String,
  negative_x nx: String,
  positive_y py: String,
  negative_y ny: String,
  positive_z pz: String,
  negative_z nz: String,
) -> Attribute(msg) {
  attribute.attribute(
    "background",
    "cube:" <> string.join([px, nx, py, ny, pz, nz], with: "|"),
  )
}

/// Set the color space used for loaded scene background textures.
pub fn background_color_space_linear() -> Attribute(msg) {
  attribute.attribute("background-color-space", "linear-srgb")
}

pub fn background_color_space_srgb() -> Attribute(msg) {
  attribute.attribute("background-color-space", "srgb")
}

/// Set linear fog on the scene.
pub fn fog(color color: Int, near near: Float, far far: Float) -> Attribute(msg) {
  attribute.attribute(
    "fog",
    "linear:"
      <> int.to_base16(color)
      <> "|"
      <> float.to_string(near)
      <> "|"
      <> float.to_string(far),
  )
}

/// Set exponential fog on the scene.
pub fn fog_exp2(color color: Int, density density: Float) -> Attribute(msg) {
  attribute.attribute(
    "fog",
    "exp2:" <> int.to_base16(color) <> "|" <> float.to_string(density),
  )
}

/// Clear fog from the scene.
pub fn clear_fog() -> Attribute(msg) {
  attribute.attribute("fog", "none")
}

pub fn on_tick(to_msg: fn(Tick) -> msg) -> Attribute(msg) {
  event.on("tiramisu:tick", {
    use tick_context <- decode.field("detail", tick_decoder())
    decode.success(to_msg(tick_context))
  })
}

/// Attach a per-frame tick handler to the current scene element.
fn tick_decoder() -> decode.Decoder(Tick) {
  use delta_ms <- decode.field("delta_ms", decode.float)
  use timestamp_ms <- decode.field("timestamp_ms", decode.int)

  let seconds = timestamp_ms / 1000
  let milliseconds = timestamp_ms - seconds * 1000
  let nanoseconds = milliseconds * 1_000_000

  Tick(
    delta_time: duration.milliseconds(float.round(delta_ms)),
    timestamp: timestamp.from_unix_seconds_and_nanoseconds(seconds, nanoseconds),
  )
  |> decode.success
}
