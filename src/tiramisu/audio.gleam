//// The tiramisu-audio element.
////
//// Global (non-positional) audio source. Audio plays at constant volume
//// regardless of camera/listener position. Use for background music,
//// UI sounds, and ambient audio.
////
//// ## Usage
////
//// ```html
//// <tiramisu-audio
////   id="music"
////   src="sounds/background.mp3"
////   volume="0.5"
////   loop="true"
////   playing="true"
//// ></tiramisu-audio>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier for the audio (required)
//// - `src`: URL to the audio file (required)
//// - `volume`: Volume level 0.0-1.0 (default: 1.0)
//// - `loop`: Whether to loop playback (default: "false")
//// - `playing`: Whether to play/pause (default: "false")
//// - `playback-rate`: Playback speed multiplier (default: 1.0)
//// - `detune`: Pitch shift in cents (default: 0.0, 100 = 1 semitone up)

// IMPORTS ---------------------------------------------------------------------

import gleam/float

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

// CONSTANTS -------------------------------------------------------------------

/// The tag name for audio elements.
pub const tag_name = "tiramisu-audio"

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-audio element (global audio).
///
/// Global audio plays at constant volume regardless of camera position.
/// Use for background music, UI sounds, and ambient audio.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/audio
///
/// audio.audio("background_music", [
///   audio.src("sounds/music.mp3"),
///   audio.volume(0.5),
///   audio.loop(True),
///   audio.playing(model.music_playing),
/// ], [])
/// ```
///
pub fn audio(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the source URL for the audio file.
///
pub fn src(url: String) -> Attribute(msg) {
  attribute.attribute("src", url)
}

/// Set the volume level (0.0-1.0).
///
pub fn volume(level: Float) -> Attribute(msg) {
  attribute.attribute("volume", float.to_string(level))
}

/// Set whether the audio should loop.
///
pub fn loop(should_loop: Bool) -> Attribute(msg) {
  attribute.attribute("loop", case should_loop {
    True -> "true"
    False -> "false"
  })
}

/// Set whether the audio is playing.
///
/// Toggle this to play/pause the audio.
///
pub fn playing(is_playing: Bool) -> Attribute(msg) {
  attribute.attribute("playing", case is_playing {
    True -> "true"
    False -> "false"
  })
}

/// Set the playback rate (1.0 = normal speed).
///
pub fn playback_rate(rate: Float) -> Attribute(msg) {
  attribute.attribute("playback-rate", float.to_string(rate))
}

/// Set the detune (pitch shift) in cents.
///
/// 100 cents = 1 semitone, 1200 cents = 1 octave up.
/// Use negative values to pitch down. Default is 0.0 (no shift).
///
pub fn detune(cents: Float) -> Attribute(msg) {
  attribute.attribute("detune", float.to_string(cents))
}
