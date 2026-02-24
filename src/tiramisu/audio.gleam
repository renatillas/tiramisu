import gleam/float
import lustre/attribute.{type Attribute}

@internal
pub const positional_tag = "tiramisu-positional-audio"

@internal
pub const global_tag = "tiramisu-global-audio"

/// Set the volume level (0.0-1.0).
///
/// Works with audio and positional audio elements.
///
pub fn volume(level: Float) -> Attribute(msg) {
  attribute.attribute("volume", float.to_string(level))
}

/// Set whether the audio is playing.
///
/// Toggle this to play/pause the audio.
/// Works with audio and positional audio elements.
///
pub fn playing(is_playing: Bool) -> Attribute(msg) {
  attribute.attribute("playing", case is_playing {
    True -> "true"
    False -> "false"
  })
}

/// Set the playback rate (1.0 = normal speed).
///
/// Works with audio and positional audio elements.
///
pub fn playback_rate(rate: Float) -> Attribute(msg) {
  attribute.attribute("playback-rate", float.to_string(rate))
}

/// Set the detune (pitch shift) in cents.
///
/// 100 cents = 1 semitone, 1200 cents = 1 octave up.
/// Use negative values to pitch down. Default is 0.0 (no shift).
/// Works with audio and positional audio elements.
///
pub fn detune(cents: Float) -> Attribute(msg) {
  attribute.attribute("detune", float.to_string(cents))
}

/// Set the reference distance for attenuation.
///
/// The distance at which the volume reduction starts.
///
pub fn ref_distance(distance: Float) -> Attribute(msg) {
  attribute.attribute("ref-distance", float.to_string(distance))
}

/// Set the maximum distance for attenuation.
///
/// Beyond this distance, audio is not attenuated further.
///
pub fn max_distance(distance: Float) -> Attribute(msg) {
  attribute.attribute("max-distance", float.to_string(distance))
}

/// Set the rolloff factor.
///
/// Higher values = faster volume dropoff with distance.
///
pub fn rolloff_factor(factor: Float) -> Attribute(msg) {
  attribute.attribute("rolloff-factor", float.to_string(factor))
}
