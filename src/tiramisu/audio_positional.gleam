//// The tiramisu-audio-positional element.
////
//// Positional (3D) audio source. Audio volume and panning are affected
//// by the listener's position relative to the audio source. Use for
//// sound effects attached to objects in the 3D scene.
////
//// ## Usage
////
//// ```html
//// <tiramisu-audio-positional
////   id="engine_sound"
////   src="sounds/engine.mp3"
////   volume="0.8"
////   loop="true"
////   playing="true"
////   transform="pos:5,0,0"
////   ref-distance="1"
////   max-distance="100"
//// ></tiramisu-audio-positional>
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
//// - `transform`: Position in 3D space (default: "pos:0,0,0")
//// - `ref-distance`: Distance where attenuation starts (default: 1.0)
//// - `max-distance`: Maximum distance for attenuation (default: 10000.0)
//// - `rolloff-factor`: How quickly volume decreases (default: 1.0)

// IMPORTS ---------------------------------------------------------------------

import gleam/float

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

import tiramisu/transform.{type Transform}

import vec/vec3

// CONSTANTS -------------------------------------------------------------------

/// The tag name for positional audio elements.
pub const tag_name = "tiramisu-audio-positional"

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-audio-positional element (3D audio).
///
/// Positional audio changes volume and panning based on distance and
/// direction relative to the listener. Use for 3D sound effects
/// attached to objects in the scene.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/audio_positional
///
/// audio_positional.audio_positional("car_engine", [
///   audio_positional.src("sounds/engine.mp3"),
///   audio_positional.volume(0.8),
///   audio_positional.loop(True),
///   audio_positional.playing(True),
///   audio_positional.audio_transform(transform.at(vec3.Vec3(5.0, 0.0, 0.0))),
///   audio_positional.ref_distance(1.0),
///   audio_positional.max_distance(50.0),
/// ], [])
/// ```
///
pub fn audio_positional(
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

/// Set the full transform (position) of the audio source.
///
pub fn audio_transform(t: Transform) -> Attribute(msg) {
  let vec3.Vec3(px, py, pz) = transform.position(t)
  let #(qx, qy, qz, qw) = transform.to_quaternion_xyzw(t)
  let vec3.Vec3(sx, sy, sz) = transform.scale(t)

  attribute.attribute(
    "transform",
    "pos:"
      <> float.to_string(px)
      <> ","
      <> float.to_string(py)
      <> ","
      <> float.to_string(pz)
      <> " quat:"
      <> float.to_string(qx)
      <> ","
      <> float.to_string(qy)
      <> ","
      <> float.to_string(qz)
      <> ","
      <> float.to_string(qw)
      <> " scale:"
      <> float.to_string(sx)
      <> ","
      <> float.to_string(sy)
      <> ","
      <> float.to_string(sz),
  )
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
