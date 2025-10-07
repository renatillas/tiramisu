import gleam/option.{type Option}
import vec/vec3

// --- Public Types ---

/// Audio buffer type (opaque, wraps Web Audio API AudioBuffer)
pub type AudioBuffer

/// Audio playback configuration
pub type AudioConfig {
  AudioConfig(
    /// Volume (0.0 to 1.0)
    volume: Float,
    /// Whether to loop the audio
    loop: Bool,
    /// Playback rate (1.0 = normal speed)
    playback_rate: Float,
    /// Whether the audio should autoplay
    autoplay: Bool,
  )
}

/// Type of audio (global or positional)
pub type AudioType {
  /// Global audio (2D, same volume everywhere)
  GlobalAudio
  /// Positional audio (3D, volume based on distance)
  PositionalAudio(
    /// Maximum hearing distance
    ref_distance: Float,
    /// How quickly audio fades with distance
    rolloff_factor: Float,
    /// Maximum distance where audio can be heard
    max_distance: Float,
  )
}

/// Opaque handle to a THREE.Audio or THREE.PositionalAudio object
pub type AudioSource

// --- Constructor Functions ---

/// Create default audio config
pub fn config() -> AudioConfig {
  AudioConfig(volume: 1.0, loop: False, playback_rate: 1.0, autoplay: False)
}

/// Set volume (0.0 to 1.0)
pub fn set_volume(config: AudioConfig, volume: Float) -> AudioConfig {
  AudioConfig(..config, volume: volume)
}

/// Set looping
pub fn set_loop(config: AudioConfig, loop: Bool) -> AudioConfig {
  AudioConfig(..config, loop: loop)
}

/// Set playback rate (1.0 = normal, 2.0 = double speed, etc.)
pub fn set_playback_rate(config: AudioConfig, rate: Float) -> AudioConfig {
  AudioConfig(..config, playback_rate: rate)
}

/// Set autoplay
pub fn set_autoplay(config: AudioConfig, autoplay: Bool) -> AudioConfig {
  AudioConfig(..config, autoplay: autoplay)
}

/// Create a default positional audio configuration
pub fn positional() -> AudioType {
  PositionalAudio(
    ref_distance: 1.0,
    rolloff_factor: 1.0,
    max_distance: 10_000.0,
  )
}

/// Set reference distance for positional audio
pub fn set_ref_distance(audio: AudioType, distance: Float) -> AudioType {
  case audio {
    PositionalAudio(_, rolloff, max) ->
      PositionalAudio(
        ref_distance: distance,
        rolloff_factor: rolloff,
        max_distance: max,
      )
    GlobalAudio -> GlobalAudio
  }
}

/// Set rolloff factor for positional audio
pub fn set_rolloff_factor(audio: AudioType, factor: Float) -> AudioType {
  case audio {
    PositionalAudio(ref, _, max) ->
      PositionalAudio(
        ref_distance: ref,
        rolloff_factor: factor,
        max_distance: max,
      )
    GlobalAudio -> GlobalAudio
  }
}

/// Set maximum distance for positional audio
pub fn set_max_distance(audio: AudioType, distance: Float) -> AudioType {
  case audio {
    PositionalAudio(ref, rolloff, _) ->
      PositionalAudio(
        ref_distance: ref,
        rolloff_factor: rolloff,
        max_distance: distance,
      )
    GlobalAudio -> GlobalAudio
  }
}

// --- Audio Control ---

/// Play an audio source
pub fn play(source_id: String) -> Nil {
  play_audio_ffi(source_id)
}

/// Pause an audio source
pub fn pause(source_id: String) -> Nil {
  pause_audio_ffi(source_id)
}

/// Stop an audio source (resets to beginning)
pub fn stop(source_id: String) -> Nil {
  stop_audio_ffi(source_id)
}

/// Set volume of a playing audio source
pub fn set_source_volume(source_id: String, volume: Float) -> Nil {
  set_volume_ffi(source_id, volume)
}

/// Check if an audio source is playing
pub fn is_playing(source_id: String) -> Bool {
  is_playing_ffi(source_id)
}

/// Get current playback time in seconds
pub fn get_current_time(source_id: String) -> Float {
  get_time_ffi(source_id)
}

/// Set current playback time in seconds
pub fn set_current_time(source_id: String, time: Float) -> Nil {
  set_time_ffi(source_id, time)
}

// --- Scene Integration ---

/// Audio node for the scene graph
/// This is a declarative way to add audio to the scene
pub type AudioNode {
  AudioNode(
    id: String,
    config: AudioConfig,
    audio_type: AudioType,
    position: Option(vec3.Vec3(Float)),
  )
}

/// Create a global audio node (2D audio)
pub fn global_audio(id: String, config: AudioConfig) -> AudioNode {
  AudioNode(
    id: id,
    config: config,
    audio_type: GlobalAudio,
    position: option.None,
  )
}

/// Create a positional audio node (3D audio at a position)
pub fn positional_audio(
  id: String,
  config: AudioConfig,
  position: vec3.Vec3(Float),
) -> AudioNode {
  AudioNode(
    id: id,
    config: config,
    audio_type: positional(),
    position: option.Some(position),
  )
}

/// Create a positional audio node with custom settings
pub fn positional_audio_custom(
  id: String,
  config: AudioConfig,
  position: vec3.Vec3(Float),
  audio_type: AudioType,
) -> AudioNode {
  AudioNode(
    id: id,
    config: config,
    audio_type: audio_type,
    position: option.Some(position),
  )
}

// --- FFI Functions ---

@external(javascript, "./ffi/audio.mjs", "playAudio")
fn play_audio_ffi(source_id: String) -> Nil

@external(javascript, "./ffi/audio.mjs", "pauseAudio")
fn pause_audio_ffi(source_id: String) -> Nil

@external(javascript, "./ffi/audio.mjs", "stopAudio")
fn stop_audio_ffi(source_id: String) -> Nil

@external(javascript, "./ffi/audio.mjs", "setAudioVolume")
fn set_volume_ffi(source_id: String, volume: Float) -> Nil

@external(javascript, "./ffi/audio.mjs", "isAudioPlaying")
fn is_playing_ffi(source_id: String) -> Bool

@external(javascript, "./ffi/audio.mjs", "getAudioTime")
fn get_time_ffi(source_id: String) -> Float

@external(javascript, "./ffi/audio.mjs", "setAudioTime")
fn set_time_ffi(source_id: String, time: Float) -> Nil
