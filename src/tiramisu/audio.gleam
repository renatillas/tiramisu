import gleam/option

// --- Public Types ---

/// Audio buffer type (opaque, wraps Web Audio API AudioBuffer)
pub type AudioBuffer

/// Audio group categories for volume control
pub type AudioGroup {
  /// Sound effects (footsteps, gunshots, etc.)
  SFX
  /// Background music
  Music
  /// Voice lines and dialogue
  Voice
  /// Ambient sounds (wind, rain, etc.)
  Ambient
  /// Custom group with a name
  Custom(String)
}

/// Audio playback state
pub type AudioState {
  /// Audio is playing
  Playing
  /// Audio is stopped (reset to beginning)
  Stopped
  /// Audio is paused (can be resumed)
  Paused
}

/// Fade configuration for smooth transitions
pub type FadeConfig {
  /// No fade (instant transition)
  NoFade
  /// Fade in/out over specified milliseconds
  Fade(duration_ms: Int)
}

/// Audio playback configuration
pub type AudioConfig {
  AudioConfig(
    /// Playback state (Playing, Stopped, Paused)
    state: AudioState,
    /// Volume (0.0 to 1.0)
    volume: Float,
    /// Whether to loop the audio
    loop: Bool,
    /// Playback rate (1.0 = normal speed)
    playback_rate: Float,
    /// Fade configuration for state transitions
    fade: FadeConfig,
    /// Audio group for volume control (optional)
    group: option.Option(AudioGroup),
    /// Callback when audio ends (for non-looping audio)
    on_end: option.Option(fn() -> Nil),
  )
}

/// Type of audio (global or positional)
pub type Audio {
  /// Global audio (2D, same volume everywhere)
  GlobalAudio(buffer: AudioBuffer, config: AudioConfig)
  /// Positional audio (3D, volume based on distance)
  PositionalAudio(
    buffer: AudioBuffer,
    config: AudioConfig,
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

/// Create default audio config (stopped, no fade)
pub fn config() -> AudioConfig {
  AudioConfig(
    state: Stopped,
    volume: 1.0,
    loop: False,
    playback_rate: 1.0,
    fade: NoFade,
    group: option.None,
    on_end: option.None,
  )
}

/// Create audio config that starts playing
pub fn playing() -> AudioConfig {
  AudioConfig(
    state: Playing,
    volume: 1.0,
    loop: False,
    playback_rate: 1.0,
    fade: NoFade,
    group: option.None,
    on_end: option.None,
  )
}

/// Set playback state (Playing, Stopped, Paused)
pub fn with_state(config: AudioConfig, state: AudioState) -> AudioConfig {
  AudioConfig(..config, state: state)
}

/// Set audio to playing
pub fn with_playing(config: AudioConfig) -> AudioConfig {
  AudioConfig(..config, state: Playing)
}

/// Set audio to stopped
pub fn with_stopped(config: AudioConfig) -> AudioConfig {
  AudioConfig(..config, state: Stopped)
}

/// Set audio to paused
pub fn with_paused(config: AudioConfig) -> AudioConfig {
  AudioConfig(..config, state: Paused)
}

/// Set fade configuration
pub fn with_fade(config: AudioConfig, duration_ms: Int) -> AudioConfig {
  AudioConfig(..config, fade: Fade(duration_ms))
}

/// Set no fade (instant transitions)
pub fn with_no_fade(config: AudioConfig) -> AudioConfig {
  AudioConfig(..config, fade: NoFade)
}

/// Set volume in config (0.0 to 1.0)
pub fn with_volume(config: AudioConfig, volume: Float) -> AudioConfig {
  AudioConfig(..config, volume: volume)
}

/// Set looping in config
pub fn with_loop(config: AudioConfig, loop: Bool) -> AudioConfig {
  AudioConfig(..config, loop: loop)
}

/// Set playback rate in config (1.0 = normal, 2.0 = double speed, etc.)
pub fn with_playback_rate(config: AudioConfig, rate: Float) -> AudioConfig {
  AudioConfig(..config, playback_rate: rate)
}

/// Set audio group in config
pub fn with_group(config: AudioConfig, group: AudioGroup) -> AudioConfig {
  AudioConfig(..config, group: option.Some(group))
}

/// Set callback to be called when audio ends (for non-looping audio)
///
/// This is useful for one-shot sounds like SFX where you need to know
/// when the sound has finished playing.
///
/// ## Example
///
/// ```gleam
/// audio.config()
/// |> audio.with_state(audio.Playing)
/// |> audio.with_on_end(fn() {
///   // Audio finished playing
///   io.println("SFX finished!")
/// })
/// ```
pub fn with_on_end(config: AudioConfig, callback: fn() -> Nil) -> AudioConfig {
  AudioConfig(..config, on_end: option.Some(callback))
}

/// Create global audio (2D, same volume everywhere)
pub fn global(buffer: AudioBuffer, config: AudioConfig) -> Audio {
  GlobalAudio(buffer: buffer, config: config)
}

/// Create a default positional audio configuration
pub fn positional(buffer: AudioBuffer, config: AudioConfig) -> Audio {
  PositionalAudio(
    buffer: buffer,
    config: config,
    ref_distance: 1.0,
    rolloff_factor: 1.0,
    max_distance: 10_000.0,
  )
}

/// Set reference distance for positional audio
pub fn with_ref_distance(audio: Audio, distance: Float) -> Audio {
  case audio {
    PositionalAudio(buffer, config, _, rolloff, max) ->
      PositionalAudio(
        buffer: buffer,
        config: config,
        ref_distance: distance,
        rolloff_factor: rolloff,
        max_distance: max,
      )
    GlobalAudio(_, _) -> audio
  }
}

/// Set rolloff factor for positional audio
pub fn with_rolloff_factor(audio: Audio, factor: Float) -> Audio {
  case audio {
    PositionalAudio(buffer, config, ref, _, max) ->
      PositionalAudio(
        buffer: buffer,
        config: config,
        ref_distance: ref,
        rolloff_factor: factor,
        max_distance: max,
      )
    GlobalAudio(_, _) -> audio
  }
}

/// Set maximum distance for positional audio
pub fn with_max_distance(audio: Audio, distance: Float) -> Audio {
  case audio {
    PositionalAudio(buffer, config, ref, rolloff, _) ->
      PositionalAudio(
        buffer: buffer,
        config: config,
        ref_distance: ref,
        rolloff_factor: rolloff,
        max_distance: distance,
      )
    GlobalAudio(_, _) -> audio
  }
}
// Note: Audio group volume control has been migrated to the audio manager.
// Group volumes are now part of the immutable audio manager state in the renderer.
// To control group volumes, use effects that manipulate the audio manager state
// (functionality to be added in future versions).
