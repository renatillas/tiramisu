import gleam/javascript/promise
import gleam/option
import gleam/time/duration
import savoiardi

// --- Public Types ---

/// Audio buffer type wrapping the Web Audio API AudioBuffer
pub type Buffer =
  savoiardi.AudioBuffer

/// Audio group categories for volume control
pub type Group {
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
  Fade(duration: duration.Duration)
}

/// Audio playback configuration
pub type Config {
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
    group: option.Option(Group),
    /// Callback when audio ends (for non-looping audio)
    on_end: option.Option(fn() -> Nil),
  )
}

/// Type of audio (global or positional)
pub type Audio {
  /// Global audio (2D, same volume everywhere)
  GlobalAudio(buffer: Buffer, config: Config)
  /// Positional audio (3D, volume based on distance)
  PositionalAudio(
    buffer: Buffer,
    config: Config,
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
pub fn config() -> Config {
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
pub fn playing() -> Config {
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
pub fn with_state(config: Config, state: AudioState) -> Config {
  AudioConfig(..config, state: state)
}

/// Set audio to playing
pub fn with_playing(config: Config) -> Config {
  AudioConfig(..config, state: Playing)
}

/// Set audio to stopped
pub fn with_stopped(config: Config) -> Config {
  AudioConfig(..config, state: Stopped)
}

/// Set audio to paused
pub fn with_paused(config: Config) -> Config {
  AudioConfig(..config, state: Paused)
}

/// Set fade configuration
pub fn with_fade(config: Config, duration: duration.Duration) -> Config {
  AudioConfig(..config, fade: Fade(duration))
}

/// Set no fade (instant transitions)
pub fn with_no_fade(config: Config) -> Config {
  AudioConfig(..config, fade: NoFade)
}

/// Set volume in config (0.0 to 1.0)
pub fn with_volume(config: Config, volume: Float) -> Config {
  AudioConfig(..config, volume: volume)
}

/// Set looping in config
pub fn with_loop(config: Config, loop: Bool) -> Config {
  AudioConfig(..config, loop: loop)
}

/// Set playback rate in config (1.0 = normal, 2.0 = double speed, etc.)
pub fn with_playback_rate(config: Config, rate: Float) -> Config {
  AudioConfig(..config, playback_rate: rate)
}

/// Set audio group in config
pub fn with_group(config: Config, group: Group) -> Config {
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
pub fn with_on_end(config: Config, callback: fn() -> Nil) -> Config {
  AudioConfig(..config, on_end: option.Some(callback))
}

/// Create global audio (2D, same volume everywhere)
pub fn global(buffer: Buffer, config: Config) -> Audio {
  GlobalAudio(buffer: buffer, config: config)
}

/// Create a default positional audio configuration
pub fn positional(buffer: Buffer, config: Config) -> Audio {
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

/// Load an audio file from URL
pub fn load_audio(
  url url: String,
) -> promise.Promise(Result(savoiardi.AudioBuffer, Nil)) {
  savoiardi.load_audio(url)
}
