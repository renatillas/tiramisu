//// <script>
//// const docs = [
////   {
////     header: "Creating audio",
////     functions: [
////       "global",
////       "positional"
////     ]
////   },
////   {
////     header: "Audio configuration",
////     functions: [
////       "config",
////       "playing",
////       "with_state",
////       "with_playing",
////       "with_stopped",
////       "with_paused",
////       "with_fade",
////       "with_no_fade",
////       "with_volume",
////       "with_loop",
////       "with_playback_rate",
////       "with_group",
////       "with_on_end"
////     ]
////   },
////   {
////     header: "Positional audio settings",
////     functions: [
////       "with_ref_distance",
////       "with_rolloff_factor",
////       "with_max_distance"
////     ]
////   }
//// ]
////
//// const callback = () => {
////   const list = document.querySelector(".sidebar > ul:last-of-type")
////   const sortedLists = document.createDocumentFragment()
////   const sortedMembers = document.createDocumentFragment()
////
////   for (const section of docs) {
////     sortedLists.append((() => {
////       const node = document.createElement("h3")
////       node.append(section.header)
////       return node
////     })())
////     sortedMembers.append((() => {
////       const node = document.createElement("h2")
////       node.append(section.header)
////       return node
////     })())
////
////     const sortedList = document.createElement("ul")
////     sortedLists.append(sortedList)
////
////
////     for (const funcName of section.functions) {
////       const href = `#${funcName}`
////       const member = document.querySelector(
////         `.member:has(h2 > a[href="${href}"])`
////       )
////       const sidebar = list.querySelector(`li:has(a[href="${href}"])`)
////       sortedList.append(sidebar)
////       sortedMembers.append(member)
////     }
////   }
////
////   document.querySelector(".sidebar").insertBefore(sortedLists, list)
////   document
////     .querySelector(".module-members:has(#module-values)")
////     .insertBefore(
////       sortedMembers,
////       document.querySelector("#module-values").nextSibling
////     )
//// }
////
//// document.readyState !== "loading"
////   ? callback()
////   : document.addEventListener(
////     "DOMContentLoaded",
////     callback,
////     { once: true }
////   )
//// </script>
//// Audio module - spatial and global audio playback with Web Audio API.
////
//// Provides type-safe audio playback configuration using the Web Audio API.
//// Supports both global (2D) and positional (3D) audio with volume groups,
//// fade transitions, and playback control.
////
//// ## Core Concepts
////
//// - **Global Audio**: 2D audio with constant volume regardless of listener position
//// - **Positional Audio**: 3D audio with distance-based volume falloff
//// - **Audio Groups**: Categorize sounds (SFX, Music, Voice, Ambient) for group volume control
//// - **Builder Pattern**: Chainable functions for configuring audio playback
//// - **Fade Transitions**: Smooth fade in/out when changing playback state
//// - **Playback Control**: Play, pause, stop, loop, and adjust playback rate
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/audio
//// import tiramisu/scene
//// import tiramisu/asset
////
//// // Global audio (background music)
//// let music_config =
////   audio.playing()
////   |> audio.with_loop(True)
////   |> audio.with_volume(0.5)
////   |> audio.with_group(audio.Music)
////   |> audio.with_fade(1000)  // 1 second fade in
////
//// scene.Audio(
////   id: "bg-music",
////   audio: audio.global(music_buffer, music_config),
////   transform: transform.identity,
//// )
////
//// // Positional audio (footsteps)
//// let sfx_config =
////   audio.playing()
////   |> audio.with_group(audio.SFX)
////   |> audio.with_on_end(fn() { io.println("Step complete!") })
////
//// scene.Audio(
////   id: "footstep",
////   audio:
////     audio.positional(footstep_buffer, sfx_config)
////     |> audio.with_ref_distance(2.0)
////     |> audio.with_rolloff_factor(1.5)
////     |> audio.with_max_distance(50.0),
////   transform: transform.at(position: player_position),
//// )
//// ```
////
//// ## Audio Groups
////
//// Audio groups allow batch volume control for categories of sounds:
////
//// - `SFX` - Sound effects (footsteps, gunshots, UI sounds)
//// - `Music` - Background music and themes
//// - `Voice` - Dialogue and voice lines
//// - `Ambient` - Environmental sounds (wind, rain, crowd noise)
//// - `Custom(name)` - User-defined groups
////
//// Set group volumes using the debug module or custom controls.
////
//// ## Positional Audio
////
//// Positional audio uses distance-based volume falloff:
////
//// - `ref_distance`: Distance where volume starts decreasing (default: 1.0)
//// - `rolloff_factor`: How quickly volume decreases with distance (default: 1.0)
//// - `max_distance`: Maximum distance where audio can be heard (default: 10000.0)
////
//// Position is determined by the Audio scene node's transform.

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
