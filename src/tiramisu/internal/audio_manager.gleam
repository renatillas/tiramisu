/// Internal audio management
///
/// This module manages audio playback, including:
/// - Audio source registry (global and positional audio)
/// - Audio group volumes (SFX, Music, Voice, Ambient, Custom)
/// - Mute state per group
/// - Audio state transitions (Playing, Paused, Stopped)
/// - Fade in/out effects
/// - AudioContext resume handling
///
/// All business logic lives in Gleam. Only pure Three.js audio API calls
/// cross the FFI boundary via savoiardi.
import gleam/dict.{type Dict}
import gleam/float
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/time/duration
import savoiardi
import tiramisu/audio.{type AudioState, type Group}

/// Audio source type - either global (2D) or positional (3D)
pub type Source {
  GlobalSource(savoiardi.Audio)
  PositionalSource(savoiardi.PositionalAudio)
}

/// Audio source metadata stored alongside Three.js object
pub type AudioSourceData {
  AudioSourceData(
    three_source: Source,
    base_volume: Float,
    group: Option(String),
    previous_state: Option(AudioState),
    on_end_callback: Option(fn() -> Nil),
  )
}

/// Pending audio playback (waiting for AudioContext to resume)
pub type PendingPlayback {
  PendingPlayback(
    id: String,
    source_data: AudioSourceData,
    fade_duration: duration.Duration,
  )
}

/// Complete audio manager state
pub type AudioManagerState {
  AudioManagerState(
    /// Registry of audio sources by ID
    sources: Dict(String, AudioSourceData),
    /// Group volumes (sfx, music, voice, ambient, custom)
    group_volumes: Dict(String, Float),
    /// Muted groups
    muted_groups: List(String),
    /// AudioContext resume state
    context_resumed: Bool,
    /// Pending playbacks (waiting for user interaction)
    pending_playbacks: List(PendingPlayback),
  )
}

// ============================================================================
// INITIALIZATION
// ============================================================================

/// Create initial audio manager state
pub fn init() -> AudioManagerState {
  AudioManagerState(
    sources: dict.new(),
    group_volumes: dict.new()
      |> dict.insert("sfx", 1.0)
      |> dict.insert("music", 1.0)
      |> dict.insert("voice", 1.0)
      |> dict.insert("ambient", 1.0),
    muted_groups: [],
    context_resumed: False,
    pending_playbacks: [],
  )
}

// ============================================================================
// AUDIO SOURCE MANAGEMENT
// ============================================================================

/// Register an audio source
pub fn register_audio_source(
  state: AudioManagerState,
  id: String,
  source_data: AudioSourceData,
) -> AudioManagerState {
  // id is already a string
  AudioManagerState(
    ..state,
    sources: dict.insert(state.sources, id, source_data),
  )
}

/// Unregister an audio source
pub fn unregister_audio_source(
  state: AudioManagerState,
  id: String,
) -> AudioManagerState {
  // Stop looping sounds before removing
  case dict.get(state.sources, id) {
    Ok(source_data) -> {
      let is_playing = is_source_playing(source_data.three_source)
      let is_looping = get_source_loop(source_data.three_source)

      case is_playing && is_looping {
        True -> stop_source(source_data.three_source)
        False -> Nil
      }
    }
    Error(_) -> Nil
  }

  AudioManagerState(..state, sources: dict.delete(state.sources, id))
}

/// Get audio source data by ID
pub fn get_audio_source(
  state: AudioManagerState,
  id: String,
) -> Option(AudioSourceData) {
  // id is already a string
  dict.get(state.sources, id) |> option.from_result
}

// ============================================================================
// AUDIO CREATION
// ============================================================================

/// Create and configure an audio source
pub fn create_audio_source(
  state: AudioManagerState,
  id: String,
  buffer: audio.Buffer,
  config: audio.Config,
  audio_type: audio.Audio,
  audio_listener: savoiardi.AudioListener,
) -> #(AudioManagerState, AudioSourceData) {
  // Remove existing source if it exists
  let state = case dict.get(state.sources, id) {
    Ok(existing) -> {
      let is_playing = is_source_playing(existing.three_source)
      case is_playing {
        True -> stop_source(existing.three_source)
        False -> Nil
      }
      unregister_audio_source(state, id)
    }
    Error(_) -> state
  }

  // Create appropriate audio source based on type
  let three_source = case audio_type {
    audio.GlobalAudio(_, _) -> {
      // Create global 2D audio
      GlobalSource(savoiardi.create_audio(audio_listener))
    }
    audio.PositionalAudio(_, _, ref_distance, rolloff_factor, max_distance) -> {
      // Create positional 3D audio
      let pos_audio = savoiardi.create_positional_audio(audio_listener)
      savoiardi.set_ref_distance(pos_audio, ref_distance)
      savoiardi.set_max_distance(pos_audio, max_distance)
      savoiardi.set_rolloff_factor(pos_audio, rolloff_factor)
      PositionalSource(pos_audio)
    }
  }

  // Store buffer (will be set right before playing)
  // We don't call setBuffer() immediately to avoid issues with suspended AudioContext

  // Get group name
  let group_name = case config.group {
    option.Some(group) -> option.Some(audio_group_to_string(group))
    option.None -> option.None
  }

  // Calculate effective volume
  let effective_volume =
    calculate_effective_volume(state, config.volume, group_name)

  set_source_volume(three_source, effective_volume)
  set_source_loop(three_source, config.loop)
  set_source_playback_rate(three_source, config.playback_rate)

  // Create source data
  let source_data =
    AudioSourceData(
      three_source: three_source,
      base_volume: config.volume,
      group: group_name,
      previous_state: option.None,
      on_end_callback: config.on_end,
    )

  // Apply initial state
  let #(state, source_data) = case config.state {
    audio.Stopped | audio.Paused -> {
      // Set initial state without playing
      let source_data =
        AudioSourceData(
          ..source_data,
          previous_state: option.Some(config.state),
        )
      #(state, source_data)
    }
    audio.Playing -> {
      // Apply playing state
      apply_audio_state(state, id, source_data, buffer, config, audio_listener)
    }
  }

  // Register the source
  let state = register_audio_source(state, id, source_data)

  #(state, source_data)
}

// ============================================================================
// AUDIO STATE TRANSITIONS
// ============================================================================

/// Apply audio state (Playing, Paused, Stopped)
pub fn apply_audio_state(
  state: AudioManagerState,
  id: String,
  source_data: AudioSourceData,
  buffer: audio.Buffer,
  config: audio.Config,
  audio_listener: savoiardi.AudioListener,
) -> #(AudioManagerState, AudioSourceData) {
  let three_source = source_data.three_source

  let previous_state = case source_data.previous_state {
    option.Some(s) -> s
    option.None -> audio.Stopped
  }

  let current_state = config.state

  // Extract fade duration
  let fade_duration = case config.fade {
    audio.Fade(duration) -> duration
    audio.NoFade -> duration.milliseconds(0)
  }

  // State transition logic
  let #(state, source_data) = case current_state {
    audio.Playing -> {
      case previous_state {
        audio.Playing -> #(state, source_data)
        _ -> {
          // Transition to Playing
          // Set buffer right before playing
          let has_buffer = has_source_buffer(three_source)
          case has_buffer {
            False -> set_source_buffer(three_source, buffer)
            True -> Nil
          }

          // Check AudioContext state using the game's actual AudioListener
          let context_state =
            get_audio_context_state_from_listener(audio_listener)

          case context_state == "suspended" {
            True -> {
              // Add to pending playbacks
              let pending = PendingPlayback(id:, source_data:, fade_duration:)
              let state =
                AudioManagerState(..state, pending_playbacks: [
                  pending,
                  ..state.pending_playbacks
                ])
              #(state, source_data)
            }
            False -> {
              // Play immediately
              let is_playing = is_source_playing(three_source)

              case is_playing {
                True -> #(state, source_data)
                False -> {
                  // Check if resuming from pause (no fade)
                  let is_resuming_from_pause = case previous_state {
                    audio.Paused -> True
                    _ -> False
                  }

                  case
                    duration.compare(fade_duration, duration.milliseconds(0))
                    == order.Gt
                    && !is_resuming_from_pause
                  {
                    True -> {
                      // Play with fade in
                      play_source_with_fade(
                        three_source,
                        fade_duration,
                        source_data.base_volume,
                      )
                    }
                    False -> {
                      // Play without fade
                      play_source(three_source)

                      // Restore volume when resuming from pause
                      case is_resuming_from_pause {
                        True -> {
                          let effective_volume =
                            calculate_effective_volume(
                              state,
                              source_data.base_volume,
                              source_data.group,
                            )
                          set_source_volume(three_source, effective_volume)
                        }
                        False -> Nil
                      }
                    }
                  }

                  let updated_source_data =
                    AudioSourceData(
                      ..source_data,
                      previous_state: option.Some(audio.Playing),
                    )
                  #(state, updated_source_data)
                }
              }
            }
          }
        }
      }
    }

    audio.Stopped -> {
      case previous_state {
        audio.Stopped -> #(state, source_data)
        _ -> {
          // Transition to Stopped
          let is_playing = is_source_playing(three_source)

          case is_playing {
            True -> {
              case
                duration.compare(fade_duration, duration.milliseconds(0))
                == order.Gt
              {
                True ->
                  stop_source_with_fade(three_source, fade_duration, False)
                False -> stop_source(three_source)
              }
            }
            False -> Nil
          }

          let updated_source_data =
            AudioSourceData(
              ..source_data,
              previous_state: option.Some(audio.Stopped),
            )
          #(state, updated_source_data)
        }
      }
    }

    audio.Paused -> {
      case previous_state {
        audio.Paused -> #(state, source_data)
        _ -> {
          // Transition to Paused
          let is_playing = is_source_playing(three_source)

          case is_playing {
            True -> pause_source(three_source)
            False -> Nil
          }

          let updated_source_data =
            AudioSourceData(
              ..source_data,
              previous_state: option.Some(audio.Paused),
            )
          #(state, updated_source_data)
        }
      }
    }
  }

  #(state, source_data)
}

/// Update audio configuration for existing source
pub fn update_audio_config(
  state: AudioManagerState,
  id: String,
  buffer: audio.Buffer,
  config: audio.Config,
  audio_listener: savoiardi.AudioListener,
) -> AudioManagerState {
  case get_audio_source(state, id) {
    option.None -> state
    option.Some(source_data) -> {
      let three_source = source_data.three_source

      // Update base volume
      let updated_source_data =
        AudioSourceData(
          ..source_data,
          base_volume: config.volume,
          on_end_callback: config.on_end,
        )

      // Calculate effective volume with group
      let effective_volume =
        calculate_effective_volume(state, config.volume, source_data.group)

      set_source_volume(three_source, effective_volume)
      set_source_loop(three_source, config.loop)
      set_source_playback_rate(three_source, config.playback_rate)

      // Apply state transitions
      let #(state, updated_source_data) =
        apply_audio_state(
          state,
          id,
          updated_source_data,
          buffer,
          config,
          audio_listener,
        )

      // Update registry
      AudioManagerState(
        ..state,
        sources: dict.insert(state.sources, id, updated_source_data),
      )
    }
  }
}

// ============================================================================
// FADE EFFECTS
// ============================================================================

/// Play audio with fade in
fn play_source_with_fade(
  source: Source,
  fade_duration: duration.Duration,
  target_volume: Float,
) -> Nil {
  let fade_ms =
    fade_duration
    |> duration.to_seconds
    |> float.multiply(1000.0)
    |> float.round

  case source {
    GlobalSource(audio) ->
      play_audio_with_fade_in_ffi(audio, fade_ms, target_volume)
    PositionalSource(audio) ->
      play_positional_audio_with_fade_in_ffi(audio, fade_ms, target_volume)
  }
}

/// Stop audio with fade out
fn stop_source_with_fade(
  source: Source,
  fade_duration: duration.Duration,
  pause_instead_of_stop: Bool,
) -> Nil {
  let fade_ms =
    fade_duration
    |> duration.to_seconds
    |> float.multiply(1000.0)
    |> float.round

  case source {
    GlobalSource(audio) ->
      stop_audio_with_fade_out_ffi(audio, fade_ms, pause_instead_of_stop)
    PositionalSource(audio) ->
      stop_positional_audio_with_fade_out_ffi(
        audio,
        fade_ms,
        pause_instead_of_stop,
      )
  }
}

// ============================================================================
// AUDIO GROUPS
// ============================================================================

/// Set volume for an audio group
pub fn set_group_volume(
  state: AudioManagerState,
  group_name: String,
  volume: Float,
) -> AudioManagerState {
  let clamped_volume = clamp_volume(volume)

  // Update group volume
  let state =
    AudioManagerState(
      ..state,
      group_volumes: dict.insert(
        state.group_volumes,
        group_name,
        clamped_volume,
      ),
    )

  // Update all sources in this group
  update_sources_in_group(state, group_name)
}

/// Get volume for an audio group
pub fn get_group_volume(state: AudioManagerState, group_name: String) -> Float {
  case dict.get(state.group_volumes, group_name) {
    Ok(volume) -> volume
    Error(_) -> 1.0
  }
}

/// Mute an audio group
pub fn mute_group(
  state: AudioManagerState,
  group_name: String,
) -> AudioManagerState {
  // Add to muted groups
  let state =
    AudioManagerState(..state, muted_groups: [group_name, ..state.muted_groups])

  // Update all sources in this group
  update_sources_in_group(state, group_name)
}

/// Unmute an audio group
pub fn unmute_group(
  state: AudioManagerState,
  group_name: String,
) -> AudioManagerState {
  // Remove from muted groups
  let state =
    AudioManagerState(
      ..state,
      muted_groups: list.filter(state.muted_groups, fn(g) { g != group_name }),
    )

  // Update all sources in this group
  update_sources_in_group(state, group_name)
}

/// Update all audio sources in a group
fn update_sources_in_group(
  state: AudioManagerState,
  group_name: String,
) -> AudioManagerState {
  let group_volume = get_group_volume(state, group_name)
  let is_muted = list.contains(state.muted_groups, group_name)

  // Update each source in the group
  let updated_sources =
    dict.map_values(state.sources, fn(_id, source_data) {
      case source_data.group {
        option.Some(g) if g == group_name -> {
          let effective_volume = case is_muted {
            True -> 0.0
            False -> source_data.base_volume *. group_volume
          }
          set_source_volume(source_data.three_source, effective_volume)
          source_data
        }
        _ -> source_data
      }
    })

  AudioManagerState(..state, sources: updated_sources)
}

// ============================================================================
// AUDIO CONTEXT RESUME
// ============================================================================

/// Resume AudioContext after user interaction
/// Takes the AudioListener from the renderer state to use the correct AudioContext
pub fn resume_audio_context(
  state: AudioManagerState,
  audio_listener: savoiardi.AudioListener,
) -> AudioManagerState {
  case state.context_resumed {
    True -> state
    False -> {
      let context_state = get_audio_context_state_from_listener(audio_listener)

      case context_state == "suspended" {
        True -> {
          // Resume context using the actual game's AudioListener
          resume_audio_context_from_listener(audio_listener)

          // Mark as resumed
          let state = AudioManagerState(..state, context_resumed: True)

          // Play pending audio
          play_pending_audio(state)
        }
        False -> {
          // Already resumed
          AudioManagerState(..state, context_resumed: True)
        }
      }
    }
  }
}

/// Play all pending audio sources
fn play_pending_audio(state: AudioManagerState) -> AudioManagerState {
  // Play each pending source
  list.each(state.pending_playbacks, fn(pending) {
    let three_source = pending.source_data.three_source
    let is_playing = is_source_playing(three_source)
    let has_buffer = has_source_buffer(three_source)

    case !is_playing && has_buffer {
      True -> {
        case
          duration.compare(pending.fade_duration, duration.milliseconds(0))
          == order.Gt
        {
          True ->
            play_source_with_fade(
              three_source,
              pending.fade_duration,
              pending.source_data.base_volume,
            )
          False -> play_source(three_source)
        }
      }
      False -> Nil
    }
  })

  // Clear pending playbacks
  AudioManagerState(..state, pending_playbacks: [])
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Calculate effective volume considering group volume and mute state
fn calculate_effective_volume(
  state: AudioManagerState,
  base_volume: Float,
  group: Option(String),
) -> Float {
  case group {
    option.None -> base_volume
    option.Some(group_name) -> {
      let group_volume = get_group_volume(state, group_name)
      let is_muted = list.contains(state.muted_groups, group_name)

      case is_muted {
        True -> 0.0
        False -> base_volume *. group_volume
      }
    }
  }
}

/// Clamp volume to 0.0-1.0 range
fn clamp_volume(volume: Float) -> Float {
  case volume <. 0.0 {
    True -> 0.0
    False ->
      case volume >. 1.0 {
        True -> 1.0
        False -> volume
      }
  }
}

/// Convert AudioGroup to string
fn audio_group_to_string(group: Group) -> String {
  case group {
    audio.SFX -> "sfx"
    audio.Music -> "music"
    audio.Voice -> "voice"
    audio.Ambient -> "ambient"
    audio.Custom(name) -> name
  }
}

// ============================================================================
// AUDIO SOURCE HELPER FUNCTIONS
// ============================================================================

/// Check if source is playing
fn is_source_playing(source: Source) -> Bool {
  case source {
    GlobalSource(audio) -> savoiardi.is_audio_playing(audio)
    PositionalSource(audio) -> savoiardi.is_positional_audio_playing(audio)
  }
}

/// Get loop state of source
fn get_source_loop(source: Source) -> Bool {
  case source {
    GlobalSource(audio) -> savoiardi.get_audio_loop(audio)
    PositionalSource(audio) -> savoiardi.get_positional_audio_loop(audio)
  }
}

/// Stop audio source
fn stop_source(source: Source) -> Nil {
  case source {
    GlobalSource(audio) -> savoiardi.stop_audio(audio)
    PositionalSource(audio) -> savoiardi.stop_positional_audio(audio)
  }
}

/// Play audio source
fn play_source(source: Source) -> Nil {
  case source {
    GlobalSource(audio) -> savoiardi.play_audio(audio)
    PositionalSource(audio) -> savoiardi.play_positional_audio(audio)
  }
}

/// Pause audio source
fn pause_source(source: Source) -> Nil {
  case source {
    GlobalSource(audio) -> savoiardi.pause_audio(audio)
    PositionalSource(audio) -> savoiardi.pause_positional_audio(audio)
  }
}

/// Set source volume
fn set_source_volume(source: Source, volume: Float) -> Nil {
  case source {
    GlobalSource(audio) -> savoiardi.set_audio_volume(audio, volume)
    PositionalSource(audio) ->
      savoiardi.set_positional_audio_volume(audio, volume)
  }
}

/// Set source loop
fn set_source_loop(source: Source, loop: Bool) -> Nil {
  case source {
    GlobalSource(audio) -> savoiardi.set_audio_loop(audio, loop)
    PositionalSource(audio) -> savoiardi.set_positional_audio_loop(audio, loop)
  }
}

/// Set source playback rate
fn set_source_playback_rate(source: Source, rate: Float) -> Nil {
  case source {
    GlobalSource(audio) -> savoiardi.set_audio_playback_rate(audio, rate)
    PositionalSource(audio) ->
      savoiardi.set_positional_audio_playback_rate(audio, rate)
  }
}

/// Check if source has buffer
fn has_source_buffer(source: Source) -> Bool {
  case source {
    GlobalSource(audio) -> savoiardi.has_audio_buffer(audio)
    PositionalSource(audio) -> savoiardi.has_positional_audio_buffer(audio)
  }
}

/// Set source buffer
fn set_source_buffer(source: Source, buffer: audio.Buffer) -> Nil {
  case source {
    GlobalSource(audio) -> savoiardi.set_audio_buffer(audio, buffer)
    PositionalSource(audio) ->
      savoiardi.set_positional_audio_buffer(audio, buffer)
  }
}

// ============================================================================
// FFI - Audio fade functions (tiramisu-specific, not pure Three.js bindings)
// ============================================================================

@external(javascript, "../../tiramisu.ffi.mjs", "playAudioWithFadeIn")
fn play_audio_with_fade_in_ffi(
  audio: savoiardi.Audio,
  fade_duration_ms: Int,
  target_volume: Float,
) -> Nil

@external(javascript, "../../tiramisu.ffi.mjs", "playAudioWithFadeIn")
fn play_positional_audio_with_fade_in_ffi(
  audio: savoiardi.PositionalAudio,
  fade_duration_ms: Int,
  target_volume: Float,
) -> Nil

@external(javascript, "../../tiramisu.ffi.mjs", "stopAudioWithFadeOut")
fn stop_audio_with_fade_out_ffi(
  audio: savoiardi.Audio,
  fade_duration_ms: Int,
  pause_instead_of_stop: Bool,
) -> Nil

@external(javascript, "../../tiramisu.ffi.mjs", "stopAudioWithFadeOut")
fn stop_positional_audio_with_fade_out_ffi(
  audio: savoiardi.PositionalAudio,
  fade_duration_ms: Int,
  pause_instead_of_stop: Bool,
) -> Nil

// ============================================================================
// FFI - Audio context management (uses actual game's AudioListener)
// ============================================================================

@external(javascript, "../../tiramisu.ffi.mjs", "getAudioContextStateFromListener")
fn get_audio_context_state_from_listener(
  audio_listener: savoiardi.AudioListener,
) -> String

@external(javascript, "../../tiramisu.ffi.mjs", "resumeAudioContextFromListener")
fn resume_audio_context_from_listener(
  audio_listener: savoiardi.AudioListener,
) -> Nil
