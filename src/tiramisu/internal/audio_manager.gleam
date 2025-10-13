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
/// cross the FFI boundary.
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option}
import tiramisu/audio.{
  type Audio, type AudioBuffer, type AudioConfig, type AudioGroup,
  type AudioState,
}
import tiramisu/internal/id

/// Opaque type wrapping THREE.Audio or THREE.PositionalAudio
pub opaque type ThreeAudioSource {
  ThreeAudioSource(source: Dynamic)
}

/// Audio source metadata stored alongside Three.js object
pub type AudioSourceData {
  AudioSourceData(
    three_source: ThreeAudioSource,
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
    fade_duration_ms: Int,
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
  id: id,
  source_data: AudioSourceData,
) -> AudioManagerState {
  let string_id = id.to_string(id)
  AudioManagerState(
    ..state,
    sources: dict.insert(state.sources, string_id, source_data),
  )
}

/// Unregister an audio source
pub fn unregister_audio_source(
  state: AudioManagerState,
  id: id,
) -> AudioManagerState {
  let string_id = id.to_string(id)

  // Stop looping sounds before removing
  case dict.get(state.sources, string_id) {
    Ok(source_data) -> {
      let three_source = unwrap_audio_source(source_data.three_source)
      let is_playing = is_audio_playing_ffi(three_source)
      let is_looping = get_audio_loop_ffi(three_source)

      case is_playing && is_looping {
        True -> stop_audio_ffi(three_source)
        False -> Nil
      }
    }
    Error(_) -> Nil
  }

  AudioManagerState(..state, sources: dict.delete(state.sources, string_id))
}

/// Get audio source data by ID
pub fn get_audio_source(
  state: AudioManagerState,
  id: id,
) -> Option(AudioSourceData) {
  let string_id = id.to_string(id)
  dict.get(state.sources, string_id) |> option.from_result
}

// ============================================================================
// AUDIO CREATION
// ============================================================================

/// Create and configure an audio source
pub fn create_audio_source(
  state: AudioManagerState,
  id: id,
  buffer: AudioBuffer,
  config: AudioConfig,
  audio_type: Audio,
  audio_listener: Dynamic,
) -> #(AudioManagerState, AudioSourceData) {
  let string_id = id.to_string(id)

  // Remove existing source if it exists
  let state = case dict.get(state.sources, string_id) {
    Ok(existing) -> {
      let three_source = unwrap_audio_source(existing.three_source)
      let is_playing = is_audio_playing_ffi(three_source)
      case is_playing {
        True -> stop_audio_ffi(three_source)
        False -> Nil
      }
      unregister_audio_source(state, id)
    }
    Error(_) -> state
  }

  // Use provided audio listener
  let listener = audio_listener

  // Create appropriate audio source based on type
  let three_source = case audio_type {
    audio.GlobalAudio(_, _) -> {
      // Create global 2D audio
      create_audio_ffi(listener)
    }
    audio.PositionalAudio(_, _, ref_distance, rolloff_factor, max_distance) -> {
      // Create positional 3D audio
      let pos_audio = create_positional_audio_ffi(listener)
      set_ref_distance_ffi(pos_audio, ref_distance)
      set_max_distance_ffi(pos_audio, max_distance)
      set_rolloff_factor_ffi(pos_audio, rolloff_factor)
      pos_audio
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

  set_audio_volume_ffi(three_source, effective_volume)
  set_audio_loop_ffi(three_source, config.loop)
  set_audio_playback_rate_ffi(three_source, config.playback_rate)

  // Create source data
  let source_data =
    AudioSourceData(
      three_source: wrap_audio_source(three_source),
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
      apply_audio_state(state, id, source_data, buffer, config)
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
  id: id,
  source_data: AudioSourceData,
  buffer: AudioBuffer,
  config: AudioConfig,
) -> #(AudioManagerState, AudioSourceData) {
  let string_id = id.to_string(id)
  let three_source = unwrap_audio_source(source_data.three_source)

  let previous_state = case source_data.previous_state {
    option.Some(s) -> s
    option.None -> audio.Stopped
  }

  let current_state = config.state

  // Extract fade duration
  let fade_duration_ms = case config.fade {
    audio.Fade(duration) -> duration
    audio.NoFade -> 0
  }

  // State transition logic
  let #(state, source_data) = case current_state {
    audio.Playing -> {
      case previous_state {
        audio.Playing -> #(state, source_data)
        _ -> {
          // Transition to Playing
          // Set buffer right before playing
          let has_buffer = has_audio_buffer_ffi(three_source)
          case has_buffer {
            False ->
              set_audio_buffer_ffi(three_source, unwrap_audio_buffer(buffer))
            True -> Nil
          }

          // Check AudioContext state
          let context_state = get_audio_context_state_ffi()

          case context_state == "suspended" {
            True -> {
              // Add to pending playbacks
              let pending =
                PendingPlayback(
                  id: string_id,
                  source_data: source_data,
                  fade_duration_ms: fade_duration_ms,
                )
              let state =
                AudioManagerState(..state, pending_playbacks: [
                  pending,
                  ..state.pending_playbacks
                ])
              #(state, source_data)
            }
            False -> {
              // Play immediately
              let is_playing = is_audio_playing_ffi(three_source)

              case is_playing {
                True -> #(state, source_data)
                False -> {
                  // Check if resuming from pause (no fade)
                  let is_resuming_from_pause = case previous_state {
                    audio.Paused -> True
                    _ -> False
                  }

                  case fade_duration_ms > 0 && !is_resuming_from_pause {
                    True -> {
                      // Play with fade in
                      play_audio_with_fade(
                        three_source,
                        fade_duration_ms,
                        source_data.base_volume,
                      )
                    }
                    False -> {
                      // Play without fade
                      play_audio_ffi(three_source)

                      // Restore volume when resuming from pause
                      case is_resuming_from_pause {
                        True -> {
                          let effective_volume =
                            calculate_effective_volume(
                              state,
                              source_data.base_volume,
                              source_data.group,
                            )
                          set_audio_volume_ffi(three_source, effective_volume)
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
          let is_playing = is_audio_playing_ffi(three_source)

          case is_playing {
            True -> {
              case fade_duration_ms > 0 {
                True ->
                  stop_audio_with_fade(three_source, fade_duration_ms, False)
                False -> stop_audio_ffi(three_source)
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
          let is_playing = is_audio_playing_ffi(three_source)

          case is_playing {
            True -> pause_audio_ffi(three_source)
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
  id: id,
  buffer: AudioBuffer,
  config: AudioConfig,
) -> AudioManagerState {
  case get_audio_source(state, id) {
    option.None -> state
    option.Some(source_data) -> {
      let three_source = unwrap_audio_source(source_data.three_source)

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

      set_audio_volume_ffi(three_source, effective_volume)
      set_audio_loop_ffi(three_source, config.loop)
      set_audio_playback_rate_ffi(three_source, config.playback_rate)

      // Apply state transitions
      let #(state, updated_source_data) =
        apply_audio_state(state, id, updated_source_data, buffer, config)

      // Update registry
      let string_id = id.to_string(id)
      AudioManagerState(
        ..state,
        sources: dict.insert(state.sources, string_id, updated_source_data),
      )
    }
  }
}

// ============================================================================
// FADE EFFECTS
// ============================================================================

/// Play audio with fade in (calls FFI to set up interval)
fn play_audio_with_fade(
  three_source: Dynamic,
  fade_duration_ms: Int,
  target_volume: Float,
) -> Nil {
  play_with_fade_internal_ffi(three_source, fade_duration_ms, target_volume)
}

/// Stop audio with fade out (calls FFI to set up interval)
fn stop_audio_with_fade(
  three_source: Dynamic,
  fade_duration_ms: Int,
  pause_instead_of_stop: Bool,
) -> Nil {
  stop_with_fade_internal_ffi(
    three_source,
    fade_duration_ms,
    pause_instead_of_stop,
  )
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
          let three_source = unwrap_audio_source(source_data.three_source)
          let effective_volume = case is_muted {
            True -> 0.0
            False -> source_data.base_volume *. group_volume
          }
          set_audio_volume_ffi(three_source, effective_volume)
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
pub fn resume_audio_context(state: AudioManagerState) -> AudioManagerState {
  case state.context_resumed {
    True -> state
    False -> {
      let context_state = get_audio_context_state_ffi()

      case context_state == "suspended" {
        True -> {
          // Resume context
          resume_audio_context_ffi()

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
    let three_source = unwrap_audio_source(pending.source_data.three_source)
    let is_playing = is_audio_playing_ffi(three_source)
    let has_buffer = has_audio_buffer_ffi(three_source)

    case !is_playing && has_buffer {
      True -> {
        case pending.fade_duration_ms > 0 {
          True ->
            play_audio_with_fade(
              three_source,
              pending.fade_duration_ms,
              pending.source_data.base_volume,
            )
          False -> play_audio_ffi(three_source)
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
fn audio_group_to_string(group: AudioGroup) -> String {
  case group {
    audio.SFX -> "sfx"
    audio.Music -> "music"
    audio.Voice -> "voice"
    audio.Ambient -> "ambient"
    audio.Custom(name) -> name
  }
}

// ============================================================================
// WRAPPING/UNWRAPPING
// ============================================================================

fn wrap_audio_source(source: Dynamic) -> ThreeAudioSource {
  ThreeAudioSource(source: source)
}

fn unwrap_audio_source(source: ThreeAudioSource) -> Dynamic {
  source.source
}

fn unwrap_audio_buffer(buffer: AudioBuffer) -> Dynamic {
  do_unwrap_audio_buffer(buffer)
}

@external(javascript, "../../threejs.ffi.mjs", "identity")
fn do_unwrap_audio_buffer(buffer: AudioBuffer) -> Dynamic

// ============================================================================
// FFI FUNCTIONS
// ============================================================================

// Audio creation
@external(javascript, "../../threejs.ffi.mjs", "createAudio")
fn create_audio_ffi(listener: Dynamic) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "createPositionalAudio")
fn create_positional_audio_ffi(listener: Dynamic) -> Dynamic

// Audio configuration
@external(javascript, "../../threejs.ffi.mjs", "setAudioBuffer")
fn set_audio_buffer_ffi(audio: Dynamic, buffer: Dynamic) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setAudioVolume")
fn set_audio_volume_ffi(audio: Dynamic, volume: Float) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setAudioLoop")
fn set_audio_loop_ffi(audio: Dynamic, loop: Bool) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setAudioPlaybackRate")
fn set_audio_playback_rate_ffi(audio: Dynamic, rate: Float) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setRefDistance")
fn set_ref_distance_ffi(audio: Dynamic, distance: Float) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setRolloffFactor")
fn set_rolloff_factor_ffi(audio: Dynamic, factor: Float) -> Nil

// Audio playback
@external(javascript, "../../threejs.ffi.mjs", "playAudio")
fn play_audio_ffi(audio: Dynamic) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "pauseAudio")
fn pause_audio_ffi(audio: Dynamic) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "stopAudio")
fn stop_audio_ffi(audio: Dynamic) -> Nil

// Audio state queries
@external(javascript, "../../threejs.ffi.mjs", "isAudioPlaying")
fn is_audio_playing_ffi(audio: Dynamic) -> Bool

// AudioContext
@external(javascript, "../../threejs.ffi.mjs", "getAudioContextState")
fn get_audio_context_state_ffi() -> String

@external(javascript, "../../threejs.ffi.mjs", "resumeAudioContext")
fn resume_audio_context_ffi() -> Nil

// Fade effects (implemented in FFI with setInterval)
@external(javascript, "../../threejs.ffi.mjs", "playAudioWithFadeIn")
fn play_with_fade_internal_ffi(
  audio: Dynamic,
  fade_duration_ms: Int,
  target_volume: Float,
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "stopAudioWithFadeOut")
fn stop_with_fade_internal_ffi(
  audio: Dynamic,
  fade_duration_ms: Int,
  pause_instead_of_stop: Bool,
) -> Nil

// Additional helpers
@external(javascript, "../../threejs.ffi.mjs", "hasAudioBuffer")
fn has_audio_buffer_ffi(audio: Dynamic) -> Bool

@external(javascript, "../../threejs.ffi.mjs", "getAudioLoop")
fn get_audio_loop_ffi(audio: Dynamic) -> Bool

@external(javascript, "../../threejs.ffi.mjs", "setMaxDistance")
fn set_max_distance_ffi(audio: Dynamic, distance: Float) -> Nil
