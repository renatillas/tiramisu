import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Ok, toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import {
  identity as do_unwrap_audio_buffer,
  createAudio as create_audio_ffi,
  createPositionalAudio as create_positional_audio_ffi,
  setAudioBuffer as set_audio_buffer_ffi,
  setAudioVolume as set_audio_volume_ffi,
  setAudioLoop as set_audio_loop_ffi,
  setAudioPlaybackRate as set_audio_playback_rate_ffi,
  setRefDistance as set_ref_distance_ffi,
  setRolloffFactor as set_rolloff_factor_ffi,
  playAudio as play_audio_ffi,
  pauseAudio as pause_audio_ffi,
  stopAudio as stop_audio_ffi,
  isAudioPlaying as is_audio_playing_ffi,
  getAudioContextState as get_audio_context_state_ffi,
  resumeAudioContext as resume_audio_context_ffi,
  playAudioWithFadeIn as play_with_fade_internal_ffi,
  stopAudioWithFadeOut as stop_with_fade_internal_ffi,
  hasAudioBuffer as has_audio_buffer_ffi,
  getAudioLoop as get_audio_loop_ffi,
  setMaxDistance as set_max_distance_ffi,
} from "../../threejs.ffi.mjs";
import { getAudioListener as get_audio_listener_ffi } from "../../tiramisu.ffi.mjs";
import * as $audio from "../../tiramisu/audio.mjs";
import * as $id from "../../tiramisu/internal/id.mjs";

class ThreeAudioSource extends $CustomType {
  constructor(source) {
    super();
    this.source = source;
  }
}

export class AudioSourceData extends $CustomType {
  constructor(three_source, base_volume, group, previous_state, on_end_callback) {
    super();
    this.three_source = three_source;
    this.base_volume = base_volume;
    this.group = group;
    this.previous_state = previous_state;
    this.on_end_callback = on_end_callback;
  }
}

export class PendingPlayback extends $CustomType {
  constructor(id, source_data, fade_duration_ms) {
    super();
    this.id = id;
    this.source_data = source_data;
    this.fade_duration_ms = fade_duration_ms;
  }
}

export class AudioManagerState extends $CustomType {
  constructor(sources, group_volumes, muted_groups, context_resumed, pending_playbacks) {
    super();
    this.sources = sources;
    this.group_volumes = group_volumes;
    this.muted_groups = muted_groups;
    this.context_resumed = context_resumed;
    this.pending_playbacks = pending_playbacks;
  }
}

/**
 * Create initial audio manager state
 */
export function init() {
  return new AudioManagerState(
    $dict.new$(),
    (() => {
      let _pipe = $dict.new$();
      let _pipe$1 = $dict.insert(_pipe, "sfx", 1.0);
      let _pipe$2 = $dict.insert(_pipe$1, "music", 1.0);
      let _pipe$3 = $dict.insert(_pipe$2, "voice", 1.0);
      return $dict.insert(_pipe$3, "ambient", 1.0);
    })(),
    toList([]),
    false,
    toList([]),
  );
}

/**
 * Register an audio source
 */
export function register_audio_source(state, id, source_data) {
  let string_id = $id.to_string(id);
  return new AudioManagerState(
    $dict.insert(state.sources, string_id, source_data),
    state.group_volumes,
    state.muted_groups,
    state.context_resumed,
    state.pending_playbacks,
  );
}

/**
 * Get audio source data by ID
 */
export function get_audio_source(state, id) {
  let string_id = $id.to_string(id);
  let _pipe = $dict.get(state.sources, string_id);
  return $option.from_result(_pipe);
}

/**
 * Get volume for an audio group
 */
export function get_group_volume(state, group_name) {
  let $ = $dict.get(state.group_volumes, group_name);
  if ($ instanceof Ok) {
    let volume = $[0];
    return volume;
  } else {
    return 1.0;
  }
}

/**
 * Calculate effective volume considering group volume and mute state
 * 
 * @ignore
 */
function calculate_effective_volume(state, base_volume, group) {
  if (group instanceof $option.Some) {
    let group_name = group[0];
    let group_volume = get_group_volume(state, group_name);
    let is_muted = $list.contains(state.muted_groups, group_name);
    if (is_muted) {
      return 0.0;
    } else {
      return base_volume * group_volume;
    }
  } else {
    return base_volume;
  }
}

/**
 * Clamp volume to 0.0-1.0 range
 * 
 * @ignore
 */
function clamp_volume(volume) {
  let $ = volume < 0.0;
  if ($) {
    return 0.0;
  } else {
    let $1 = volume > 1.0;
    if ($1) {
      return 1.0;
    } else {
      return volume;
    }
  }
}

/**
 * Convert AudioGroup to string
 * 
 * @ignore
 */
function audio_group_to_string(group) {
  if (group instanceof $audio.SFX) {
    return "sfx";
  } else if (group instanceof $audio.Music) {
    return "music";
  } else if (group instanceof $audio.Voice) {
    return "voice";
  } else if (group instanceof $audio.Ambient) {
    return "ambient";
  } else {
    let name = group[0];
    return name;
  }
}

function wrap_audio_source(source) {
  return new ThreeAudioSource(source);
}

function unwrap_audio_source(source) {
  return source.source;
}

function unwrap_audio_buffer(buffer) {
  return do_unwrap_audio_buffer(buffer);
}

/**
 * Update all audio sources in a group
 * 
 * @ignore
 */
function update_sources_in_group(state, group_name) {
  let group_volume = get_group_volume(state, group_name);
  let is_muted = $list.contains(state.muted_groups, group_name);
  let updated_sources = $dict.map_values(
    state.sources,
    (_, source_data) => {
      let $ = source_data.group;
      if ($ instanceof $option.Some) {
        let g = $[0];
        if (g === group_name) {
          let three_source = unwrap_audio_source(source_data.three_source);
          let _block;
          if (is_muted) {
            _block = 0.0;
          } else {
            _block = source_data.base_volume * group_volume;
          }
          let effective_volume = _block;
          set_audio_volume_ffi(three_source, effective_volume);
          return source_data;
        } else {
          return source_data;
        }
      } else {
        return source_data;
      }
    },
  );
  return new AudioManagerState(
    updated_sources,
    state.group_volumes,
    state.muted_groups,
    state.context_resumed,
    state.pending_playbacks,
  );
}

/**
 * Set volume for an audio group
 */
export function set_group_volume(state, group_name, volume) {
  let clamped_volume = clamp_volume(volume);
  let state$1 = new AudioManagerState(
    state.sources,
    $dict.insert(state.group_volumes, group_name, clamped_volume),
    state.muted_groups,
    state.context_resumed,
    state.pending_playbacks,
  );
  return update_sources_in_group(state$1, group_name);
}

/**
 * Mute an audio group
 */
export function mute_group(state, group_name) {
  let state$1 = new AudioManagerState(
    state.sources,
    state.group_volumes,
    listPrepend(group_name, state.muted_groups),
    state.context_resumed,
    state.pending_playbacks,
  );
  return update_sources_in_group(state$1, group_name);
}

/**
 * Unmute an audio group
 */
export function unmute_group(state, group_name) {
  let state$1 = new AudioManagerState(
    state.sources,
    state.group_volumes,
    $list.filter(state.muted_groups, (g) => { return g !== group_name; }),
    state.context_resumed,
    state.pending_playbacks,
  );
  return update_sources_in_group(state$1, group_name);
}

/**
 * Play audio with fade in (calls FFI to set up interval)
 * 
 * @ignore
 */
function play_audio_with_fade(three_source, fade_duration_ms, target_volume) {
  return play_with_fade_internal_ffi(
    three_source,
    fade_duration_ms,
    target_volume,
  );
}

/**
 * Stop audio with fade out (calls FFI to set up interval)
 * 
 * @ignore
 */
function stop_audio_with_fade(
  three_source,
  fade_duration_ms,
  pause_instead_of_stop
) {
  return stop_with_fade_internal_ffi(
    three_source,
    fade_duration_ms,
    pause_instead_of_stop,
  );
}

/**
 * Apply audio state (Playing, Paused, Stopped)
 */
export function apply_audio_state(state, id, source_data, buffer, config) {
  let string_id = $id.to_string(id);
  let three_source = unwrap_audio_source(source_data.three_source);
  let _block;
  let $ = source_data.previous_state;
  if ($ instanceof $option.Some) {
    let s = $[0];
    _block = s;
  } else {
    _block = new $audio.Stopped();
  }
  let previous_state = _block;
  let current_state = config.state;
  let _block$1;
  let $1 = config.fade;
  if ($1 instanceof $audio.NoFade) {
    _block$1 = 0;
  } else {
    let duration = $1.duration_ms;
    _block$1 = duration;
  }
  let fade_duration_ms = _block$1;
  let _block$2;
  if (current_state instanceof $audio.Playing) {
    if (previous_state instanceof $audio.Playing) {
      _block$2 = [state, source_data];
    } else {
      let has_buffer = has_audio_buffer_ffi(three_source);
      if (has_buffer) {
        undefined
      } else {
        set_audio_buffer_ffi(three_source, unwrap_audio_buffer(buffer))
      }
      let context_state = get_audio_context_state_ffi();
      let $3 = context_state === "suspended";
      if ($3) {
        let pending = new PendingPlayback(
          string_id,
          source_data,
          fade_duration_ms,
        );
        let state$1 = new AudioManagerState(
          state.sources,
          state.group_volumes,
          state.muted_groups,
          state.context_resumed,
          listPrepend(pending, state.pending_playbacks),
        );
        _block$2 = [state$1, source_data];
      } else {
        let is_playing = is_audio_playing_ffi(three_source);
        if (is_playing) {
          _block$2 = [state, source_data];
        } else {
          let _block$3;
          if (previous_state instanceof $audio.Paused) {
            _block$3 = true;
          } else {
            _block$3 = false;
          }
          let is_resuming_from_pause = _block$3;
          let $4 = (fade_duration_ms > 0) && !is_resuming_from_pause;
          if ($4) {
            play_audio_with_fade(
              three_source,
              fade_duration_ms,
              source_data.base_volume,
            )
          } else {
            play_audio_ffi(three_source);
            if (is_resuming_from_pause) {
              let effective_volume = calculate_effective_volume(
                state,
                source_data.base_volume,
                source_data.group,
              );
              set_audio_volume_ffi(three_source, effective_volume)
            } else {
              undefined
            }
          }
          let updated_source_data = new AudioSourceData(
            source_data.three_source,
            source_data.base_volume,
            source_data.group,
            new $option.Some(new $audio.Playing()),
            source_data.on_end_callback,
          );
          _block$2 = [state, updated_source_data];
        }
      }
    }
  } else if (current_state instanceof $audio.Stopped) {
    if (previous_state instanceof $audio.Stopped) {
      _block$2 = [state, source_data];
    } else {
      let is_playing = is_audio_playing_ffi(three_source);
      if (is_playing) {
        let $3 = fade_duration_ms > 0;
        if ($3) {
          stop_audio_with_fade(three_source, fade_duration_ms, false)
        } else {
          stop_audio_ffi(three_source)
        }
      } else {
        undefined
      }
      let updated_source_data = new AudioSourceData(
        source_data.three_source,
        source_data.base_volume,
        source_data.group,
        new $option.Some(new $audio.Stopped()),
        source_data.on_end_callback,
      );
      _block$2 = [state, updated_source_data];
    }
  } else {
    if (previous_state instanceof $audio.Paused) {
      _block$2 = [state, source_data];
    } else {
      let is_playing = is_audio_playing_ffi(three_source);
      if (is_playing) {
        pause_audio_ffi(three_source)
      } else {
        undefined
      }
      let updated_source_data = new AudioSourceData(
        source_data.three_source,
        source_data.base_volume,
        source_data.group,
        new $option.Some(new $audio.Paused()),
        source_data.on_end_callback,
      );
      _block$2 = [state, updated_source_data];
    }
  }
  let $2 = _block$2;
  let state$1;
  let source_data$1;
  state$1 = $2[0];
  source_data$1 = $2[1];
  return [state$1, source_data$1];
}

/**
 * Update audio configuration for existing source
 */
export function update_audio_config(state, id, buffer, config) {
  let $ = get_audio_source(state, id);
  if ($ instanceof $option.Some) {
    let source_data = $[0];
    let three_source = unwrap_audio_source(source_data.three_source);
    let updated_source_data = new AudioSourceData(
      source_data.three_source,
      config.volume,
      source_data.group,
      source_data.previous_state,
      config.on_end,
    );
    let effective_volume = calculate_effective_volume(
      state,
      config.volume,
      source_data.group,
    );
    set_audio_volume_ffi(three_source, effective_volume);
    set_audio_loop_ffi(three_source, config.loop);
    set_audio_playback_rate_ffi(three_source, config.playback_rate);
    let $1 = apply_audio_state(state, id, updated_source_data, buffer, config);
    let state$1;
    let updated_source_data$1;
    state$1 = $1[0];
    updated_source_data$1 = $1[1];
    let string_id = $id.to_string(id);
    return new AudioManagerState(
      $dict.insert(state$1.sources, string_id, updated_source_data$1),
      state$1.group_volumes,
      state$1.muted_groups,
      state$1.context_resumed,
      state$1.pending_playbacks,
    );
  } else {
    return state;
  }
}

/**
 * Play all pending audio sources
 * 
 * @ignore
 */
function play_pending_audio(state) {
  $list.each(
    state.pending_playbacks,
    (pending) => {
      let three_source = unwrap_audio_source(pending.source_data.three_source);
      let is_playing = is_audio_playing_ffi(three_source);
      let has_buffer = has_audio_buffer_ffi(three_source);
      let $ = !is_playing && has_buffer;
      if ($) {
        let $1 = pending.fade_duration_ms > 0;
        if ($1) {
          return play_audio_with_fade(
            three_source,
            pending.fade_duration_ms,
            pending.source_data.base_volume,
          );
        } else {
          return play_audio_ffi(three_source);
        }
      } else {
        return undefined;
      }
    },
  );
  return new AudioManagerState(
    state.sources,
    state.group_volumes,
    state.muted_groups,
    state.context_resumed,
    toList([]),
  );
}

/**
 * Resume AudioContext after user interaction
 */
export function resume_audio_context(state) {
  let $ = state.context_resumed;
  if ($) {
    return state;
  } else {
    let context_state = get_audio_context_state_ffi();
    let $1 = context_state === "suspended";
    if ($1) {
      resume_audio_context_ffi();
      let state$1 = new AudioManagerState(
        state.sources,
        state.group_volumes,
        state.muted_groups,
        true,
        state.pending_playbacks,
      );
      return play_pending_audio(state$1);
    } else {
      return new AudioManagerState(
        state.sources,
        state.group_volumes,
        state.muted_groups,
        true,
        state.pending_playbacks,
      );
    }
  }
}

/**
 * Unregister an audio source
 */
export function unregister_audio_source(state, id) {
  let string_id = $id.to_string(id);
  let $ = $dict.get(state.sources, string_id);
  if ($ instanceof Ok) {
    let source_data = $[0];
    let three_source = unwrap_audio_source(source_data.three_source);
    let is_playing = is_audio_playing_ffi(three_source);
    let is_looping = get_audio_loop_ffi(three_source);
    let $1 = is_playing && is_looping;
    if ($1) {
      stop_audio_ffi(three_source)
    } else {
      undefined
    }
  } else {
    undefined
  }
  return new AudioManagerState(
    $dict.delete$(state.sources, string_id),
    state.group_volumes,
    state.muted_groups,
    state.context_resumed,
    state.pending_playbacks,
  );
}

/**
 * Create and configure an audio source
 */
export function create_audio_source(state, id, buffer, config, audio_type) {
  let string_id = $id.to_string(id);
  let _block;
  let $ = $dict.get(state.sources, string_id);
  if ($ instanceof Ok) {
    let existing = $[0];
    let three_source = unwrap_audio_source(existing.three_source);
    let is_playing = is_audio_playing_ffi(three_source);
    if (is_playing) {
      stop_audio_ffi(three_source)
    } else {
      undefined
    }
    _block = unregister_audio_source(state, id);
  } else {
    _block = state;
  }
  let state$1 = _block;
  let listener = get_audio_listener_ffi();
  let _block$1;
  if (audio_type instanceof $audio.GlobalAudio) {
    _block$1 = create_audio_ffi(listener);
  } else {
    let ref_distance = audio_type.ref_distance;
    let rolloff_factor = audio_type.rolloff_factor;
    let max_distance = audio_type.max_distance;
    let pos_audio = create_positional_audio_ffi(listener);
    set_ref_distance_ffi(pos_audio, ref_distance);
    set_max_distance_ffi(pos_audio, max_distance);
    set_rolloff_factor_ffi(pos_audio, rolloff_factor);
    _block$1 = pos_audio;
  }
  let three_source = _block$1;
  let _block$2;
  let $1 = config.group;
  if ($1 instanceof $option.Some) {
    let group = $1[0];
    _block$2 = new $option.Some(audio_group_to_string(group));
  } else {
    _block$2 = $1;
  }
  let group_name = _block$2;
  let effective_volume = calculate_effective_volume(
    state$1,
    config.volume,
    group_name,
  );
  set_audio_volume_ffi(three_source, effective_volume);
  set_audio_loop_ffi(three_source, config.loop);
  set_audio_playback_rate_ffi(three_source, config.playback_rate);
  let source_data = new AudioSourceData(
    wrap_audio_source(three_source),
    config.volume,
    group_name,
    new $option.None(),
    config.on_end,
  );
  let _block$3;
  let $3 = config.state;
  if ($3 instanceof $audio.Playing) {
    _block$3 = apply_audio_state(state$1, id, source_data, buffer, config);
  } else if ($3 instanceof $audio.Stopped) {
    let source_data$1 = new AudioSourceData(
      source_data.three_source,
      source_data.base_volume,
      source_data.group,
      new $option.Some(config.state),
      source_data.on_end_callback,
    );
    _block$3 = [state$1, source_data$1];
  } else {
    let source_data$1 = new AudioSourceData(
      source_data.three_source,
      source_data.base_volume,
      source_data.group,
      new $option.Some(config.state),
      source_data.on_end_callback,
    );
    _block$3 = [state$1, source_data$1];
  }
  let $2 = _block$3;
  let state$2;
  let source_data$1;
  state$2 = $2[0];
  source_data$1 = $2[1];
  let state$3 = register_audio_source(state$2, id, source_data$1);
  return [state$3, source_data$1];
}
