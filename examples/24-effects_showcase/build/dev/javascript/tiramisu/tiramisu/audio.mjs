import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import {
  setGroupVolume as set_group_volume_ffi,
  getGroupVolume as get_group_volume_ffi,
  muteGroup as mute_group_ffi,
  unmuteGroup as unmute_group_ffi,
} from "../tiramisu.ffi.mjs";

export class SFX extends $CustomType {}

export class Music extends $CustomType {}

export class Voice extends $CustomType {}

export class Ambient extends $CustomType {}

/**
 * Custom group with a name
 */
export class Custom extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Playing extends $CustomType {}

export class Stopped extends $CustomType {}

export class Paused extends $CustomType {}

export class NoFade extends $CustomType {}

/**
 * Fade in/out over specified milliseconds
 */
export class Fade extends $CustomType {
  constructor(duration_ms) {
    super();
    this.duration_ms = duration_ms;
  }
}

export class AudioConfig extends $CustomType {
  constructor(state, volume, loop, playback_rate, fade, group, on_end) {
    super();
    this.state = state;
    this.volume = volume;
    this.loop = loop;
    this.playback_rate = playback_rate;
    this.fade = fade;
    this.group = group;
    this.on_end = on_end;
  }
}

/**
 * Global audio (2D, same volume everywhere)
 */
export class GlobalAudio extends $CustomType {
  constructor(buffer, config) {
    super();
    this.buffer = buffer;
    this.config = config;
  }
}

/**
 * Positional audio (3D, volume based on distance)
 */
export class PositionalAudio extends $CustomType {
  constructor(buffer, config, ref_distance, rolloff_factor, max_distance) {
    super();
    this.buffer = buffer;
    this.config = config;
    this.ref_distance = ref_distance;
    this.rolloff_factor = rolloff_factor;
    this.max_distance = max_distance;
  }
}

/**
 * Create default audio config (stopped, no fade)
 */
export function config() {
  return new AudioConfig(
    new Stopped(),
    1.0,
    false,
    1.0,
    new NoFade(),
    new $option.None(),
    new $option.None(),
  );
}

/**
 * Create audio config with a group
 */
export function config_with_group(group) {
  return new AudioConfig(
    new Stopped(),
    1.0,
    false,
    1.0,
    new NoFade(),
    new $option.Some(group),
    new $option.None(),
  );
}

/**
 * Create audio config that starts playing
 */
export function playing() {
  return new AudioConfig(
    new Playing(),
    1.0,
    false,
    1.0,
    new NoFade(),
    new $option.None(),
    new $option.None(),
  );
}

/**
 * Set playback state (Playing, Stopped, Paused)
 */
export function with_state(config, state) {
  return new AudioConfig(
    state,
    config.volume,
    config.loop,
    config.playback_rate,
    config.fade,
    config.group,
    config.on_end,
  );
}

/**
 * Set audio to playing
 */
export function with_playing(config) {
  return new AudioConfig(
    new Playing(),
    config.volume,
    config.loop,
    config.playback_rate,
    config.fade,
    config.group,
    config.on_end,
  );
}

/**
 * Set audio to stopped
 */
export function with_stopped(config) {
  return new AudioConfig(
    new Stopped(),
    config.volume,
    config.loop,
    config.playback_rate,
    config.fade,
    config.group,
    config.on_end,
  );
}

/**
 * Set audio to paused
 */
export function with_paused(config) {
  return new AudioConfig(
    new Paused(),
    config.volume,
    config.loop,
    config.playback_rate,
    config.fade,
    config.group,
    config.on_end,
  );
}

/**
 * Set fade configuration
 */
export function with_fade(config, duration_ms) {
  return new AudioConfig(
    config.state,
    config.volume,
    config.loop,
    config.playback_rate,
    new Fade(duration_ms),
    config.group,
    config.on_end,
  );
}

/**
 * Set no fade (instant transitions)
 */
export function with_no_fade(config) {
  return new AudioConfig(
    config.state,
    config.volume,
    config.loop,
    config.playback_rate,
    new NoFade(),
    config.group,
    config.on_end,
  );
}

/**
 * Set volume in config (0.0 to 1.0)
 */
export function with_volume(config, volume) {
  return new AudioConfig(
    config.state,
    volume,
    config.loop,
    config.playback_rate,
    config.fade,
    config.group,
    config.on_end,
  );
}

/**
 * Set looping in config
 */
export function with_loop(config, loop) {
  return new AudioConfig(
    config.state,
    config.volume,
    loop,
    config.playback_rate,
    config.fade,
    config.group,
    config.on_end,
  );
}

/**
 * Set playback rate in config (1.0 = normal, 2.0 = double speed, etc.)
 */
export function with_playback_rate(config, rate) {
  return new AudioConfig(
    config.state,
    config.volume,
    config.loop,
    rate,
    config.fade,
    config.group,
    config.on_end,
  );
}

/**
 * Set audio group in config
 */
export function with_group(config, group) {
  return new AudioConfig(
    config.state,
    config.volume,
    config.loop,
    config.playback_rate,
    config.fade,
    new $option.Some(group),
    config.on_end,
  );
}

/**
 * Set callback to be called when audio ends (for non-looping audio)
 *
 * This is useful for one-shot sounds like SFX where you need to know
 * when the sound has finished playing.
 *
 * ## Example
 *
 * ```gleam
 * audio.config()
 * |> audio.with_state(audio.Playing)
 * |> audio.with_on_end(fn() {
 *   // Audio finished playing
 *   io.println("SFX finished!")
 * })
 * ```
 */
export function with_on_end(config, callback) {
  return new AudioConfig(
    config.state,
    config.volume,
    config.loop,
    config.playback_rate,
    config.fade,
    config.group,
    new $option.Some(callback),
  );
}

/**
 * Create global audio (2D, same volume everywhere)
 */
export function global(buffer, config) {
  return new GlobalAudio(buffer, config);
}

/**
 * Create a default positional audio configuration
 */
export function positional(buffer, config) {
  return new PositionalAudio(buffer, config, 1.0, 1.0, 10_000.0);
}

/**
 * Set reference distance for positional audio
 */
export function with_ref_distance(audio, distance) {
  if (audio instanceof GlobalAudio) {
    return audio;
  } else {
    let buffer = audio.buffer;
    let config$1 = audio.config;
    let rolloff = audio.rolloff_factor;
    let max = audio.max_distance;
    return new PositionalAudio(buffer, config$1, distance, rolloff, max);
  }
}

/**
 * Set rolloff factor for positional audio
 */
export function with_rolloff_factor(audio, factor) {
  if (audio instanceof GlobalAudio) {
    return audio;
  } else {
    let buffer = audio.buffer;
    let config$1 = audio.config;
    let ref = audio.ref_distance;
    let max = audio.max_distance;
    return new PositionalAudio(buffer, config$1, ref, factor, max);
  }
}

/**
 * Set maximum distance for positional audio
 */
export function with_max_distance(audio, distance) {
  if (audio instanceof GlobalAudio) {
    return audio;
  } else {
    let buffer = audio.buffer;
    let config$1 = audio.config;
    let ref = audio.ref_distance;
    let rolloff = audio.rolloff_factor;
    return new PositionalAudio(buffer, config$1, ref, rolloff, distance);
  }
}

/**
 * Convert AudioGroup to string for FFI
 * 
 * @ignore
 */
function audio_group_to_string(group) {
  if (group instanceof SFX) {
    return "sfx";
  } else if (group instanceof Music) {
    return "music";
  } else if (group instanceof Voice) {
    return "voice";
  } else if (group instanceof Ambient) {
    return "ambient";
  } else {
    let name = group[0];
    return name;
  }
}

/**
 *
 * Audio groups provide global volume control for categories of sounds.
 * These are the only imperative functions in the audio API, as they control
 * global settings rather than individual audio source state.
 * Set volume for an entire audio group (0.0 to 1.0)
 */
export function set_group_volume(group, volume) {
  return set_group_volume_ffi(audio_group_to_string(group), volume);
}

/**
 * Get current volume for an audio group
 */
export function get_group_volume(group) {
  return get_group_volume_ffi(audio_group_to_string(group));
}

/**
 * Mute an entire audio group
 */
export function mute_group(group) {
  return mute_group_ffi(audio_group_to_string(group));
}

/**
 * Unmute an entire audio group
 */
export function unmute_group(group) {
  return unmute_group_ffi(audio_group_to_string(group));
}
