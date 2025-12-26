import gleam/option
import gleam/time/duration
import gleeunit
import tiramisu/audio

pub fn main() {
  gleeunit.main()
}

// AudioConfig Constructor Tests

pub fn config_default_test() {
  let cfg = audio.config()

  assert cfg.state == audio.Stopped
  assert cfg.volume == 1.0
  assert cfg.loop == False
  assert cfg.playback_rate == 1.0
  assert cfg.fade == audio.NoFade
  assert cfg.group == option.None
  assert cfg.on_end == option.None
}

pub fn playing_config_test() {
  let cfg = audio.playing()

  assert cfg.state == audio.Playing
  assert cfg.volume == 1.0
}

// AudioConfig Builder Tests

pub fn with_state_test() {
  let cfg_playing = audio.config() |> audio.with_state(audio.Playing)
  assert cfg_playing.state == audio.Playing

  let cfg_paused = audio.config() |> audio.with_state(audio.Paused)
  assert cfg_paused.state == audio.Paused
}

pub fn with_playing_test() {
  let cfg = audio.config() |> audio.with_playing()
  assert cfg.state == audio.Playing
}

pub fn with_stopped_test() {
  let cfg = audio.config() |> audio.with_playing() |> audio.with_stopped()
  assert cfg.state == audio.Stopped
}

pub fn with_paused_test() {
  let cfg = audio.config() |> audio.with_paused()
  assert cfg.state == audio.Paused
}

pub fn with_fade_test() {
  let cfg = audio.config() |> audio.with_fade(duration.milliseconds(500))
  let assert audio.Fade(duration) = cfg.fade
  assert duration == duration.milliseconds(500)
}

pub fn with_no_fade_test() {
  let cfg =
    audio.config()
    |> audio.with_fade(duration.milliseconds(500))
    |> audio.with_no_fade()

  assert cfg.fade == audio.NoFade
}

pub fn with_volume_test() {
  let cfg = audio.config() |> audio.with_volume(0.5)
  assert cfg.volume == 0.5
}

pub fn with_loop_test() {
  let cfg = audio.config() |> audio.with_loop(True)
  assert cfg.loop == True
}

pub fn with_playback_rate_test() {
  let cfg = audio.config() |> audio.with_playback_rate(2.0)
  assert cfg.playback_rate == 2.0
}

pub fn with_group_test() {
  // Test built-in groups
  let cfg_sfx = audio.config() |> audio.with_group(audio.SFX)
  assert cfg_sfx.group == option.Some(audio.SFX)

  let cfg_music = audio.config() |> audio.with_group(audio.Music)
  assert cfg_music.group == option.Some(audio.Music)

  // Test custom group
  let cfg_custom = audio.config() |> audio.with_group(audio.Custom("ui"))
  let assert option.Some(audio.Custom(name)) = cfg_custom.group
  assert name == "ui"
}

pub fn with_on_end_test() {
  let callback = fn() { Nil }
  let cfg = audio.config() |> audio.with_on_end(callback)

  let assert option.Some(_) = cfg.on_end
}
