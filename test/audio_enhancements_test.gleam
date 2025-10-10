import gleam/option
import gleeunit
import tiramisu/audio

pub fn main() {
  gleeunit.main()
}

// --- Audio Group Tests ---

pub fn audio_group_sfx_test() {
  let group = audio.SFX
  let config = audio.config_with_group(group)

  // Verify defaults
  assert config.volume == 1.0
  assert config.loop == False
  assert config.playback_rate == 1.0

  // Verify group is set
  let assert True = case config.group {
    option.Some(audio.SFX) -> True
    _ -> False
  }
}

pub fn audio_group_music_test() {
  let config =
    audio.config()
    |> audio.with_group(audio.Music)

  let audio.AudioConfig(group: group_opt, ..) = config

  let assert True = case group_opt {
    option.Some(audio.Music) -> True
    _ -> False
  }
}

pub fn audio_group_voice_test() {
  let config =
    audio.config()
    |> audio.with_group(audio.Voice)

  let audio.AudioConfig(group: group_opt, ..) = config

  let assert True = case group_opt {
    option.Some(audio.Voice) -> True
    _ -> False
  }
}

pub fn audio_group_ambient_test() {
  let config =
    audio.config()
    |> audio.with_group(audio.Ambient)

  let audio.AudioConfig(group: group_opt, ..) = config

  let assert True = case group_opt {
    option.Some(audio.Ambient) -> True
    _ -> False
  }
}

pub fn audio_group_custom_test() {
  let config =
    audio.config()
    |> audio.with_group(audio.Custom("ui"))

  let audio.AudioConfig(group: group_opt, ..) = config

  let assert True = case group_opt {
    option.Some(audio.Custom("ui")) -> True
    _ -> False
  }
}

pub fn audio_config_builder_with_group_test() {
  let config =
    audio.config_with_group(audio.SFX)
    |> audio.with_volume(0.8)
    |> audio.with_loop(True)
    |> audio.with_playback_rate(1.5)
    |> audio.with_playing()

  assert config.volume == 0.8
  assert config.loop == True
  assert config.playback_rate == 1.5

  let assert True = case config.group {
    option.Some(audio.SFX) -> True
    _ -> False
  }

  let assert True = case config.state {
    audio.Playing -> True
    _ -> False
  }
}

pub fn audio_config_no_group_test() {
  let config = audio.config()

  let audio.AudioConfig(group: group_opt, ..) = config

  let assert True = case group_opt {
    option.None -> True
    _ -> False
  }
}

// --- Music System Tests ---

pub fn music_config_test() {
  let config =
    audio.config_with_group(audio.Music)
    |> audio.with_volume(0.7)
    |> audio.with_loop(True)

  let assert audio.AudioConfig(volume: 0.7, loop: True, group: group_opt, ..) =
    config

  let assert True = case group_opt {
    option.Some(audio.Music) -> True
    _ -> False
  }
}

pub fn voice_config_test() {
  let config =
    audio.config_with_group(audio.Voice)
    |> audio.with_volume(1.0)
    |> audio.with_loop(False)

  let assert audio.AudioConfig(volume: 1.0, loop: False, group: group_opt, ..) =
    config

  let assert True = case group_opt {
    option.Some(audio.Voice) -> True
    _ -> False
  }
}

pub fn ambient_config_test() {
  let config =
    audio.config_with_group(audio.Ambient)
    |> audio.with_volume(0.5)
    |> audio.with_loop(True)

  let assert audio.AudioConfig(volume: 0.5, loop: True, group: group_opt, ..) =
    config

  let assert True = case group_opt {
    option.Some(audio.Ambient) -> True
    _ -> False
  }
}
