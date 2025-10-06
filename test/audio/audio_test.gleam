import tiramisu/audio

// Test: AudioConfig creation with default values
pub fn audio_config_default_test() {
  let config = audio.config()

  assert config.volume == 1.0
  assert config.loop == False
  assert config.playback_rate == 1.0
  assert config.autoplay == False
}

// Test: Set volume on AudioConfig
pub fn audio_config_set_volume_test() {
  let config =
    audio.config()
    |> audio.set_volume(0.5)

  assert config.volume == 0.5
  assert config.loop == False
  assert config.playback_rate == 1.0
  assert config.autoplay == False
}

// Test: Set loop on AudioConfig
pub fn audio_config_set_loop_test() {
  let config =
    audio.config()
    |> audio.set_loop(True)

  assert config.volume == 1.0
  assert config.loop == True
  assert config.playback_rate == 1.0
  assert config.autoplay == False
}

// Test: Set playback rate on AudioConfig
pub fn audio_config_set_playback_rate_test() {
  let config =
    audio.config()
    |> audio.set_playback_rate(2.0)

  assert config.volume == 1.0
  assert config.loop == False
  assert config.playback_rate == 2.0
  assert config.autoplay == False
}

// Test: Set autoplay on AudioConfig
pub fn audio_config_set_autoplay_test() {
  let config =
    audio.config()
    |> audio.set_autoplay(True)

  assert config.volume == 1.0
  assert config.loop == False
  assert config.playback_rate == 1.0
  assert config.autoplay == True
}

// Test: Chain multiple AudioConfig updates
pub fn audio_config_chaining_test() {
  let config =
    audio.config()
    |> audio.set_volume(0.7)
    |> audio.set_loop(True)
    |> audio.set_playback_rate(1.5)
    |> audio.set_autoplay(True)

  assert config.volume == 0.7
  assert config.loop == True
  assert config.playback_rate == 1.5
  assert config.autoplay == True
}

// Test: GlobalAudio type
pub fn global_audio_type_test() {
  let audio_type = audio.GlobalAudio

  // Should match GlobalAudio pattern
  assert case audio_type {
    audio.GlobalAudio -> True
  }
}

// Test: PositionalAudio type with default values
pub fn positional_audio_default_test() {
  let audio_type = audio.positional()

  // Should match PositionalAudio pattern with defaults
  assert case audio_type {
    audio.PositionalAudio(ref_distance, rolloff_factor, max_distance) ->
      ref_distance == 1.0
      && rolloff_factor == 1.0
      && max_distance == 10000.0
    _ -> False
  }
}

// Test: PositionalAudio with custom ref_distance
pub fn positional_audio_ref_distance_test() {
  let audio_type =
    audio.positional()
    |> audio.set_ref_distance(5.0)

  assert case audio_type {
    audio.PositionalAudio(ref_distance, _, _) -> ref_distance == 5.0
    _ -> False
  }
}

// Test: PositionalAudio with custom max_distance
pub fn positional_audio_max_distance_test() {
  let audio_type =
    audio.positional()
    |> audio.set_max_distance(100.0)

  assert case audio_type {
    audio.PositionalAudio(_, _, max_distance) -> max_distance == 100.0
    _ -> False
  }
}

// Test: PositionalAudio with custom rolloff_factor
pub fn positional_audio_rolloff_factor_test() {
  let audio_type =
    audio.positional()
    |> audio.set_rolloff_factor(2.0)

  assert case audio_type {
    audio.PositionalAudio(_, rolloff_factor, _) -> rolloff_factor == 2.0
    _ -> False
  }
}

// Test: PositionalAudio with all custom values
pub fn positional_audio_full_config_test() {
  let audio_type =
    audio.positional()
    |> audio.set_ref_distance(3.0)
    |> audio.set_max_distance(50.0)
    |> audio.set_rolloff_factor(1.5)

  assert case audio_type {
    audio.PositionalAudio(ref_distance, rolloff_factor, max_distance) ->
      ref_distance == 3.0 && rolloff_factor == 1.5 && max_distance == 50.0
    _ -> False
  }
}
