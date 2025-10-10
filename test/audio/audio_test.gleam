import tiramisu/audio

// Test: AudioConfig creation with default values
pub fn audio_config_default_test() {
  let config = audio.config()

  assert config.volume == 1.0
  assert config.loop == False
  assert config.playback_rate == 1.0
  // Check state is Stopped by default
  assert case config.state {
    audio.Stopped -> True
    _ -> False
  }
  // Check fade is NoFade by default
  assert case config.fade {
    audio.NoFade -> True
    _ -> False
  }
}

// Test: Set volume on AudioConfig
pub fn audio_config_set_volume_test() {
  let config =
    audio.config()
    |> audio.with_volume(0.5)

  assert config.volume == 0.5
  assert config.loop == False
  assert config.playback_rate == 1.0
}

// Test: Set loop on AudioConfig
pub fn audio_config_set_loop_test() {
  let config =
    audio.config()
    |> audio.with_loop(True)

  assert config.volume == 1.0
  assert config.loop == True
  assert config.playback_rate == 1.0
}

// Test: Set playback rate on AudioConfig
pub fn audio_config_set_playback_rate_test() {
  let config =
    audio.config()
    |> audio.with_playback_rate(2.0)

  assert config.volume == 1.0
  assert config.loop == False
  assert config.playback_rate == 2.0
}

// Test: Set state to Playing
pub fn audio_config_set_playing_test() {
  let config =
    audio.config()
    |> audio.with_playing()

  assert case config.state {
    audio.Playing -> True
    _ -> False
  }
}

// Test: Set state with fade
pub fn audio_config_with_fade_test() {
  let config =
    audio.config()
    |> audio.with_fade(1000)

  assert case config.fade {
    audio.Fade(duration_ms) -> duration_ms == 1000
    _ -> False
  }
}

// Test: Chain multiple AudioConfig updates
pub fn audio_config_chaining_test() {
  let config =
    audio.config()
    |> audio.with_volume(0.7)
    |> audio.with_loop(True)
    |> audio.with_playback_rate(1.5)
    |> audio.with_playing()
    |> audio.with_fade(500)

  assert config.volume == 0.7
  assert config.loop == True
  assert config.playback_rate == 1.5
  assert case config.state {
    audio.Playing -> True
    _ -> False
  }
  assert case config.fade {
    audio.Fade(duration_ms) -> duration_ms == 500
    _ -> False
  }
}

// Test: GlobalAudio type
pub fn global_audio_type_test() {
  let buffer = unsafe_mock_audio()
  let config = audio.config()
  let audio_type = audio.global(buffer, config)

  // Should match GlobalAudio pattern
  assert case audio_type {
    audio.GlobalAudio(_, _) -> True
    _ -> False
  }
}

// Test: PositionalAudio type with default values
pub fn positional_audio_default_test() {
  let buffer = unsafe_mock_audio()
  let config = audio.config()
  let audio_type = audio.positional(buffer, config)

  // Should match PositionalAudio pattern with defaults
  assert case audio_type {
    audio.PositionalAudio(_, _, ref_distance, rolloff_factor, max_distance) ->
      ref_distance == 1.0 && rolloff_factor == 1.0 && max_distance == 10_000.0
    _ -> False
  }
}

// Test: PositionalAudio with custom ref_distance
pub fn positional_audio_ref_distance_test() {
  let buffer = unsafe_mock_audio()
  let config = audio.config()
  let audio_type =
    audio.positional(buffer, config)
    |> audio.with_ref_distance(5.0)

  assert case audio_type {
    audio.PositionalAudio(_, _, ref_distance, _, _) -> ref_distance == 5.0
    _ -> False
  }
}

// Test: PositionalAudio with custom max_distance
pub fn positional_audio_max_distance_test() {
  let buffer = unsafe_mock_audio()
  let config = audio.config()
  let audio_type =
    audio.positional(buffer, config)
    |> audio.with_max_distance(100.0)

  assert case audio_type {
    audio.PositionalAudio(_, _, _, _, max_distance) -> max_distance == 100.0
    _ -> False
  }
}

// Test: PositionalAudio with custom rolloff_factor
pub fn positional_audio_rolloff_factor_test() {
  let buffer = unsafe_mock_audio()
  let config = audio.config()
  let audio_type =
    audio.positional(buffer, config)
    |> audio.with_rolloff_factor(2.0)

  assert case audio_type {
    audio.PositionalAudio(_, _, _, rolloff_factor, _) -> rolloff_factor == 2.0
    _ -> False
  }
}

// Test: PositionalAudio with all custom values
pub fn positional_audio_full_config_test() {
  let buffer = unsafe_mock_audio()
  let config = audio.config()
  let audio_type =
    audio.positional(buffer, config)
    |> audio.with_ref_distance(3.0)
    |> audio.with_max_distance(50.0)
    |> audio.with_rolloff_factor(1.5)

  assert case audio_type {
    audio.PositionalAudio(_, _, ref_distance, rolloff_factor, max_distance) ->
      ref_distance == 3.0 && rolloff_factor == 1.5 && max_distance == 50.0
    _ -> False
  }
}

// Mock audio buffer for testing
@external(javascript, "../ffi/test_helpers.mjs", "mockAudio")
fn unsafe_mock_audio() -> audio.AudioBuffer
