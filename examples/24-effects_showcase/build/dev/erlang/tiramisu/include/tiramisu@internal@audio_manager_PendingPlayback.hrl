-record(pending_playback, {
    id :: binary(),
    source_data :: tiramisu@internal@audio_manager:audio_source_data(),
    fade_duration_ms :: integer()
}).
