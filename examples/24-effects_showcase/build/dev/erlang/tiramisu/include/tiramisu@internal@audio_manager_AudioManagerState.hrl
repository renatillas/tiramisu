-record(audio_manager_state, {
    sources :: gleam@dict:dict(binary(), tiramisu@internal@audio_manager:audio_source_data()),
    group_volumes :: gleam@dict:dict(binary(), float()),
    muted_groups :: list(binary()),
    context_resumed :: boolean(),
    pending_playbacks :: list(tiramisu@internal@audio_manager:pending_playback())
}).
