-record(audio_source_data, {
    three_source :: tiramisu@internal@audio_manager:three_audio_source(),
    base_volume :: float(),
    group :: gleam@option:option(binary()),
    previous_state :: gleam@option:option(tiramisu@audio:audio_state()),
    on_end_callback :: gleam@option:option(fun(() -> nil))
}).
