-record(audio_config, {
    state :: tiramisu@audio:audio_state(),
    volume :: float(),
    loop :: boolean(),
    playback_rate :: float(),
    fade :: tiramisu@audio:fade_config(),
    group :: gleam@option:option(tiramisu@audio:audio_group()),
    on_end :: gleam@option:option(fun(() -> nil))
}).
