-record(positional_audio, {
    buffer :: tiramisu@audio:audio_buffer(),
    config :: tiramisu@audio:audio_config(),
    ref_distance :: float(),
    rolloff_factor :: float(),
    max_distance :: float()
}).
