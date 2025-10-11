-record(context, {
    delta_time :: float(),
    input :: tiramisu@input:input_state(),
    canvas_width :: float(),
    canvas_height :: float(),
    physics_world :: gleam@option:option(tiramisu@physics:physics_world(any())),
    input_manager :: tiramisu@internal@managers:input_manager(),
    audio_manager :: tiramisu@internal@managers:audio_manager()
}).
