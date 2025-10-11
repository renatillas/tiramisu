-record(particle_system_state, {
    emitter :: tiramisu@particle_emitter:particle_emitter(),
    transform :: tiramisu@transform:transform(),
    active :: boolean(),
    particles :: list(tiramisu@internal@particle_manager:particle()),
    time_since_last_spawn :: float(),
    points_object :: gleam@dynamic:dynamic_(),
    start_color :: gleam@dynamic:dynamic_(),
    end_color :: gleam@dynamic:dynamic_()
}).
