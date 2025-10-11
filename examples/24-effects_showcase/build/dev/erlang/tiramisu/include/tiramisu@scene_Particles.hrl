-record(particles, {
    id :: any(),
    emitter :: tiramisu@particle_emitter:particle_emitter(),
    transform :: tiramisu@transform:transform(),
    active :: boolean()
}).
