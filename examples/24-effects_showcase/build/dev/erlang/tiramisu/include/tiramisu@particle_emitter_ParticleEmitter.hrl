-record(particle_emitter, {
    rate :: float(),
    lifetime :: float(),
    velocity :: vec@vec3:vec3(float()),
    velocity_variance :: vec@vec3:vec3(float()),
    size :: float(),
    size_variance :: float(),
    color :: integer(),
    color_end :: gleam@option:option(integer()),
    gravity_scale :: float(),
    max_particles :: integer()
}).
