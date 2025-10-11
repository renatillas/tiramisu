-record(rigid_body_builder, {
    kind :: tiramisu@physics:body(),
    collider :: gleam@option:option(tiramisu@physics:collider_shape()),
    mass :: gleam@option:option(float()),
    restitution :: float(),
    friction :: float(),
    linear_damping :: float(),
    angular_damping :: float(),
    ccd_enabled :: boolean(),
    axis_locks :: tiramisu@physics:axis_lock(),
    collision_groups :: gleam@option:option(tiramisu@physics:collision_groups())
}).
