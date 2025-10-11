-record(rigid_body, {
    kind :: tiramisu@physics:body(),
    mass :: gleam@option:option(float()),
    restitution :: float(),
    friction :: float(),
    linear_damping :: float(),
    angular_damping :: float(),
    collider :: tiramisu@physics:collider_shape(),
    ccd_enabled :: boolean(),
    axis_locks :: tiramisu@physics:axis_lock(),
    collision_groups :: gleam@option:option(tiramisu@physics:collision_groups())
}).
