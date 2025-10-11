-record(physics_world, {
    world :: tiramisu@physics:rapier_world(),
    queue :: tiramisu@physics:rapier_event_queue(),
    bodies :: gleam@dict:dict(any(), tiramisu@physics:rigid_body()),
    rapier_bodies :: gleam@dict:dict(binary(), tiramisu@physics:rapier_rigid_body()),
    pending_commands :: list(tiramisu@physics:physics_command(any())),
    collider_to_body :: gleam@dict:dict(integer(), binary()),
    collision_events :: list(tiramisu@physics:collision_event()),
    bimap :: structures@bimap:bi_map(any(), binary())
}).
