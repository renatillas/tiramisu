-record(cache_state, {
    objects :: gleam@dict:dict(binary(), tiramisu@internal@object_cache:three_object()),
    mixers :: gleam@dict:dict(binary(), tiramisu@internal@object_cache:animation_mixer()),
    actions :: gleam@dict:dict(binary(), tiramisu@internal@object_cache:animation_actions()),
    viewports :: gleam@dict:dict(binary(), tiramisu@internal@object_cache:viewport()),
    particles :: gleam@dict:dict(binary(), tiramisu@internal@object_cache:particle_system())
}).
