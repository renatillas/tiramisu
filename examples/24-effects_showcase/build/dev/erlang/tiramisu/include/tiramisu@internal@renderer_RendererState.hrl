-record(renderer_state, {
    renderer :: tiramisu@internal@renderer:web_g_l_renderer(),
    scene :: tiramisu@internal@renderer:scene(),
    cache :: tiramisu@internal@object_cache:cache_state(),
    physics_world :: gleam@option:option(tiramisu@physics:physics_world(any())),
    audio_manager :: gleam@dynamic:dynamic_()
}).
