-record(model3_d, {
    id :: any(),
    object :: tiramisu@object3d:object3_d(),
    transform :: tiramisu@transform:transform(),
    animation :: gleam@option:option(tiramisu@object3d:animation_playback()),
    physics :: gleam@option:option(tiramisu@physics:rigid_body())
}).
