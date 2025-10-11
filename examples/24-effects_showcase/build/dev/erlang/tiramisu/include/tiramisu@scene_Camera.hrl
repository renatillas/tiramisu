-record(camera, {
    id :: any(),
    camera :: tiramisu@camera:camera(),
    transform :: tiramisu@transform:transform(),
    look_at :: gleam@option:option(vec@vec3:vec3(float())),
    active :: boolean(),
    viewport :: gleam@option:option({integer(), integer(), integer(), integer()})
}).
