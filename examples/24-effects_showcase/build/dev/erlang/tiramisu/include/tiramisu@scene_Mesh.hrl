-record(mesh, {
    id :: any(),
    geometry :: tiramisu@geometry:geometry(),
    material :: tiramisu@material:material(),
    transform :: tiramisu@transform:transform(),
    physics :: gleam@option:option(tiramisu@physics:rigid_body())
}).
