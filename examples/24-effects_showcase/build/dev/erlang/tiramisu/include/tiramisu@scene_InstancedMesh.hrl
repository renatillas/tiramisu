-record(instanced_mesh, {
    id :: any(),
    geometry :: tiramisu@geometry:geometry(),
    material :: tiramisu@material:material(),
    instances :: list(tiramisu@transform:transform())
}).
