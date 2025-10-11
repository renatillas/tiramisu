-record(toon_material, {
    color :: integer(),
    map :: gleam@option:option(tiramisu@asset:texture()),
    normal_map :: gleam@option:option(tiramisu@asset:texture()),
    ambient_oclusion_map :: gleam@option:option(tiramisu@asset:texture())
}).
