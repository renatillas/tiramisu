-record(standard_material, {
    color :: integer(),
    map :: gleam@option:option(tiramisu@asset:texture()),
    normal_map :: gleam@option:option(tiramisu@asset:texture()),
    ambient_oclusion_map :: gleam@option:option(tiramisu@asset:texture()),
    roughness_map :: gleam@option:option(tiramisu@asset:texture()),
    metalness_map :: gleam@option:option(tiramisu@asset:texture()),
    metalness :: float(),
    roughness :: float(),
    transparent :: boolean(),
    opacity :: float()
}).
