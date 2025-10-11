-record(geolocation_position, {
    latitude :: float(),
    longitude :: float(),
    altitude :: gleam@option:option(float()),
    accuracy :: float(),
    altitude_accuracy :: gleam@option:option(float()),
    heading :: gleam@option:option(float()),
    speed :: gleam@option:option(float()),
    timestamp :: float()
}).
