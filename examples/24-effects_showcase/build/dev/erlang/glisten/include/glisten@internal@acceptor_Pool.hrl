-record(pool, {
    handler :: fun((any(), glisten@internal@handler:loop_message(any()), glisten@internal@handler:connection(any())) -> glisten@internal@handler:next(any(), glisten@internal@handler:loop_message(any()))),
    pool_count :: integer(),
    on_init :: fun((glisten@internal@handler:connection(any())) -> {any(),
        gleam@option:option(gleam@erlang@process:selector(any()))}),
    on_close :: gleam@option:option(fun((any()) -> nil)),
    transport :: glisten@transport:transport()
}).
