-record(builder, {
    interface :: glisten@socket@options:interface(),
    on_init :: fun((glisten:connection(any())) -> {any(),
        gleam@option:option(gleam@erlang@process:selector(any()))}),
    loop :: fun((any(), glisten:message(any()), glisten:connection(any())) -> glisten:next(any(), glisten:message(any()))),
    on_close :: gleam@option:option(fun((any()) -> nil)),
    pool_size :: integer(),
    http2_support :: boolean(),
    ipv6_support :: boolean(),
    tls_options :: gleam@option:option(glisten@socket@options:tls_certs())
}).
