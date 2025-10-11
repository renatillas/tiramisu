-record(builder, {
    port :: integer(),
    handler :: fun((gleam@http@request:request(any())) -> gleam@http@response:response(any())),
    after_start :: fun((integer(), gleam@http:scheme(), mist:ip_address()) -> nil),
    interface :: binary(),
    ipv6_support :: boolean(),
    tls_options :: gleam@option:option(mist:tls_options())
}).
