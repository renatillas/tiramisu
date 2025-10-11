-record(loop_state, {
    client_ip :: {ok, {glisten@socket@options:ip_address(), integer()}} |
        {error, nil},
    socket :: glisten@socket:socket(),
    sender :: gleam@erlang@process:subject(glisten@internal@handler:message(any())),
    transport :: glisten@transport:transport(),
    state :: any()
}).
