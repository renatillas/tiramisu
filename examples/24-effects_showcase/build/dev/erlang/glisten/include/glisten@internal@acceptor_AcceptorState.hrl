-record(acceptor_state, {
    sender :: gleam@erlang@process:subject(glisten@internal@acceptor:acceptor_message()),
    socket :: gleam@option:option(glisten@socket:socket()),
    transport :: glisten@transport:transport()
}).
