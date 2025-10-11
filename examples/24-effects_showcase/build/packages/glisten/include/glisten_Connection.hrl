-record(connection, {
    socket :: glisten@socket:socket(),
    transport :: glisten@transport:transport(),
    subject :: gleam@erlang@process:subject(glisten@internal@handler:message(any()))
}).
