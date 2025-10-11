-record(port_down, {
    monitor :: gleam@erlang@process:monitor(),
    port :: gleam@erlang@port:port_(),
    reason :: gleam@erlang@process:exit_reason()
}).
