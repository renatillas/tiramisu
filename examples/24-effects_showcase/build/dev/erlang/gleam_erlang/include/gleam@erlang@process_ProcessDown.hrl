-record(process_down, {
    monitor :: gleam@erlang@process:monitor(),
    pid :: gleam@erlang@process:pid_(),
    reason :: gleam@erlang@process:exit_reason()
}).
