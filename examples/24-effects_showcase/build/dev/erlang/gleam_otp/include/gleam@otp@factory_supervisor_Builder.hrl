-record(builder, {
    child_type :: gleam@otp@supervision:child_type(),
    template :: fun((any()) -> {ok, gleam@otp@actor:started(any())} |
        {error, gleam@otp@actor:start_error()}),
    restart_strategy :: gleam@otp@supervision:restart(),
    intensity :: integer(),
    period :: integer(),
    name :: gleam@option:option(gleam@erlang@process:name(gleam@otp@factory_supervisor:message(any(), any())))
}).
