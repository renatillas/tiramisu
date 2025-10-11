-record(builder, {
    initialise :: fun((gleam@erlang@process:subject(any())) -> {ok,
            gleam@otp@actor:initialised(any(), any(), any())} |
        {error, binary()}),
    initialisation_timeout :: integer(),
    on_message :: fun((any(), any()) -> gleam@otp@actor:next(any(), any())),
    name :: gleam@option:option(gleam@erlang@process:name(any()))
}).
