-record(child_specification, {
    start :: fun(() -> {ok, gleam@otp@actor:started(any())} |
        {error, gleam@otp@actor:start_error()}),
    restart :: gleam@otp@supervision:restart(),
    significant :: boolean(),
    child_type :: gleam@otp@supervision:child_type()
}).
