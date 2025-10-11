-record(flag, {
    name :: binary(),
    desc :: binary(),
    parser :: fun((binary()) -> {ok, any()} | {error, snag:snag()}),
    value :: fun((glint:flag_internals(any())) -> glint:value()),
    getter :: fun((glint:flags(), binary()) -> {ok, any()} |
        {error, snag:snag()}),
    default :: gleam@option:option(any())
}).
