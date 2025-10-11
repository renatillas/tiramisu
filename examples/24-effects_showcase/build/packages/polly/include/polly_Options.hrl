-record(options, {
    interval :: integer(),
    paths :: list(binary()),
    max_depth :: integer(),
    filter :: fun((simplifile:file_type(), binary()) -> boolean()),
    ignore_initial_missing :: boolean()
}).
