-record(connection, {
    reader :: fun((integer()) -> {ok, wisp@internal:read()} | {error, nil}),
    max_body_size :: integer(),
    max_files_size :: integer(),
    read_chunk_size :: integer(),
    secret_key_base :: binary(),
    temporary_directory :: binary()
}).
