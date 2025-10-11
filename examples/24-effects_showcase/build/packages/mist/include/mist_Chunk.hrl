-record(chunk, {
    data :: bitstring(),
    consume :: fun((integer()) -> {ok, mist:chunk()} |
        {error, mist:read_error()})
}).
