-record(stream, {
    selector :: gleam@erlang@process:selector(bitstring()),
    data :: bitstring(),
    remaining :: integer(),
    attempts :: integer()
}).
