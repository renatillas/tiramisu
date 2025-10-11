-record(batch, {
    kind :: integer(),
    messages :: list(lustre@runtime@transport:server_message())
}).
