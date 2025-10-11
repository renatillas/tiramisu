-record(state, {
    listen_socket :: glisten@socket:listen_socket(),
    port :: integer(),
    ip_address :: glisten@socket@options:ip_address()
}).
