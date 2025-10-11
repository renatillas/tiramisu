-record(http2_settings, {
    header_table_size :: integer(),
    server_push :: mist@internal@http2@frame:push_state(),
    max_concurrent_streams :: integer(),
    initial_window_size :: integer(),
    max_frame_size :: integer(),
    max_header_list_size :: gleam@option:option(integer())
}).
