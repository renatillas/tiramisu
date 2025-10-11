-record(state, {
    id :: mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
    state :: mist@internal@http2@stream:stream_state(),
    subject :: gleam@erlang@process:subject(mist@internal@http2@stream:message()),
    receive_window_size :: integer(),
    send_window_size :: integer(),
    pending_content_length :: gleam@option:option(integer())
}).
