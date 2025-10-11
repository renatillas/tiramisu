-record(state, {
    fragment :: gleam@option:option(mist@internal@http2@frame:frame()),
    frame_buffer :: mist@internal@buffer:buffer(),
    pending_sends :: list(mist@internal@http2@handler:pending_send()),
    receive_hpack_context :: mist@internal@http2:hpack_context(),
    self :: gleam@erlang@process:subject(mist@internal@http2@stream:send_message()),
    send_hpack_context :: mist@internal@http2:hpack_context(),
    send_window_size :: integer(),
    receive_window_size :: integer(),
    settings :: mist@internal@http2:http2_settings(),
    streams :: gleam@dict:dict(mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()), mist@internal@http2@stream:state())
}).
