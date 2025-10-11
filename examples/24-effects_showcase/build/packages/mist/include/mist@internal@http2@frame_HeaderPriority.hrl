-record(header_priority, {
    exclusive :: boolean(),
    stream_dependency :: mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
    weight :: integer()
}).
