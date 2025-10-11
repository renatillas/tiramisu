-record(priority, {
    exclusive :: boolean(),
    identifier :: mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
    stream_dependency :: mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
    weight :: integer()
}).
