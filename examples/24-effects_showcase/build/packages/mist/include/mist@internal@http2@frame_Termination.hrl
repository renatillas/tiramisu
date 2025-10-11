-record(termination, {
    error :: mist@internal@http2@frame:connection_error(),
    identifier :: mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame())
}).
