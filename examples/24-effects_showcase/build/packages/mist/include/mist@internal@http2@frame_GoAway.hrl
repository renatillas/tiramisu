-record(go_away, {
    data :: bitstring(),
    error :: mist@internal@http2@frame:connection_error(),
    last_stream_id :: mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame())
}).
