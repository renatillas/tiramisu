-record(data, {
    data :: bitstring(),
    end_stream :: boolean(),
    identifier :: mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame())
}).
