-record(push_promise, {
    data :: mist@internal@http2@frame:data(),
    identifier :: mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
    promised_stream_id :: mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame())
}).
