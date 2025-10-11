-record(header, {
    data :: mist@internal@http2@frame:data(),
    end_stream :: boolean(),
    identifier :: mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
    priority :: gleam@option:option(mist@internal@http2@frame:header_priority())
}).
