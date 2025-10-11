-record(http1_request, {
    request :: gleam@http@request:request(mist@internal@http:connection()),
    version :: mist@internal@http:http_version()
}).
