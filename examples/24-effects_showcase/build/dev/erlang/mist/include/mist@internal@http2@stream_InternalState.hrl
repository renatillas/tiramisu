-record(internal_state, {
    data_selector :: gleam@erlang@process:selector(mist@internal@http2@stream:message()),
    data_subject :: gleam@erlang@process:subject(mist@internal@http2@stream:message()),
    'end' :: boolean(),
    pending_response :: gleam@option:option(gleam@http@response:response(mist@internal@http:response_data())),
    to_remove :: bitstring()
}).
