-record(settings, {
    ack :: boolean(),
    settings :: list(mist@internal@http2@frame:setting())
}).
