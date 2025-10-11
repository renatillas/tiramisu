-record(connection, {
    body :: mist@internal@http:body(),
    socket :: glisten@socket:socket(),
    transport :: glisten@transport:transport()
}).
