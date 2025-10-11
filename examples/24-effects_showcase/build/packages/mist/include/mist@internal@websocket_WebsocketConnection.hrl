-record(websocket_connection, {
    socket :: glisten@socket:socket(),
    transport :: glisten@transport:transport(),
    deflate :: gleam@option:option(gramps@websocket@compression:context())
}).
