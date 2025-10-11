-record(websocket_state, {
    buffer :: bitstring(),
    user :: any(),
    permessage_deflate :: gleam@option:option(gramps@websocket@compression:compression())
}).
