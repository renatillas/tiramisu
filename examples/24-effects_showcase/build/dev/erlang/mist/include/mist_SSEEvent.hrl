-record(s_s_e_event, {
    id :: gleam@option:option(binary()),
    event :: gleam@option:option(binary()),
    retry :: gleam@option:option(integer()),
    data :: gleam@string_tree:string_tree()
}).
