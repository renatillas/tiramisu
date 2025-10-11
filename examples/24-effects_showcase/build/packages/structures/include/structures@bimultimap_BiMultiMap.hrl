-record(bi_multi_map, {
    direct :: gleam@dict:dict(any(), gleam@set:set(any())),
    reverse :: gleam@dict:dict(any(), gleam@set:set(any()))
}).
