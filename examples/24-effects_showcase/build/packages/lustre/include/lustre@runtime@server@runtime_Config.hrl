-record(config, {
    open_shadow_root :: boolean(),
    adopt_styles :: boolean(),
    attributes :: gleam@dict:dict(binary(), fun((binary()) -> {ok, any()} |
        {error, nil})),
    properties :: gleam@dict:dict(binary(), gleam@dynamic@decode:decoder(any())),
    contexts :: gleam@dict:dict(binary(), gleam@dynamic@decode:decoder(any()))
}).
