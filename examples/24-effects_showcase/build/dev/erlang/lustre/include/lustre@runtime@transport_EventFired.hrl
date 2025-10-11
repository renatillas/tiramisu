-record(event_fired, {
    kind :: integer(),
    path :: binary(),
    name :: binary(),
    event :: gleam@dynamic:dynamic_()
}).
