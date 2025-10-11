-record(text, {
    kind :: integer(),
    key :: binary(),
    mapper :: fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    content :: binary()
}).
