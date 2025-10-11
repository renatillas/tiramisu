-record(fragment, {
    kind :: integer(),
    key :: binary(),
    mapper :: fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    children :: list(lustre@vdom@vnode:element(any())),
    keyed_children :: lustre@internals@mutable_map:mutable_map(binary(), lustre@vdom@vnode:element(any()))
}).
