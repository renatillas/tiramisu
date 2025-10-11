-record(insert, {
    kind :: integer(),
    children :: list(lustre@vdom@vnode:element(any())),
    before :: integer()
}).
