-record(unsafe_inner_html, {
    kind :: integer(),
    key :: binary(),
    mapper :: fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    namespace :: binary(),
    tag :: binary(),
    attributes :: list(lustre@vdom@vattr:attribute(any())),
    inner_html :: binary()
}).
