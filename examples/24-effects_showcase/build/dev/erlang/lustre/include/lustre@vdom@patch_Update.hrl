-record(update, {
    kind :: integer(),
    added :: list(lustre@vdom@vattr:attribute(any())),
    removed :: list(lustre@vdom@vattr:attribute(any()))
}).
