-record(patch, {
    index :: integer(),
    removed :: integer(),
    changes :: list(lustre@vdom@patch:change(any())),
    children :: list(lustre@vdom@patch:patch(any()))
}).
