-record(event, {
    kind :: integer(),
    name :: binary(),
    handler :: gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(any())),
    include :: list(binary()),
    prevent_default :: lustre@vdom@vattr:event_behaviour(),
    stop_propagation :: lustre@vdom@vattr:event_behaviour(),
    immediate :: boolean(),
    debounce :: integer(),
    throttle :: integer()
}).
