-record(state, {
    self :: gleam@erlang@process:subject(lustre@runtime@server@runtime:message(any())),
    selector :: gleam@erlang@process:selector(lustre@runtime@server@runtime:message(any())),
    base_selector :: gleam@erlang@process:selector(lustre@runtime@server@runtime:message(any())),
    model :: any(),
    update :: fun((any(), any()) -> {any(), lustre@effect:effect(any())}),
    view :: fun((any()) -> lustre@vdom@vnode:element(any())),
    config :: lustre@runtime@server@runtime:config(any()),
    vdom :: lustre@vdom@vnode:element(any()),
    events :: lustre@vdom@events:events(any()),
    providers :: gleam@dict:dict(binary(), gleam@json:json()),
    subscribers :: gleam@dict:dict(gleam@erlang@process:subject(lustre@runtime@transport:client_message(any())), gleam@erlang@process:monitor()),
    callbacks :: gleam@set:set(fun((lustre@runtime@transport:client_message(any())) -> nil))
}).
