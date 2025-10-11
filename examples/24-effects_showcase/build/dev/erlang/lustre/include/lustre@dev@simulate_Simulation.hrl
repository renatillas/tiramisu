-record(simulation, {
    update :: fun((any(), any()) -> {any(), lustre@effect:effect(any())}),
    view :: fun((any()) -> lustre@vdom@vnode:element(any())),
    history :: list(lustre@dev@simulate:event(any())),
    model :: any(),
    html :: lustre@vdom@vnode:element(any())
}).
