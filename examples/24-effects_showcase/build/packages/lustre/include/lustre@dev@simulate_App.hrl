-record(app, {
    init :: fun((any()) -> {any(), lustre@effect:effect(any())}),
    update :: fun((any(), any()) -> {any(), lustre@effect:effect(any())}),
    view :: fun((any()) -> lustre@vdom@vnode:element(any()))
}).
