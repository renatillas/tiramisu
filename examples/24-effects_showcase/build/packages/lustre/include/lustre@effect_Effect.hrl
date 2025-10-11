-record(effect, {
    synchronous :: list(fun((lustre@effect:actions(any())) -> nil)),
    before_paint :: list(fun((lustre@effect:actions(any())) -> nil)),
    after_paint :: list(fun((lustre@effect:actions(any())) -> nil))
}).
