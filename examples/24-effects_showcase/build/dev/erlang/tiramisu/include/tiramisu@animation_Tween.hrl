-record(tween, {
    start_value :: any(),
    end_value :: any(),
    duration :: float(),
    elapsed :: float(),
    easing :: tiramisu@animation:easing(),
    lerp_fn :: fun((any(), any(), float()) -> any())
}).
