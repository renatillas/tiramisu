-record(transition, {
    from :: any(),
    to :: any(),
    condition :: tiramisu@state_machine:condition(any()),
    blend_duration :: float()
}).
