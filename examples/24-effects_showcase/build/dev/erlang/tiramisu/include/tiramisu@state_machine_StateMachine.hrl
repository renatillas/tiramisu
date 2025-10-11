-record(state_machine, {
    states :: gleam@dict:dict(any(), tiramisu@state_machine:state(any())),
    transitions :: list(tiramisu@state_machine:transition(any(), any())),
    current :: tiramisu@state_machine:state_machine_state(any()),
    default_blend :: float()
}).
