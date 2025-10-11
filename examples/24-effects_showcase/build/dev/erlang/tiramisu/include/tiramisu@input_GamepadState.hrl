-record(gamepad_state, {
    connected :: boolean(),
    buttons :: list(float()),
    axes :: list(float())
}).
