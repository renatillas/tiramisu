-record(input_bindings, {
    key_to_action :: list({tiramisu@input:key(), any()}),
    mouse_to_action :: list({tiramisu@input:mouse_button(), any()}),
    gamepad_to_action :: list({tiramisu@input:gamepad_button(), any()})
}).
