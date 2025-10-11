-record(input_state, {
    keyboard :: tiramisu@input:keyboard_state(),
    mouse :: tiramisu@input:mouse_state(),
    gamepad :: list(tiramisu@input:gamepad_state()),
    touch :: tiramisu@input:touch_state()
}).
