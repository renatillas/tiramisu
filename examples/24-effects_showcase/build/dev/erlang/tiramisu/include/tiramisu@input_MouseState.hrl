-record(mouse_state, {
    x :: float(),
    y :: float(),
    delta_x :: float(),
    delta_y :: float(),
    wheel_delta :: float(),
    left_button :: tiramisu@input:button_state(),
    middle_button :: tiramisu@input:button_state(),
    right_button :: tiramisu@input:button_state()
}).
