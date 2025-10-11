-record(touch_state, {
    touches :: list(tiramisu@input:touch()),
    touches_just_started :: list(tiramisu@input:touch()),
    touches_just_ended :: list(tiramisu@input:touch())
}).
