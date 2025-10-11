-record(buffered_input, {
    buffer :: list(tiramisu@input:buffered_action(any())),
    buffer_frames :: integer(),
    frame_counter :: integer()
}).
