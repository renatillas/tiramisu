-record(keyboard_state, {
    pressed_keys :: list(binary()),
    just_pressed_keys :: list(binary()),
    just_released_keys :: list(binary())
}).
