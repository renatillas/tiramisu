import tiramisu/input
import vec/vec2

// --- Input State Creation Tests ---

pub fn new_input_state_test() {
  let state = input.new()
  // Test that new state has no keys pressed
  assert input.is_key_pressed(state, input.KeyW) == False
  assert input.is_left_button_pressed(state) == False
  assert input.touch_count(state) == 0
}

// --- Mouse Position Tests ---

pub fn mouse_position_test() {
  let state = input.new()
  let assert vec2.Vec2(0.0, 0.0) = input.mouse_position(state)
}

pub fn mouse_delta_test() {
  let state = input.new()
  let assert vec2.Vec2(0.0, 0.0) = input.mouse_delta(state)
}

pub fn mouse_wheel_delta_test() {
  let state = input.new()
  assert input.mouse_wheel_delta(state) == 0.0
}

// --- Gamepad Tests ---

pub fn gamepad_not_connected_test() {
  let state = input.new()
  assert input.is_gamepad_connected(state, 0) == False
  assert input.is_primary_connected(state) == False
}

pub fn gamepad_button_disconnected_test() {
  let state = input.new()
  assert input.gamepad_button(state, 0, input.ButtonA) == 0.0
  assert input.is_gamepad_button_pressed(state, 0, input.ButtonA) == False
}

pub fn gamepad_axis_disconnected_test() {
  let state = input.new()
  assert input.gamepad_axis(state, 0, input.LeftStickX) == 0.0
}

pub fn primary_gamepad_helpers_test() {
  let state = input.new()
  assert input.get_primary_button(state, input.ButtonA) == 0.0
  assert input.get_primary_axis(state, input.LeftStickX) == 0.0
  assert input.is_primary_gamepad_button_pressed(state, input.ButtonA) == False
}

pub fn gamepad_deadzone_test() {
  let state = input.new()
  let value = input.get_axis_with_deadzone(state, 0, input.LeftStickX, 0.1)
  assert value == 0.0
}

pub fn left_stick_not_active_test() {
  let state = input.new()
  assert input.is_left_stick_active(state, 0, 0.1) == False
}

pub fn right_stick_not_active_test() {
  let state = input.new()
  assert input.is_right_stick_active(state, 0, 0.1) == False
}

// --- Touch Tests ---

pub fn touches_empty_test() {
  let state = input.new()
  assert input.touches(state) == []
  assert input.touches_just_started(state) == []
  assert input.touches_just_ended(state) == []
}

// --- Action Mapping Tests ---

type TestAction {
  Jump
  Shoot
  MoveForward
}

pub fn new_bindings_test() {
  let bindings = input.new_bindings()
  let state = input.new()
  // No bindings, so no actions should be pressed
  assert input.is_action_pressed(state, bindings, Jump) == False
}

pub fn bind_key_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)

  let state = input.new()
  // Key not pressed, action not active
  assert input.is_action_pressed(state, bindings, Jump) == False
}

pub fn bind_mouse_button_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_mouse_button(input.LeftButton, Shoot)

  let state = input.new()
  assert input.is_action_pressed(state, bindings, Shoot) == False
}

pub fn bind_gamepad_button_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_gamepad_button(input.ButtonA, Jump)

  let state = input.new()
  assert input.is_action_pressed(state, bindings, Jump) == False
}

pub fn multiple_bindings_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)
    |> input.bind_key(input.KeyW, MoveForward)
    |> input.bind_mouse_button(input.LeftButton, Shoot)

  let state = input.new()
  assert input.is_action_pressed(state, bindings, Jump) == False
  assert input.is_action_pressed(state, bindings, MoveForward) == False
  assert input.is_action_pressed(state, bindings, Shoot) == False
}

pub fn action_value_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)

  let state = input.new()
  assert input.get_action_value(state, bindings, Jump) == 0.0
}

pub fn action_just_pressed_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)

  let state = input.new()
  assert input.is_action_just_pressed(state, bindings, Jump) == False
}

pub fn action_just_released_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)

  let state = input.new()
  assert input.is_action_just_released(state, bindings, Jump) == False
}

// --- Input Buffering Tests ---

pub fn buffered_input_creation_test() {
  let buffered = input.with_buffer(buffer_frames: 5)
  let _bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)

  assert input.was_action_pressed_buffered(buffered, Jump) == False
}

pub fn buffered_input_clear_test() {
  let buffered = input.with_buffer(buffer_frames: 5)
  let cleared = input.clear_buffer(buffered)
  assert input.was_action_pressed_buffered(cleared, Jump) == False
}

pub fn buffered_input_consume_test() {
  let buffered = input.with_buffer(buffer_frames: 5)
  let consumed = input.consume_buffered_action(buffered, Jump)
  assert input.was_action_pressed_buffered(consumed, Jump) == False
}

pub fn buffered_input_update_test() {
  let buffered = input.with_buffer(buffer_frames: 5)
  let state = input.new()
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)

  let updated = input.update_buffer(buffered, state, bindings)
  assert input.was_action_pressed_buffered(updated, Jump) == False
}
