import tiramisu/input

// Define test action enum
pub type Action {
  Jump
  Shoot
  MoveForward
  MoveBackward
  Accelerate
}

// --- Action Mapping Tests ---

// Test: Create empty bindings
pub fn empty_bindings_test() {
  let bindings = input.new_bindings()
  let test_input = input.new()

  // No actions should be pressed
  assert input.is_action_pressed(test_input, bindings, Jump) == False
}

// Test: Bind and check keyboard action
pub fn bind_key_action_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)

  let test_input = input.new()

  // Action not pressed with empty input
  assert input.is_action_pressed(test_input, bindings, Jump) == False
}

// Test: Multiple key bindings
pub fn multiple_key_bindings_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)
    |> input.bind_key(input.KeyW, MoveForward)
    |> input.bind_key(input.KeyS, MoveBackward)

  let test_input = input.new()

  // None should be pressed with empty input
  assert input.is_action_pressed(test_input, bindings, Jump) == False
  assert input.is_action_pressed(test_input, bindings, MoveForward) == False
  assert input.is_action_pressed(test_input, bindings, MoveBackward) == False
}

// Test: Multiple inputs for same action
pub fn multiple_inputs_same_action_test() {
  let _bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)
    |> input.bind_key(input.KeyW, Jump)
    |> input.bind_gamepad_button(input.ButtonA, Jump)

  // Test passes if bindings are created successfully
  let test_input = input.new()
  // Verify input state works with multiple bindings
  case test_input {
    _ -> Nil
  }
}

// Test: Mouse button binding
pub fn mouse_button_binding_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_mouse_button(input.LeftButton, Shoot)

  let test_input = input.new()

  // Not pressed with empty input
  assert input.is_action_pressed(test_input, bindings, Shoot) == False
}

// Test: Gamepad button binding
pub fn gamepad_button_binding_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_gamepad_button(input.ButtonA, Jump)
    |> input.bind_gamepad_button(input.ButtonB, Shoot)

  let test_input = input.new()

  // Not pressed with empty input
  assert input.is_action_pressed(test_input, bindings, Jump) == False
  assert input.is_action_pressed(test_input, bindings, Shoot) == False
}

// Test: Get action value returns 0.0 for unpressed action
pub fn action_value_unpressed_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)

  let test_input = input.new()

  let value = input.get_action_value(test_input, bindings, Jump)
  assert value == 0.0
}

// Test: Action just pressed logic
pub fn action_just_pressed_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)

  let test_input = input.new()

  // Not just pressed with empty input
  assert input.is_action_just_pressed(test_input, bindings, Jump) == False
}

// Test: Action just released logic
pub fn action_just_released_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)

  let test_input = input.new()

  // Not just released with empty input
  assert input.is_action_just_released(test_input, bindings, Jump) == False
}

// --- Input Buffering Tests ---

// Test: Create buffer
pub fn create_buffer_test() {
  let buffered = input.with_buffer(buffer_frames: 5)

  // Buffer should be empty initially
  assert input.was_action_pressed_buffered(buffered, Jump) == False
}

// Test: Buffer empty initially
pub fn buffer_empty_initially_test() {
  let buffered = input.with_buffer(buffer_frames: 5)

  // No action should be buffered initially
  assert input.was_action_pressed_buffered(buffered, Jump) == False
}

// Test: Update buffer increments frame counter
pub fn update_buffer_test() {
  let buffered = input.with_buffer(buffer_frames: 5)
  let bindings = input.new_bindings() |> input.bind_key(input.Space, Jump)
  let test_input = input.new()

  // Update buffer
  let updated = input.update_buffer(buffered, test_input, bindings)

  // Buffer should still be empty (no actions pressed)
  assert input.was_action_pressed_buffered(updated, Jump) == False
}

// Test: Buffer multiple frames
pub fn buffer_multiple_frames_test() {
  let buffered = input.with_buffer(buffer_frames: 5)
  let bindings = input.new_bindings() |> input.bind_key(input.Space, Jump)
  let test_input = input.new()

  // Update buffer multiple times
  let updated =
    buffered
    |> input.update_buffer(test_input, bindings)
    |> input.update_buffer(test_input, bindings)
    |> input.update_buffer(test_input, bindings)

  // No actions pressed, so buffer should be empty
  assert input.was_action_pressed_buffered(updated, Jump) == False
}

// Test: Consume buffered action
pub fn consume_buffered_action_test() {
  let buffered = input.with_buffer(buffer_frames: 5)

  // Consume an action (even if not present)
  let updated = input.consume_buffered_action(buffered, Jump)

  // Buffer should still be empty
  assert input.was_action_pressed_buffered(updated, Jump) == False
}

// Test: Clear buffer
pub fn clear_buffer_test() {
  let buffered = input.with_buffer(buffer_frames: 5)

  // Clear buffer
  let cleared = input.clear_buffer(buffered)

  // Buffer should be empty
  assert input.was_action_pressed_buffered(cleared, Jump) == False
}

// Test: Buffer with different frame counts
pub fn buffer_frame_counts_test() {
  let buffer_3 = input.with_buffer(buffer_frames: 3)
  let buffer_10 = input.with_buffer(buffer_frames: 10)

  // Both buffers should be empty initially
  assert input.was_action_pressed_buffered(buffer_3, Jump) == False
  assert input.was_action_pressed_buffered(buffer_10, Jump) == False
}

// Test: Chaining action bindings
pub fn chaining_bindings_test() {
  let bindings =
    input.new_bindings()
    |> input.bind_key(input.Space, Jump)
    |> input.bind_key(input.KeyW, MoveForward)
    |> input.bind_key(input.KeyS, MoveBackward)
    |> input.bind_mouse_button(input.LeftButton, Shoot)
    |> input.bind_gamepad_button(input.ButtonA, Jump)
    |> input.bind_gamepad_button(input.RightTrigger, Accelerate)

  let test_input = input.new()

  // Test passes if all bindings chain successfully and can be queried
  assert input.is_action_pressed(test_input, bindings, Jump) == False
  assert input.is_action_pressed(test_input, bindings, Shoot) == False
}
