import gleam/list
import gleam/result
import tiramisu/input/gamepad as gamepad_module
import tiramisu/input/keyboard as keyboard_module

/// Complete immutable snapshot of all input state for a frame
pub opaque type InputState {
  InputState(
    keyboard: KeyboardState,
    mouse: MouseState,
    gamepad: GamepadState,
    touch: TouchState,
  )
}

/// Keyboard state snapshot
pub opaque type KeyboardState {
  KeyboardState(
    pressed_keys: List(String),
    just_pressed_keys: List(String),
    just_released_keys: List(String),
  )
}

/// Mouse state snapshot
pub opaque type MouseState {
  MouseState(
    x: Float,
    y: Float,
    delta_x: Float,
    delta_y: Float,
    wheel_delta: Float,
    left_button: ButtonState,
    middle_button: ButtonState,
    right_button: ButtonState,
  )
}

/// Button state (pressed, just pressed, just released)
pub type ButtonState {
  ButtonState(pressed: Bool, just_pressed: Bool, just_released: Bool)
}

/// Gamepad state snapshot
pub type GamepadState {
  GamepadState(
    connected: Bool,
    buttons: List(Float),
    // Button values 0.0-1.0
    axes: List(Float),
  )
}

/// Touch state snapshot
pub type TouchState {
  TouchState(
    touches: List(Touch),
    touches_just_started: List(Touch),
    touches_just_ended: List(Touch),
  )
}

/// Touch point
pub type Touch {
  Touch(id: Int, x: Float, y: Float)
}

/// Create an empty input state (for initialization)
pub fn empty() -> InputState {
  InputState(
    keyboard: KeyboardState(
      pressed_keys: [],
      just_pressed_keys: [],
      just_released_keys: [],
    ),
    mouse: MouseState(
      x: 0.0,
      y: 0.0,
      delta_x: 0.0,
      delta_y: 0.0,
      wheel_delta: 0.0,
      left_button: ButtonState(
        pressed: False,
        just_pressed: False,
        just_released: False,
      ),
      middle_button: ButtonState(
        pressed: False,
        just_pressed: False,
        just_released: False,
      ),
      right_button: ButtonState(
        pressed: False,
        just_pressed: False,
        just_released: False,
      ),
    ),
    gamepad: GamepadState(connected: False, buttons: [], axes: []),
    touch: TouchState(
      touches: [],
      touches_just_started: [],
      touches_just_ended: [],
    ),
  )
}

// --- Keyboard Helpers ---

/// Check if a key is currently pressed
pub fn is_key_pressed(input: InputState, key: keyboard_module.Key) -> Bool {
  let key_code = keyboard_module.key_to_code(key)
  list.contains(input.keyboard.pressed_keys, key_code)
}

/// Check if a key was just pressed this frame
pub fn is_key_just_pressed(input: InputState, key: keyboard_module.Key) -> Bool {
  let key_code = keyboard_module.key_to_code(key)
  list.contains(input.keyboard.just_pressed_keys, key_code)
}

/// Check if a key was just released this frame
pub fn is_key_just_released(input: InputState, key: keyboard_module.Key) -> Bool {
  let key_code = keyboard_module.key_to_code(key)
  list.contains(input.keyboard.just_released_keys, key_code)
}

// --- Mouse Helpers ---

/// Get mouse position
pub fn mouse_position(input: InputState) -> #(Float, Float) {
  #(input.mouse.x, input.mouse.y)
}

/// Get mouse delta
pub fn mouse_delta(input: InputState) -> #(Float, Float) {
  #(input.mouse.delta_x, input.mouse.delta_y)
}

/// Check if left mouse button is pressed
pub fn is_left_button_pressed(input: InputState) -> Bool {
  input.mouse.left_button.pressed
}

/// Check if left mouse button was just pressed
pub fn is_left_button_just_pressed(input: InputState) -> Bool {
  input.mouse.left_button.just_pressed
}

/// Check if right mouse button is pressed
pub fn is_right_button_pressed(input: InputState) -> Bool {
  input.mouse.right_button.pressed
}

/// Check if right mouse button was just pressed
pub fn is_right_button_just_pressed(input: InputState) -> Bool {
  input.mouse.right_button.just_pressed
}

/// Get mouse wheel delta
pub fn mouse_wheel_delta(input: InputState) -> Float {
  input.mouse.wheel_delta
}

// --- Gamepad Helpers ---

/// Check if primary gamepad is connected
pub fn is_gamepad_connected(input: InputState) -> Bool {
  input.gamepad.connected
}

/// Get gamepad button value
pub fn gamepad_button(
  input: InputState,
  button: gamepad_module.GamepadButton,
) -> Float {
  let index = gamepad_button_to_index(button)
  list_get(input.gamepad.buttons, index)
  |> result.unwrap(0.0)
}

/// Check if gamepad button is pressed
pub fn is_gamepad_button_pressed(
  input: InputState,
  button: gamepad_module.GamepadButton,
) -> Bool {
  gamepad_button(input, button) >. 0.5
}

/// Get gamepad axis value
pub fn gamepad_axis(
  input: InputState,
  axis: gamepad_module.GamepadAxis,
) -> Float {
  let index = gamepad_axis_to_index(axis)
  list_get(input.gamepad.axes, index)
  |> result.unwrap(0.0)
}

// --- Touch Helpers ---

/// Get current touches
pub fn touches(input: InputState) -> List(Touch) {
  input.touch.touches
}

/// Get touches that just started
pub fn touches_just_started(input: InputState) -> List(Touch) {
  input.touch.touches_just_started
}

/// Get touches that just ended
pub fn touches_just_ended(input: InputState) -> List(Touch) {
  input.touch.touches_just_ended
}

/// Get touch count
pub fn touch_count(input: InputState) -> Int {
  list.length(input.touch.touches)
}

fn list_get(list: List(a), index: Int) -> Result(a, Nil) {
  case list, index {
    [], _ -> Error(Nil)
    [x, ..], 0 -> Ok(x)
    [_, ..rest], n -> list_get(rest, n - 1)
  }
}

// Map gamepad enums to indices
fn gamepad_button_to_index(button: gamepad_module.GamepadButton) -> Int {
  case button {
    gamepad_module.ButtonA -> 0
    gamepad_module.ButtonB -> 1
    gamepad_module.ButtonX -> 2
    gamepad_module.ButtonY -> 3
    gamepad_module.LeftBumper -> 4
    gamepad_module.RightBumper -> 5
    gamepad_module.LeftTrigger -> 6
    gamepad_module.RightTrigger -> 7
    gamepad_module.Select -> 8
    gamepad_module.Start -> 9
    gamepad_module.LeftStick -> 10
    gamepad_module.RightStick -> 11
    gamepad_module.DPadUp -> 12
    gamepad_module.DPadDown -> 13
    gamepad_module.DPadLeft -> 14
    gamepad_module.DPadRight -> 15
    gamepad_module.Home -> 16
  }
}

fn gamepad_axis_to_index(axis: gamepad_module.GamepadAxis) -> Int {
  case axis {
    gamepad_module.LeftStickX -> 0
    gamepad_module.LeftStickY -> 1
    gamepad_module.RightStickX -> 2
    gamepad_module.RightStickY -> 3
  }
}
