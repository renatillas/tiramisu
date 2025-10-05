/// Gamepad button enumeration (standard mapping)
pub type GamepadButton {
  ButtonA
  // 0
  ButtonB
  // 1
  ButtonX
  // 2
  ButtonY
  // 3
  LeftBumper
  // 4
  RightBumper
  // 5
  LeftTrigger
  // 6
  RightTrigger
  // 7
  Select
  // 8
  Start
  // 9
  LeftStick
  // 10
  RightStick
  // 11
  DPadUp
  // 12
  DPadDown
  // 13
  DPadLeft
  // 14
  DPadRight
  // 15
  Home
  // 16
}

/// Gamepad axis enumeration
pub type GamepadAxis {
  LeftStickX
  // 0
  LeftStickY
  // 1
  RightStickX
  // 2
  RightStickY
  // 3
}

/// Convert button enum to index
fn button_to_index(button: GamepadButton) -> Int {
  case button {
    ButtonA -> 0
    ButtonB -> 1
    ButtonX -> 2
    ButtonY -> 3
    LeftBumper -> 4
    RightBumper -> 5
    LeftTrigger -> 6
    RightTrigger -> 7
    Select -> 8
    Start -> 9
    LeftStick -> 10
    RightStick -> 11
    DPadUp -> 12
    DPadDown -> 13
    DPadLeft -> 14
    DPadRight -> 15
    Home -> 16
  }
}

/// Convert axis enum to index
fn axis_to_index(axis: GamepadAxis) -> Int {
  case axis {
    LeftStickX -> 0
    LeftStickY -> 1
    RightStickX -> 2
    RightStickY -> 3
  }
}

/// Check if a gamepad is connected
@external(javascript, "../input/ffi/gamepad.mjs", "isConnected")
pub fn is_connected(gamepad_index: Int) -> Bool

/// Check if a specific button is pressed
@external(javascript, "../input/ffi/gamepad.mjs", "isButtonPressed")
fn ffi_is_button_pressed(gamepad_index: Int, button_index: Int) -> Bool

/// Get button pressure value (0.0 to 1.0)
@external(javascript, "../input/ffi/gamepad.mjs", "getButtonValue")
fn ffi_get_button_value(gamepad_index: Int, button_index: Int) -> Float

/// Get axis value (-1.0 to 1.0)
@external(javascript, "../input/ffi/gamepad.mjs", "getAxisValue")
fn ffi_get_axis_value(gamepad_index: Int, axis_index: Int) -> Float

/// Get number of connected gamepads
@external(javascript, "../input/ffi/gamepad.mjs", "getGamepadCount")
pub fn get_gamepad_count() -> Int

/// Check if a button is pressed on a gamepad
pub fn is_button_pressed(
  gamepad_index: Int,
  button: GamepadButton,
) -> Bool {
  ffi_is_button_pressed(gamepad_index, button_to_index(button))
}

/// Get button pressure value (useful for analog triggers)
pub fn get_button_value(gamepad_index: Int, button: GamepadButton) -> Float {
  ffi_get_button_value(gamepad_index, button_to_index(button))
}

/// Get axis value with optional dead zone
pub fn get_axis_value(gamepad_index: Int, axis: GamepadAxis) -> Float {
  ffi_get_axis_value(gamepad_index, axis_to_index(axis))
}

/// Get axis value with dead zone applied
pub fn get_axis_with_deadzone(
  gamepad_index: Int,
  axis: GamepadAxis,
  deadzone: Float,
) -> Float {
  let value = get_axis_value(gamepad_index, axis)
  case value >. deadzone || value <. 0.0 -. deadzone {
    True -> value
    False -> 0.0
  }
}

/// Check if left stick is moved in any direction
pub fn is_left_stick_active(gamepad_index: Int, threshold: Float) -> Bool {
  let x = get_axis_value(gamepad_index, LeftStickX)
  let y = get_axis_value(gamepad_index, LeftStickY)
  x >. threshold || x <. 0.0 -. threshold || y >. threshold || y <. 0.0
    -. threshold
}

/// Check if right stick is moved in any direction
pub fn is_right_stick_active(gamepad_index: Int, threshold: Float) -> Bool {
  let x = get_axis_value(gamepad_index, RightStickX)
  let y = get_axis_value(gamepad_index, RightStickY)
  x >. threshold || x <. 0.0 -. threshold || y >. threshold || y <. 0.0
    -. threshold
}

/// Convenience: Check if primary gamepad (index 0) is connected
pub fn is_primary_connected() -> Bool {
  is_connected(0)
}

/// Convenience: Check button on primary gamepad
pub fn is_pressed(button: GamepadButton) -> Bool {
  is_button_pressed(0, button)
}

/// Convenience: Get button value on primary gamepad
pub fn get_button(button: GamepadButton) -> Float {
  get_button_value(0, button)
}

/// Convenience: Get axis value on primary gamepad
pub fn get_axis(axis: GamepadAxis) -> Float {
  get_axis_value(0, axis)
}
