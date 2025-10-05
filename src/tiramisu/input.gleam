import gleam/list
import gleam/result

pub opaque type InputState {
  InputState(
    keyboard: KeyboardState,
    mouse: MouseState,
    gamepad: List(GamepadState),
    touch: TouchState,
  )
}

pub opaque type KeyboardState {
  KeyboardState(
    pressed_keys: List(String),
    just_pressed_keys: List(String),
    just_released_keys: List(String),
  )
}

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

pub type ButtonState {
  ButtonState(pressed: Bool, just_pressed: Bool, just_released: Bool)
}

pub type GamepadState {
  GamepadState(
    connected: Bool,
    /// Buttons: Values clamped between 0.0 and 1.0
    buttons: List(Float),
    axes: List(Float),
  )
}

pub type TouchState {
  TouchState(
    touches: List(Touch),
    touches_just_started: List(Touch),
    touches_just_ended: List(Touch),
  )
}

pub type Touch {
  Touch(id: Int, x: Float, y: Float)
}

pub fn new() -> InputState {
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
    gamepad: [],
    touch: TouchState(
      touches: [],
      touches_just_started: [],
      touches_just_ended: [],
    ),
  )
}

// --- Keyboard Helpers ---

/// Check if a key is currently pressed
pub fn is_key_pressed(input: InputState, key: Key) -> Bool {
  let key_code = key_to_code(key)
  list.contains(input.keyboard.pressed_keys, key_code)
}

/// Check if a key was just pressed this frame
pub fn is_key_just_pressed(input: InputState, key: Key) -> Bool {
  let key_code = key_to_code(key)
  list.contains(input.keyboard.just_pressed_keys, key_code)
}

/// Check if a key was just released this frame
pub fn is_key_just_released(input: InputState, key: Key) -> Bool {
  let key_code = key_to_code(key)
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

/// Check if gamepad at index is connected
pub fn is_gamepad_connected(input: InputState, index: Int) -> Bool {
  case list_get(input.gamepad, index) {
    Ok(gamepad) -> gamepad.connected
    Error(_) -> False
  }
}

/// Get gamepad button value
pub fn gamepad_button(
  input: InputState,
  gamepad_index: Int,
  button: GamepadButton,
) -> Float {
  let button_index = gamepad_button_to_index(button)
  case list_get(input.gamepad, gamepad_index) {
    Ok(gamepad) ->
      list_get(gamepad.buttons, button_index)
      |> result.unwrap(0.0)
    Error(_) -> 0.0
  }
}

/// Check if gamepad button is pressed
pub fn is_gamepad_button_pressed(
  input: InputState,
  gamepad_index: Int,
  button: GamepadButton,
) -> Bool {
  gamepad_button(input, gamepad_index, button) >. 0.5
}

/// Get gamepad axis value
pub fn gamepad_axis(
  input: InputState,
  gamepad_index: Int,
  axis: GamepadAxis,
) -> Float {
  let axis_index = gamepad_axis_to_index(axis)
  case list_get(input.gamepad, gamepad_index) {
    Ok(gamepad) ->
      list_get(gamepad.axes, axis_index)
      |> result.unwrap(0.0)
    Error(_) -> 0.0
  }
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
fn gamepad_button_to_index(button: GamepadButton) -> Int {
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

fn gamepad_axis_to_index(axis: GamepadAxis) -> Int {
  case axis {
    LeftStickX -> 0
    LeftStickY -> 1
    RightStickX -> 2
    RightStickY -> 3
  }
}

/// Common keyboard codes (standard KeyboardEvent.code values)
pub type Key {
  KeyW
  KeyA
  KeyS
  KeyD
  ArrowUp
  ArrowDown
  ArrowLeft
  ArrowRight
  Space
  Enter
  Escape
  Shift
  Control
  Alt
  Custom(String)
}

/// Convert Key to JavaScript KeyboardEvent.code string
fn key_to_code(key: Key) -> String {
  case key {
    KeyW -> "KeyW"
    KeyA -> "KeyA"
    KeyS -> "KeyS"
    KeyD -> "KeyD"
    ArrowUp -> "ArrowUp"
    ArrowDown -> "ArrowDown"
    ArrowLeft -> "ArrowLeft"
    ArrowRight -> "ArrowRight"
    Space -> "Space"
    Enter -> "Enter"
    Escape -> "Escape"
    Shift -> "ShiftLeft"
    Control -> "ControlLeft"
    Alt -> "AltLeft"
    Custom(code) -> code
  }
}

/// Gamepad button enumeration (standard mapping)
pub type GamepadButton {
  ButtonA
  ButtonB
  ButtonX
  ButtonY
  LeftBumper
  RightBumper
  LeftTrigger
  RightTrigger
  Select
  Start
  LeftStick
  RightStick
  DPadUp
  DPadDown
  DPadLeft
  DPadRight
  Home
}

pub type GamepadAxis {
  LeftStickX
  LeftStickY
  RightStickX
  RightStickY
}


/// Get axis value with dead zone applied
pub fn get_axis_with_deadzone(
  input: InputState,
  gamepad_index: Int,
  axis: GamepadAxis,
  deadzone: Float,
) -> Float {
  let value = gamepad_axis(input, gamepad_index, axis)
  case value >. deadzone || value <. 0.0 -. deadzone {
    True -> value
    False -> 0.0
  }
}

/// Check if left stick is moved in any direction
pub fn is_left_stick_active(
  input: InputState,
  gamepad_index: Int,
  threshold: Float,
) -> Bool {
  let x = gamepad_axis(input, gamepad_index, LeftStickX)
  let y = gamepad_axis(input, gamepad_index, LeftStickY)
  x >. threshold
  || x <. 0.0 -. threshold
  || y >. threshold
  || y <. 0.0 -. threshold
}

/// Check if right stick is moved in any direction
pub fn is_right_stick_active(
  input: InputState,
  gamepad_index: Int,
  threshold: Float,
) -> Bool {
  let x = gamepad_axis(input, gamepad_index, RightStickX)
  let y = gamepad_axis(input, gamepad_index, RightStickY)
  x >. threshold
  || x <. 0.0 -. threshold
  || y >. threshold
  || y <. 0.0 -. threshold
}

/// Convenience: Check if primary gamepad (index 0) is connected
pub fn is_primary_connected(input: InputState) -> Bool {
  is_gamepad_connected(input, 0)
}

/// Convenience: Check button on primary gamepad
pub fn is_primary_gamepad_button_pressed(
  input: InputState,
  button: GamepadButton,
) -> Bool {
  is_gamepad_button_pressed(input, 0, button)
}

/// Convenience: Get button value on primary gamepad
pub fn get_primary_button(input: InputState, button: GamepadButton) -> Float {
  gamepad_button(input, 0, button)
}

/// Convenience: Get axis value on primary gamepad
pub fn get_primary_axis(input: InputState, axis: GamepadAxis) -> Float {
  gamepad_axis(input, 0, axis)
}
