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
    HomeButton -> 16
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
  // Letters A-Z
  KeyA
  KeyB
  KeyC
  KeyD
  KeyE
  KeyF
  KeyG
  KeyH
  KeyI
  KeyJ
  KeyK
  KeyL
  KeyM
  KeyN
  KeyO
  KeyP
  KeyQ
  KeyR
  KeyS
  KeyT
  KeyU
  KeyV
  KeyW
  KeyX
  KeyY
  KeyZ
  // Numbers 0-9
  Digit0
  Digit1
  Digit2
  Digit3
  Digit4
  Digit5
  Digit6
  Digit7
  Digit8
  Digit9
  // Function keys F1-F12
  F1
  F2
  F3
  F4
  F5
  F6
  F7
  F8
  F9
  F10
  F11
  F12
  // Arrow keys
  ArrowUp
  ArrowDown
  ArrowLeft
  ArrowRight
  // Modifier keys
  ShiftLeft
  ShiftRight
  ControlLeft
  ControlRight
  AltLeft
  AltRight
  MetaLeft
  MetaRight
  // Special keys
  Space
  Enter
  Escape
  Tab
  Backspace
  Delete
  Insert
  Home
  End
  PageUp
  PageDown
  CapsLock
  // Symbols
  Minus
  Equal
  BracketLeft
  BracketRight
  Backslash
  Semicolon
  Quote
  Comma
  Period
  Slash
  Backquote
  // Numpad
  Numpad0
  Numpad1
  Numpad2
  Numpad3
  Numpad4
  Numpad5
  Numpad6
  Numpad7
  Numpad8
  Numpad9
  NumpadAdd
  NumpadSubtract
  NumpadMultiply
  NumpadDivide
  NumpadDecimal
  NumpadEnter
  NumLock
  // Media keys
  AudioVolumeUp
  AudioVolumeDown
  AudioVolumeMute
  MediaPlayPause
  MediaStop
  MediaTrackNext
  MediaTrackPrevious
  // Other
  PrintScreen
  ScrollLock
  Pause
  ContextMenu
  // For any key not explicitly defined
  Custom(String)
}

/// Convert Key to JavaScript KeyboardEvent.code string
fn key_to_code(key: Key) -> String {
  case key {
    // Letters
    KeyA -> "KeyA"
    KeyB -> "KeyB"
    KeyC -> "KeyC"
    KeyD -> "KeyD"
    KeyE -> "KeyE"
    KeyF -> "KeyF"
    KeyG -> "KeyG"
    KeyH -> "KeyH"
    KeyI -> "KeyI"
    KeyJ -> "KeyJ"
    KeyK -> "KeyK"
    KeyL -> "KeyL"
    KeyM -> "KeyM"
    KeyN -> "KeyN"
    KeyO -> "KeyO"
    KeyP -> "KeyP"
    KeyQ -> "KeyQ"
    KeyR -> "KeyR"
    KeyS -> "KeyS"
    KeyT -> "KeyT"
    KeyU -> "KeyU"
    KeyV -> "KeyV"
    KeyW -> "KeyW"
    KeyX -> "KeyX"
    KeyY -> "KeyY"
    KeyZ -> "KeyZ"
    // Numbers
    Digit0 -> "Digit0"
    Digit1 -> "Digit1"
    Digit2 -> "Digit2"
    Digit3 -> "Digit3"
    Digit4 -> "Digit4"
    Digit5 -> "Digit5"
    Digit6 -> "Digit6"
    Digit7 -> "Digit7"
    Digit8 -> "Digit8"
    Digit9 -> "Digit9"
    // Function keys
    F1 -> "F1"
    F2 -> "F2"
    F3 -> "F3"
    F4 -> "F4"
    F5 -> "F5"
    F6 -> "F6"
    F7 -> "F7"
    F8 -> "F8"
    F9 -> "F9"
    F10 -> "F10"
    F11 -> "F11"
    F12 -> "F12"
    // Arrow keys
    ArrowUp -> "ArrowUp"
    ArrowDown -> "ArrowDown"
    ArrowLeft -> "ArrowLeft"
    ArrowRight -> "ArrowRight"
    // Modifier keys
    ShiftLeft -> "ShiftLeft"
    ShiftRight -> "ShiftRight"
    ControlLeft -> "ControlLeft"
    ControlRight -> "ControlRight"
    AltLeft -> "AltLeft"
    AltRight -> "AltRight"
    MetaLeft -> "MetaLeft"
    MetaRight -> "MetaRight"
    // Special keys
    Space -> "Space"
    Enter -> "Enter"
    Escape -> "Escape"
    Tab -> "Tab"
    Backspace -> "Backspace"
    Delete -> "Delete"
    Insert -> "Insert"
    Home -> "Home"
    End -> "End"
    PageUp -> "PageUp"
    PageDown -> "PageDown"
    CapsLock -> "CapsLock"
    // Symbols
    Minus -> "Minus"
    Equal -> "Equal"
    BracketLeft -> "BracketLeft"
    BracketRight -> "BracketRight"
    Backslash -> "Backslash"
    Semicolon -> "Semicolon"
    Quote -> "Quote"
    Comma -> "Comma"
    Period -> "Period"
    Slash -> "Slash"
    Backquote -> "Backquote"
    // Numpad
    Numpad0 -> "Numpad0"
    Numpad1 -> "Numpad1"
    Numpad2 -> "Numpad2"
    Numpad3 -> "Numpad3"
    Numpad4 -> "Numpad4"
    Numpad5 -> "Numpad5"
    Numpad6 -> "Numpad6"
    Numpad7 -> "Numpad7"
    Numpad8 -> "Numpad8"
    Numpad9 -> "Numpad9"
    NumpadAdd -> "NumpadAdd"
    NumpadSubtract -> "NumpadSubtract"
    NumpadMultiply -> "NumpadMultiply"
    NumpadDivide -> "NumpadDivide"
    NumpadDecimal -> "NumpadDecimal"
    NumpadEnter -> "NumpadEnter"
    NumLock -> "NumLock"
    // Media keys
    AudioVolumeUp -> "AudioVolumeUp"
    AudioVolumeDown -> "AudioVolumeDown"
    AudioVolumeMute -> "AudioVolumeMute"
    MediaPlayPause -> "MediaPlayPause"
    MediaStop -> "MediaStop"
    MediaTrackNext -> "MediaTrackNext"
    MediaTrackPrevious -> "MediaTrackPrevious"
    // Other
    PrintScreen -> "PrintScreen"
    ScrollLock -> "ScrollLock"
    Pause -> "Pause"
    ContextMenu -> "ContextMenu"
    // Custom
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
  HomeButton
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
