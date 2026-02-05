//// Input state types and query functions for game loops.
////
//// This module provides types and functions to query input state from within
//// tick handlers. Input state is automatically captured and provided via the
//// `TickContext` on each animation frame.
////
//// ## Usage
////
//// ```gleam
//// import tiramisu/tick.{type TickContext}
//// import tiramisu/input
////
//// fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
////   case msg {
////     Tick(ctx) -> {
////       // Check if W key is currently held down
////       let moving_forward = input.is_key_pressed(ctx.input, input.KeyW)
////
////       // Check if Space was just pressed this frame
////       let jumped = input.is_key_just_pressed(ctx.input, input.KeySpace)
////
////       // Get mouse movement delta (for camera control)
////       let #(dx, dy) = input.mouse_delta(ctx.input)
////
////       // Check gamepad input
////       let axis_x = input.gamepad_axis(ctx.input, 0, input.LeftStickX)
////
////       // ...
////     }
////   }
//// }
//// ```

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/set.{type Set}

// TYPES -----------------------------------------------------------------------

/// Complete input state for a single frame.
pub type InputState {
  InputState(
    /// Currently pressed keys
    keyboard: KeyboardState,
    /// Mouse position and button state
    mouse: MouseState,
    /// Connected gamepad states (up to 4)
    gamepads: Dict(Int, GamepadState),
    /// Active touch points
    touches: TouchState,
    /// Whether pointer lock is active
    pointer_locked: Bool,
  )
}

/// Keyboard input state.
pub type KeyboardState {
  KeyboardState(
    /// Keys currently being held down
    pressed: Set(Key),
    /// Keys that were pressed this frame (not held last frame)
    just_pressed: Set(Key),
    /// Keys that were released this frame (held last frame, not now)
    just_released: Set(Key),
  )
}

/// Mouse input state.
pub type MouseState {
  MouseState(
    /// X position relative to canvas (pixels)
    x: Float,
    /// Y position relative to canvas (pixels)
    y: Float,
    /// Movement delta since last frame (useful for pointer lock)
    delta_x: Float,
    /// Movement delta since last frame
    delta_y: Float,
    /// Currently pressed mouse buttons
    buttons: Set(MouseButton),
    /// Buttons just pressed this frame
    just_pressed: Set(MouseButton),
    /// Buttons just released this frame
    just_released: Set(MouseButton),
    /// Scroll wheel delta (positive = scroll up)
    wheel_delta: Float,
  )
}

/// Gamepad input state (Xbox controller layout).
pub type GamepadState {
  GamepadState(
    /// Gamepad index (0-3)
    index: Int,
    /// Whether the gamepad is connected
    connected: Bool,
    /// Currently pressed buttons
    buttons: Set(GamepadButton),
    /// Buttons just pressed this frame
    just_pressed: Set(GamepadButton),
    /// Buttons just released this frame
    just_released: Set(GamepadButton),
    /// Axis values (-1.0 to 1.0)
    axes: Dict(GamepadAxis, Float),
  )
}

/// Touch input state for mobile/tablet.
pub type TouchState {
  TouchState(
    /// Currently active touch points
    touches: List(TouchPoint),
    /// Touch points that started this frame
    just_started: List(TouchPoint),
    /// Touch points that ended this frame
    just_ended: List(TouchPoint),
  )
}

/// A single touch point.
pub type TouchPoint {
  TouchPoint(
    /// Unique identifier for this touch
    id: Int,
    /// X position relative to canvas
    x: Float,
    /// Y position relative to canvas
    y: Float,
    /// Force of the touch (0.0 to 1.0, if supported)
    force: Float,
  )
}

// KEYBOARD KEYS ---------------------------------------------------------------

/// Keyboard key codes.
/// Uses the standard KeyboardEvent.code values.
pub type Key {
  // Letters
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
  // Numbers
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
  // Arrow keys
  ArrowUp
  ArrowDown
  ArrowLeft
  ArrowRight
  // Modifiers
  ShiftLeft
  ShiftRight
  ControlLeft
  ControlRight
  AltLeft
  AltRight
  MetaLeft
  MetaRight
  // Common keys
  Space
  Enter
  Escape
  Tab
  Backspace
  Delete
  // Function keys
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
  // Other
  Other(String)
}

// MOUSE BUTTONS ---------------------------------------------------------------

/// Mouse button identifiers.
pub type MouseButton {
  /// Left mouse button (button 0)
  LeftButton
  /// Middle mouse button / wheel click (button 1)
  MiddleButton
  /// Right mouse button (button 2)
  RightButton
  /// Back button (button 3)
  BackButton
  /// Forward button (button 4)
  ForwardButton
}

// GAMEPAD ---------------------------------------------------------------------

/// Gamepad button identifiers (Xbox layout).
pub type GamepadButton {
  /// A / Cross
  ButtonA
  /// B / Circle
  ButtonB
  /// X / Square
  ButtonX
  /// Y / Triangle
  ButtonY
  /// Left bumper
  LeftBumper
  /// Right bumper
  RightBumper
  /// Left trigger (digital)
  LeftTrigger
  /// Right trigger (digital)
  RightTrigger
  /// Back / Select
  ButtonBack
  /// Start
  ButtonStart
  /// Left stick press
  LeftStickButton
  /// Right stick press
  RightStickButton
  /// D-pad up
  DpadUp
  /// D-pad down
  DpadDown
  /// D-pad left
  DpadLeft
  /// D-pad right
  DpadRight
  /// Home / Guide button
  ButtonHome
}

/// Gamepad axis identifiers.
pub type GamepadAxis {
  /// Left stick horizontal (-1 = left, 1 = right)
  LeftStickX
  /// Left stick vertical (-1 = up, 1 = down)
  LeftStickY
  /// Right stick horizontal (-1 = left, 1 = right)
  RightStickX
  /// Right stick vertical (-1 = up, 1 = down)
  RightStickY
  /// Left trigger (0 = released, 1 = fully pressed)
  LeftTriggerAxis
  /// Right trigger (0 = released, 1 = fully pressed)
  RightTriggerAxis
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Create an empty input state.
/// Used internally for initialization.
pub fn empty() -> InputState {
  InputState(
    keyboard: KeyboardState(
      pressed: set.new(),
      just_pressed: set.new(),
      just_released: set.new(),
    ),
    mouse: MouseState(
      x: 0.0,
      y: 0.0,
      delta_x: 0.0,
      delta_y: 0.0,
      buttons: set.new(),
      just_pressed: set.new(),
      just_released: set.new(),
      wheel_delta: 0.0,
    ),
    gamepads: dict.new(),
    touches: TouchState(touches: [], just_started: [], just_ended: []),
    pointer_locked: False,
  )
}

// KEYBOARD QUERIES ------------------------------------------------------------

/// Check if a key is currently being held down.
pub fn is_key_pressed(input: InputState, key: Key) -> Bool {
  set.contains(input.keyboard.pressed, key)
}

/// Check if a key was just pressed this frame (not held last frame).
pub fn is_key_just_pressed(input: InputState, key: Key) -> Bool {
  set.contains(input.keyboard.just_pressed, key)
}

/// Check if a key was just released this frame.
pub fn is_key_just_released(input: InputState, key: Key) -> Bool {
  set.contains(input.keyboard.just_released, key)
}

/// Check if any of the given keys is currently pressed.
pub fn is_any_key_pressed(input: InputState, keys: List(Key)) -> Bool {
  list.any(keys, fn(key) { is_key_pressed(input, key) })
}

/// Get a list of all currently pressed keys.
pub fn pressed_keys(input: InputState) -> List(Key) {
  set.to_list(input.keyboard.pressed)
}

// MOUSE QUERIES ---------------------------------------------------------------

/// Get the current mouse position relative to the canvas.
pub fn mouse_position(input: InputState) -> #(Float, Float) {
  #(input.mouse.x, input.mouse.y)
}

/// Get the mouse movement delta since the last frame.
/// Particularly useful when pointer lock is active.
pub fn mouse_delta(input: InputState) -> #(Float, Float) {
  #(input.mouse.delta_x, input.mouse.delta_y)
}

/// Check if a mouse button is currently pressed.
pub fn is_mouse_button_pressed(input: InputState, button: MouseButton) -> Bool {
  set.contains(input.mouse.buttons, button)
}

/// Check if a mouse button was just pressed this frame.
pub fn is_mouse_button_just_pressed(
  input: InputState,
  button: MouseButton,
) -> Bool {
  set.contains(input.mouse.just_pressed, button)
}

/// Check if a mouse button was just released this frame.
pub fn is_mouse_button_just_released(
  input: InputState,
  button: MouseButton,
) -> Bool {
  set.contains(input.mouse.just_released, button)
}

/// Get the scroll wheel delta (positive = scroll up/forward).
pub fn wheel_delta(input: InputState) -> Float {
  input.mouse.wheel_delta
}

/// Check if pointer lock is currently active.
pub fn is_pointer_locked(input: InputState) -> Bool {
  input.pointer_locked
}

// GAMEPAD QUERIES -------------------------------------------------------------

/// Check if a gamepad is connected at the given index (0-3).
pub fn is_gamepad_connected(input: InputState, index: Int) -> Bool {
  case dict.get(input.gamepads, index) {
    Ok(gamepad) -> gamepad.connected
    Error(_) -> False
  }
}

/// Get the state of a gamepad axis (-1.0 to 1.0).
/// Returns 0.0 if gamepad not connected or axis not available.
pub fn gamepad_axis(input: InputState, index: Int, axis: GamepadAxis) -> Float {
  case dict.get(input.gamepads, index) {
    Ok(gamepad) ->
      case dict.get(gamepad.axes, axis) {
        Ok(value) -> value
        Error(_) -> 0.0
      }
    Error(_) -> 0.0
  }
}

/// Check if a gamepad button is currently pressed.
pub fn is_gamepad_button_pressed(
  input: InputState,
  index: Int,
  button: GamepadButton,
) -> Bool {
  case dict.get(input.gamepads, index) {
    Ok(gamepad) -> set.contains(gamepad.buttons, button)
    Error(_) -> False
  }
}

/// Check if a gamepad button was just pressed this frame.
pub fn is_gamepad_button_just_pressed(
  input: InputState,
  index: Int,
  button: GamepadButton,
) -> Bool {
  case dict.get(input.gamepads, index) {
    Ok(gamepad) -> set.contains(gamepad.just_pressed, button)
    Error(_) -> False
  }
}

/// Check if a gamepad button was just released this frame.
pub fn is_gamepad_button_just_released(
  input: InputState,
  index: Int,
  button: GamepadButton,
) -> Bool {
  case dict.get(input.gamepads, index) {
    Ok(gamepad) -> set.contains(gamepad.just_released, button)
    Error(_) -> False
  }
}

/// Get all connected gamepad indices.
pub fn connected_gamepads(input: InputState) -> List(Int) {
  input.gamepads
  |> dict.filter(fn(_index, gamepad) { gamepad.connected })
  |> dict.keys
}

// TOUCH QUERIES ---------------------------------------------------------------

/// Get all currently active touch points.
pub fn active_touches(input: InputState) -> List(TouchPoint) {
  input.touches.touches
}

/// Get touch points that started this frame.
pub fn touches_just_started(input: InputState) -> List(TouchPoint) {
  input.touches.just_started
}

/// Get touch points that ended this frame.
pub fn touches_just_ended(input: InputState) -> List(TouchPoint) {
  input.touches.just_ended
}

/// Get the number of active touch points.
pub fn touch_count(input: InputState) -> Int {
  list.length(input.touches.touches)
}

/// Get a specific touch point by ID.
pub fn get_touch(input: InputState, id: Int) -> Option(TouchPoint) {
  list.find(input.touches.touches, fn(t) { t.id == id })
  |> option.from_result
}

// KEY CODE CONVERSION ---------------------------------------------------------

/// Convert a JavaScript key code string to a Key type.
@internal
pub fn key_from_code(code: String) -> Key {
  case code {
    // Letters
    "KeyA" -> KeyA
    "KeyB" -> KeyB
    "KeyC" -> KeyC
    "KeyD" -> KeyD
    "KeyE" -> KeyE
    "KeyF" -> KeyF
    "KeyG" -> KeyG
    "KeyH" -> KeyH
    "KeyI" -> KeyI
    "KeyJ" -> KeyJ
    "KeyK" -> KeyK
    "KeyL" -> KeyL
    "KeyM" -> KeyM
    "KeyN" -> KeyN
    "KeyO" -> KeyO
    "KeyP" -> KeyP
    "KeyQ" -> KeyQ
    "KeyR" -> KeyR
    "KeyS" -> KeyS
    "KeyT" -> KeyT
    "KeyU" -> KeyU
    "KeyV" -> KeyV
    "KeyW" -> KeyW
    "KeyX" -> KeyX
    "KeyY" -> KeyY
    "KeyZ" -> KeyZ
    // Numbers
    "Digit0" -> Digit0
    "Digit1" -> Digit1
    "Digit2" -> Digit2
    "Digit3" -> Digit3
    "Digit4" -> Digit4
    "Digit5" -> Digit5
    "Digit6" -> Digit6
    "Digit7" -> Digit7
    "Digit8" -> Digit8
    "Digit9" -> Digit9
    // Arrow keys
    "ArrowUp" -> ArrowUp
    "ArrowDown" -> ArrowDown
    "ArrowLeft" -> ArrowLeft
    "ArrowRight" -> ArrowRight
    // Modifiers
    "ShiftLeft" -> ShiftLeft
    "ShiftRight" -> ShiftRight
    "ControlLeft" -> ControlLeft
    "ControlRight" -> ControlRight
    "AltLeft" -> AltLeft
    "AltRight" -> AltRight
    "MetaLeft" -> MetaLeft
    "MetaRight" -> MetaRight
    // Common keys
    "Space" -> Space
    "Enter" -> Enter
    "Escape" -> Escape
    "Tab" -> Tab
    "Backspace" -> Backspace
    "Delete" -> Delete
    // Function keys
    "F1" -> F1
    "F2" -> F2
    "F3" -> F3
    "F4" -> F4
    "F5" -> F5
    "F6" -> F6
    "F7" -> F7
    "F8" -> F8
    "F9" -> F9
    "F10" -> F10
    "F11" -> F11
    "F12" -> F12
    // Unknown
    other -> Other(other)
  }
}
