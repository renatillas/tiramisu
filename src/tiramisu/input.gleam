import gleam/list
import gleam/result
import gleam/set
import vec/vec2.{type Vec2}

/// Input state for all input devices (automatically updated each frame).
///
/// Access via `context.input` in your `update` function.
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
    pressed_keys: set.Set(String),
    just_pressed_keys: set.Set(String),
    just_released_keys: set.Set(String),
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
  Touch(id: Int, position: Vec2(Float))
}

pub fn new() -> InputState {
  InputState(
    keyboard: KeyboardState(
      pressed_keys: set.new(),
      just_pressed_keys: set.new(),
      just_released_keys: set.new(),
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
  set.contains(input.keyboard.pressed_keys, key_code)
}

/// Check if a key was just pressed this frame
pub fn is_key_just_pressed(input: InputState, key: Key) -> Bool {
  let key_code = key_to_code(key)
  set.contains(input.keyboard.just_pressed_keys, key_code)
}

/// Check if a key was just released this frame
pub fn is_key_just_released(input: InputState, key: Key) -> Bool {
  let key_code = key_to_code(key)
  set.contains(input.keyboard.just_released_keys, key_code)
}

// --- Mouse Helpers ---

/// Get mouse position
pub fn mouse_position(input: InputState) -> vec2.Vec2(Float) {
  vec2.Vec2(input.mouse.x, input.mouse.y)
}

/// Get mouse delta
pub fn mouse_delta(input: InputState) -> vec2.Vec2(Float) {
  vec2.Vec2(input.mouse.delta_x, input.mouse.delta_y)
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

/// Check if there was any user interaction this frame (for audio context resume)
/// Returns True if any key was just pressed, mouse button clicked, or touch started
pub fn has_user_interaction(input: InputState) -> Bool {
  // Check for any key just pressed
  let has_key_press = !set.is_empty(input.keyboard.just_pressed_keys)

  // Check for mouse button just pressed
  let has_mouse_click =
    input.mouse.left_button.just_pressed
    || input.mouse.middle_button.just_pressed
    || input.mouse.right_button.just_pressed

  // Check for touch just started
  let has_touch_start = !list.is_empty(input.touch.touches_just_started)

  has_key_press || has_mouse_click || has_touch_start
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

// --- Action Mapping System ---

/// Mouse button enumeration for action mapping
pub type MouseButton {
  LeftButton
  RightButton
  MiddleButton
}

/// Input bindings that map inputs to user-defined actions
///
/// Generic over action type, so you can define your own action enum:
///
/// ```gleam
/// pub type Action {
///   Jump
///   MoveForward
///   Shoot
/// }
///
/// let bindings = input.new_bindings()
///   |> input.bind_key(input.Space, Jump)
///   |> input.bind_key(input.KeyW, MoveForward)
///   |> input.bind_mouse_button(input.LeftButton, Shoot)
/// ```
pub opaque type InputBindings(action) {
  InputBindings(
    key_to_action: List(#(Key, action)),
    mouse_to_action: List(#(MouseButton, action)),
    gamepad_to_action: List(#(GamepadButton, action)),
  )
}

/// Create a new empty input bindings configuration
pub fn new_bindings() -> InputBindings(action) {
  InputBindings(key_to_action: [], mouse_to_action: [], gamepad_to_action: [])
}

/// Bind a keyboard key to an action
///
/// ## Example
///
/// ```gleam
/// let bindings = input.new_bindings()
///   |> input.bind_key(input.Space, Jump)
///   |> input.bind_key(input.KeyW, MoveForward)
/// ```
pub fn bind_key(
  bindings: InputBindings(action),
  key: Key,
  action: action,
) -> InputBindings(action) {
  InputBindings(..bindings, key_to_action: [
    #(key, action),
    ..bindings.key_to_action
  ])
}

/// Bind a mouse button to an action
pub fn bind_mouse_button(
  bindings: InputBindings(action),
  button: MouseButton,
  action: action,
) -> InputBindings(action) {
  InputBindings(..bindings, mouse_to_action: [
    #(button, action),
    ..bindings.mouse_to_action
  ])
}

/// Bind a gamepad button to an action
pub fn bind_gamepad_button(
  bindings: InputBindings(action),
  button: GamepadButton,
  action: action,
) -> InputBindings(action) {
  InputBindings(..bindings, gamepad_to_action: [
    #(button, action),
    ..bindings.gamepad_to_action
  ])
}

/// Check if an action is currently pressed
///
/// Returns True if any input bound to this action is pressed.
///
/// ## Example
///
/// ```gleam
/// if input.is_action_pressed(ctx.input, bindings, Jump) {
///   // Player wants to jump
/// }
/// ```
pub fn is_action_pressed(
  input: InputState,
  bindings: InputBindings(action),
  action: action,
) -> Bool {
  // Check keyboard bindings
  let key_pressed =
    list.any(bindings.key_to_action, fn(binding) {
      let #(key, bound_action) = binding
      bound_action == action && is_key_pressed(input, key)
    })

  // Check mouse bindings
  let mouse_pressed =
    list.any(bindings.mouse_to_action, fn(binding) {
      let #(button, bound_action) = binding
      bound_action == action
      && case button {
        LeftButton -> is_left_button_pressed(input)
        RightButton -> is_right_button_pressed(input)
        MiddleButton -> input.mouse.middle_button.pressed
      }
    })

  // Check gamepad bindings (primary gamepad only for now)
  let gamepad_pressed =
    list.any(bindings.gamepad_to_action, fn(binding) {
      let #(button, bound_action) = binding
      bound_action == action && is_gamepad_button_pressed(input, 0, button)
    })

  key_pressed || mouse_pressed || gamepad_pressed
}

/// Check if an action was just pressed this frame
///
/// Returns True if any input bound to this action was just pressed.
pub fn is_action_just_pressed(
  input: InputState,
  bindings: InputBindings(action),
  action: action,
) -> Bool {
  // Check keyboard bindings
  let key_just_pressed =
    list.any(bindings.key_to_action, fn(binding) {
      let #(key, bound_action) = binding
      bound_action == action && is_key_just_pressed(input, key)
    })

  // Check mouse bindings
  let mouse_just_pressed =
    list.any(bindings.mouse_to_action, fn(binding) {
      let #(button, bound_action) = binding
      bound_action == action
      && case button {
        LeftButton -> is_left_button_just_pressed(input)
        RightButton -> is_right_button_just_pressed(input)
        MiddleButton -> input.mouse.middle_button.just_pressed
      }
    })

  // Check gamepad bindings
  // Note: Current gamepad API doesn't track just_pressed, so we use pressed with threshold
  let gamepad_just_pressed =
    list.any(bindings.gamepad_to_action, fn(binding) {
      let #(button, bound_action) = binding
      bound_action == action && is_gamepad_button_pressed(input, 0, button)
    })

  key_just_pressed || mouse_just_pressed || gamepad_just_pressed
}

/// Check if an action was just released this frame
pub fn is_action_just_released(
  input: InputState,
  bindings: InputBindings(action),
  action: action,
) -> Bool {
  // Check keyboard bindings
  let key_just_released =
    list.any(bindings.key_to_action, fn(binding) {
      let #(key, bound_action) = binding
      bound_action == action && is_key_just_released(input, key)
    })

  // Check mouse bindings
  let mouse_just_released =
    list.any(bindings.mouse_to_action, fn(binding) {
      let #(button, bound_action) = binding
      bound_action == action
      && case button {
        LeftButton -> input.mouse.left_button.just_released
        RightButton -> input.mouse.right_button.just_released
        MiddleButton -> input.mouse.middle_button.just_released
      }
    })

  // Check gamepad bindings
  // Note: Current gamepad API doesn't track just_released
  let _gamepad_just_released = False

  key_just_released || mouse_just_released
}

/// Get the analog value (0.0 to 1.0) for an action
///
/// Useful for actions that can have analog input like gamepad triggers.
/// Returns 1.0 for digital inputs (keyboard/mouse) when pressed, 0.0 when not pressed.
pub fn get_action_value(
  input: InputState,
  bindings: InputBindings(action),
  action: action,
) -> Float {
  // Check keyboard bindings (digital: 0.0 or 1.0)
  let key_value =
    list.find_map(bindings.key_to_action, fn(binding) {
      let #(key, bound_action) = binding
      case bound_action == action && is_key_pressed(input, key) {
        True -> Ok(1.0)
        False -> Error(Nil)
      }
    })
    |> result.unwrap(0.0)

  // Check mouse bindings (digital: 0.0 or 1.0)
  let mouse_value =
    list.find_map(bindings.mouse_to_action, fn(binding) {
      let #(button, bound_action) = binding
      case bound_action == action {
        True ->
          case button {
            LeftButton ->
              case is_left_button_pressed(input) {
                True -> Ok(1.0)
                False -> Error(Nil)
              }
            RightButton ->
              case is_right_button_pressed(input) {
                True -> Ok(1.0)
                False -> Error(Nil)
              }
            MiddleButton ->
              case input.mouse.middle_button.pressed {
                True -> Ok(1.0)
                False -> Error(Nil)
              }
          }
        False -> Error(Nil)
      }
    })
    |> result.unwrap(0.0)

  // Check gamepad bindings (analog: 0.0 to 1.0)
  let gamepad_value =
    list.find_map(bindings.gamepad_to_action, fn(binding) {
      let #(button, bound_action) = binding
      case bound_action == action {
        True -> Ok(gamepad_button(input, 0, button))
        False -> Error(Nil)
      }
    })
    |> result.unwrap(0.0)

  // Return highest value from any input
  case key_value >. 0.0 {
    True -> key_value
    False ->
      case mouse_value >. 0.0 {
        True -> mouse_value
        False -> gamepad_value
      }
  }
}

// --- Input Buffering System ---

/// Buffered input for more forgiving gameplay
///
/// Buffers action presses for a specified number of frames, allowing players
/// to press inputs slightly before they're valid (e.g., jump before landing).
///
/// ## Example
///
/// ```gleam
/// // In your model
/// pub type Model {
///   Model(
///     buffered_input: input.BufferedInput(Action),
///     // ...
///   )
/// }
///
/// // In init
/// fn init(_flags) {
///   let buffered = input.with_buffer(buffer_frames: 5)
///   Model(buffered_input: buffered, ...)
/// }
///
/// // In update
/// fn update(model, msg, ctx) {
///   // Update buffer each frame
///   let buffered = input.update_buffer(
///     model.buffered_input,
///     ctx.input,
///     bindings,
///   )
///
///   // Check if action was pressed within buffer window
///   let can_jump = input.was_action_pressed_buffered(buffered, bindings, Jump)
///
///   Model(..model, buffered_input: buffered)
/// }
/// ```
pub opaque type BufferedInput(action) {
  BufferedInput(
    buffer: List(BufferedAction(action)),
    buffer_frames: Int,
    frame_counter: Int,
  )
}

type BufferedAction(action) {
  BufferedAction(action: action, frame: Int)
}

/// Create a new buffered input system
///
pub fn with_buffer(buffer_frames buffer_frames: Int) -> BufferedInput(action) {
  BufferedInput(buffer: [], buffer_frames: buffer_frames, frame_counter: 0)
}

/// Update the input buffer each frame
///
/// Call this once per frame in your update function to:
/// 1. Add newly pressed actions to the buffer
/// 2. Remove expired actions from the buffer
///
/// ## Example
///
/// ```gleam
/// let buffered = input.update_buffer(
///   model.buffered_input,
///   ctx.input,
///   bindings,
/// )
/// ```
pub fn update_buffer(
  buffered: BufferedInput(action),
  input: InputState,
  bindings: InputBindings(action),
) -> BufferedInput(action) {
  let frame = buffered.frame_counter + 1

  // Find all actions that were just pressed this frame
  let new_actions =
    list.filter_map(
      // Get all unique actions from bindings
      list.flatten([
        list.map(bindings.key_to_action, fn(pair) { pair.1 }),
        list.map(bindings.mouse_to_action, fn(pair) { pair.1 }),
        list.map(bindings.gamepad_to_action, fn(pair) { pair.1 }),
      ]),
      fn(action) {
        case is_action_just_pressed(input, bindings, action) {
          True -> Ok(BufferedAction(action: action, frame: frame))
          False -> Error(Nil)
        }
      },
    )

  // Add new actions to buffer
  let updated_buffer = list.append(buffered.buffer, new_actions)

  // Remove expired actions (older than buffer_frames)
  let cutoff_frame = frame - buffered.buffer_frames
  let cleaned_buffer =
    list.filter(updated_buffer, fn(buffered_action) {
      buffered_action.frame >= cutoff_frame
    })

  BufferedInput(
    buffer: cleaned_buffer,
    buffer_frames: buffered.buffer_frames,
    frame_counter: frame,
  )
}

/// Check if an action was pressed within the buffer window
///
/// Returns True if the action was pressed in the last N frames (where N is buffer_frames).
/// This allows for more forgiving input timing.
///
/// ## Example
///
/// ```gleam
/// // Allow jump input to be buffered - player can press jump slightly
/// // before landing and it will still work
/// let can_jump = is_grounded
///   && input.was_action_pressed_buffered(buffered, bindings, Jump)
/// ```
pub fn was_action_pressed_buffered(
  buffered: BufferedInput(action),
  action: action,
) -> Bool {
  list.any(buffered.buffer, fn(buffered_action) {
    buffered_action.action == action
  })
}

/// Consume a buffered action (remove it from buffer)
///
/// Use this when you've acted on a buffered input to prevent it from being
/// used multiple times.
///
/// ## Example
///
/// ```gleam
/// let can_jump = is_grounded
///   && input.was_action_pressed_buffered(buffered, Jump)
///
/// case can_jump {
///   True -> {
///     // Perform jump
///     let buffered = input.consume_buffered_action(buffered, Jump)
///     // ...
///   }
///   False -> // ...
/// }
/// ```
pub fn consume_buffered_action(
  buffered: BufferedInput(action),
  action: action,
) -> BufferedInput(action) {
  // Remove first occurrence of this action from buffer
  let updated_buffer = case
    list.split_while(buffered.buffer, fn(buffered_action) {
      buffered_action.action != action
    })
  {
    #(before, []) -> before
    #(before, [_consumed, ..after]) -> list.append(before, after)
  }

  BufferedInput(..buffered, buffer: updated_buffer)
}

/// Clear all buffered actions
///
/// Useful when switching game states or when you want to reset the buffer.
pub fn clear_buffer(buffered: BufferedInput(action)) -> BufferedInput(action) {
  BufferedInput(..buffered, buffer: [])
}

// ============================================================================
// BUILDER FUNCTIONS (for internal use by input_manager)
// ============================================================================

/// Build a KeyboardState (internal use only)
@internal
pub fn build_keyboard_state(
  pressed pressed: set.Set(String),
  just_pressed just_pressed: set.Set(String),
  just_released just_released: set.Set(String),
) -> KeyboardState {
  KeyboardState(
    pressed_keys: pressed,
    just_pressed_keys: just_pressed,
    just_released_keys: just_released,
  )
}

/// Build a MouseState (internal use only)
@internal
pub fn build_mouse_state(
  x x: Float,
  y y: Float,
  delta_x delta_x: Float,
  delta_y delta_y: Float,
  wheel_delta wheel_delta: Float,
  left_pressed left_pressed: Bool,
  left_just_pressed left_just_pressed: Bool,
  left_just_released left_just_released: Bool,
  middle_pressed middle_pressed: Bool,
  middle_just_pressed middle_just_pressed: Bool,
  middle_just_released middle_just_released: Bool,
  right_pressed right_pressed: Bool,
  right_just_pressed right_just_pressed: Bool,
  right_just_released right_just_released: Bool,
) -> MouseState {
  MouseState(
    x: x,
    y: y,
    delta_x: delta_x,
    delta_y: delta_y,
    wheel_delta: wheel_delta,
    left_button: ButtonState(
      pressed: left_pressed,
      just_pressed: left_just_pressed,
      just_released: left_just_released,
    ),
    middle_button: ButtonState(
      pressed: middle_pressed,
      just_pressed: middle_just_pressed,
      just_released: middle_just_released,
    ),
    right_button: ButtonState(
      pressed: right_pressed,
      just_pressed: right_just_pressed,
      just_released: right_just_released,
    ),
  )
}

/// Build a Touch (internal use only)
@internal
pub fn build_touch(id id: Int, position position: Vec2(Float)) -> Touch {
  Touch(id: id, position: position)
}

/// Build a TouchState (internal use only)
@internal
pub fn build_touch_state(
  active active: List(Touch),
  just_started just_started: List(Touch),
  just_ended just_ended: List(Touch),
) -> TouchState {
  TouchState(
    touches: active,
    touches_just_started: just_started,
    touches_just_ended: just_ended,
  )
}

/// Build a GamepadState (internal use only)
@internal
pub fn build_gamepad_state(
  connected connected: Bool,
  buttons buttons: List(Float),
  axes axes: List(Float),
) -> GamepadState {
  GamepadState(connected: connected, buttons: buttons, axes: axes)
}

/// Build an InputState (internal use only)
@internal
pub fn build_input_state(
  keyboard keyboard: KeyboardState,
  mouse mouse: MouseState,
  gamepads gamepads: List(GamepadState),
  touch touch: TouchState,
) -> InputState {
  InputState(keyboard: keyboard, mouse: mouse, gamepad: gamepads, touch: touch)
}
