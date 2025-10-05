/// Keyboard input handling module

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
pub fn key_to_code(key: Key) -> String {
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

/// Initialize keyboard input system (call once at startup)
@external(javascript, "./ffi/keyboard.mjs", "initKeyboard")
pub fn init() -> Nil

/// Check if a key is currently pressed
@external(javascript, "./ffi/keyboard.mjs", "isKeyPressed")
fn is_key_pressed_raw(key_code: String) -> Bool

/// Check if a key was just pressed this frame
@external(javascript, "./ffi/keyboard.mjs", "isKeyJustPressed")
fn is_key_just_pressed_raw(key_code: String) -> Bool

/// Check if a key was just released this frame
@external(javascript, "./ffi/keyboard.mjs", "isKeyJustReleased")
fn is_key_just_released_raw(key_code: String) -> Bool

/// Clear per-frame state (call at end of each frame)
@external(javascript, "./ffi/keyboard.mjs", "clearFrameState")
pub fn clear_frame_state() -> Nil

/// Get all currently pressed keys
@external(javascript, "./ffi/keyboard.mjs", "getAllPressedKeys")
pub fn get_all_pressed_keys() -> List(String)

/// Check if a key is currently pressed
pub fn is_pressed(key: Key) -> Bool {
  is_key_pressed_raw(key_to_code(key))
}

/// Check if a key was just pressed this frame
pub fn is_just_pressed(key: Key) -> Bool {
  is_key_just_pressed_raw(key_to_code(key))
}

/// Check if a key was just released this frame
pub fn is_just_released(key: Key) -> Bool {
  is_key_just_released_raw(key_to_code(key))
}
