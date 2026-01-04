import gleam/float
import gleam/javascript/array
import gleam/javascript/promise.{type Promise}
import gleam/list
import gleam/time/duration
import plinth/browser/clipboard
import plinth/browser/element

/// Write text to clipboard
pub fn clipboard_write(text: String) -> Promise(Result(Nil, String)) {
  clipboard.write_text(text)
}

/// Read text from clipboard
pub fn clipboard_read() -> Promise(Result(String, String)) {
  clipboard.read_text()
}

/// Request pointer lock for an element
/// Hides the cursor and provides unlimited mouse movement
pub fn request_pointer_lock(
  elem: element.Element,
) -> Promise(Result(Nil, String)) {
  request_pointer_lock_ffi(elem)
}

/// Exit pointer lock mode
pub fn exit_pointer_lock() -> Nil {
  exit_pointer_lock_ffi()
}

/// Check if pointer is currently locked
pub fn is_pointer_locked() -> Bool {
  is_pointer_locked_ffi()
}

@external(javascript, "./browser.ffi.mjs", "requestPointerLock")
fn request_pointer_lock_ffi(
  elem: element.Element,
) -> Promise(Result(Nil, String))

@external(javascript, "./browser.ffi.mjs", "exitPointerLock")
fn exit_pointer_lock_ffi() -> Nil

@external(javascript, "./browser.ffi.mjs", "isPointerLocked")
fn is_pointer_locked_ffi() -> Bool

// ============================================================================
// HAPTIC FEEDBACK - Minimal FFI (not in Plinth yet)
// ============================================================================

/// Stop any ongoing vibration
pub fn cancel_vibrate() -> Nil {
  do_vibrate(array.from_list([]))
}

/// Trigger haptic feedback on mobile devices
/// Pattern is a list of vibration durations in milliseconds
/// Example: [duration.milliseconds(200), duration.milliseconds(100), duration.milliseconds(200)] vibrates 200ms, pauses 100ms, vibrates 200ms
pub fn mobile_vibrate(pattern: List(duration.Duration)) -> Nil {
  pattern
  |> list.map(fn(duration) {
    duration
    |> duration.to_seconds()
    |> float.multiply(1000.0)
    |> float.round()
  })
  |> array.from_list()
  |> do_vibrate
}

@external(javascript, "./browser.ffi.mjs", "vibrate")
fn do_vibrate(pattern: array.Array(Int)) -> Nil

// ============================================================================
// GAMEPAD HAPTICS - Minimal FFI (not in Plinth yet)
// ============================================================================

/// Trigger haptic feedback on a gamepad
/// - gamepad: Gamepad index (0-3)
/// - intensity: Vibration intensity (0.0 to 1.0)
/// - duration: Duration of the vibration
pub fn gamepad_vibrate(
  gamepad: Int,
  intensity: Float,
  duration: duration.Duration,
) -> Nil {
  duration
  |> duration.to_seconds()
  |> float.multiply(1000.0)
  |> do_gamepad_vibrate(gamepad, intensity, _)
}

@external(javascript, "./browser.ffi.mjs", "gamepadVibrate")
fn do_gamepad_vibrate(gamepad: Int, intensity: Float, duration_ms: Float) -> Nil
