// Browser API effects - System-level browser interactions
//
// This module provides access to browser APIs like fullscreen, pointer lock,
// haptic feedback, and clipboard operations.
//
// Most functionality uses Plinth's browser bindings. Some features (pointer lock,
// vibration) require minimal FFI as they're not yet in Plinth.

import gleam/javascript/array
import gleam/javascript/promise.{type Promise}
import plinth/browser/clipboard
import plinth/browser/document
import plinth/browser/element
import plinth/browser/window

// ============================================================================
// FULLSCREEN
// ============================================================================

/// Request fullscreen mode for an element
pub fn request_fullscreen(elem: element.Element) -> Promise(Result(Nil, String)) {
  element.request_fullscreen(elem)
}

/// Exit fullscreen mode
pub fn exit_fullscreen() -> Promise(Result(Nil, String)) {
  window.self()
  |> window.document
  |> document.exit_fullscreen
}

/// Check if an element is currently fullscreen
pub fn is_fullscreen(elem: element.Element) -> Bool {
  let document =
    window.self()
    |> window.document

  case document.fullscreen_element(document) {
    Ok(fullscreen_elem) -> fullscreen_elem == elem
    Error(_) -> False
  }
}

// ============================================================================
// CLIPBOARD
// ============================================================================

/// Write text to clipboard
pub fn clipboard_write(text: String) -> Promise(Result(Nil, String)) {
  clipboard.write_text(text)
}

/// Read text from clipboard
pub fn clipboard_read() -> Promise(Result(String, String)) {
  clipboard.read_text()
}

// ============================================================================
// POINTER LOCK - Minimal FFI (not in Plinth yet)
// ============================================================================

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

@external(javascript, "../browser.ffi.mjs", "requestPointerLock")
fn request_pointer_lock_ffi(
  elem: element.Element,
) -> Promise(Result(Nil, String))

@external(javascript, "../browser.ffi.mjs", "exitPointerLock")
fn exit_pointer_lock_ffi() -> Nil

@external(javascript, "../browser.ffi.mjs", "isPointerLocked")
fn is_pointer_locked_ffi() -> Bool

// ============================================================================
// HAPTIC FEEDBACK - Minimal FFI (not in Plinth yet)
// ============================================================================

/// Stop any ongoing vibration
pub fn cancel_vibrate() -> Nil {
  vibrate(array.from_list([]))
}

/// Trigger haptic feedback on mobile devices
/// Pattern is a list of vibration durations in milliseconds
/// Example: [200, 100, 200] vibrates 200ms, pauses 100ms, vibrates 200ms
@external(javascript, "../browser.ffi.mjs", "vibrate")
pub fn vibrate(pattern: array.Array(Int)) -> Nil

// ============================================================================
// GAMEPAD HAPTICS - Minimal FFI (not in Plinth yet)
// ============================================================================

/// Trigger haptic feedback on a gamepad
/// - gamepad: Gamepad index (0-3)
/// - intensity: Vibration intensity (0.0 to 1.0)
/// - duration_ms: Duration in milliseconds
@external(javascript, "../browser.ffi.mjs", "gamepadVibrate")
pub fn gamepad_vibrate(gamepad: Int, intensity: Float, duration_ms: Int) -> Nil
