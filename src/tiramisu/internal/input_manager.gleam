/// Input manager FFI bindings
///
/// This module wraps the JavaScript InputManager class as an opaque type.
/// Input state is still managed mutably in JavaScript for performance.
import tiramisu/input
import tiramisu/internal/renderer

/// External type wrapping the JavaScript InputManager class
/// Manages all input state (keyboard, mouse, touch, gamepad)
pub type InputManager

/// Create a new InputManager instance
/// @param canvas The canvas element to attach input listeners to
@external(javascript, "../../tiramisu.ffi.mjs", "createInputManager")
pub fn new(canvas: renderer.DomElement) -> InputManager

/// Capture the current input state as an immutable snapshot
@external(javascript, "../../tiramisu.ffi.mjs", "inputManagerCaptureState")
pub fn capture_state(manager: InputManager) -> input.InputState

/// Clear per-frame input state
/// Should be called at the end of each frame
@external(javascript, "../../tiramisu.ffi.mjs", "inputManagerClearFrameState")
pub fn clear_frame_state(manager: InputManager) -> Nil

/// Cleanup the InputManager and remove event listeners
@external(javascript, "../../tiramisu.ffi.mjs", "inputManagerDestroy")
pub fn destroy(manager: InputManager) -> Nil
