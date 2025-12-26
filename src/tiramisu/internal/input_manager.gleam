////
//// Internal input manager - captures and tracks input state
////
//// This module manages input event listeners and state tracking.
//// It uses minimal FFI - only for addEventListener/removeEventListener.
////

import gleam/dict.{type Dict}
import gleam/list
import gleam/set.{type Set}
import tiramisu/input

// ============================================================================
// TYPES
// ============================================================================

/// Internal mutable input manager state
/// This is passed through FFI and mutated by event handlers
pub opaque type InputManager {
  InputManager(
    // Keyboard state
    keyboard_pressed: Set(String),
    keyboard_just_pressed: Set(String),
    keyboard_just_released: Set(String),
    // Mouse state
    mouse_x: Float,
    mouse_y: Float,
    mouse_last_x: Float,
    mouse_last_y: Float,
    mouse_delta_x: Float,
    mouse_delta_y: Float,
    mouse_wheel_delta: Float,
    mouse_left_pressed: Bool,
    mouse_left_just_pressed: Bool,
    mouse_left_just_released: Bool,
    mouse_middle_pressed: Bool,
    mouse_middle_just_pressed: Bool,
    mouse_middle_just_released: Bool,
    mouse_right_pressed: Bool,
    mouse_right_just_pressed: Bool,
    mouse_right_just_released: Bool,
    // Touch state
    touch_active: Dict(Int, #(Float, Float)),
    touch_just_started: Dict(Int, #(Float, Float)),
    touch_just_ended: Dict(Int, #(Float, Float)),
    // Gamepad state
    gamepad_connected: Bool,
  )
}

// ============================================================================
// PUBLIC API
// ============================================================================

/// Create a new input manager
pub fn new() -> InputManager {
  InputManager(
    keyboard_pressed: set.new(),
    keyboard_just_pressed: set.new(),
    keyboard_just_released: set.new(),
    mouse_x: 0.0,
    mouse_y: 0.0,
    mouse_last_x: 0.0,
    mouse_last_y: 0.0,
    mouse_delta_x: 0.0,
    mouse_delta_y: 0.0,
    mouse_wheel_delta: 0.0,
    mouse_left_pressed: False,
    mouse_left_just_pressed: False,
    mouse_left_just_released: False,
    mouse_middle_pressed: False,
    mouse_middle_just_pressed: False,
    mouse_middle_just_released: False,
    mouse_right_pressed: False,
    mouse_right_just_pressed: False,
    mouse_right_just_released: False,
    touch_active: dict.new(),
    touch_just_started: dict.new(),
    touch_just_ended: dict.new(),
    gamepad_connected: False,
  )
}

/// Capture current input state as immutable snapshot
pub fn capture_state(manager: InputManager) -> input.InputState {
  // Build keyboard state
  let keyboard_state =
    input.build_keyboard_state(
      pressed: manager.keyboard_pressed,
      just_pressed: manager.keyboard_just_pressed,
      just_released: manager.keyboard_just_released,
    )

  // Build mouse state
  let mouse_state =
    input.build_mouse_state(
      x: manager.mouse_x,
      y: manager.mouse_y,
      delta_x: manager.mouse_delta_x,
      delta_y: manager.mouse_delta_y,
      wheel_delta: manager.mouse_wheel_delta,
      left_pressed: manager.mouse_left_pressed,
      left_just_pressed: manager.mouse_left_just_pressed,
      left_just_released: manager.mouse_left_just_released,
      middle_pressed: manager.mouse_middle_pressed,
      middle_just_pressed: manager.mouse_middle_just_pressed,
      middle_just_released: manager.mouse_middle_just_released,
      right_pressed: manager.mouse_right_pressed,
      right_just_pressed: manager.mouse_right_just_pressed,
      right_just_released: manager.mouse_right_just_released,
    )

  // Build touch state
  let active_touches =
    dict.to_list(manager.touch_active)
    |> list.map(fn(entry) {
      let #(id, #(x, y)) = entry
      input.build_touch(id: id, x: x, y: y)
    })

  let just_started_touches =
    dict.to_list(manager.touch_just_started)
    |> list.map(fn(entry) {
      let #(id, #(x, y)) = entry
      input.build_touch(id: id, x: x, y: y)
    })

  let just_ended_touches =
    dict.to_list(manager.touch_just_ended)
    |> list.map(fn(entry) {
      let #(id, #(x, y)) = entry
      input.build_touch(id: id, x: x, y: y)
    })

  let touch_state =
    input.build_touch_state(
      active: active_touches,
      just_started: just_started_touches,
      just_ended: just_ended_touches,
    )

  // Build gamepad states (4 gamepads)
  let gamepad_states = case manager.gamepad_connected {
    True -> capture_gamepad_states_ffi()
    False -> [
      input.build_gamepad_state(connected: False, buttons: [], axes: []),
      input.build_gamepad_state(connected: False, buttons: [], axes: []),
      input.build_gamepad_state(connected: False, buttons: [], axes: []),
      input.build_gamepad_state(connected: False, buttons: [], axes: []),
    ]
  }

  // Build final input state
  input.build_input_state(
    keyboard: keyboard_state,
    mouse: mouse_state,
    gamepads: gamepad_states,
    touch: touch_state,
  )
}

/// Clear per-frame input state
/// Should be called at the end of each frame
pub fn clear_frame_state(manager: InputManager) -> InputManager {
  InputManager(
    ..manager,
    keyboard_just_pressed: set.new(),
    keyboard_just_released: set.new(),
    mouse_last_x: manager.mouse_x,
    mouse_last_y: manager.mouse_y,
    mouse_delta_x: 0.0,
    mouse_delta_y: 0.0,
    mouse_wheel_delta: 0.0,
    mouse_left_just_pressed: False,
    mouse_left_just_released: False,
    mouse_middle_just_pressed: False,
    mouse_middle_just_released: False,
    mouse_right_just_pressed: False,
    mouse_right_just_released: False,
    touch_just_started: dict.new(),
    touch_just_ended: dict.new(),
  )
}

// ============================================================================
// EVENT HANDLERS (called from FFI)
// ============================================================================

/// Handle keydown event
pub fn on_keydown(manager: InputManager, key_code: String) -> InputManager {
  let was_pressed = set.contains(manager.keyboard_pressed, key_code)
  case was_pressed {
    True -> manager
    False ->
      InputManager(
        ..manager,
        keyboard_pressed: set.insert(manager.keyboard_pressed, key_code),
        keyboard_just_pressed: set.insert(
          manager.keyboard_just_pressed,
          key_code,
        ),
      )
  }
}

/// Handle keyup event
pub fn on_keyup(manager: InputManager, key_code: String) -> InputManager {
  InputManager(
    ..manager,
    keyboard_pressed: set.delete(manager.keyboard_pressed, key_code),
    keyboard_just_released: set.insert(manager.keyboard_just_released, key_code),
  )
}

/// Handle mousemove event
pub fn on_mousemove(
  manager: InputManager,
  x: Float,
  y: Float,
  delta_x: Float,
  delta_y: Float,
) -> InputManager {
  InputManager(
    ..manager,
    mouse_x: x,
    mouse_y: y,
    mouse_delta_x: delta_x,
    mouse_delta_y: delta_y,
  )
}

/// Handle mousedown event
pub fn on_mousedown(manager: InputManager, button: Int) -> InputManager {
  case button {
    0 ->
      InputManager(
        ..manager,
        mouse_left_pressed: True,
        mouse_left_just_pressed: case manager.mouse_left_pressed {
          True -> False
          False -> True
        },
      )
    1 ->
      InputManager(
        ..manager,
        mouse_middle_pressed: True,
        mouse_middle_just_pressed: case manager.mouse_middle_pressed {
          True -> False
          False -> True
        },
      )
    2 ->
      InputManager(
        ..manager,
        mouse_right_pressed: True,
        mouse_right_just_pressed: case manager.mouse_right_pressed {
          True -> False
          False -> True
        },
      )
    _ -> manager
  }
}

/// Handle mouseup event
pub fn on_mouseup(manager: InputManager, button: Int) -> InputManager {
  case button {
    0 ->
      InputManager(
        ..manager,
        mouse_left_pressed: False,
        mouse_left_just_released: True,
      )
    1 ->
      InputManager(
        ..manager,
        mouse_middle_pressed: False,
        mouse_middle_just_released: True,
      )
    2 ->
      InputManager(
        ..manager,
        mouse_right_pressed: False,
        mouse_right_just_released: True,
      )
    _ -> manager
  }
}

/// Handle wheel event
pub fn on_wheel(manager: InputManager, delta_y: Float) -> InputManager {
  InputManager(..manager, mouse_wheel_delta: delta_y)
}

/// Handle touchstart event
pub fn on_touchstart(
  manager: InputManager,
  touch_id: Int,
  x: Float,
  y: Float,
) -> InputManager {
  InputManager(
    ..manager,
    touch_active: dict.insert(manager.touch_active, touch_id, #(x, y)),
    touch_just_started: dict.insert(manager.touch_just_started, touch_id, #(
      x,
      y,
    )),
  )
}

/// Handle touchmove event
pub fn on_touchmove(
  manager: InputManager,
  touch_id: Int,
  x: Float,
  y: Float,
) -> InputManager {
  InputManager(
    ..manager,
    touch_active: dict.insert(manager.touch_active, touch_id, #(x, y)),
  )
}

/// Handle touchend event
pub fn on_touchend(manager: InputManager, touch_id: Int) -> InputManager {
  case dict.get(manager.touch_active, touch_id) {
    Ok(pos) ->
      InputManager(
        ..manager,
        touch_active: dict.delete(manager.touch_active, touch_id),
        touch_just_ended: dict.insert(manager.touch_just_ended, touch_id, pos),
      )
    Error(_) -> manager
  }
}

/// Handle touchcancel event
pub fn on_touchcancel(manager: InputManager, touch_id: Int) -> InputManager {
  InputManager(
    ..manager,
    touch_active: dict.delete(manager.touch_active, touch_id),
    touch_just_ended: dict.delete(manager.touch_just_ended, touch_id),
  )
}

/// Handle gamepad connected event
pub fn on_gamepad_connected(manager: InputManager) -> InputManager {
  InputManager(..manager, gamepad_connected: True)
}

/// Handle gamepad disconnected event
pub fn on_gamepad_disconnected(manager: InputManager) -> InputManager {
  // Check if any gamepads are still connected
  let still_connected = check_gamepads_connected_ffi()
  InputManager(..manager, gamepad_connected: still_connected)
}

// ============================================================================
// FFI - Minimal Browser API
// ============================================================================

/// Capture gamepad states from browser API
@external(javascript, "./input_manager.ffi.mjs", "captureGamepadStates")
fn capture_gamepad_states_ffi() -> List(input.GamepadState)

/// Check if any gamepads are connected
@external(javascript, "./input_manager.ffi.mjs", "checkGamepadsConnected")
fn check_gamepads_connected_ffi() -> Bool
