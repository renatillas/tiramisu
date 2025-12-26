////
//// Input initialization - sets up event listeners using Plinth
////
//// This module uses Plinth to attach browser event listeners and
//// wires them up to the InputManager's event handlers.
////

import gleam/dynamic
import gleam/list
import plinth/browser/element
import plinth/browser/event
import plinth/browser/window
import tiramisu/internal/input_manager

// ============================================================================
// PUBLIC API
// ============================================================================

/// Initialize input system - attach all event listeners to canvas
/// Returns a cleanup function to remove all listeners
pub fn initialize(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> Nil {
  // Keyboard listeners (on window)
  attach_keydown(get_manager, set_manager)
  attach_keyup(get_manager, set_manager)

  // Mouse listeners (on canvas)
  attach_mousemove(canvas, get_manager, set_manager)
  attach_mousedown(canvas, get_manager, set_manager)
  attach_mouseup(canvas, get_manager, set_manager)
  attach_contextmenu(canvas, get_manager, set_manager)
  attach_wheel(canvas, get_manager, set_manager)

  // Touch listeners (on canvas)
  attach_touchstart(canvas, get_manager, set_manager)
  attach_touchmove(canvas, get_manager, set_manager)
  attach_touchend(canvas, get_manager, set_manager)
  attach_touchcancel(canvas, get_manager, set_manager)

  // Gamepad listeners (on window)
  attach_gamepadconnected(canvas, get_manager, set_manager)
  attach_gamepaddisconnected(canvas, get_manager, set_manager)
  // Return cleanup function
  Nil
}

// ============================================================================
// KEYBOARD EVENTS
// ============================================================================

fn attach_keydown(
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> Nil {
  let handler = fn(event: event.Event(event.UIEvent(event.KeyboardEvent))) {
    let current = get_manager()
    event.code(event)
    |> input_manager.on_keydown(current, _)
    |> set_manager
  }

  window.add_event_listener("keydown", handler)
}

fn attach_keyup(
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> Nil {
  let handler = fn(event: event.Event(event.UIEvent(event.KeyboardEvent))) {
    let current = get_manager()
    event.code(event)
    |> input_manager.on_keyup(current, _)
    |> set_manager
  }

  window.add_event_listener("keyup", handler)
}

// ============================================================================
// MOUSE EVENTS
// ============================================================================

fn attach_mousemove(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(evt: event.Event(dynamic.Dynamic)) {
    // Get mouse position relative to canvas
    let #(x, y, delta_x, delta_y) = get_mouse_position(canvas, evt)

    let current = get_manager()
    input_manager.on_mousemove(current, x, y, delta_x, delta_y)
    |> set_manager
  }

  element.add_event_listener(canvas, "mousemove", handler)
}

fn attach_mousedown(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(event: event.Event(dynamic.Dynamic)) {
    let current = get_manager()
    get_mouse_button(event)
    |> input_manager.on_mousedown(current, _)
    |> set_manager
  }

  element.add_event_listener(canvas, "mousedown", handler)
}

fn attach_mouseup(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(event: event.Event(dynamic.Dynamic)) {
    let current = get_manager()
    get_mouse_button(event)
    |> input_manager.on_mouseup(current, _)
    |> set_manager
  }

  element.add_event_listener(canvas, "mouseup", handler)
}

fn attach_contextmenu(
  canvas: element.Element,
  _get_manager: fn() -> input_manager.InputManager,
  _set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(event: event.Event(event.UIEvent(event.KeyboardEvent))) {
    event.prevent_default(event)
  }

  element.add_event_listener(canvas, "contextmenu", handler)
}

fn attach_wheel(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(event: event.Event(dynamic.Dynamic)) {
    let current = get_manager()
    get_wheel_delta(event)
    |> input_manager.on_wheel(current, _)
    |> set_manager
  }

  element.add_event_listener(canvas, "wheel", handler)
}

// ============================================================================
// TOUCH EVENTS
// ============================================================================

fn attach_touchstart(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(event: event.Event(dynamic.Dynamic)) {
    event.prevent_default(event)
    let current = get_manager()
    get_changed_touches(canvas, event)
    |> list.fold(current, fn(manager, touch) {
      let #(id, x, y) = touch
      input_manager.on_touchstart(manager, id, x, y)
    })
    |> set_manager
  }

  element.add_event_listener(canvas, "touchstart", handler)
}

fn attach_touchmove(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(event: event.Event(dynamic.Dynamic)) {
    event.prevent_default(event)
    let current = get_manager()
    get_changed_touches(canvas, event)
    |> list.fold(current, fn(manager, touch) {
      let #(id, x, y) = touch
      input_manager.on_touchmove(manager, id, x, y)
    })
    |> set_manager
  }

  element.add_event_listener(canvas, "touchmove", handler)
}

fn attach_touchend(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(event: event.Event(dynamic.Dynamic)) {
    event.prevent_default(event)
    let current = get_manager()
    get_changed_touch_ids(event)
    |> list.fold(current, fn(manager, id) {
      input_manager.on_touchend(manager, id)
    })
    |> set_manager
  }

  element.add_event_listener(canvas, "touchend", handler)
}

fn attach_touchcancel(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(evt: event.Event(dynamic.Dynamic)) {
    event.prevent_default(evt)
    let current = get_manager()
    get_changed_touch_ids(evt)
    |> list.fold(current, fn(manager, id) {
      input_manager.on_touchcancel(manager, id)
    })
    |> set_manager
  }

  element.add_event_listener(canvas, "touchcancel", handler)
}

// ============================================================================
// GAMEPAD EVENTS
// ============================================================================

fn attach_gamepadconnected(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(_event) {
    let current = get_manager()
    let updated = input_manager.on_gamepad_connected(current)
    set_manager(updated)
    Nil
  }

  element.add_event_listener(canvas, "gamepadconnected", handler)
}

fn attach_gamepaddisconnected(
  canvas: element.Element,
  get_manager: fn() -> input_manager.InputManager,
  set_manager: fn(input_manager.InputManager) -> Nil,
) -> fn() -> Nil {
  let handler = fn(_event) {
    let current = get_manager()
    let updated = input_manager.on_gamepad_disconnected(current)
    set_manager(updated)
    Nil
  }

  element.add_event_listener(canvas, "gamepaddisconnected", handler)
}

// ============================================================================
// EVENT DATA EXTRACTION (FFI)
// ============================================================================

/// Extract mouse button from mouse event
@external(javascript, "./input_init.ffi.mjs", "getMouseButton")
fn get_mouse_button(event: event.Event(dynamic.Dynamic)) -> Int

/// Extract mouse position from mouse event
@external(javascript, "./input_init.ffi.mjs", "getMousePosition")
fn get_mouse_position(
  canvas: element.Element,
  event: event.Event(dynamic.Dynamic),
) -> #(Float, Float, Float, Float)

/// Extract wheel delta from wheel event
@external(javascript, "./input_init.ffi.mjs", "getWheelDelta")
fn get_wheel_delta(event: event.Event(dynamic.Dynamic)) -> Float

/// Extract changed touches from touch event
@external(javascript, "./input_init.ffi.mjs", "getChangedTouches")
fn get_changed_touches(
  canvas: element.Element,
  event: event.Event(dynamic.Dynamic),
) -> List(#(Int, Float, Float))

/// Extract changed touch IDs from touch event
@external(javascript, "./input_init.ffi.mjs", "getChangedTouchIds")
fn get_changed_touch_ids(event: event.Event(dynamic.Dynamic)) -> List(Int)
