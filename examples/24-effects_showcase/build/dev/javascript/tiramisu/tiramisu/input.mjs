import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";

class InputState extends $CustomType {
  constructor(keyboard, mouse, gamepad, touch) {
    super();
    this.keyboard = keyboard;
    this.mouse = mouse;
    this.gamepad = gamepad;
    this.touch = touch;
  }
}

class KeyboardState extends $CustomType {
  constructor(pressed_keys, just_pressed_keys, just_released_keys) {
    super();
    this.pressed_keys = pressed_keys;
    this.just_pressed_keys = just_pressed_keys;
    this.just_released_keys = just_released_keys;
  }
}

class MouseState extends $CustomType {
  constructor(x, y, delta_x, delta_y, wheel_delta, left_button, middle_button, right_button) {
    super();
    this.x = x;
    this.y = y;
    this.delta_x = delta_x;
    this.delta_y = delta_y;
    this.wheel_delta = wheel_delta;
    this.left_button = left_button;
    this.middle_button = middle_button;
    this.right_button = right_button;
  }
}

export class ButtonState extends $CustomType {
  constructor(pressed, just_pressed, just_released) {
    super();
    this.pressed = pressed;
    this.just_pressed = just_pressed;
    this.just_released = just_released;
  }
}

export class GamepadState extends $CustomType {
  constructor(connected, buttons, axes) {
    super();
    this.connected = connected;
    this.buttons = buttons;
    this.axes = axes;
  }
}

export class TouchState extends $CustomType {
  constructor(touches, touches_just_started, touches_just_ended) {
    super();
    this.touches = touches;
    this.touches_just_started = touches_just_started;
    this.touches_just_ended = touches_just_ended;
  }
}

export class Touch extends $CustomType {
  constructor(id, x, y) {
    super();
    this.id = id;
    this.x = x;
    this.y = y;
  }
}

export class KeyA extends $CustomType {}

export class KeyB extends $CustomType {}

export class KeyC extends $CustomType {}

export class KeyD extends $CustomType {}

export class KeyE extends $CustomType {}

export class KeyF extends $CustomType {}

export class KeyG extends $CustomType {}

export class KeyH extends $CustomType {}

export class KeyI extends $CustomType {}

export class KeyJ extends $CustomType {}

export class KeyK extends $CustomType {}

export class KeyL extends $CustomType {}

export class KeyM extends $CustomType {}

export class KeyN extends $CustomType {}

export class KeyO extends $CustomType {}

export class KeyP extends $CustomType {}

export class KeyQ extends $CustomType {}

export class KeyR extends $CustomType {}

export class KeyS extends $CustomType {}

export class KeyT extends $CustomType {}

export class KeyU extends $CustomType {}

export class KeyV extends $CustomType {}

export class KeyW extends $CustomType {}

export class KeyX extends $CustomType {}

export class KeyY extends $CustomType {}

export class KeyZ extends $CustomType {}

export class Digit0 extends $CustomType {}

export class Digit1 extends $CustomType {}

export class Digit2 extends $CustomType {}

export class Digit3 extends $CustomType {}

export class Digit4 extends $CustomType {}

export class Digit5 extends $CustomType {}

export class Digit6 extends $CustomType {}

export class Digit7 extends $CustomType {}

export class Digit8 extends $CustomType {}

export class Digit9 extends $CustomType {}

export class F1 extends $CustomType {}

export class F2 extends $CustomType {}

export class F3 extends $CustomType {}

export class F4 extends $CustomType {}

export class F5 extends $CustomType {}

export class F6 extends $CustomType {}

export class F7 extends $CustomType {}

export class F8 extends $CustomType {}

export class F9 extends $CustomType {}

export class F10 extends $CustomType {}

export class F11 extends $CustomType {}

export class F12 extends $CustomType {}

export class ArrowUp extends $CustomType {}

export class ArrowDown extends $CustomType {}

export class ArrowLeft extends $CustomType {}

export class ArrowRight extends $CustomType {}

export class ShiftLeft extends $CustomType {}

export class ShiftRight extends $CustomType {}

export class ControlLeft extends $CustomType {}

export class ControlRight extends $CustomType {}

export class AltLeft extends $CustomType {}

export class AltRight extends $CustomType {}

export class MetaLeft extends $CustomType {}

export class MetaRight extends $CustomType {}

export class Space extends $CustomType {}

export class Enter extends $CustomType {}

export class Escape extends $CustomType {}

export class Tab extends $CustomType {}

export class Backspace extends $CustomType {}

export class Delete extends $CustomType {}

export class Insert extends $CustomType {}

export class Home extends $CustomType {}

export class End extends $CustomType {}

export class PageUp extends $CustomType {}

export class PageDown extends $CustomType {}

export class CapsLock extends $CustomType {}

export class Minus extends $CustomType {}

export class Equal extends $CustomType {}

export class BracketLeft extends $CustomType {}

export class BracketRight extends $CustomType {}

export class Backslash extends $CustomType {}

export class Semicolon extends $CustomType {}

export class Quote extends $CustomType {}

export class Comma extends $CustomType {}

export class Period extends $CustomType {}

export class Slash extends $CustomType {}

export class Backquote extends $CustomType {}

export class Numpad0 extends $CustomType {}

export class Numpad1 extends $CustomType {}

export class Numpad2 extends $CustomType {}

export class Numpad3 extends $CustomType {}

export class Numpad4 extends $CustomType {}

export class Numpad5 extends $CustomType {}

export class Numpad6 extends $CustomType {}

export class Numpad7 extends $CustomType {}

export class Numpad8 extends $CustomType {}

export class Numpad9 extends $CustomType {}

export class NumpadAdd extends $CustomType {}

export class NumpadSubtract extends $CustomType {}

export class NumpadMultiply extends $CustomType {}

export class NumpadDivide extends $CustomType {}

export class NumpadDecimal extends $CustomType {}

export class NumpadEnter extends $CustomType {}

export class NumLock extends $CustomType {}

export class AudioVolumeUp extends $CustomType {}

export class AudioVolumeDown extends $CustomType {}

export class AudioVolumeMute extends $CustomType {}

export class MediaPlayPause extends $CustomType {}

export class MediaStop extends $CustomType {}

export class MediaTrackNext extends $CustomType {}

export class MediaTrackPrevious extends $CustomType {}

export class PrintScreen extends $CustomType {}

export class ScrollLock extends $CustomType {}

export class Pause extends $CustomType {}

export class ContextMenu extends $CustomType {}

export class Custom extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class ButtonA extends $CustomType {}

export class ButtonB extends $CustomType {}

export class ButtonX extends $CustomType {}

export class ButtonY extends $CustomType {}

export class LeftBumper extends $CustomType {}

export class RightBumper extends $CustomType {}

export class LeftTrigger extends $CustomType {}

export class RightTrigger extends $CustomType {}

export class Select extends $CustomType {}

export class Start extends $CustomType {}

export class LeftStick extends $CustomType {}

export class RightStick extends $CustomType {}

export class DPadUp extends $CustomType {}

export class DPadDown extends $CustomType {}

export class DPadLeft extends $CustomType {}

export class DPadRight extends $CustomType {}

export class HomeButton extends $CustomType {}

export class LeftStickX extends $CustomType {}

export class LeftStickY extends $CustomType {}

export class RightStickX extends $CustomType {}

export class RightStickY extends $CustomType {}

export class LeftButton extends $CustomType {}

export class RightButton extends $CustomType {}

export class MiddleButton extends $CustomType {}

class InputBindings extends $CustomType {
  constructor(key_to_action, mouse_to_action, gamepad_to_action) {
    super();
    this.key_to_action = key_to_action;
    this.mouse_to_action = mouse_to_action;
    this.gamepad_to_action = gamepad_to_action;
  }
}

class BufferedInput extends $CustomType {
  constructor(buffer, buffer_frames, frame_counter) {
    super();
    this.buffer = buffer;
    this.buffer_frames = buffer_frames;
    this.frame_counter = frame_counter;
  }
}

class BufferedAction extends $CustomType {
  constructor(action, frame) {
    super();
    this.action = action;
    this.frame = frame;
  }
}

export function new$() {
  return new InputState(
    new KeyboardState(toList([]), toList([]), toList([])),
    new MouseState(
      0.0,
      0.0,
      0.0,
      0.0,
      0.0,
      new ButtonState(false, false, false),
      new ButtonState(false, false, false),
      new ButtonState(false, false, false),
    ),
    toList([]),
    new TouchState(toList([]), toList([]), toList([])),
  );
}

/**
 * Get mouse position
 */
export function mouse_position(input) {
  return [input.mouse.x, input.mouse.y];
}

/**
 * Get mouse delta
 */
export function mouse_delta(input) {
  return [input.mouse.delta_x, input.mouse.delta_y];
}

/**
 * Check if left mouse button is pressed
 */
export function is_left_button_pressed(input) {
  return input.mouse.left_button.pressed;
}

/**
 * Check if left mouse button was just pressed
 */
export function is_left_button_just_pressed(input) {
  return input.mouse.left_button.just_pressed;
}

/**
 * Check if right mouse button is pressed
 */
export function is_right_button_pressed(input) {
  return input.mouse.right_button.pressed;
}

/**
 * Check if right mouse button was just pressed
 */
export function is_right_button_just_pressed(input) {
  return input.mouse.right_button.just_pressed;
}

/**
 * Get mouse wheel delta
 */
export function mouse_wheel_delta(input) {
  return input.mouse.wheel_delta;
}

/**
 * Get current touches
 */
export function touches(input) {
  return input.touch.touches;
}

/**
 * Get touches that just started
 */
export function touches_just_started(input) {
  return input.touch.touches_just_started;
}

/**
 * Get touches that just ended
 */
export function touches_just_ended(input) {
  return input.touch.touches_just_ended;
}

/**
 * Get touch count
 */
export function touch_count(input) {
  return $list.length(input.touch.touches);
}

function list_get(loop$list, loop$index) {
  while (true) {
    let list = loop$list;
    let index = loop$index;
    if (list instanceof $Empty) {
      return new Error(undefined);
    } else if (index === 0) {
      let x = list.head;
      return new Ok(x);
    } else {
      let n = index;
      let rest = list.tail;
      loop$list = rest;
      loop$index = n - 1;
    }
  }
}

/**
 * Check if gamepad at index is connected
 */
export function is_gamepad_connected(input, index) {
  let $ = list_get(input.gamepad, index);
  if ($ instanceof Ok) {
    let gamepad = $[0];
    return gamepad.connected;
  } else {
    return false;
  }
}

function gamepad_button_to_index(button) {
  if (button instanceof ButtonA) {
    return 0;
  } else if (button instanceof ButtonB) {
    return 1;
  } else if (button instanceof ButtonX) {
    return 2;
  } else if (button instanceof ButtonY) {
    return 3;
  } else if (button instanceof LeftBumper) {
    return 4;
  } else if (button instanceof RightBumper) {
    return 5;
  } else if (button instanceof LeftTrigger) {
    return 6;
  } else if (button instanceof RightTrigger) {
    return 7;
  } else if (button instanceof Select) {
    return 8;
  } else if (button instanceof Start) {
    return 9;
  } else if (button instanceof LeftStick) {
    return 10;
  } else if (button instanceof RightStick) {
    return 11;
  } else if (button instanceof DPadUp) {
    return 12;
  } else if (button instanceof DPadDown) {
    return 13;
  } else if (button instanceof DPadLeft) {
    return 14;
  } else if (button instanceof DPadRight) {
    return 15;
  } else {
    return 16;
  }
}

/**
 * Get gamepad button value
 */
export function gamepad_button(input, gamepad_index, button) {
  let button_index = gamepad_button_to_index(button);
  let $ = list_get(input.gamepad, gamepad_index);
  if ($ instanceof Ok) {
    let gamepad = $[0];
    let _pipe = list_get(gamepad.buttons, button_index);
    return $result.unwrap(_pipe, 0.0);
  } else {
    return 0.0;
  }
}

/**
 * Check if gamepad button is pressed
 */
export function is_gamepad_button_pressed(input, gamepad_index, button) {
  return gamepad_button(input, gamepad_index, button) > 0.5;
}

function gamepad_axis_to_index(axis) {
  if (axis instanceof LeftStickX) {
    return 0;
  } else if (axis instanceof LeftStickY) {
    return 1;
  } else if (axis instanceof RightStickX) {
    return 2;
  } else {
    return 3;
  }
}

/**
 * Get gamepad axis value
 */
export function gamepad_axis(input, gamepad_index, axis) {
  let axis_index = gamepad_axis_to_index(axis);
  let $ = list_get(input.gamepad, gamepad_index);
  if ($ instanceof Ok) {
    let gamepad = $[0];
    let _pipe = list_get(gamepad.axes, axis_index);
    return $result.unwrap(_pipe, 0.0);
  } else {
    return 0.0;
  }
}

/**
 * Convert Key to JavaScript KeyboardEvent.code string
 * 
 * @ignore
 */
function key_to_code(key) {
  if (key instanceof KeyA) {
    return "KeyA";
  } else if (key instanceof KeyB) {
    return "KeyB";
  } else if (key instanceof KeyC) {
    return "KeyC";
  } else if (key instanceof KeyD) {
    return "KeyD";
  } else if (key instanceof KeyE) {
    return "KeyE";
  } else if (key instanceof KeyF) {
    return "KeyF";
  } else if (key instanceof KeyG) {
    return "KeyG";
  } else if (key instanceof KeyH) {
    return "KeyH";
  } else if (key instanceof KeyI) {
    return "KeyI";
  } else if (key instanceof KeyJ) {
    return "KeyJ";
  } else if (key instanceof KeyK) {
    return "KeyK";
  } else if (key instanceof KeyL) {
    return "KeyL";
  } else if (key instanceof KeyM) {
    return "KeyM";
  } else if (key instanceof KeyN) {
    return "KeyN";
  } else if (key instanceof KeyO) {
    return "KeyO";
  } else if (key instanceof KeyP) {
    return "KeyP";
  } else if (key instanceof KeyQ) {
    return "KeyQ";
  } else if (key instanceof KeyR) {
    return "KeyR";
  } else if (key instanceof KeyS) {
    return "KeyS";
  } else if (key instanceof KeyT) {
    return "KeyT";
  } else if (key instanceof KeyU) {
    return "KeyU";
  } else if (key instanceof KeyV) {
    return "KeyV";
  } else if (key instanceof KeyW) {
    return "KeyW";
  } else if (key instanceof KeyX) {
    return "KeyX";
  } else if (key instanceof KeyY) {
    return "KeyY";
  } else if (key instanceof KeyZ) {
    return "KeyZ";
  } else if (key instanceof Digit0) {
    return "Digit0";
  } else if (key instanceof Digit1) {
    return "Digit1";
  } else if (key instanceof Digit2) {
    return "Digit2";
  } else if (key instanceof Digit3) {
    return "Digit3";
  } else if (key instanceof Digit4) {
    return "Digit4";
  } else if (key instanceof Digit5) {
    return "Digit5";
  } else if (key instanceof Digit6) {
    return "Digit6";
  } else if (key instanceof Digit7) {
    return "Digit7";
  } else if (key instanceof Digit8) {
    return "Digit8";
  } else if (key instanceof Digit9) {
    return "Digit9";
  } else if (key instanceof F1) {
    return "F1";
  } else if (key instanceof F2) {
    return "F2";
  } else if (key instanceof F3) {
    return "F3";
  } else if (key instanceof F4) {
    return "F4";
  } else if (key instanceof F5) {
    return "F5";
  } else if (key instanceof F6) {
    return "F6";
  } else if (key instanceof F7) {
    return "F7";
  } else if (key instanceof F8) {
    return "F8";
  } else if (key instanceof F9) {
    return "F9";
  } else if (key instanceof F10) {
    return "F10";
  } else if (key instanceof F11) {
    return "F11";
  } else if (key instanceof F12) {
    return "F12";
  } else if (key instanceof ArrowUp) {
    return "ArrowUp";
  } else if (key instanceof ArrowDown) {
    return "ArrowDown";
  } else if (key instanceof ArrowLeft) {
    return "ArrowLeft";
  } else if (key instanceof ArrowRight) {
    return "ArrowRight";
  } else if (key instanceof ShiftLeft) {
    return "ShiftLeft";
  } else if (key instanceof ShiftRight) {
    return "ShiftRight";
  } else if (key instanceof ControlLeft) {
    return "ControlLeft";
  } else if (key instanceof ControlRight) {
    return "ControlRight";
  } else if (key instanceof AltLeft) {
    return "AltLeft";
  } else if (key instanceof AltRight) {
    return "AltRight";
  } else if (key instanceof MetaLeft) {
    return "MetaLeft";
  } else if (key instanceof MetaRight) {
    return "MetaRight";
  } else if (key instanceof Space) {
    return "Space";
  } else if (key instanceof Enter) {
    return "Enter";
  } else if (key instanceof Escape) {
    return "Escape";
  } else if (key instanceof Tab) {
    return "Tab";
  } else if (key instanceof Backspace) {
    return "Backspace";
  } else if (key instanceof Delete) {
    return "Delete";
  } else if (key instanceof Insert) {
    return "Insert";
  } else if (key instanceof Home) {
    return "Home";
  } else if (key instanceof End) {
    return "End";
  } else if (key instanceof PageUp) {
    return "PageUp";
  } else if (key instanceof PageDown) {
    return "PageDown";
  } else if (key instanceof CapsLock) {
    return "CapsLock";
  } else if (key instanceof Minus) {
    return "Minus";
  } else if (key instanceof Equal) {
    return "Equal";
  } else if (key instanceof BracketLeft) {
    return "BracketLeft";
  } else if (key instanceof BracketRight) {
    return "BracketRight";
  } else if (key instanceof Backslash) {
    return "Backslash";
  } else if (key instanceof Semicolon) {
    return "Semicolon";
  } else if (key instanceof Quote) {
    return "Quote";
  } else if (key instanceof Comma) {
    return "Comma";
  } else if (key instanceof Period) {
    return "Period";
  } else if (key instanceof Slash) {
    return "Slash";
  } else if (key instanceof Backquote) {
    return "Backquote";
  } else if (key instanceof Numpad0) {
    return "Numpad0";
  } else if (key instanceof Numpad1) {
    return "Numpad1";
  } else if (key instanceof Numpad2) {
    return "Numpad2";
  } else if (key instanceof Numpad3) {
    return "Numpad3";
  } else if (key instanceof Numpad4) {
    return "Numpad4";
  } else if (key instanceof Numpad5) {
    return "Numpad5";
  } else if (key instanceof Numpad6) {
    return "Numpad6";
  } else if (key instanceof Numpad7) {
    return "Numpad7";
  } else if (key instanceof Numpad8) {
    return "Numpad8";
  } else if (key instanceof Numpad9) {
    return "Numpad9";
  } else if (key instanceof NumpadAdd) {
    return "NumpadAdd";
  } else if (key instanceof NumpadSubtract) {
    return "NumpadSubtract";
  } else if (key instanceof NumpadMultiply) {
    return "NumpadMultiply";
  } else if (key instanceof NumpadDivide) {
    return "NumpadDivide";
  } else if (key instanceof NumpadDecimal) {
    return "NumpadDecimal";
  } else if (key instanceof NumpadEnter) {
    return "NumpadEnter";
  } else if (key instanceof NumLock) {
    return "NumLock";
  } else if (key instanceof AudioVolumeUp) {
    return "AudioVolumeUp";
  } else if (key instanceof AudioVolumeDown) {
    return "AudioVolumeDown";
  } else if (key instanceof AudioVolumeMute) {
    return "AudioVolumeMute";
  } else if (key instanceof MediaPlayPause) {
    return "MediaPlayPause";
  } else if (key instanceof MediaStop) {
    return "MediaStop";
  } else if (key instanceof MediaTrackNext) {
    return "MediaTrackNext";
  } else if (key instanceof MediaTrackPrevious) {
    return "MediaTrackPrevious";
  } else if (key instanceof PrintScreen) {
    return "PrintScreen";
  } else if (key instanceof ScrollLock) {
    return "ScrollLock";
  } else if (key instanceof Pause) {
    return "Pause";
  } else if (key instanceof ContextMenu) {
    return "ContextMenu";
  } else {
    let code = key[0];
    return code;
  }
}

/**
 * Check if a key is currently pressed
 */
export function is_key_pressed(input, key) {
  let key_code = key_to_code(key);
  return $list.contains(input.keyboard.pressed_keys, key_code);
}

/**
 * Check if a key was just pressed this frame
 */
export function is_key_just_pressed(input, key) {
  let key_code = key_to_code(key);
  return $list.contains(input.keyboard.just_pressed_keys, key_code);
}

/**
 * Check if a key was just released this frame
 */
export function is_key_just_released(input, key) {
  let key_code = key_to_code(key);
  return $list.contains(input.keyboard.just_released_keys, key_code);
}

/**
 * Get axis value with dead zone applied
 */
export function get_axis_with_deadzone(input, gamepad_index, axis, deadzone) {
  let value = gamepad_axis(input, gamepad_index, axis);
  let $ = (value > deadzone) || (value < (0.0 - deadzone));
  if ($) {
    return value;
  } else {
    return 0.0;
  }
}

/**
 * Check if left stick is moved in any direction
 */
export function is_left_stick_active(input, gamepad_index, threshold) {
  let x = gamepad_axis(input, gamepad_index, new LeftStickX());
  let y = gamepad_axis(input, gamepad_index, new LeftStickY());
  return (((x > threshold) || (x < (0.0 - threshold))) || (y > threshold)) || (y < (0.0 - threshold));
}

/**
 * Check if right stick is moved in any direction
 */
export function is_right_stick_active(input, gamepad_index, threshold) {
  let x = gamepad_axis(input, gamepad_index, new RightStickX());
  let y = gamepad_axis(input, gamepad_index, new RightStickY());
  return (((x > threshold) || (x < (0.0 - threshold))) || (y > threshold)) || (y < (0.0 - threshold));
}

/**
 * Convenience: Check if primary gamepad (index 0) is connected
 */
export function is_primary_connected(input) {
  return is_gamepad_connected(input, 0);
}

/**
 * Convenience: Check button on primary gamepad
 */
export function is_primary_gamepad_button_pressed(input, button) {
  return is_gamepad_button_pressed(input, 0, button);
}

/**
 * Convenience: Get button value on primary gamepad
 */
export function get_primary_button(input, button) {
  return gamepad_button(input, 0, button);
}

/**
 * Convenience: Get axis value on primary gamepad
 */
export function get_primary_axis(input, axis) {
  return gamepad_axis(input, 0, axis);
}

/**
 * Create a new empty input bindings configuration
 */
export function new_bindings() {
  return new InputBindings(toList([]), toList([]), toList([]));
}

/**
 * Bind a keyboard key to an action
 *
 * ## Example
 *
 * ```gleam
 * let bindings = input.new_bindings()
 *   |> input.bind_key(input.Space, Jump)
 *   |> input.bind_key(input.KeyW, MoveForward)
 * ```
 */
export function bind_key(bindings, key, action) {
  return new InputBindings(
    listPrepend([key, action], bindings.key_to_action),
    bindings.mouse_to_action,
    bindings.gamepad_to_action,
  );
}

/**
 * Bind a mouse button to an action
 */
export function bind_mouse_button(bindings, button, action) {
  return new InputBindings(
    bindings.key_to_action,
    listPrepend([button, action], bindings.mouse_to_action),
    bindings.gamepad_to_action,
  );
}

/**
 * Bind a gamepad button to an action
 */
export function bind_gamepad_button(bindings, button, action) {
  return new InputBindings(
    bindings.key_to_action,
    bindings.mouse_to_action,
    listPrepend([button, action], bindings.gamepad_to_action),
  );
}

/**
 * Check if an action is currently pressed
 *
 * Returns True if any input bound to this action is pressed.
 *
 * ## Example
 *
 * ```gleam
 * if input.is_action_pressed(ctx.input, bindings, Jump) {
 *   // Player wants to jump
 * }
 * ```
 */
export function is_action_pressed(input, bindings, action) {
  let key_pressed = $list.any(
    bindings.key_to_action,
    (binding) => {
      let key;
      let bound_action;
      key = binding[0];
      bound_action = binding[1];
      return (isEqual(bound_action, action)) && is_key_pressed(input, key);
    },
  );
  let mouse_pressed = $list.any(
    bindings.mouse_to_action,
    (binding) => {
      let button;
      let bound_action;
      button = binding[0];
      bound_action = binding[1];
      return (isEqual(bound_action, action)) && (() => {
        if (button instanceof LeftButton) {
          return is_left_button_pressed(input);
        } else if (button instanceof RightButton) {
          return is_right_button_pressed(input);
        } else {
          return input.mouse.middle_button.pressed;
        }
      })();
    },
  );
  let gamepad_pressed = $list.any(
    bindings.gamepad_to_action,
    (binding) => {
      let button;
      let bound_action;
      button = binding[0];
      bound_action = binding[1];
      return (isEqual(bound_action, action)) && is_gamepad_button_pressed(
        input,
        0,
        button,
      );
    },
  );
  return (key_pressed || mouse_pressed) || gamepad_pressed;
}

/**
 * Check if an action was just pressed this frame
 *
 * Returns True if any input bound to this action was just pressed.
 */
export function is_action_just_pressed(input, bindings, action) {
  let key_just_pressed = $list.any(
    bindings.key_to_action,
    (binding) => {
      let key;
      let bound_action;
      key = binding[0];
      bound_action = binding[1];
      return (isEqual(bound_action, action)) && is_key_just_pressed(input, key);
    },
  );
  let mouse_just_pressed = $list.any(
    bindings.mouse_to_action,
    (binding) => {
      let button;
      let bound_action;
      button = binding[0];
      bound_action = binding[1];
      return (isEqual(bound_action, action)) && (() => {
        if (button instanceof LeftButton) {
          return is_left_button_just_pressed(input);
        } else if (button instanceof RightButton) {
          return is_right_button_just_pressed(input);
        } else {
          return input.mouse.middle_button.just_pressed;
        }
      })();
    },
  );
  let gamepad_just_pressed = $list.any(
    bindings.gamepad_to_action,
    (binding) => {
      let button;
      let bound_action;
      button = binding[0];
      bound_action = binding[1];
      return (isEqual(bound_action, action)) && is_gamepad_button_pressed(
        input,
        0,
        button,
      );
    },
  );
  return (key_just_pressed || mouse_just_pressed) || gamepad_just_pressed;
}

/**
 * Check if an action was just released this frame
 */
export function is_action_just_released(input, bindings, action) {
  let key_just_released = $list.any(
    bindings.key_to_action,
    (binding) => {
      let key;
      let bound_action;
      key = binding[0];
      bound_action = binding[1];
      return (isEqual(bound_action, action)) && is_key_just_released(input, key);
    },
  );
  let mouse_just_released = $list.any(
    bindings.mouse_to_action,
    (binding) => {
      let button;
      let bound_action;
      button = binding[0];
      bound_action = binding[1];
      return (isEqual(bound_action, action)) && (() => {
        if (button instanceof LeftButton) {
          return input.mouse.left_button.just_released;
        } else if (button instanceof RightButton) {
          return input.mouse.right_button.just_released;
        } else {
          return input.mouse.middle_button.just_released;
        }
      })();
    },
  );
  let $ = false;
  
  return key_just_released || mouse_just_released;
}

/**
 * Get the analog value (0.0 to 1.0) for an action
 *
 * Useful for actions that can have analog input like gamepad triggers.
 * Returns 1.0 for digital inputs (keyboard/mouse) when pressed, 0.0 when not pressed.
 */
export function get_action_value(input, bindings, action) {
  let _block;
  let _pipe = $list.find_map(
    bindings.key_to_action,
    (binding) => {
      let key;
      let bound_action;
      key = binding[0];
      bound_action = binding[1];
      let $ = (isEqual(bound_action, action)) && is_key_pressed(input, key);
      if ($) {
        return new Ok(1.0);
      } else {
        return new Error(undefined);
      }
    },
  );
  _block = $result.unwrap(_pipe, 0.0);
  let key_value = _block;
  let _block$1;
  let _pipe$1 = $list.find_map(
    bindings.mouse_to_action,
    (binding) => {
      let button;
      let bound_action;
      button = binding[0];
      bound_action = binding[1];
      let $ = isEqual(bound_action, action);
      if ($) {
        if (button instanceof LeftButton) {
          let $1 = is_left_button_pressed(input);
          if ($1) {
            return new Ok(1.0);
          } else {
            return new Error(undefined);
          }
        } else if (button instanceof RightButton) {
          let $1 = is_right_button_pressed(input);
          if ($1) {
            return new Ok(1.0);
          } else {
            return new Error(undefined);
          }
        } else {
          let $1 = input.mouse.middle_button.pressed;
          if ($1) {
            return new Ok(1.0);
          } else {
            return new Error(undefined);
          }
        }
      } else {
        return new Error(undefined);
      }
    },
  );
  _block$1 = $result.unwrap(_pipe$1, 0.0);
  let mouse_value = _block$1;
  let _block$2;
  let _pipe$2 = $list.find_map(
    bindings.gamepad_to_action,
    (binding) => {
      let button;
      let bound_action;
      button = binding[0];
      bound_action = binding[1];
      let $ = isEqual(bound_action, action);
      if ($) {
        return new Ok(gamepad_button(input, 0, button));
      } else {
        return new Error(undefined);
      }
    },
  );
  _block$2 = $result.unwrap(_pipe$2, 0.0);
  let gamepad_value = _block$2;
  let $ = key_value > 0.0;
  if ($) {
    return key_value;
  } else {
    let $1 = mouse_value > 0.0;
    if ($1) {
      return mouse_value;
    } else {
      return gamepad_value;
    }
  }
}

/**
 * Create a new buffered input system
 *
 * ## Arguments
 *
 * - `buffer_frames`: Number of frames to keep actions in buffer (e.g., 5 frames = ~83ms at 60fps)
 */
export function with_buffer(buffer_frames) {
  return new BufferedInput(toList([]), buffer_frames, 0);
}

/**
 * Update the input buffer each frame
 *
 * Call this once per frame in your update function to:
 * 1. Add newly pressed actions to the buffer
 * 2. Remove expired actions from the buffer
 *
 * ## Example
 *
 * ```gleam
 * let buffered = input.update_buffer(
 *   model.buffered_input,
 *   ctx.input,
 *   bindings,
 * )
 * ```
 */
export function update_buffer(buffered, input, bindings) {
  let frame = buffered.frame_counter + 1;
  let new_actions = $list.filter_map(
    $list.flatten(
      toList([
        $list.map(bindings.key_to_action, (pair) => { return pair[1]; }),
        $list.map(bindings.mouse_to_action, (pair) => { return pair[1]; }),
        $list.map(bindings.gamepad_to_action, (pair) => { return pair[1]; }),
      ]),
    ),
    (action) => {
      let $ = is_action_just_pressed(input, bindings, action);
      if ($) {
        return new Ok(new BufferedAction(action, frame));
      } else {
        return new Error(undefined);
      }
    },
  );
  let updated_buffer = $list.append(buffered.buffer, new_actions);
  let cutoff_frame = frame - buffered.buffer_frames;
  let cleaned_buffer = $list.filter(
    updated_buffer,
    (buffered_action) => { return buffered_action.frame >= cutoff_frame; },
  );
  return new BufferedInput(cleaned_buffer, buffered.buffer_frames, frame);
}

/**
 * Check if an action was pressed within the buffer window
 *
 * Returns True if the action was pressed in the last N frames (where N is buffer_frames).
 * This allows for more forgiving input timing.
 *
 * ## Example
 *
 * ```gleam
 * // Allow jump input to be buffered - player can press jump slightly
 * // before landing and it will still work
 * let can_jump = is_grounded
 *   && input.was_action_pressed_buffered(buffered, bindings, Jump)
 * ```
 */
export function was_action_pressed_buffered(buffered, action) {
  return $list.any(
    buffered.buffer,
    (buffered_action) => { return isEqual(buffered_action.action, action); },
  );
}

/**
 * Consume a buffered action (remove it from buffer)
 *
 * Use this when you've acted on a buffered input to prevent it from being
 * used multiple times.
 *
 * ## Example
 *
 * ```gleam
 * let can_jump = is_grounded
 *   && input.was_action_pressed_buffered(buffered, Jump)
 *
 * case can_jump {
 *   True -> {
 *     // Perform jump
 *     let buffered = input.consume_buffered_action(buffered, Jump)
 *     // ...
 *   }
 *   False -> // ...
 * }
 * ```
 */
export function consume_buffered_action(buffered, action) {
  let _block;
  let $ = $list.split_while(
    buffered.buffer,
    (buffered_action) => { return !isEqual(buffered_action.action, action); },
  );
  let $1 = $[1];
  if ($1 instanceof $Empty) {
    let before = $[0];
    _block = before;
  } else {
    let before = $[0];
    let after = $1.tail;
    _block = $list.append(before, after);
  }
  let updated_buffer = _block;
  return new BufferedInput(
    updated_buffer,
    buffered.buffer_frames,
    buffered.frame_counter,
  );
}

/**
 * Clear all buffered actions
 *
 * Useful when switching game states or when you want to reset the buffer.
 */
export function clear_buffer(buffered) {
  return new BufferedInput(
    toList([]),
    buffered.buffer_frames,
    buffered.frame_counter,
  );
}
