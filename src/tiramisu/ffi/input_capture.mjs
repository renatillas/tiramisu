// FFI for capturing immutable input state snapshots
import * as keyboardFFI from '../input/ffi/keyboard.mjs';
import * as mouseFFI from '../input/ffi/mouse.mjs';
import * as gamepadFFI from '../input/ffi/gamepad.mjs';
import * as touchFFI from '../input/ffi/touch.mjs';
import { to_list } from '../../../../gleam_javascript/gleam/javascript/array.mjs';

/**
 * Capture a complete snapshot of all input state for the current frame
 * @returns {InputState} Immutable input state
 */
export function captureInputState() {
  // Capture keyboard state
  const keyboard = {
    pressed_keys: to_list(keyboardFFI.getAllPressedKeys()),
    just_pressed_keys: to_list(captureJustPressedKeys()),
    just_released_keys: to_list(captureJustReleasedKeys()),
  };

  // Capture mouse state
  const mouse = {
    x: mouseFFI.getMouseX(),
    y: mouseFFI.getMouseY(),
    delta_x: mouseFFI.getMouseDeltaX(),
    delta_y: mouseFFI.getMouseDeltaY(),
    wheel_delta: mouseFFI.getWheelDelta(),
    left_button: {
      pressed: mouseFFI.isButtonPressed(0),
      just_pressed: mouseFFI.isButtonJustPressed(0),
      just_released: mouseFFI.isButtonJustReleased(0),
    },
    middle_button: {
      pressed: mouseFFI.isButtonPressed(1),
      just_pressed: mouseFFI.isButtonJustPressed(1),
      just_released: mouseFFI.isButtonJustReleased(1),
    },
    right_button: {
      pressed: mouseFFI.isButtonPressed(2),
      just_pressed: mouseFFI.isButtonJustPressed(2),
      just_released: mouseFFI.isButtonJustReleased(2),
    },
  };

  // Capture gamepad state (primary gamepad only for now)
  const gamepadConnected = gamepadFFI.isConnected(0);
  const gamepad = {
    connected: gamepadConnected,
    buttons: to_list(gamepadConnected ? captureGamepadButtons(0) : []),
    axes: to_list(gamepadConnected ? captureGamepadAxes(0) : []),
  };

  // Capture touch state
  const touch = {
    touches: touchFFI.getTouches(),
    touches_just_started: touchFFI.getTouchesJustStarted(),
    touches_just_ended: touchFFI.getTouchesJustEnded(),
  };

  return { keyboard, mouse, gamepad, touch };
}

/**
 * Clear per-frame input state
 */
export function clearInputFrameState() {
  keyboardFFI.clearFrameState();
  mouseFFI.clearFrameState();
  touchFFI.clearFrameState();
}

// --- Helper Functions ---

function captureJustPressedKeys() {
  // This would need to be tracked by the keyboard FFI
  // For now, return empty array - needs keyboard FFI enhancement
  return [];
}

function captureJustReleasedKeys() {
  // This would need to be tracked by the keyboard FFI
  // For now, return empty array - needs keyboard FFI enhancement
  return [];
}

function captureGamepadButtons(index) {
  const buttons = [];
  for (let i = 0; i < 17; i++) {
    buttons.push(gamepadFFI.getButtonValue(index, i));
  }
  return buttons;
}

function captureGamepadAxes(index) {
  const axes = [];
  for (let i = 0; i < 4; i++) {
    axes.push(gamepadFFI.getAxisValue(index, i));
  }
  return axes;
}
