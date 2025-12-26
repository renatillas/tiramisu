// Minimal FFI for input manager - only gamepad polling
//
// All other input (keyboard, mouse, touch) is handled via event listeners
// attached directly in Gleam using Plinth

import * as INPUT from '../input.mjs';
import { toList, Empty } from '../../gleam.mjs';

/**
 * Capture gamepad states from browser API
 * Returns array of 4 gamepad states
 */
export function captureGamepadStates() {
  const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
  const states = [];

  for (let i = 0; i < 4; i++) {
    const gamepad = gamepads[i];
    if (gamepad && gamepad.connected) {
      const buttons = [];
      const axes = [];

      for (const button of gamepad.buttons) {
        buttons.push(button.value);
      }

      for (const axis of gamepad.axes) {
        axes.push(axis);
      }

      states.push(INPUT.build_gamepad_state(
        true,
        toList(buttons),
        toList(axes)
      ));
    } else {
      states.push(INPUT.build_gamepad_state(
        false,
        new Empty(),
        new Empty()
      ));
    }
  }

  return toList(states);
}

/**
 * Check if any gamepads are connected
 */
export function checkGamepadsConnected() {
  const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
  return Array.from(gamepads).some(gp => gp && gp.connected);
}
