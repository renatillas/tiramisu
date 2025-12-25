// Minimal browser API FFI for features not yet in Plinth
//
// This file only contains browser APIs that Plinth doesn't provide yet:
// - Pointer Lock API
// - Vibration API
// - Gamepad Haptics API

import { Result$Ok, Result$Error } from '../gleam_stdlib/gleam.mjs';

// ============================================================================
// POINTER LOCK API
// ============================================================================

/**
 * Request pointer lock for an element
 * @param {HTMLElement} element
 * @returns {Promise<Result<Nil, String>>}
 */
export function requestPointerLock(element) {
  return new Promise((resolve) => {
    // Try different vendor-specific APIs
    const requestFn =
      element.requestPointerLock ||
      element.webkitRequestPointerLock ||
      element.mozRequestPointerLock;

    if (!requestFn) {
      resolve(Result$Error('Pointer Lock API not supported'));
      return;
    }

    const onLockChange = () => {
      // Check if pointer is locked to our element
      const lockedElement =
        document.pointerLockElement ||
        document.webkitPointerLockElement ||
        document.mozPointerLockElement;

      document.removeEventListener('pointerlockchange', onLockChange);
      document.removeEventListener('webkitpointerlockchange', onLockChange);
      document.removeEventListener('mozpointerlockchange', onLockChange);

      if (lockedElement === element) {
        resolve(Result$Ok());
      } else {
        resolve(Result$Error('Failed to lock pointer'));
      }
    };

    document.addEventListener('pointerlockchange', onLockChange);
    document.addEventListener('webkitpointerlockchange', onLockChange);
    document.addEventListener('mozpointerlockchange', onLockChange);

    requestFn.call(element);

    // Timeout after 2 seconds
    setTimeout(() => {
      document.removeEventListener('pointerlockchange', onLockChange);
      document.removeEventListener('webkitpointerlockchange', onLockChange);
      document.removeEventListener('mozpointerlockchange', onLockChange);
      resolve(Result$Error('Pointer lock request timed out'));
    }, 2000);
  });
}

/**
 * Exit pointer lock mode
 */
export function exitPointerLock() {
  const exitFn =
    document.exitPointerLock ||
    document.webkitExitPointerLock ||
    document.mozExitPointerLock;

  if (exitFn) {
    exitFn.call(document);
  }
}

/**
 * Check if pointer is currently locked
 * @returns {boolean}
 */
export function isPointerLocked() {
  const lockedElement =
    document.pointerLockElement ||
    document.webkitPointerLockElement ||
    document.mozPointerLockElement;

  return lockedElement !== null && lockedElement !== undefined;
}

// ============================================================================
// VIBRATION API
// ============================================================================

/**
 * Trigger device vibration
 * @param {Array<number>} pattern - Array of vibration durations in milliseconds
 */
export function vibrate(pattern) {
  if ('vibrate' in navigator) {
    navigator.vibrate(pattern);
  }
}

// ============================================================================
// GAMEPAD HAPTICS API
// ============================================================================

/**
 * Trigger gamepad vibration (haptic feedback)
 * @param {number} gamepadIndex - Index of the gamepad (0-3)
 * @param {number} intensity - Vibration intensity (0.0 to 1.0)
 * @param {number} durationMs - Duration in milliseconds
 */
export function gamepadVibrate(gamepadIndex, intensity, durationMs) {
  const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
  const gamepad = gamepads[gamepadIndex];

  if (!gamepad) {
    console.warn(`[Tiramisu] Gamepad ${gamepadIndex} not found`);
    return;
  }

  if (!gamepad.vibrationActuator) {
    console.warn(`[Tiramisu] Gamepad ${gamepadIndex} does not support vibration`);
    return;
  }

  // Clamp intensity to valid range
  const clampedIntensity = Math.max(0.0, Math.min(1.0, intensity));

  // Use the Gamepad Haptics API
  gamepad.vibrationActuator
    .playEffect('dual-rumble', {
      startDelay: 0,
      duration: durationMs,
      weakMagnitude: clampedIntensity,
      strongMagnitude: clampedIntensity,
    })
    .catch((error) => {
      console.warn(`[Tiramisu] Failed to vibrate gamepad: ${error.message}`);
    });
}
