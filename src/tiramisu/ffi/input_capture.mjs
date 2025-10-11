// Input capture system that creates immutable InputState snapshots
// Note: Opaque types (InputState, KeyboardState, MouseState) are not exported
// so we recreate their constructors here to match the generated structure
import * as INPUT_GLEAM from '../input.mjs';
import * as GLEAM from '../../gleam.mjs';

// Recreate opaque type constructors to match generated input.mjs
class InputState {
  constructor(keyboard, mouse, gamepad, touch) {
    this.keyboard = keyboard;
    this.mouse = mouse;
    this.gamepad = gamepad;
    this.touch = touch;
  }
}

class KeyboardState {
  constructor(pressed_keys, just_pressed_keys, just_released_keys) {
    this.pressed_keys = pressed_keys;
    this.just_pressed_keys = just_pressed_keys;
    this.just_released_keys = just_released_keys;
  }
}

class MouseState {
  constructor(x, y, delta_x, delta_y, wheel_delta, left_button, middle_button, right_button) {
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

// Keyboard state management (internal mutable state)
let keysPressed = new Set();
let keysJustPressed = new Set();
let keysJustReleased = new Set();

// Mouse state management (internal mutable state)
let mouseX = 0.0;
let mouseY = 0.0;
let lastMouseX = 0.0;
let lastMouseY = 0.0;
let deltaX = 0.0;
let deltaY = 0.0;
let wheelDelta = 0.0;

let leftButton = { pressed: false, justPressed: false, justReleased: false };
let middleButton = { pressed: false, justPressed: false, justReleased: false };
let rightButton = { pressed: false, justPressed: false, justReleased: false };

// Touch state management (internal mutable state)
let touches = new Map(); // id -> {x, y}
let touchesJustStarted = new Map();
let touchesJustEnded = new Map();

// Gamepad connection tracking (optimization to avoid expensive polling)
let gamepadConnected = false;

/**
 * Initialize keyboard input
 */
export function initKeyboard() {
  window.addEventListener('keydown', (e) => {
    if (!keysPressed.has(e.code)) {
      keysJustPressed.add(e.code);
    }
    keysPressed.add(e.code);
  });

  window.addEventListener('keyup', (e) => {
    keysPressed.delete(e.code);
    keysJustReleased.add(e.code);
  });

  // Track gamepad connections to optimize polling
  window.addEventListener('gamepadconnected', () => {
    gamepadConnected = true;
  });

  window.addEventListener('gamepaddisconnected', () => {
    // Check if any gamepads are still connected
    const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
    gamepadConnected = Array.from(gamepads).some(gp => gp && gp.connected);
  });
}

/**
 * Initialize mouse input on canvas
 */
export function initMouse(canvas) {
  canvas.addEventListener('mousemove', (e) => {
    const rect = canvas.getBoundingClientRect();
    mouseX = e.clientX - rect.left;
    mouseY = e.clientY - rect.top;
    deltaX = mouseX - lastMouseX;
    deltaY = mouseY - lastMouseY;
  });

  canvas.addEventListener('mousedown', (e) => {
    if (e.button === 0) {
      if (!leftButton.pressed) leftButton.justPressed = true;
      leftButton.pressed = true;
    } else if (e.button === 1) {
      if (!middleButton.pressed) middleButton.justPressed = true;
      middleButton.pressed = true;
    } else if (e.button === 2) {
      if (!rightButton.pressed) rightButton.justPressed = true;
      rightButton.pressed = true;
    }
  });

  canvas.addEventListener('mouseup', (e) => {
    if (e.button === 0) {
      leftButton.pressed = false;
      leftButton.justReleased = true;
    } else if (e.button === 1) {
      middleButton.pressed = false;
      middleButton.justReleased = true;
    } else if (e.button === 2) {
      rightButton.pressed = false;
      rightButton.justReleased = true;
    }
  });

  canvas.addEventListener('contextmenu', (e) => {
    e.preventDefault();
  });

  canvas.addEventListener('wheel', (e) => {
    wheelDelta = e.deltaY;
  });
}

/**
 * Initialize touch input on canvas
 */
export function initTouch(canvas) {
  canvas.addEventListener('touchstart', (e) => {
    e.preventDefault();
    for (let i = 0; i < e.changedTouches.length; i++) {
      const touch = e.changedTouches[i];
      const rect = canvas.getBoundingClientRect();
      // Clamp coordinates to canvas bounds
      const x = Math.max(0, Math.min(rect.width, touch.clientX - rect.left));
      const y = Math.max(0, Math.min(rect.height, touch.clientY - rect.top));
      touches.set(touch.identifier, { x, y });
      touchesJustStarted.set(touch.identifier, { x, y });
    }
  });

  canvas.addEventListener('touchmove', (e) => {
    e.preventDefault();
    for (let i = 0; i < e.changedTouches.length; i++) {
      const touch = e.changedTouches[i];
      const rect = canvas.getBoundingClientRect();
      // Clamp coordinates to canvas bounds
      const x = Math.max(0, Math.min(rect.width, touch.clientX - rect.left));
      const y = Math.max(0, Math.min(rect.height, touch.clientY - rect.top));
      touches.set(touch.identifier, { x, y });
    }
  });

  canvas.addEventListener('touchend', (e) => {
    e.preventDefault();
    for (let i = 0; i < e.changedTouches.length; i++) {
      const touch = e.changedTouches[i];
      const data = touches.get(touch.identifier);
      if (data) {
        touchesJustEnded.set(touch.identifier, data);
        touches.delete(touch.identifier);
      }
    }
  });

  canvas.addEventListener('touchcancel', (e) => {
    e.preventDefault();
    for (let i = 0; i < e.changedTouches.length; i++) {
      const touch = e.changedTouches[i];
      touches.delete(touch.identifier);
      touchesJustEnded.delete(touch.identifier);
    }
  });
}

/**
 * Capture current input state as immutable Gleam InputState
 */
export function captureInputState() {
  // Keyboard state
  const pressedKeys = Array.from(keysPressed);
  const justPressedKeys = Array.from(keysJustPressed);
  const justReleasedKeys = Array.from(keysJustReleased);

  const keyboardState = new KeyboardState(
    GLEAM.toList(pressedKeys),
    GLEAM.toList(justPressedKeys),
    GLEAM.toList(justReleasedKeys)
  );

  // Mouse state
  const mouseState = new MouseState(
    mouseX,
    mouseY,
    deltaX,
    deltaY,
    wheelDelta,
    new INPUT_GLEAM.ButtonState(leftButton.pressed, leftButton.justPressed, leftButton.justReleased),
    new INPUT_GLEAM.ButtonState(middleButton.pressed, middleButton.justPressed, middleButton.justReleased),
    new INPUT_GLEAM.ButtonState(rightButton.pressed, rightButton.justPressed, rightButton.justReleased)
  );

  // Gamepad state (support up to 4 gamepads)
  const gamepadStates = [];

  // Optimization: Skip expensive polling if no gamepads are connected
  if (gamepadConnected) {
    const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
    for (let i = 0; i < 4; i++) {
      const gamepad = gamepads[i];

      if (gamepad && gamepad.connected) {
        const buttons = Array.from(gamepad.buttons).map(b => b.value);
        const axes = Array.from(gamepad.axes);
        gamepadStates.push(new INPUT_GLEAM.GamepadState(
          true,
          GLEAM.toList(buttons),
          GLEAM.toList(axes)
        ));
      } else {
        // Disconnected gamepad
        gamepadStates.push(new INPUT_GLEAM.GamepadState(
          false,
          new GLEAM.Empty(),
          new GLEAM.Empty()
        ));
      }
    }
  } else {
    // No gamepads connected - return 4 disconnected states without polling
    for (let i = 0; i < 4; i++) {
      gamepadStates.push(new INPUT_GLEAM.GamepadState(
        false,
        new GLEAM.Empty(),
        new GLEAM.Empty()
      ));
    }
  }

  // Touch state
  const touchList = [];
  touches.forEach((data, id) => {
    touchList.push(new INPUT_GLEAM.Touch(id, data.x, data.y));
  });

  const touchJustStartedList = [];
  touchesJustStarted.forEach((data, id) => {
    touchJustStartedList.push(new INPUT_GLEAM.Touch(id, data.x, data.y));
  });

  const touchJustEndedList = [];
  touchesJustEnded.forEach((data, id) => {
    touchJustEndedList.push(new INPUT_GLEAM.Touch(id, data.x, data.y));
  });

  const touchState = new INPUT_GLEAM.TouchState(
    GLEAM.toList(touchList),
    GLEAM.toList(touchJustStartedList),
    GLEAM.toList(touchJustEndedList)
  );

  // Create immutable InputState
  return new InputState(
    keyboardState,
    mouseState,
    GLEAM.toList(gamepadStates),
    touchState
  );
}

/**
 * Clear per-frame input state (call at end of frame)
 */
export function clearInputFrameState() {
  // Keyboard
  keysJustPressed.clear();
  keysJustReleased.clear();

  // Mouse
  lastMouseX = mouseX;
  lastMouseY = mouseY;
  deltaX = 0.0;
  deltaY = 0.0;
  wheelDelta = 0.0;
  leftButton.justPressed = false;
  leftButton.justReleased = false;
  middleButton.justPressed = false;
  middleButton.justReleased = false;
  rightButton.justPressed = false;
  rightButton.justReleased = false;

  // Touch
  touchesJustStarted.clear();
  touchesJustEnded.clear();
}
