// Tick subscription FFI for animation frame timing and input capture.
//
// This module manages per-scene tick subscriptions that get called
// from the render loop with frame timing and input information.

import * as $duration from "../../gleam_time/gleam/time/duration.mjs";
import * as $timestamp from "../../gleam_time/gleam/time/timestamp.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import { toList } from "../../gleam_stdlib/gleam.mjs";
import { TickContext } from "./tick.mjs";
import {
  InputState,
  KeyboardState,
  MouseState,
  GamepadState,
  TouchState,
  TouchPoint,
  // Key variants
  KeyA, KeyB, KeyC, KeyD, KeyE, KeyF, KeyG, KeyH, KeyI, KeyJ, KeyK, KeyL, KeyM,
  KeyN, KeyO, KeyP, KeyQ, KeyR, KeyS, KeyT, KeyU, KeyV, KeyW, KeyX, KeyY, KeyZ,
  Digit0, Digit1, Digit2, Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9,
  ArrowUp, ArrowDown, ArrowLeft, ArrowRight,
  ShiftLeft, ShiftRight, ControlLeft, ControlRight, AltLeft, AltRight, MetaLeft, MetaRight,
  Space, Enter, Escape, Tab, Backspace, Delete,
  F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
  Other,
  // Mouse button variants
  LeftButton, MiddleButton, RightButton, BackButton, ForwardButton,
  // Gamepad button variants
  ButtonA, ButtonB, ButtonX, ButtonY, LeftBumper, RightBumper, LeftTrigger, RightTrigger,
  ButtonBack, ButtonStart, LeftStickButton, RightStickButton,
  DpadUp, DpadDown, DpadLeft, DpadRight, ButtonHome,
  // Gamepad axis variants
  LeftStickX, LeftStickY, RightStickX, RightStickY, LeftTriggerAxis, RightTriggerAxis,
} from "./input.mjs";

// Map of scene_id -> handler function
const subscriptions = new Map();

// Input state tracking
let inputState = {
  // Keyboard
  keysPressed: new Set(),
  keysPressedLastFrame: new Set(),

  // Mouse
  mouseX: 0,
  mouseY: 0,
  mouseDeltaX: 0,
  mouseDeltaY: 0,
  mouseButtons: new Set(),
  mouseButtonsLastFrame: new Set(),
  wheelDelta: 0,

  // Pointer lock
  pointerLocked: false,

  // Touch
  activeTouches: new Map(), // id -> { x, y, force }
  touchesLastFrame: new Map(),

  // Gamepads (polled each frame)
  gamepadsLastFrame: new Map(), // index -> Set of pressed buttons
};

// Convert JS key code to Gleam Key type
function codeToKey(code) {
  switch (code) {
    // Letters
    case "KeyA": return new KeyA();
    case "KeyB": return new KeyB();
    case "KeyC": return new KeyC();
    case "KeyD": return new KeyD();
    case "KeyE": return new KeyE();
    case "KeyF": return new KeyF();
    case "KeyG": return new KeyG();
    case "KeyH": return new KeyH();
    case "KeyI": return new KeyI();
    case "KeyJ": return new KeyJ();
    case "KeyK": return new KeyK();
    case "KeyL": return new KeyL();
    case "KeyM": return new KeyM();
    case "KeyN": return new KeyN();
    case "KeyO": return new KeyO();
    case "KeyP": return new KeyP();
    case "KeyQ": return new KeyQ();
    case "KeyR": return new KeyR();
    case "KeyS": return new KeyS();
    case "KeyT": return new KeyT();
    case "KeyU": return new KeyU();
    case "KeyV": return new KeyV();
    case "KeyW": return new KeyW();
    case "KeyX": return new KeyX();
    case "KeyY": return new KeyY();
    case "KeyZ": return new KeyZ();
    // Numbers
    case "Digit0": return new Digit0();
    case "Digit1": return new Digit1();
    case "Digit2": return new Digit2();
    case "Digit3": return new Digit3();
    case "Digit4": return new Digit4();
    case "Digit5": return new Digit5();
    case "Digit6": return new Digit6();
    case "Digit7": return new Digit7();
    case "Digit8": return new Digit8();
    case "Digit9": return new Digit9();
    // Arrow keys
    case "ArrowUp": return new ArrowUp();
    case "ArrowDown": return new ArrowDown();
    case "ArrowLeft": return new ArrowLeft();
    case "ArrowRight": return new ArrowRight();
    // Modifiers
    case "ShiftLeft": return new ShiftLeft();
    case "ShiftRight": return new ShiftRight();
    case "ControlLeft": return new ControlLeft();
    case "ControlRight": return new ControlRight();
    case "AltLeft": return new AltLeft();
    case "AltRight": return new AltRight();
    case "MetaLeft": return new MetaLeft();
    case "MetaRight": return new MetaRight();
    // Common keys
    case "Space": return new Space();
    case "Enter": return new Enter();
    case "Escape": return new Escape();
    case "Tab": return new Tab();
    case "Backspace": return new Backspace();
    case "Delete": return new Delete();
    // Function keys
    case "F1": return new F1();
    case "F2": return new F2();
    case "F3": return new F3();
    case "F4": return new F4();
    case "F5": return new F5();
    case "F6": return new F6();
    case "F7": return new F7();
    case "F8": return new F8();
    case "F9": return new F9();
    case "F10": return new F10();
    case "F11": return new F11();
    case "F12": return new F12();
    default: return new Other(code);
  }
}

// Convert mouse button index to Gleam MouseButton type
function buttonToMouseButton(button) {
  switch (button) {
    case 0: return new LeftButton();
    case 1: return new MiddleButton();
    case 2: return new RightButton();
    case 3: return new BackButton();
    case 4: return new ForwardButton();
    default: return new LeftButton();
  }
}

// Convert gamepad button index to Gleam GamepadButton type
function indexToGamepadButton(index) {
  switch (index) {
    case 0: return new ButtonA();
    case 1: return new ButtonB();
    case 2: return new ButtonX();
    case 3: return new ButtonY();
    case 4: return new LeftBumper();
    case 5: return new RightBumper();
    case 6: return new LeftTrigger();
    case 7: return new RightTrigger();
    case 8: return new ButtonBack();
    case 9: return new ButtonStart();
    case 10: return new LeftStickButton();
    case 11: return new RightStickButton();
    case 12: return new DpadUp();
    case 13: return new DpadDown();
    case 14: return new DpadLeft();
    case 15: return new DpadRight();
    case 16: return new ButtonHome();
    default: return new ButtonA();
  }
}

// Convert gamepad axis index to Gleam GamepadAxis type
function indexToGamepadAxis(index) {
  switch (index) {
    case 0: return new LeftStickX();
    case 1: return new LeftStickY();
    case 2: return new RightStickX();
    case 3: return new RightStickY();
    default: return new LeftStickX();
  }
}

// Set up input event listeners (called once)
let listenersInitialized = false;

function initInputListeners() {
  if (listenersInitialized) return;
  listenersInitialized = true;

  // Keyboard events
  window.addEventListener("keydown", (e) => {
    inputState.keysPressed.add(e.code);
  });

  window.addEventListener("keyup", (e) => {
    inputState.keysPressed.delete(e.code);
  });

  // Mouse events
  window.addEventListener("mousemove", (e) => {
    inputState.mouseX = e.clientX;
    inputState.mouseY = e.clientY;
    inputState.mouseDeltaX += e.movementX;
    inputState.mouseDeltaY += e.movementY;
  });

  window.addEventListener("mousedown", (e) => {
    inputState.mouseButtons.add(e.button);
  });

  window.addEventListener("mouseup", (e) => {
    inputState.mouseButtons.delete(e.button);
  });

  window.addEventListener("wheel", (e) => {
    // Normalize wheel delta: positive = scroll up
    inputState.wheelDelta -= e.deltaY > 0 ? 1 : e.deltaY < 0 ? -1 : 0;
  });

  // Context menu (right-click) - prevent default in games
  window.addEventListener("contextmenu", (e) => {
    // Only prevent if we have active subscriptions (game is running)
    if (subscriptions.size > 0) {
      e.preventDefault();
    }
  });

  // Pointer lock change
  document.addEventListener("pointerlockchange", () => {
    inputState.pointerLocked = document.pointerLockElement !== null;
  });

  // Touch events
  window.addEventListener("touchstart", (e) => {
    for (const touch of e.changedTouches) {
      inputState.activeTouches.set(touch.identifier, {
        x: touch.clientX,
        y: touch.clientY,
        force: touch.force || 0,
      });
    }
  });

  window.addEventListener("touchmove", (e) => {
    for (const touch of e.changedTouches) {
      inputState.activeTouches.set(touch.identifier, {
        x: touch.clientX,
        y: touch.clientY,
        force: touch.force || 0,
      });
    }
  });

  window.addEventListener("touchend", (e) => {
    for (const touch of e.changedTouches) {
      inputState.activeTouches.delete(touch.identifier);
    }
  });

  window.addEventListener("touchcancel", (e) => {
    for (const touch of e.changedTouches) {
      inputState.activeTouches.delete(touch.identifier);
    }
  });

  // Handle window blur - clear all keys to prevent stuck keys
  window.addEventListener("blur", () => {
    inputState.keysPressed.clear();
    inputState.mouseButtons.clear();
  });
}

// Build Gleam InputState from current JS state
function buildInputState() {
  // Keyboard state
  let pressedKeys = $set.new$();
  let justPressedKeys = $set.new$();
  let justReleasedKeys = $set.new$();

  for (const code of inputState.keysPressed) {
    const key = codeToKey(code);
    pressedKeys = $set.insert(pressedKeys, key);
    if (!inputState.keysPressedLastFrame.has(code)) {
      justPressedKeys = $set.insert(justPressedKeys, key);
    }
  }

  for (const code of inputState.keysPressedLastFrame) {
    if (!inputState.keysPressed.has(code)) {
      const key = codeToKey(code);
      justReleasedKeys = $set.insert(justReleasedKeys, key);
    }
  }

  const keyboardState = new KeyboardState(
    pressedKeys,
    justPressedKeys,
    justReleasedKeys
  );

  // Mouse state
  let mouseButtonsSet = $set.new$();
  let mouseJustPressed = $set.new$();
  let mouseJustReleased = $set.new$();

  for (const button of inputState.mouseButtons) {
    const mb = buttonToMouseButton(button);
    mouseButtonsSet = $set.insert(mouseButtonsSet, mb);
    if (!inputState.mouseButtonsLastFrame.has(button)) {
      mouseJustPressed = $set.insert(mouseJustPressed, mb);
    }
  }

  for (const button of inputState.mouseButtonsLastFrame) {
    if (!inputState.mouseButtons.has(button)) {
      const mb = buttonToMouseButton(button);
      mouseJustReleased = $set.insert(mouseJustReleased, mb);
    }
  }

  const mouseState = new MouseState(
    inputState.mouseX,
    inputState.mouseY,
    inputState.mouseDeltaX,
    inputState.mouseDeltaY,
    mouseButtonsSet,
    mouseJustPressed,
    mouseJustReleased,
    inputState.wheelDelta
  );

  // Gamepad state
  let gamepadsDict = $dict.new$();
  const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];

  for (let i = 0; i < gamepads.length; i++) {
    const gp = gamepads[i];
    if (!gp) continue;

    let buttonsSet = $set.new$();
    let justPressedBtns = $set.new$();
    let justReleasedBtns = $set.new$();
    let axesDict = $dict.new$();

    // Get previous button state
    const prevButtons = inputState.gamepadsLastFrame.get(i) || new Set();
    const currentButtons = new Set();

    // Process buttons
    for (let j = 0; j < gp.buttons.length && j < 17; j++) {
      if (gp.buttons[j].pressed) {
        const btn = indexToGamepadButton(j);
        buttonsSet = $set.insert(buttonsSet, btn);
        currentButtons.add(j);

        if (!prevButtons.has(j)) {
          justPressedBtns = $set.insert(justPressedBtns, btn);
        }
      }
    }

    // Check for just released
    for (const j of prevButtons) {
      if (!currentButtons.has(j)) {
        const btn = indexToGamepadButton(j);
        justReleasedBtns = $set.insert(justReleasedBtns, btn);
      }
    }

    // Store current buttons for next frame
    inputState.gamepadsLastFrame.set(i, currentButtons);

    // Process axes
    for (let j = 0; j < Math.min(gp.axes.length, 4); j++) {
      const axis = indexToGamepadAxis(j);
      axesDict = $dict.insert(axesDict, axis, gp.axes[j]);
    }

    // Add trigger axes (buttons 6 and 7 as analog values)
    if (gp.buttons.length > 6) {
      axesDict = $dict.insert(axesDict, new LeftTriggerAxis(), gp.buttons[6].value);
    }
    if (gp.buttons.length > 7) {
      axesDict = $dict.insert(axesDict, new RightTriggerAxis(), gp.buttons[7].value);
    }

    const gamepadState = new GamepadState(
      i,
      gp.connected,
      buttonsSet,
      justPressedBtns,
      justReleasedBtns,
      axesDict
    );

    gamepadsDict = $dict.insert(gamepadsDict, i, gamepadState);
  }

  // Touch state
  const touches = [];
  const justStarted = [];
  const justEnded = [];

  for (const [id, touch] of inputState.activeTouches) {
    touches.push(new TouchPoint(id, touch.x, touch.y, touch.force));

    if (!inputState.touchesLastFrame.has(id)) {
      justStarted.push(new TouchPoint(id, touch.x, touch.y, touch.force));
    }
  }

  for (const [id, touch] of inputState.touchesLastFrame) {
    if (!inputState.activeTouches.has(id)) {
      justEnded.push(new TouchPoint(id, touch.x, touch.y, touch.force));
    }
  }

  const touchState = new TouchState(
    toList(touches),
    toList(justStarted),
    toList(justEnded)
  );

  return new InputState(
    keyboardState,
    mouseState,
    gamepadsDict,
    touchState,
    inputState.pointerLocked
  );
}

// Update last frame state (called after building InputState)
function updateLastFrameState() {
  inputState.keysPressedLastFrame = new Set(inputState.keysPressed);
  inputState.mouseButtonsLastFrame = new Set(inputState.mouseButtons);
  inputState.touchesLastFrame = new Map(inputState.activeTouches);

  // Reset per-frame values
  inputState.mouseDeltaX = 0;
  inputState.mouseDeltaY = 0;
  inputState.wheelDelta = 0;
}

/**
 * Subscribe to tick updates for a scene.
 * @param {string} sceneId - Scene identifier (use "" for global)
 * @param {function} handler - Called with TickContext on each frame
 */
export function subscribeToTicks(sceneId, handler) {
  initInputListeners();
  subscriptions.set(sceneId, handler);
}

/**
 * Unsubscribe from tick updates for a scene.
 * @param {string} sceneId - Scene identifier
 */
export function unsubscribeFromTicks(sceneId) {
  subscriptions.delete(sceneId);
}

/**
 * Broadcast a tick to all subscribers.
 * Called by the render loop with the delta time since last frame.
 * @param {string} sceneId - Scene that is ticking
 * @param {number} deltaMs - Milliseconds since last frame
 */
export function broadcastTick(sceneId, deltaMs) {
  // Create typed Duration and Timestamp using gleam_time constructors
  const deltaTime = $duration.milliseconds(deltaMs);
  const timestamp = $timestamp.system_time();

  // Build input state
  const input = buildInputState();

  const ctx = new TickContext(deltaTime, timestamp, input);

  // Call scene-specific handler if exists
  const sceneHandler = subscriptions.get(sceneId);
  if (sceneHandler) {
    sceneHandler(ctx);
  }

  // Also call global handler (empty string key) if exists and different
  const globalHandler = subscriptions.get("");
  if (globalHandler && globalHandler !== sceneHandler) {
    globalHandler(ctx);
  }

  // Update last frame state after all handlers have run
  updateLastFrameState();
}

/**
 * Request pointer lock on the canvas element.
 * Call this from an effect in response to user interaction.
 * @param {Element} element - The element to lock pointer to
 */
export function requestPointerLock(element) {
  if (element && element.requestPointerLock) {
    element.requestPointerLock();
  }
}

/**
 * Exit pointer lock.
 */
export function exitPointerLock() {
  if (document.exitPointerLock) {
    document.exitPointerLock();
  }
}
