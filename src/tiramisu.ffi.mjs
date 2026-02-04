// FFI implementation for Tiramisu subscriptions

import { Ok as Result$Ok } from "./gleam.mjs";
import { milliseconds } from "../gleam_time/gleam/time/duration.mjs";

// Global state for tick subscription
let tickSubscriptionActive = false;
let tickHandler = null;
let tickDispatch = null;
let previousTimestamp = null;
let inputState = null;

// Global state for Three.js rendering (set by platform)
let globalRenderer = null;
let globalScene = null;

// Initialize input state (will be populated by input manager)
function initializeInputState() {
  return {
    // Keyboard state
    keys_pressed: new Set(),
    keys_just_pressed: new Set(),
    keys_just_released: new Set(),

    // Mouse state
    mouse_position: { x: 0, y: 0 },
    mouse_buttons_pressed: new Set(),
    mouse_buttons_just_pressed: new Set(),
    mouse_buttons_just_released: new Set(),
    mouse_delta: { x: 0, y: 0 },
    mouse_wheel_delta: 0,

    // Touch state
    touches: [],

    // Gamepad state
    gamepads: []
  };
}

// Subscribe to game ticks using requestAnimationFrame
export function subscribe_to_ticks(handler, dispatch) {
  if (tickSubscriptionActive) {
    console.warn("Tick subscription already active");
    return;
  }

  tickHandler = handler;
  tickDispatch = dispatch;
  tickSubscriptionActive = true;
  previousTimestamp = null;

  // Initialize input state if not already done
  if (!inputState) {
    inputState = initializeInputState();
    setupInputListeners();
  }

  // Start the animation loop
  function tick(timestamp) {
    if (!tickSubscriptionActive) return;

    // Calculate delta time
    const delta = previousTimestamp === null ? 16.67 : timestamp - previousTimestamp;
    previousTimestamp = timestamp;

    // Get canvas dimensions (from first canvas element for now)
    const canvas = document.querySelector('canvas');
    const canvasSize = canvas
      ? { x: canvas.width, y: canvas.height }
      : { x: 800, y: 600 };

    // Create Context
    // Use public API to create Duration from milliseconds
    const deltaMs = Math.floor(delta);

    const context = {
      delta_time: milliseconds(deltaMs), // Use public duration.milliseconds() API
      input: createGleamInputState(),
      canvas_size: canvasSize,
      physics_world: { isNone: true } // Option(PhysicsWorld) - None for now
    };

    // Dispatch the message (this triggers Lustre update + vdom reconciliation)
    const msg = handler(context);
    dispatch(msg);

    // Clear "just pressed/released" state for next frame
    clearFrameInputState();

    // Render the Three.js scene AFTER the reconciler has updated objects
    if (globalRenderer && globalScene) {
      const camera = findActiveCamera(globalScene);
      if (camera) {
        globalRenderer.render(globalScene, camera);
      }
    }

    // Schedule next frame
    requestAnimationFrame(tick);
  }

  // Start first tick
  requestAnimationFrame(tick);
}

// Convert JavaScript input state to Gleam InputState format
function createGleamInputState() {
  // For now, return a minimal input state
  // This should match the InputState type from tiramisu/input.gleam
  return {
    keys_pressed: Array.from(inputState.keys_pressed),
    keys_just_pressed: Array.from(inputState.keys_just_pressed),
    keys_just_released: Array.from(inputState.keys_just_released),
    mouse_position: inputState.mouse_position,
    mouse_buttons_pressed: Array.from(inputState.mouse_buttons_pressed),
    mouse_buttons_just_pressed: Array.from(inputState.mouse_buttons_just_pressed),
    mouse_buttons_just_released: Array.from(inputState.mouse_buttons_just_released),
    mouse_delta: inputState.mouse_delta,
    mouse_wheel_delta: inputState.mouse_wheel_delta,
    touches: inputState.touches,
    gamepads: inputState.gamepads
  };
}

// Setup global input listeners
function setupInputListeners() {
  // Keyboard events
  window.addEventListener('keydown', (e) => {
    if (!inputState.keys_pressed.has(e.key)) {
      inputState.keys_just_pressed.add(e.key);
    }
    inputState.keys_pressed.add(e.key);
  });

  window.addEventListener('keyup', (e) => {
    inputState.keys_pressed.delete(e.key);
    inputState.keys_just_released.add(e.key);
  });

  // Mouse events
  window.addEventListener('mousemove', (e) => {
    const prevX = inputState.mouse_position.x;
    const prevY = inputState.mouse_position.y;
    inputState.mouse_position = { x: e.clientX, y: e.clientY };
    inputState.mouse_delta = {
      x: e.clientX - prevX,
      y: e.clientY - prevY
    };
  });

  window.addEventListener('mousedown', (e) => {
    if (!inputState.mouse_buttons_pressed.has(e.button)) {
      inputState.mouse_buttons_just_pressed.add(e.button);
    }
    inputState.mouse_buttons_pressed.add(e.button);
  });

  window.addEventListener('mouseup', (e) => {
    inputState.mouse_buttons_pressed.delete(e.button);
    inputState.mouse_buttons_just_released.add(e.button);
  });

  window.addEventListener('wheel', (e) => {
    inputState.mouse_wheel_delta += e.deltaY;
  });

  // Touch events
  window.addEventListener('touchstart', (e) => {
    inputState.touches = Array.from(e.touches).map(t => ({
      id: t.identifier,
      x: t.clientX,
      y: t.clientY
    }));
  });

  window.addEventListener('touchmove', (e) => {
    inputState.touches = Array.from(e.touches).map(t => ({
      id: t.identifier,
      x: t.clientX,
      y: t.clientY
    }));
  });

  window.addEventListener('touchend', (e) => {
    inputState.touches = Array.from(e.touches).map(t => ({
      id: t.identifier,
      x: t.clientX,
      y: t.clientY
    }));
  });
}

// Clear "just pressed/released" state after each frame
function clearFrameInputState() {
  inputState.keys_just_pressed.clear();
  inputState.keys_just_released.clear();
  inputState.mouse_buttons_just_pressed.clear();
  inputState.mouse_buttons_just_released.clear();
  inputState.mouse_delta = { x: 0, y: 0 };
  inputState.mouse_wheel_delta = 0;
}

// Stop the tick subscription
export function unsubscribe_from_ticks() {
  tickSubscriptionActive = false;
  tickHandler = null;
  tickDispatch = null;
  previousTimestamp = null;
}

// Set the renderer and scene for rendering (called by platform)
export function setRendererAndScene(renderer, scene) {
  globalRenderer = renderer;
  globalScene = scene;
}

// Find the active camera in the scene
function findActiveCamera(scene) {
  let activeCamera = null;

  // Search for a camera marked as active
  scene.traverse((object) => {
    if (object.isCamera && object.userData?.["data-active"] === "true") {
      activeCamera = object;
    }
  });

  // If no active camera found, use the first camera
  if (!activeCamera) {
    scene.traverse((object) => {
      if (object.isCamera && !activeCamera) {
        activeCamera = object;
      }
    });
  }

  return activeCamera;
}
