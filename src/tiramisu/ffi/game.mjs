// Immutable game loop with effect system
import * as THREE from 'three';
import * as inputCapture from './input_capture.mjs';
import * as keyboardFFI from '../input/ffi/keyboard.mjs';
import * as mouseFFI from '../input/ffi/mouse.mjs';
import * as touchFFI from '../input/ffi/touch.mjs';
import { applyPatches, applyPatch, createGeometry, createMaterial, createLight, applyTransform } from '../scene/ffi/renderer.mjs';
import { diff } from '../../tiramisu/scene/diff.mjs';
import { AddNode } from '../../tiramisu/scene/diff.mjs';

/**
 * Create a Three.js scene with background color
 */
export function createScene(background) {
  console.log('[Tiramisu] Creating scene with background:', background);
  const scene = new THREE.Scene();
  scene.background = new THREE.Color(background);
  return scene;
}

/**
 * Append renderer canvas to DOM
 */
export function appendToDom(canvas) {
  document.body.appendChild(canvas);
}

/**
 * Initialize all input systems
 */
export function initializeInputSystems(canvas) {
  keyboardFFI.initKeyboard();
  mouseFFI.initMouse(canvas);
  touchFFI.initTouch(canvas);
}

/**
 * Apply initial scene nodes to Three.js scene
 */
export function applyInitialScene(scene, nodes) {
  // Convert Gleam list to JS array
  const nodeArray = gleamListToArray(nodes);

  console.log('[Tiramisu] Applying initial scene with', nodeArray.length, 'nodes');

  // Convert nodes to AddNode patches and apply them
  // This ensures objects are added to the objectCache for later updates
  nodeArray.forEach(node => {
    const patch = new AddNode(node.id, node, undefined);
    applyPatch(scene, patch);
  });
}

/**
 * Start the immutable game loop with effect system
 */
export function startLoop(
  state,
  prevNodes,
  effect,
  context,
  scene,
  renderer,
  camera,
  update,
  view,
) {
  console.log('[Tiramisu] Starting game loop');
  console.log('[Tiramisu] Initial state:', state);
  console.log('[Tiramisu] Scene:', scene);
  console.log('[Tiramisu] Camera:', camera);
  console.log('[Tiramisu] Camera position:', camera.position);
  console.log('[Tiramisu] Camera rotation:', camera.rotation);

  let currentState = state;
  let currentNodes = prevNodes;
  let messageQueue = [];

  // Dispatch function for effects
  const dispatch = (msg) => {
    messageQueue.push(msg);
  };

  // Run initial effect
  runEffect(effect, dispatch);

  let lastTime = performance.now();
  let frameCount = 0;

  function gameLoop() {
    frameCount++;
    if (frameCount <= 3) {
      console.log('[Tiramisu] Game loop frame', frameCount);
    }
    const currentTime = performance.now();
    const deltaTime = (currentTime - lastTime) / 1000; // Convert to seconds
    lastTime = currentTime;

    // Capture input state snapshot
    const inputState = inputCapture.captureInputState();

    // Update context with new delta and input
    const newContext = {
      ...context,
      delta_time: deltaTime,
      input: inputState,
    };

    // Process all messages in queue
    while (messageQueue.length > 0) {
      const msg = messageQueue.shift();
      const [newState, newEffect] = update(currentState, msg, newContext);
      currentState = newState;
      runEffect(newEffect, dispatch);
    }

    // Call update function (for frame-based logic)
    // In Lustre-style, update is typically message-driven, but we keep this for delta time updates

    // Generate new scene nodes
    const newNodes = view(currentState);

    if (frameCount <= 3) {
      console.log('[Tiramisu] New nodes:', newNodes);
    }

    // Diff and patch
    const patches = diff(currentNodes, newNodes);

    if (frameCount <= 3) {
      console.log('[Tiramisu] Patches:', patches);
    }

    applyPatches(scene, patches);
    currentNodes = newNodes;

    // Render
    renderer.render(scene, camera);

    if (frameCount === 1) {
      console.log('[Tiramisu] First render complete. Scene children:', scene.children.length);
    }

    // Clear per-frame input state
    inputCapture.clearInputFrameState();

    // Continue loop
    requestAnimationFrame(gameLoop);
  }

  // Start the loop
  requestAnimationFrame(gameLoop);
}

/**
 * Run an effect and dispatch any resulting messages
 */
function runEffect(effect, dispatch) {
  // Effects have a `perform` property that is a function taking a dispatch callback
  if (effect && effect.perform) {
    effect.perform(dispatch);
  }
}

// --- Helper Functions ---

function gleamListToArray(gleamList) {
  const result = [];
  let current = gleamList;

  while (current && current.head !== undefined) {
    result.push(current.head);
    current = current.tail;
  }

  return result;
}
