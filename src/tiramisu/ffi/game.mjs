// Immutable game loop with effect system
import * as THREE from 'three';
import * as inputCapture from './input_capture.mjs';
import { applyPatches, applyPatch, createGeometry, createMaterial, createLight, applyTransform, updateMixers, syncPhysicsTransforms } from './renderer.mjs';
import { AddNode, diff } from '../scene.mjs';
import { updateCamera as updateInternalCamera, getInternalCamera } from './camera.mjs';
import { toList } from '../../../gleam_stdlib/gleam.mjs';
import { stepWorld, getBodyTransform } from './physics.mjs';
import { startPerformanceMonitoring, updatePerformanceStats, setRenderStats } from './debug.mjs';

/**
 * Create a Three.js scene with background color
 */
export function createScene(background) {
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
  inputCapture.initKeyboard();
  inputCapture.initMouse(canvas);
  inputCapture.initTouch(canvas);
}

/**
 * Apply initial scene nodes to Three.js scene
 */
export function applyInitialScene(scene, nodes) {
  // Use diff with empty previous scene to generate proper patches
  // This ensures hierarchy is respected and parent_id is set correctly
  const emptyList = toList([]);
  const patches = diff(emptyList, nodes);
  applyPatches(scene, patches);
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
  let currentState = state;
  let currentNodes = prevNodes;
  let messageQueue = [];

  // Dispatch function for effects
  const dispatch = (msg) => {
    messageQueue.push(msg);
  };

  // Initialize internal camera with initial config
  updateInternalCamera(context.camera);

  // Initialize performance monitoring
  startPerformanceMonitoring();

  // Run initial effect
  runEffect(effect, dispatch);

  let lastTime = performance.now();

  function gameLoop() {
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

    // Diff and patch
    const patches = diff(currentNodes, newNodes);
    applyPatches(scene, patches);

    currentNodes = newNodes;

    // Step physics simulation
    stepWorld(deltaTime);

    // Sync physics transforms to Three.js objects
    syncPhysicsTransforms();

    // Update animation mixers
    updateMixers(deltaTime);

    // Update internal Three.js camera from immutable config
    updateInternalCamera(newContext.camera);

    // Render with internal Three.js camera
    renderer.render(scene, getInternalCamera());

    // Update performance stats
    updatePerformanceStats(deltaTime);
    setRenderStats(renderer.info);

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
