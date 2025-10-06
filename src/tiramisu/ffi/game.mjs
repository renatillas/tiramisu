// Immutable game loop with effect system
import * as THREE from 'three';
import * as inputCapture from './input_capture.mjs';
import { applyPatches, applyPatch, createGeometry, createMaterial, createLight, applyTransform, updateMixers, syncPhysicsTransforms, getCamerasWithViewports } from './renderer.mjs';
import { AddNode, diff } from '../scene.mjs';
import { toList } from '../../../gleam_stdlib/gleam.mjs';
import { stepWorld, getBodyTransform } from './physics.mjs';
import { startPerformanceMonitoring, updatePerformanceStats, setRenderStats } from './debug.mjs';
import { setCamera, getCamera } from './effects.mjs';

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
 * Initialize camera from Gleam camera config (for backwards compatibility)
 */
export function initializeCamera(camera) {
  // This is now deprecated - cameras should be scene nodes
  // But kept for backwards compatibility with existing examples
  console.warn('[Game] initializeCamera is deprecated. Use Camera scene nodes instead.');
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

  // Note: Camera is now initialized either via:
  // 1. initializeCamera() call (backwards compatibility) before startLoop
  // 2. Camera scene nodes in the initial scene (applyInitialScene already handled)

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

    // Clear the entire canvas first
    renderer.setScissorTest(false);
    renderer.clear();

    // Get canvas dimensions
    const canvas = renderer.domElement;
    const canvasWidth = canvas.clientWidth;
    const canvasHeight = canvas.clientHeight;

    // Render main camera (active camera without viewport)
    const activeCamera = getCamera();
    if (activeCamera) {
      renderer.setScissorTest(false);
      renderer.setViewport(0, 0, canvasWidth, canvasHeight);
      renderer.render(scene, activeCamera);
    } else {
      console.warn('[Game] No active camera found. Add a Camera scene node with active=True.');
    }

    // Render viewport cameras (picture-in-picture)
    const viewportCameras = getCamerasWithViewports();
    if (viewportCameras.length > 0) {
      renderer.setScissorTest(true);
      for (const { camera, viewport } of viewportCameras) {
        const [x, y, width, height] = viewport;
        renderer.setViewport(x, y, width, height);
        renderer.setScissor(x, y, width, height);
        renderer.render(scene, camera);
      }
      renderer.setScissorTest(false);
    }

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
