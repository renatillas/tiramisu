// Immutable game loop with effect system
import * as THREE from 'three';
import * as INPUT_CAPTURE from './tiramisu/ffi/input_capture.mjs';
import * as RENDERER from './tiramisu/ffi/renderer.mjs';
import * as SCENE_GLEAM from './tiramisu/scene.mjs';
import * as GLEAM from '../../gleam_stdlib/gleam.mjs';
import * as PHYSICS from './tiramisu/ffi/physics.mjs';
import * as DEBUG from './tiramisu/ffi/debug.mjs';
import * as CAMERA from './tiramisu/ffi/camera.mjs';

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
  INPUT_CAPTURE.initKeyboard();
  INPUT_CAPTURE.initMouse(canvas);
  INPUT_CAPTURE.initTouch(canvas);
}

/**
 * Apply initial scene nodes to Three.js scene
 */
export function applyInitialScene(scene, nodes) {
  // Use diff with empty previous scene to generate proper patches
  // This ensures hierarchy is respected and parent_id is set correctly
  const emptyList = GLEAM.toList([]);
  const patches = SCENE_GLEAM.diff(emptyList, nodes);
  RENDERER.applyPatches(scene, patches);
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
  DEBUG.startPerformanceMonitoring();

  // Run initial effect
  runEffect(effect, dispatch);

  let lastTime = performance.now();

  function gameLoop() {
    const currentTime = performance.now();
    const deltaTime = (currentTime - lastTime) / 1000; // Convert to seconds
    lastTime = currentTime;

    // Capture input state snapshot
    const inputState = INPUT_CAPTURE.captureInputState();

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
    const patches = SCENE_GLEAM.diff(currentNodes, newNodes);
    RENDERER.applyPatches(scene, patches);

    currentNodes = newNodes;

    // Step physics simulation
    PHYSICS.stepWorld(deltaTime);

    // Sync physics transforms to Three.js objects
    RENDERER.syncPhysicsTransforms();

    // Update animation mixers
    RENDERER.updateMixers(deltaTime);

    // Clear the entire canvas first
    renderer.setScissorTest(false);
    renderer.clear();

    // Get canvas dimensions
    const canvas = renderer.domElement;
    const canvasWidth = canvas.clientWidth;
    const canvasHeight = canvas.clientHeight;

    // Render main camera (active camera without viewport)
    const activeCamera = CAMERA.getCamera();
    if (activeCamera) {
      renderer.setScissorTest(false);
      renderer.setViewport(0, 0, canvasWidth, canvasHeight);
      renderer.render(scene, activeCamera);
    } else {
      console.warn('[Game] No active camera found. Add a Camera scene node with active=True.');
    }

    // Render viewport cameras (picture-in-picture)
    const viewportCameras = RENDERER.getCamerasWithViewports();
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
    DEBUG.updatePerformanceStats(deltaTime);
    DEBUG.setRenderStats(renderer.info);

    // Clear per-frame input state
    INPUT_CAPTURE.clearInputFrameState();

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
