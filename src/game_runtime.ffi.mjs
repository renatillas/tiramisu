// Game runtime - Manages game loop state without globals
//
// This follows Lustre's pattern: the Runtime class holds all mutable state
// as instance variables, and the dispatch function is a closure that captures
// the instance. No global variables!

import * as THREE from 'three';
import { toList } from '../gleam_stdlib/gleam.mjs';
import * as SCENE from './tiramisu/scene.mjs';
import { FrameData$FrameData } from './tiramisu.mjs'
import { createEffectComposer } from './postprocessing.ffi.mjs';
import { Option$isSome, Option$Some$0 } from '../gleam_stdlib/gleam/option.mjs';
import { Vec2$Vec2 } from '../vec/vec/vec2.mjs';

/**
 * GameRuntime - Encapsulates all game loop state
 * Similar to Lustre's Runtime class
 */
export class GameRuntime {
  constructor(bridge, bridgeWrapper, state, prevNode, context, rendererState, inputManager) {
    // UI Bridge for Lustre integration (may be null)
    this.bridge = bridge;
    // Wrapper function to convert bridge messages to game messages (may be null)
    this.bridgeWrapper = bridgeWrapper;

    this.state = state;
    this.prevNode = prevNode;
    this.context = context;
    this.rendererState = rendererState;
    this.inputManager = inputManager; // Gleam InputManager instance

    // Timing for delta_time calculation
    this.lastTime = performance.now();

    // Message queue as JavaScript array (not Gleam list!)
    this.queue = [];

    // Dispatch function bound to this instance
    this.dispatch = (msg) => {
      this.queue.push(msg);
    };

    // Register Tiramisu's dispatch with the bridge (if bridge exists)
    // The dispatch is composed with the wrapper to convert bridge_msg -> game_msg
    if (this.bridge && this.bridgeWrapper) {
      const wrappedDispatch = (bridgeMsg) => {
        const gameMsg = this.bridgeWrapper(bridgeMsg);
        this.dispatch(gameMsg);
      };
      this.bridge.setTiramisuDispatch(wrappedDispatch);
    }
  }

  /**
   * Get the current game state
   */
  getState() {
    return this.state;
  }

  /**
   * Get the previous scene node
   */
  getPrevNode() {
    return this.prevNode;
  }

  /**
   * Get the current context
   */
  getContext() {
    return this.context;
  }

  /**
   * Get the renderer state
   */
  getRendererState() {
    return this.rendererState;
  }

  /**
   * Get queued messages and clear the queue
   * Returns a Gleam list, not a JavaScript array
   */
  getQueuedMessages() {
    const messages = this.queue;
    this.queue = [];
    return toList(messages);  // Convert JS array to Gleam list
  }

  /**
   * Update the runtime state after processing a frame
   */
  updateState(state, prevNode, context, rendererState) {
    this.state = state;
    this.prevNode = prevNode;
    this.context = context;
    this.rendererState = rendererState;
  }

  /**
   * Get the input manager
   */
  getInputManager() {
    return this.inputManager;
  }

  /**
   * Update the input manager
   */
  updateInputManager(inputManager) {
    this.inputManager = inputManager;
  }
}

/**
 * Create a new game runtime instance with optional UI bridge
 * @param {Option<Bridge>} bridgeOption - Gleam Option type containing the bridge
 * @param {Option<fn(bridge_msg) -> msg>} wrapperOption - Gleam Option containing the wrapper function
 */
export function createRuntime(bridgeOption, wrapperOption, state, prevNode, context, rendererState, inputManager) {
  // Extract bridge and wrapper from Gleam Option types
  const bridge = Option$isSome(bridgeOption) ? Option$Some$0(bridgeOption) : null;
  const bridgeWrapper = Option$isSome(wrapperOption) ? Option$Some$0(wrapperOption) : null;
  return new GameRuntime(bridge, bridgeWrapper, state, prevNode, context, rendererState, inputManager);
}

/**
 * Get the dispatch function from a runtime
 */
export function getRuntimeDispatch(runtime) {
  return runtime.dispatch;
}

/**
 * Process a frame: return data Gleam needs to process messages
 */
export function processFrame(runtime, _, __, timestamp, captureInputState, ___) {
  // Calculate delta time
  const deltaTimeMs = timestamp - runtime.lastTime;
  runtime.lastTime = timestamp;

  // Capture input state using Gleam function
  const inputState = captureInputState(runtime.getInputManager());

  // Get canvas dimensions
  const renderer = SCENE.get_renderer(runtime.getRendererState());
  const canvas = renderer.domElement;
  const canvasSize = Vec2$Vec2(canvas.clientWidth, canvas.clientHeight);

  // Return a FrameData record matching the Gleam type
  return FrameData$FrameData(
    runtime.getQueuedMessages(),
    runtime.getState(),
    runtime.getPrevNode(),
    runtime.getContext(),
    runtime.getRendererState(),
    deltaTimeMs,
    inputState,
    canvasSize,
  );
}

/**
 * Update runtime state after processing a frame
 */
export function updateRuntimeState(runtime, state, prevNode, context, rendererState) {
  runtime.updateState(state, prevNode, context, rendererState);
}

/**
 * Clear input frame state (just-pressed, just-released, etc.)
 * Call this after processing the frame
 */
export function clearRuntimeInputFrameState(runtime, clearInputFrameState) {
  const clearedManager = clearInputFrameState(runtime.getInputManager());
  runtime.updateInputManager(clearedManager);
}

/**
 * Update the runtime's input manager
 * Called by input event handlers
 */
export function updateRuntimeInputManager(runtime, inputManager) {
  runtime.updateInputManager(inputManager);
}

/**
 * Get the runtime's input manager
 * Called by input event handlers to get current state
 */
export function getRuntimeInputManager(runtime) {
  return runtime.getInputManager();
}


// Cache for EffectComposers per camera ID
const composerCache = new Map();

/**
 * Render cameras to the canvas
 * Handles main camera and viewport cameras with postprocessing
 */
export function renderCameras(rendererState, mainCameras, viewportCameras) {
  const renderer = SCENE.get_renderer(rendererState);
  const scene = SCENE.get_scene(rendererState);
  const css2dRenderer = getCSS2DRenderer(rendererState);

  // Clear the entire canvas first
  renderer.setScissorTest(false);
  renderer.clear();

  // Render main cameras (fullscreen, no viewport)
  for (const cameraData of mainCameras) {
    const [id, threeCamera, _viewport, postprocessing] = cameraData;
    renderCameraWithPostprocessing(renderer, scene, threeCamera, postprocessing, id, null);

    // Render CSS2D labels for this camera
    if (css2dRenderer) {
      css2dRenderer.render(scene, threeCamera);
    }
  }

  // Render viewport cameras
  for (const cameraData of viewportCameras) {
    const [id, threeCamera, viewport, postprocessing] = cameraData;
    if (Option$isSome(viewport)) {
      const vp = Option$Some$0(viewport);
      renderCameraWithPostprocessing(renderer, scene, threeCamera, postprocessing, id, vp);

      // Render CSS2D labels for viewport camera
      if (css2dRenderer) {
        css2dRenderer.render(scene, threeCamera);
      }
    }
  }
}

/**
 * Get CSS2D renderer from renderer state (handles Gleam Option type)
 */
function getCSS2DRenderer(rendererState) {
  const css2dOpt = rendererState.css2d_renderer;
  if (Option$isSome(css2dOpt)) {
    return Option$Some$0(css2dOpt);
  }
  return null;
}

/**
 * Render a single camera with optional postprocessing
 */
function renderCameraWithPostprocessing(renderer, scene, camera, postprocessing, cameraId, viewport) {
  // Get renderer size (the actual WebGL buffer size)
  const size = renderer.getSize(new THREE.Vector2());

  if (viewport) {
    // Set viewport for this camera - viewport uses position: Vec2, size: Vec2
    const x = viewport.position.x;
    const y = size.y - viewport.position.y - viewport.size.y; // Flip Y for WebGL
    renderer.setViewport(x, y, viewport.size.x, viewport.size.y);
    renderer.setScissor(x, y, viewport.size.x, viewport.size.y);
    renderer.setScissorTest(true);
  } else {
    // Fullscreen - reset to full renderer size
    renderer.setViewport(0, 0, size.x, size.y);
    renderer.setScissorTest(false);
  }

  if (Option$isSome(postprocessing)) {
    const ppConfig = Option$Some$0(postprocessing);

    // Get or create composer for this camera
    let composer = composerCache.get(cameraId);

    // Check if we need to recreate the composer (config changed)
    if (!composer || composer._ppConfig !== ppConfig) {
      if (composer) {
        composer.dispose();
      }
      composer = createEffectComposer(renderer, scene, camera, ppConfig);
      composer._ppConfig = ppConfig; // Store config for comparison
      composerCache.set(cameraId, composer);
    }

    // Update composer size
    if (viewport) {
      composer.setSize(viewport.size.x, viewport.size.y);
    } else {
      composer.setSize(size.x, size.y);
    }

    composer.render();
  } else {
    // No postprocessing, render directly
    renderer.render(scene, camera);
  }
}


/**
 * Clean up composer cache for removed cameras
 */
export function cleanupComposerCache(activeCameraIds) {
  const activeSet = new Set(activeCameraIds);
  for (const [id, composer] of composerCache) {
    if (!activeSet.has(id)) {
      composer.dispose();
      composerCache.delete(id);
    }
  }
}
