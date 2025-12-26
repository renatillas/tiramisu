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
import { Some } from '../gleam_stdlib/gleam/option.mjs';

/**
 * GameRuntime - Encapsulates all game loop state
 * Similar to Lustre's Runtime class
 */
export class GameRuntime {
  constructor(state, prevNode, context, rendererState, _, inputManager) {
    this.state = state;
    this.prevNode = prevNode;
    this.context = context;
    this.rendererState = rendererState;
    this.inputManager = inputManager; // Gleam InputManager instance

    // Timing for delta_time calculation
    this.lastTime = performance.now();

    // Message queue as JavaScript array (not Gleam list!)
    this.shouldQueue = false;
    this.queue = [];  // Always start with empty JS array

    // Dispatch function bound to this instance
    this.dispatch = (msg) => {
      if (this.shouldQueue) {
        this.queue.push(msg);
      } else {
        // When not in a processing cycle, just queue for next frame
        this.queue.push(msg);
      }
    };
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
 * Create a new game runtime instance
 */
export function createRuntime(state, prevNode, context, rendererState, messageQueue, inputManager) {
  return new GameRuntime(state, prevNode, context, rendererState, messageQueue, inputManager);
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
  const canvasWidth = canvas.clientWidth;
  const canvasHeight = canvas.clientHeight;

  // Return a FrameData record matching the Gleam type
  return FrameData$FrameData(
    runtime.getQueuedMessages(),
    runtime.getState(),
    runtime.getPrevNode(),
    runtime.getContext(),
    runtime.getRendererState(),
    deltaTimeMs,
    inputState,
    canvasWidth,
    canvasHeight,
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

  // Clear the entire canvas first
  renderer.setScissorTest(false);
  renderer.clear();

  // Render main cameras (fullscreen, no viewport)
  for (const cameraData of mainCameras) {
    const [id, threeCamera, _viewport, postprocessing] = cameraData;
    renderCameraWithPostprocessing(renderer, scene, threeCamera, postprocessing, id, null);
  }

  // Render viewport cameras
  for (const cameraData of viewportCameras) {
    const [id, threeCamera, viewport, postprocessing] = cameraData;
    if (viewport instanceof Some) {
      const vp = viewport[0];
      renderCameraWithPostprocessing(renderer, scene, threeCamera, postprocessing, id, vp);
    }
  }
}

/**
 * Render a single camera with optional postprocessing
 */
function renderCameraWithPostprocessing(renderer, scene, camera, postprocessing, cameraId, viewport) {
  // Get renderer size (the actual WebGL buffer size)
  const size = renderer.getSize(new THREE.Vector2());

  if (viewport) {
    // Set viewport for this camera
    const x = viewport.x;
    const y = size.y - viewport.y - viewport.height; // Flip Y for WebGL
    renderer.setViewport(x, y, viewport.width, viewport.height);
    renderer.setScissor(x, y, viewport.width, viewport.height);
    renderer.setScissorTest(true);
  } else {
    // Fullscreen - reset to full renderer size
    renderer.setViewport(0, 0, size.x, size.y);
    renderer.setScissorTest(false);
  }

  if (postprocessing instanceof Some) {
    const ppConfig = postprocessing[0];

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
      composer.setSize(viewport.width, viewport.height);
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
