// Immutable game loop with effect system
import * as THREE from 'three';
import { CSS2DRenderer } from 'three/addons/renderers/CSS2DRenderer.js';
import { CSS3DRenderer } from 'three/addons/renderers/CSS3DRenderer.js';
import * as SCENE from './tiramisu/scene.mjs';
import * as GLEAM from '../gleam_stdlib/gleam.mjs';
import * as INPUT from './tiramisu/input.mjs';
import { getActiveCamera, setSceneBackgroundColor, setSceneBackgroundTexture, setSceneBackgroundCubeTexture, loadTexture, loadEquirectangularTexture, loadCubeTexture } from './threejs.ffi.mjs';
import { Background$isColor, Background$isTexture, Background$isEquirectangularTexture, Background$isCubeTexture, Background$Color$0, Background$Texture$0, Background$EquirectangularTexture$0, Background$CubeTexture$0 } from './tiramisu/background.mjs';

// ============================================================================
// MANAGER CLASSES - Encapsulate mutable state
// ============================================================================

/**
 * @typedef {Object} ButtonState
 * @property {boolean} pressed - Is button currently pressed
 * @property {boolean} justPressed - Was button just pressed this frame
 * @property {boolean} justReleased - Was button just released this frame
 */

/**
 * @typedef {Object} KeyboardState
 * @property {Set<string>} pressed - Set of currently pressed key codes
 * @property {Set<string>} justPressed - Set of keys pressed this frame
 * @property {Set<string>} justReleased - Set of keys released this frame
 */

/**
 * @typedef {Object} MouseState
 * @property {number} x - Current mouse X position
 * @property {number} y - Current mouse Y position
 * @property {number} lastX - Previous frame mouse X position
 * @property {number} lastY - Previous frame mouse Y position
 * @property {number} deltaX - Mouse X movement this frame
 * @property {number} deltaY - Mouse Y movement this frame
 * @property {number} wheelDelta - Mouse wheel delta this frame
 * @property {ButtonState} leftButton - Left mouse button state
 * @property {ButtonState} middleButton - Middle mouse button state
 * @property {ButtonState} rightButton - Right mouse button state
 */

/**
 * @typedef {Object} TouchData
 * @property {number} x - Touch X position
 * @property {number} y - Touch Y position
 */

/**
 * @typedef {Object} TouchState
 * @property {Map<number, TouchData>} active - Currently active touches
 * @property {Map<number, TouchData>} justStarted - Touches started this frame
 * @property {Map<number, TouchData>} justEnded - Touches ended this frame
 */

/**
 * @typedef {Object} GamepadState
 * @property {boolean} connected - Is any gamepad connected
 */

// Helper classes for Gleam input state (match compiled output structure)
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

/**
 * InputManager - Manages all input state (keyboard, mouse, touch, gamepad)
 * Encapsulates mutable input state and provides immutable snapshots for Gleam
 */
export class InputManager {
  /**
   * Create a new InputManager
   * @param {HTMLCanvasElement} canvas - The canvas element to attach input listeners to
   */
  constructor(canvas) {
    /**
     * @type {KeyboardState}
     */
    this.keyboard = {
      pressed: new Set(),
      justPressed: new Set(),
      justReleased: new Set(),
    };

    /**
     * @type {MouseState}
     */
    this.mouse = {
      x: 0.0,
      y: 0.0,
      lastX: 0.0,
      lastY: 0.0,
      deltaX: 0.0,
      deltaY: 0.0,
      wheelDelta: 0.0,
      leftButton: { pressed: false, justPressed: false, justReleased: false },
      middleButton: { pressed: false, justPressed: false, justReleased: false },
      rightButton: { pressed: false, justPressed: false, justReleased: false },
    };

    /**
     * @type {TouchState}
     */
    this.touch = {
      active: new Map(),
      justStarted: new Map(),
      justEnded: new Map(),
    };

    /**
     * @type {GamepadState}
     */
    this.gamepad = {
      connected: false,
    };

    /**
     * Store canvas reference for cleanup
     * @type {HTMLCanvasElement | null}
     */
    this.canvas = null;

    /**
     * Store all listener functions for removal
     * @type {Object}
     */
    this.listeners = {};

    // Initialize event listeners
    this._initEventListeners(canvas);
  }

  /**
   * Initialize all input event listeners
   * @private
   * @param {HTMLCanvasElement} canvas - The canvas element to attach listeners to
   */
  _initEventListeners(canvas) {
    // Store canvas reference for cleanup
    this.canvas = canvas;

    // Keyboard listeners
    this.listeners.keydown = (e) => {
      if (!this.keyboard.pressed.has(e.code)) {
        this.keyboard.justPressed.add(e.code);
      }
      this.keyboard.pressed.add(e.code);
    };
    window.addEventListener('keydown', this.listeners.keydown);

    this.listeners.keyup = (e) => {
      this.keyboard.pressed.delete(e.code);
      this.keyboard.justReleased.add(e.code);
    };
    window.addEventListener('keyup', this.listeners.keyup);

    // Gamepad connection listeners
    this.listeners.gamepadconnected = () => {
      this.gamepad.connected = true;
    };
    window.addEventListener('gamepadconnected', this.listeners.gamepadconnected);

    this.listeners.gamepaddisconnected = () => {
      const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
      this.gamepad.connected = Array.from(gamepads).some(gp => gp && gp.connected);
    };
    window.addEventListener('gamepaddisconnected', this.listeners.gamepaddisconnected);

    // Mouse listeners
    this.listeners.mousemove = (e) => {
      // When pointer lock is active, use movementX/Y directly
      // Otherwise, calculate delta from position change
      const isPointerLocked = document.pointerLockElement === canvas ||
        document.webkitPointerLockElement === canvas ||
        document.mozPointerLockElement === canvas;

      if (isPointerLocked) {
        // Use movement deltas directly from pointer lock API
        this.mouse.deltaX = e.movementX || e.webkitMovementX || e.mozMovementX || 0;
        this.mouse.deltaY = e.movementY || e.webkitMovementY || e.mozMovementY || 0;
      } else {
        // Calculate delta from position change
        const rect = canvas.getBoundingClientRect();
        this.mouse.x = e.clientX - rect.left;
        this.mouse.y = e.clientY - rect.top;
        this.mouse.deltaX = this.mouse.x - this.mouse.lastX;
        this.mouse.deltaY = this.mouse.y - this.mouse.lastY;
      }
    };
    canvas.addEventListener('mousemove', this.listeners.mousemove);

    this.listeners.mousedown = (e) => {
      if (e.button === 0) {
        if (!this.mouse.leftButton.pressed) this.mouse.leftButton.justPressed = true;
        this.mouse.leftButton.pressed = true;
      } else if (e.button === 1) {
        if (!this.mouse.middleButton.pressed) this.mouse.middleButton.justPressed = true;
        this.mouse.middleButton.pressed = true;
      } else if (e.button === 2) {
        if (!this.mouse.rightButton.pressed) this.mouse.rightButton.justPressed = true;
        this.mouse.rightButton.pressed = true;
      }
    };
    canvas.addEventListener('mousedown', this.listeners.mousedown);

    this.listeners.mouseup = (e) => {
      if (e.button === 0) {
        this.mouse.leftButton.pressed = false;
        this.mouse.leftButton.justReleased = true;
      } else if (e.button === 1) {
        this.mouse.middleButton.pressed = false;
        this.mouse.middleButton.justReleased = true;
      } else if (e.button === 2) {
        this.mouse.rightButton.pressed = false;
        this.mouse.rightButton.justReleased = true;
      }
    };
    canvas.addEventListener('mouseup', this.listeners.mouseup);

    this.listeners.contextmenu = (e) => {
      e.preventDefault();
    };
    canvas.addEventListener('contextmenu', this.listeners.contextmenu);

    this.listeners.wheel = (e) => {
      this.mouse.wheelDelta = e.deltaY;
    };
    canvas.addEventListener('wheel', this.listeners.wheel);

    // Touch listeners
    this.listeners.touchstart = (e) => {
      e.preventDefault();
      for (let i = 0; i < e.changedTouches.length; i++) {
        const touch = e.changedTouches[i];
        const rect = canvas.getBoundingClientRect();
        const x = Math.max(0, Math.min(rect.width, touch.clientX - rect.left));
        const y = Math.max(0, Math.min(rect.height, touch.clientY - rect.top));
        this.touch.active.set(touch.identifier, { x, y });
        this.touch.justStarted.set(touch.identifier, { x, y });
      }
    };
    canvas.addEventListener('touchstart', this.listeners.touchstart);

    this.listeners.touchmove = (e) => {
      e.preventDefault();
      for (let i = 0; i < e.changedTouches.length; i++) {
        const touch = e.changedTouches[i];
        const rect = canvas.getBoundingClientRect();
        const x = Math.max(0, Math.min(rect.width, touch.clientX - rect.left));
        const y = Math.max(0, Math.min(rect.height, touch.clientY - rect.top));
        this.touch.active.set(touch.identifier, { x, y });
      }
    };
    canvas.addEventListener('touchmove', this.listeners.touchmove);

    this.listeners.touchend = (e) => {
      e.preventDefault();
      for (let i = 0; i < e.changedTouches.length; i++) {
        const touch = e.changedTouches[i];
        const data = this.touch.active.get(touch.identifier);
        if (data) {
          this.touch.justEnded.set(touch.identifier, data);
          this.touch.active.delete(touch.identifier);
        }
      }
    };
    canvas.addEventListener('touchend', this.listeners.touchend);

    this.listeners.touchcancel = (e) => {
      e.preventDefault();
      for (let i = 0; i < e.changedTouches.length; i++) {
        const touch = e.changedTouches[i];
        this.touch.active.delete(touch.identifier);
        this.touch.justEnded.delete(touch.identifier);
      }
    };
    canvas.addEventListener('touchcancel', this.listeners.touchcancel);
  }

  /**
   * Capture current input state as immutable snapshot
   * Returns data compatible with Gleam InputState type
   * @returns {Object} Gleam InputState object with keyboard, mouse, gamepad, and touch state
   */
  captureState() {
    // Keyboard state
    const pressedKeys = Array.from(this.keyboard.pressed);
    const justPressedKeys = Array.from(this.keyboard.justPressed);
    const justReleasedKeys = Array.from(this.keyboard.justReleased);

    const keyboardState = new KeyboardState(
      GLEAM.toList(pressedKeys),
      GLEAM.toList(justPressedKeys),
      GLEAM.toList(justReleasedKeys)
    );

    // Mouse state
    const mouseState = new MouseState(
      this.mouse.x,
      this.mouse.y,
      this.mouse.deltaX,
      this.mouse.deltaY,
      this.mouse.wheelDelta,
      new INPUT.ButtonState(
        this.mouse.leftButton.pressed,
        this.mouse.leftButton.justPressed,
        this.mouse.leftButton.justReleased
      ),
      new INPUT.ButtonState(
        this.mouse.middleButton.pressed,
        this.mouse.middleButton.justPressed,
        this.mouse.middleButton.justReleased
      ),
      new INPUT.ButtonState(
        this.mouse.rightButton.pressed,
        this.mouse.rightButton.justPressed,
        this.mouse.rightButton.justReleased
      )
    );

    // Gamepad state (support up to 4 gamepads)
    const gamepadStates = [];
    if (this.gamepad.connected) {
      const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
      for (let i = 0; i < 4; i++) {
        const gamepad = gamepads[i];
        if (gamepad && gamepad.connected) {
          const buttons = Array.from(gamepad.buttons).map(b => b.value);
          const axes = Array.from(gamepad.axes);
          gamepadStates.push(new INPUT.GamepadState(
            true,
            GLEAM.toList(buttons),
            GLEAM.toList(axes)
          ));
        } else {
          gamepadStates.push(new INPUT.GamepadState(
            false,
            new GLEAM.Empty(),
            new GLEAM.Empty()
          ));
        }
      }
    } else {
      for (let i = 0; i < 4; i++) {
        gamepadStates.push(new INPUT.GamepadState(
          false,
          new GLEAM.Empty(),
          new GLEAM.Empty()
        ));
      }
    }

    // Touch state
    const touchList = [];
    this.touch.active.forEach((data, id) => {
      touchList.push(new INPUT.Touch(id, data.x, data.y));
    });

    const touchJustStartedList = [];
    this.touch.justStarted.forEach((data, id) => {
      touchJustStartedList.push(new INPUT.Touch(id, data.x, data.y));
    });

    const touchJustEndedList = [];
    this.touch.justEnded.forEach((data, id) => {
      touchJustEndedList.push(new INPUT.Touch(id, data.x, data.y));
    });

    const touchState = new INPUT.TouchState(
      GLEAM.toList(touchList),
      GLEAM.toList(touchJustStartedList),
      GLEAM.toList(touchJustEndedList)
    );

    return new InputState(
      keyboardState,
      mouseState,
      GLEAM.toList(gamepadStates),
      touchState
    );
  }

  /**
   * Clear per-frame input state
   * Should be called at the end of each frame to reset just-pressed/just-released states
   * @returns {void}
   */
  clearFrameState() {
    // Keyboard
    this.keyboard.justPressed.clear();
    this.keyboard.justReleased.clear();

    // Mouse
    this.mouse.lastX = this.mouse.x;
    this.mouse.lastY = this.mouse.y;
    this.mouse.deltaX = 0.0;
    this.mouse.deltaY = 0.0;
    this.mouse.wheelDelta = 0.0;
    this.mouse.leftButton.justPressed = false;
    this.mouse.leftButton.justReleased = false;
    this.mouse.middleButton.justPressed = false;
    this.mouse.middleButton.justReleased = false;
    this.mouse.rightButton.justPressed = false;
    this.mouse.rightButton.justReleased = false;

    // Touch
    this.touch.justStarted.clear();
    this.touch.justEnded.clear();
  }

  /**
   * Cleanup and remove event listeners
   * Called when the manager is no longer needed
   * @returns {void}
   */
  destroy() {
    if (!this.canvas || !this.listeners) {
      return;
    }

    // Remove window listeners (keyboard, gamepad)
    if (this.listeners.keydown) {
      window.removeEventListener('keydown', this.listeners.keydown);
    }
    if (this.listeners.keyup) {
      window.removeEventListener('keyup', this.listeners.keyup);
    }
    if (this.listeners.gamepadconnected) {
      window.removeEventListener('gamepadconnected', this.listeners.gamepadconnected);
    }
    if (this.listeners.gamepaddisconnected) {
      window.removeEventListener('gamepaddisconnected', this.listeners.gamepaddisconnected);
    }

    // Remove canvas listeners (mouse, touch)
    if (this.listeners.mousemove) {
      this.canvas.removeEventListener('mousemove', this.listeners.mousemove);
    }
    if (this.listeners.mousedown) {
      this.canvas.removeEventListener('mousedown', this.listeners.mousedown);
    }
    if (this.listeners.mouseup) {
      this.canvas.removeEventListener('mouseup', this.listeners.mouseup);
    }
    if (this.listeners.contextmenu) {
      this.canvas.removeEventListener('contextmenu', this.listeners.contextmenu);
    }
    if (this.listeners.wheel) {
      this.canvas.removeEventListener('wheel', this.listeners.wheel);
    }
    if (this.listeners.touchstart) {
      this.canvas.removeEventListener('touchstart', this.listeners.touchstart);
    }
    if (this.listeners.touchmove) {
      this.canvas.removeEventListener('touchmove', this.listeners.touchmove);
    }
    if (this.listeners.touchend) {
      this.canvas.removeEventListener('touchend', this.listeners.touchend);
    }
    if (this.listeners.touchcancel) {
      this.canvas.removeEventListener('touchcancel', this.listeners.touchcancel);
    }

    // Clear references
    this.listeners = {};
    this.canvas = null;
  }
}

// FFI helper functions for Gleam to call InputManager methods

/**
 * Capture input state from an InputManager instance
 * @param {InputManager} manager - The InputManager instance
 * @returns {Object} Gleam InputState object
 */
export function inputManagerCaptureState(manager) {
  return manager.captureState();
}

/**
 * Clear frame state on an InputManager instance
 * @param {InputManager} manager - The InputManager instance
 * @returns {void}
 */
export function inputManagerClearFrameState(manager) {
  manager.clearFrameState();
}

/**
 * Destroy an InputManager instance
 * @param {InputManager} manager - The InputManager instance
 * @returns {void}
 */
export function inputManagerDestroy(manager) {
  manager.destroy();
}

/**
 * Create a new InputManager instance (wrapper for Gleam FFI)
 * Uses singleton pattern to prevent multiple instances and listener leaks
 * @param {HTMLCanvasElement} canvas - The canvas element to attach input listeners to
 * @returns {InputManager} InputManager instance (singleton)
 */
export function createInputManager(canvas) {
  // Destroy existing instance if it exists
  if (typeof window !== 'undefined' && window.__tiramisu && window.__tiramisu.inputManager) {
    window.__tiramisu.inputManager.destroy();
    window.__tiramisu.inputManager = null;
  }

  // Create new instance and store as singleton
  const manager = new InputManager(canvas);

  if (typeof window !== 'undefined' && window.__tiramisu) {
    window.__tiramisu.inputManager = manager;
  }

  return manager;
}

// AudioManager has been migrated to Gleam (see tiramisu/internal/audio_manager.gleam)
// Audio state is now managed immutably in Gleam, with only pure Three.js API calls via FFI

// ============================================================================
// BACKGROUND SETUP
// ============================================================================

/**
 * Set background on Three.js scene based on Gleam Background type
 * Uses Gleam-generated variant checking functions to safely inspect the type
 * without relying on constructor names (which may be minified in production).
 *
 * @param {THREE.Scene} scene - Three.js scene object
 * @param {Object} background - Gleam Background ADT (Color, Texture, EquirectangularTexture, or CubeTexture)
 */
export async function setBackground(scene, background) {
  if (Background$isColor(background)) {
    // Background.Color(Int) - hex color
    const color = Background$Color$0(background);
    setSceneBackgroundColor(scene, color);
  } else if (Background$isTexture(background)) {
    // Background.Texture(String) - texture URL
    const url = Background$Texture$0(background);
    try {
      const texture = await loadTexture(url);
      setSceneBackgroundTexture(scene, texture);
    } catch (error) {
      console.error('[Tiramisu] Failed to load background texture:', error);
      // Fallback to black color
      setSceneBackgroundColor(scene, 0x000000);
    }
  } else if (Background$isEquirectangularTexture(background)) {
    // Background.EquirectangularTexture(String) - spherical 360° texture URL
    const url = Background$EquirectangularTexture$0(background);
    try {
      const texture = await loadEquirectangularTexture(url);
      console.log(texture)
      setSceneBackgroundTexture(scene, texture);
    } catch (error) {
      console.error('[Tiramisu] Failed to load equirectangular texture:', error);
      // Fallback to black color
      setSceneBackgroundColor(scene, 0x000000);
    }
  } else if (Background$isCubeTexture(background)) {
    // Background.CubeTexture(List(String)) - 6 face URLs
    const urlsList = Background$CubeTexture$0(background);
    // Convert Gleam list to JavaScript array
    const urls = urlsList.toArray();
    try {
      const cubeTexture = await loadCubeTexture(urls);
      setSceneBackgroundCubeTexture(scene, cubeTexture);
    } catch (error) {
      console.error('[Tiramisu] Failed to load cube texture:', error);
      // Fallback to black color
      setSceneBackgroundColor(scene, 0x000000);
    }
  } else {
    console.warn('[Tiramisu] Unknown background type');
    // Fallback to black color
    setSceneBackgroundColor(scene, 0x000000);
  }
}

// ============================================================================
// ASSET LOADING - Audio loader and batch loading utilities
// ============================================================================

import * as ASSETS_GLEAM from './tiramisu/asset.mjs';
import {
  AssetType$isModelAsset, AssetType$isTextureAsset, AssetType$isAudioAsset,
  AssetType$isSTLAsset, AssetType$isFBXAsset, AssetType$isFontAsset, AssetType$isOBJAsset,
  AssetType$ModelAsset$url, AssetType$TextureAsset$url, AssetType$AudioAsset$url,
  AssetType$STLAsset$url, AssetType$FBXAsset$url, AssetType$FBXAsset$texture_path,
  AssetType$FontAsset$url, AssetType$OBJAsset$obj_url, AssetType$OBJAsset$mtl_url
} from './tiramisu/asset.mjs';
import * as DICT from '../gleam_stdlib/gleam/dict.mjs';

/**
 * Load an audio file and return an AudioBuffer
 * Uses a standalone loader - for single asset loading from Gleam
 * @param {string} url - URL of the audio file
 * @returns {Promise<Result<AudioBuffer, string>>}
 */
export function loadAudio(url) {
  return new Promise((resolve) => {
    const loader = new THREE.AudioLoader();

    loader.load(
      url,
      // Success
      (audioBuffer) => {
        console.log(`[Tiramisu] Audio loaded: ${url}`);
        resolve(new GLEAM.Ok(audioBuffer));
      },
      // Progress (optional)
      undefined,
      // Error
      (error) => {
        console.error(`[Tiramisu] Audio load failed: ${url}`, error);
        resolve(new GLEAM.Error(error.message || 'Failed to load audio'));
      }
    );
  });
}

// Wrapper functions for Gleam types
function LoadProgress(loaded, total, current_url) {
  return new ASSETS_GLEAM.LoadProgress(loaded, total, current_url);
}

function BatchLoadResult(cache, errors) {
  return new ASSETS_GLEAM.BatchLoadResult(cache, errors);
}

function AssetCache(assetsDict) {
  // assetsDict is already a Gleam Dict with url -> LoadedAsset
  // We need to wrap each LoadedAsset in a CacheEntry
  let cacheDict = DICT.new$();

  // Use fold to iterate through the dict and wrap entries
  cacheDict = DICT.fold(
    assetsDict,
    cacheDict,
    (acc, url, loadedAsset) => {
      // Create CacheEntry manually matching the compiled structure
      const cacheEntry = Object.create(Object.getPrototypeOf({}));
      cacheEntry.asset = loadedAsset;
      cacheEntry.last_accessed = Date.now();

      return DICT.insert(acc, url, cacheEntry);
    }
  );

  // Create CacheConfig manually
  const cacheConfig = Object.create(Object.getPrototypeOf({}));
  cacheConfig.max_size = 100;
  cacheConfig.current_time = Date.now();

  // Create AssetCache manually
  const assetCache = Object.create(Object.getPrototypeOf({}));
  assetCache.asset = cacheDict;
  assetCache.config = cacheConfig;

  return assetCache;
}

function AssetLoadError(url, reason) {
  return new ASSETS_GLEAM.AssetLoadError(url, reason);
}

/**
 * Load multiple assets with progress tracking
 * @param {Array} assets - JavaScript Array of AssetType (converted from Gleam List)
 * @param {Function} onProgress - Progress callback
 * @returns {Promise<BatchLoadResult>}
 */
export async function loadBatch(assets, onProgress) {
  const total = assets.length;
  let loaded = 0;
  let loadedAssetsDict = DICT.new$(); // Create Gleam Dict
  const loadedResults = []; // Track loaded assets to insert into dict later
  const errors = [];

  if (total === 0) {
    // Empty batch, return immediately
    return Promise.resolve(
      BatchLoadResult(
        AssetCache(DICT.new$()),
        GLEAM.toList([]) // Convert empty array to Gleam List
      )
    );
  }

  // Create promises for each asset
  const promises = assets.map(async (asset, index) => {
    const url = getAssetUrl(asset);

    // Report initial progress
    if (index === 0) {
      onProgress(LoadProgress(0, total, url));
    }

    return loadAssetByType(asset)
      .then((result) => {
        loaded++;

        // Store result
        if (result.isOk()) {
          loadedResults.push({ url, asset: result[0] });
        } else {
          errors.push(AssetLoadError(url, result[0]));
        }

        // Report progress
        onProgress(LoadProgress(loaded, total, url));

        return result;
      })
      .catch((error) => {
        loaded++;
        const errorMsg = error.message || 'Unknown error';
        errors.push(AssetLoadError(url, errorMsg));
        onProgress(LoadProgress(loaded, total, url));
        return new GLEAM.Error(errorMsg);
      });
  });

  // Wait for all assets to finish (success or failure)
  return Promise.all(promises).then(() => {
    // Build Gleam Dict from loaded results
    for (const { url, asset } of loadedResults) {
      loadedAssetsDict = DICT.insert(loadedAssetsDict, url, asset);
    }

    console.log(`[Tiramisu] Batch load complete: ${loadedResults.length}/${total} succeeded, ${errors.length} failed`);

    return BatchLoadResult(
      AssetCache(loadedAssetsDict),
      GLEAM.toList(errors) // Convert JavaScript array to Gleam List
    );
  });
}

/**
 * Get URL from AssetType
 */
function getAssetUrl(asset) {
  // AssetType variants: ModelAsset, TextureAsset, AudioAsset, STLAsset, FontAsset
  // Each has a 'url' field
  return asset.url;
}

/**
 * Load an asset based on its type
 * Uses Gleam-generated variant checking functions to safely inspect the type
 * without relying on constructor names (which may be minified in production).
 *
 * Note: This routing logic is duplicated from asset.gleam for performance
 * (avoids FFI round-trips during batch loading)
 */
async function loadAssetByType(asset) {
  if (AssetType$isModelAsset(asset)) {
    // Load GLTF model using safe wrapper
    const url = AssetType$ModelAsset$url(asset);
    return loadGLTFSafe(url).then(result => {
      if (result.isOk()) {
        return new GLEAM.Ok(ASSETS_GLEAM.loaded_model(result[0]));
      }
      return result;
    });
  } else if (AssetType$isTextureAsset(asset)) {
    // Load texture using safe wrapper
    const url = AssetType$TextureAsset$url(asset);
    return loadTextureSafe(url).then(result => {
      if (result.isOk()) {
        return new GLEAM.Ok(ASSETS_GLEAM.loaded_texture(result[0]));
      }
      return result;
    });
  } else if (AssetType$isAudioAsset(asset)) {
    // Load audio using standalone loader
    const url = AssetType$AudioAsset$url(asset);
    return loadAudio(url).then(result => {
      if (result.isOk()) {
        return new GLEAM.Ok(ASSETS_GLEAM.loaded_audio(result[0]));
      }
      return result;
    });
  } else if (AssetType$isSTLAsset(asset)) {
    // Load STL using safe wrapper
    const url = AssetType$STLAsset$url(asset);
    return loadSTLSafe(url).then(result => {
      if (result.isOk()) {
        return new GLEAM.Ok(ASSETS_GLEAM.loaded_stl(result[0]));
      }
      return result;
    });
  } else if (AssetType$isFBXAsset(asset)) {
    // Load FBX using safe wrapper - extract texture_path from asset
    const url = AssetType$FBXAsset$url(asset);
    const texturePathOption = AssetType$FBXAsset$texture_path(asset);
    // Extract value from Option(String) - Gleam Option is represented as {[0]: value} for Some(value) or null for None
    const texturePath = texturePathOption && texturePathOption[0] ? texturePathOption[0] : '';
    return loadFBXSafe(url, texturePath).then(result => {
      if (result.isOk()) {
        return new GLEAM.Ok(ASSETS_GLEAM.loaded_fbx(result[0]));
      }
      return result;
    });
  } else if (AssetType$isFontAsset(asset)) {
    // Load font using safe wrapper
    const url = AssetType$FontAsset$url(asset);
    return loadFontSafe(url).then(result => {
      if (result.isOk()) {
        return new GLEAM.Ok(ASSETS_GLEAM.loaded_font(result[0]));
      }
      return result;
    });
  } else {
    return Promise.resolve(new GLEAM.Error('Unknown asset type'));
  }
}

// ============================================================================
// ASSET LOADERS - Safe wrappers for Three.js loaders
// ============================================================================

/**
 * Safe texture loader that wraps Three.js loader with Result type
 * @param {string} url
 * @returns {Promise<Result<Texture, string>>}
 */
export async function loadTextureSafe(url) {
  const THREE_FFI = await import('./threejs.ffi.mjs');
  try {
    const texture = await THREE_FFI.loadTexture(url);
    return new GLEAM.Ok(texture);
  } catch (error) {
    return new GLEAM.Error(error.message || 'Failed to load texture');
  }
}

/**
 * Safe font loader that wraps Three.js FontLoader with Result type
 * @param {string} url
 * @returns {Promise<Result<Font, string>>}
 */
export async function loadFontSafe(url) {
  const THREE_FFI = await import('./threejs.ffi.mjs');
  try {
    const font = await THREE_FFI.loadFont(url);
    return new GLEAM.Ok(font);
  } catch (error) {
    return new GLEAM.Error(error.message || 'Failed to load font');
  }
}

/**
 * Safe GLTF loader that wraps Three.js loader with Result type
 * @param {string} url
 * @returns {Promise<Result<GLTFData, string>>}
 */
export async function loadGLTFSafe(url) {
  const THREE_FFI = await import('./threejs.ffi.mjs');
  try {
    const gltf = await THREE_FFI.loadGLTF(url);
    // Convert animations array to Gleam list and create GLTFData
    const animationsList = GLEAM.toList(gltf.animations);
    const data = new ASSETS_GLEAM.GLTFData(gltf.scene, animationsList);
    return new GLEAM.Ok(data);
  } catch (error) {
    return new GLEAM.Error(error.message || 'Failed to load GLTF');
  }
}

/**
 * Safe STL loader with validation and processing
 * @param {string} url
 * @returns {Promise<Result<BufferGeometry, string>>}
 */
export async function loadSTLSafe(url) {
  try {
    // Fetch the STL file
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`);
    }

    const arrayBuffer = await response.arrayBuffer();

    // Validate the file format
    const validation = validateSTLFile(arrayBuffer);
    if (!validation.valid) {
      throw new Error(validation.error || 'Invalid STL file');
    }

    // Parse using STLLoader
    const { STLLoader } = await import('three/addons/loaders/STLLoader.js');
    const loader = new STLLoader();
    const geometry = loader.parse(arrayBuffer);

    // Center the geometry
    geometry.computeBoundingBox();
    const center = geometry.boundingBox.getCenter(new THREE.Vector3());
    geometry.translate(-center.x, -center.y, -center.z);

    // Recompute normals for smooth shading
    geometry.deleteAttribute('normal');
    geometry.computeVertexNormals();

    return new GLEAM.Ok(geometry);
  } catch (error) {
    // Categorize errors
    if (error.message && error.message.includes('404')) {
      return new GLEAM.Error('File not found: ' + url);
    } else if (error.message && (error.message.includes('Invalid') || error.message.includes('corrupted'))) {
      return new GLEAM.Error(error.message);
    } else {
      return new GLEAM.Error(error.message || 'Failed to load STL');
    }
  }
}

/**
 * Validate STL file format
 * @param {ArrayBuffer} data
 * @returns {{format: string, valid: boolean, error?: string, triangleCount?: number}}
 */
function validateSTLFile(data) {
  // Check if it's ASCII STL
  const decoder = new TextDecoder('utf-8');
  const headerText = decoder.decode(data.slice(0, 80));

  if (headerText.toLowerCase().includes('solid')) {
    console.log('[STL Loader] Detected ASCII STL format');
    return { format: 'ascii', valid: true };
  }

  if (data.byteLength < 84) {
    return { format: 'binary', valid: false, error: 'File too small to be valid binary STL' };
  }

  const view = new DataView(data);
  const triangleCount = view.getUint32(80, true); // little-endian

  if (triangleCount > 100000000 || triangleCount === 0) {
    return {
      format: 'binary',
      valid: false,
      error: `Invalid triangle count: ${triangleCount}. File may be corrupted or not a valid STL.`
    };
  }

  return { format: 'binary', valid: true, triangleCount };
}

/**
 * Safe OBJ loader with MTL material support
 * @param {string} objUrl
 * @param {string} mtlUrl
 * @returns {Promise<Result<Object3D, string>>}
 */
export async function loadOBJSafe(objUrl, mtlUrl) {
  try {
    const { OBJLoader } = await import('three/addons/loaders/OBJLoader.js');
    const THREE = await import('three');

    const objLoader = new OBJLoader();

    // Helper function to load OBJ after materials are ready
    const loadOBJPromise = (materials) => {
      return new Promise((resolve, reject) => {
        if (materials) {
          objLoader.setMaterials(materials);
        }

        objLoader.load(
          objUrl,
          // Success
          (object) => {
            // Center the model
            const box = new THREE.Box3().setFromObject(object);
            const center = box.getCenter(new THREE.Vector3());
            object.position.sub(center);

            // Ensure all meshes have proper normals
            object.traverse((child) => {
              if (child.isMesh && !child.geometry.attributes.normal) {
                child.geometry.computeVertexNormals();
              }
            });

            resolve(object);
          },
          // Progress
          undefined,
          // Error
          (error) => reject(error)
        );
      });
    };

    // If MTL file is provided, load materials first
    if (mtlUrl && mtlUrl.trim() !== '') {
      try {
        const materials = await loadMTLWithTextures(mtlUrl, THREE);
        const object = await loadOBJPromise(materials);
        return new GLEAM.Ok(object);
      } catch (_error) {
        // If MTL loading fails, try loading OBJ without materials
        const object = await loadOBJPromise(null);
        return new GLEAM.Ok(object);
      }
    } else {
      // No MTL file, load OBJ directly
      const object = await loadOBJPromise(null);
      return new GLEAM.Ok(object);
    }
  } catch (error) {
    // Categorize errors
    if (error.message && error.message.includes('404')) {
      return new GLEAM.Error('File not found: ' + objUrl);
    } else if (error.message && (error.message.includes('Invalid') || error.message.includes('parse'))) {
      return new GLEAM.Error(error.message);
    } else {
      return new GLEAM.Error(error.message || 'Failed to load OBJ');
    }
  }
}

/**
 * Load MTL materials with all texture types
 * @param {string} mtlUrl
 * @param {THREE} THREE
 * @returns {Promise<MTLLoader.MaterialCreator>}
 */
async function loadMTLWithTextures(mtlUrl, THREE) {
  const { MTLLoader } = await import('three/addons/loaders/MTLLoader.js');

  return new Promise((resolve, reject) => {
    const loadingManager = new THREE.LoadingManager();
    const mtlLoader = new MTLLoader(loadingManager);

    // Extract base path for texture loading
    const basePath = mtlUrl.substring(0, mtlUrl.lastIndexOf('/') + 1);
    mtlLoader.setResourcePath(basePath);

    mtlLoader.load(
      mtlUrl,
      // Success
      (materials) => {
        materials.preload();

        // Load additional texture types not handled by MTLLoader
        setTimeout(() => {
          const textureLoader = new THREE.TextureLoader(loadingManager);

          // Helper to extract filename from MTL texture paths
          const extractFilename = (path) => {
            if (!path) return null;
            const parts = path.trim().split(/\s+/);
            return parts[parts.length - 1];
          };

          for (let matName in materials.materials) {
            const mat = materials.materials[matName];
            const info = materials.materialsInfo[matName];

            if (info) {
              // Load diffuse/color map
              if (info.map_kd && !mat.map) {
                mat.map = textureLoader.load(basePath + extractFilename(info.map_kd));
              }

              // Load normal map
              if (info.map_bump && !mat.normalMap) {
                mat.normalMap = textureLoader.load(basePath + extractFilename(info.map_bump));
              }

              // Load ambient occlusion map
              if (info.map_ka && !mat.aoMap) {
                mat.aoMap = textureLoader.load(basePath + extractFilename(info.map_ka));
                mat.aoMapIntensity = 1.0;
              }

              mat.needsUpdate = true;
            }
          }

          // Wait for all textures to load
          const checkTexturesLoaded = () => {
            if (loadingManager.itemsLoaded === loadingManager.itemsTotal) {
              resolve(materials);
            } else {
              setTimeout(checkTexturesLoaded, 100);
            }
          };

          setTimeout(checkTexturesLoaded, 100);
        }, 50);
      },
      // Progress
      undefined,
      // Error
      (error) => reject(error)
    );
  });
}

/**
 * Safe FBX loader with animations and texture fixing
 * @param {string} url
 * @param {string} texturePath - Optional path where textures are located (defaults to FBX file directory)
 * @returns {Promise<Result<FBXData, string>>}
 */
export async function loadFBXSafe(url, texturePath = '') {
  try {
    const { FBXLoader } = await import('three/addons/loaders/FBXLoader.js');
    const THREE = await import('three');

    return new Promise((resolve, reject) => {
      const loadingManager = new THREE.LoadingManager();

      // Track texture loading failures
      const failedTextures = new Map();

      loadingManager.onError = (url) => {
        console.warn('[FBX Loader] Failed to load:', url);
        failedTextures.set(url, true);
      };

      const fbxLoader = new FBXLoader(loadingManager);

      // Set texture path - if provided, use it; otherwise use FBX file directory
      const textureBasePath = texturePath.trim() !== ''
        ? texturePath
        : url.substring(0, url.lastIndexOf('/') + 1);

      fbxLoader.setResourcePath(textureBasePath);

      fbxLoader.load(
        url,
        // Success
        async (fbx) => {
          console.log('[FBX Loader] Loaded FBX:', url);
          console.log('[FBX Loader] Using texture base path:', textureBasePath);

          // Try to fix missing textures by searching for them
          const textureLoader = new THREE.TextureLoader();
          let texturesFixed = 0;

          fbx.traverse((child) => {
            if (child.isMesh) {
              const material = child.material;

              if (Array.isArray(material)) {
                // Handle multi-material meshes
                material.forEach(mat => fixMaterialTextures(mat, textureLoader, textureBasePath));
              } else if (material) {
                // Single material
                if (fixMaterialTextures(material, textureLoader, textureBasePath)) {
                  texturesFixed++;
                }
              }
            }
          });

          if (texturesFixed > 0) {
            console.log(`[FBX Loader] Fixed textures for ${texturesFixed} materials`);
          }

          // Center the model at origin
          const box = new THREE.Box3().setFromObject(fbx);
          const center = box.getCenter(new THREE.Vector3());
          const size = box.getSize(new THREE.Vector3());

          // Move all child meshes to center the model
          fbx.traverse((child) => {
            if (child.isMesh) {
              child.position.sub(center);
            }
          });

          console.log('[FBX Loader] Centered model at origin. Original center:', center);
          console.log('[FBX Loader] Model size (width x height x depth):', size);

          // Convert animations array to Gleam list and create FBXData
          const animationsList = GLEAM.toList(fbx.animations || []);
          const data = new ASSETS_GLEAM.FBXData(fbx, animationsList);
          resolve(new GLEAM.Ok(data));
        },
        // Progress
        undefined,
        // Error
        (error) => {
          // Categorize errors
          if (error.message && error.message.includes('404')) {
            reject(new GLEAM.Error('File not found: ' + url));
          } else if (error.message && (error.message.includes('Invalid') || error.message.includes('parse'))) {
            reject(new GLEAM.Error(error.message));
          } else {
            reject(new GLEAM.Error(error.message || 'Failed to load FBX'));
          }
        }
      );
    });
  } catch (error) {
    return new GLEAM.Error(error.message || 'Failed to initialize FBX loader');
  }
}

/**
 * Try to fix material textures by loading them from the texture base path
 * @param {THREE.Material} material
 * @param {THREE.TextureLoader} textureLoader
 * @param {string} textureBasePath
 * @returns {boolean} - True if any textures were fixed
 */
function fixMaterialTextures(material, textureLoader, textureBasePath) {
  let fixed = false;

  // Check if material has a map property but it's not loaded or is broken
  if (material.map && material.map.image === undefined) {
    console.log('[FBX Loader] Material has broken map, attempting to fix:', material.name);

    // Try to extract texture name from the map's source or userData
    let textureName = null;

    if (material.map.userData && material.map.userData.fileName) {
      textureName = material.map.userData.fileName;
    } else if (material.map.name) {
      textureName = material.map.name;
    }

    if (textureName) {
      // Remove any directory paths, just keep filename
      textureName = textureName.split('/').pop().split('\\').pop();

      console.log('[FBX Loader] Attempting to load texture:', textureBasePath + textureName);

      try {
        const newTexture = textureLoader.load(textureBasePath + textureName);
        material.map = newTexture;
        material.needsUpdate = true;
        fixed = true;
        console.log('[FBX Loader] Successfully loaded texture:', textureName);
      } catch (error) {
        console.warn('[FBX Loader] Failed to load texture:', textureName, error);
      }
    }
  }

  // Also check if there's no map at all - look in material name for hints
  if (!material.map && material.name) {
    console.log('[FBX Loader] Material has no map:', material.name);
    // Material name might give us a hint about which texture to use
    // This is very specific to the asset pack - you might need to adjust this
  }

  return fixed;
}

// ============================================================================
// ASSET DISPOSAL - Memory management for Three.js resources
// ============================================================================

/**
 * Dispose of a Three.js texture and free GPU memory
 * @param {THREE.Texture} texture - The texture to dispose
 */
export function disposeTexture(texture) {
  if (texture && texture.dispose) {
    texture.dispose();
    console.log('[Tiramisu] Texture disposed');
  }
}

/**
 * Dispose of a Three.js BufferGeometry and free GPU memory
 * @param {THREE.BufferGeometry} geometry - The geometry to dispose
 */
export function disposeGeometry(geometry) {
  if (geometry && geometry.dispose) {
    geometry.dispose();
    console.log('[Tiramisu] Geometry disposed');
  }
}

/**
 * Dispose of a Three.js Material and free GPU memory
 * @param {THREE.Material} material - The material to dispose
 */
export function disposeMaterial(material) {
  if (material) {
    // Dispose of material textures if they exist
    if (material.map) material.map.dispose();
    if (material.lightMap) material.lightMap.dispose();
    if (material.bumpMap) material.bumpMap.dispose();
    if (material.normalMap) material.normalMap.dispose();
    if (material.specularMap) material.specularMap.dispose();
    if (material.envMap) material.envMap.dispose();
    if (material.alphaMap) material.alphaMap.dispose();
    if (material.aoMap) material.aoMap.dispose();
    if (material.displacementMap) material.displacementMap.dispose();
    if (material.emissiveMap) material.emissiveMap.dispose();
    if (material.gradientMap) material.gradientMap.dispose();
    if (material.metalnessMap) material.metalnessMap.dispose();
    if (material.roughnessMap) material.roughnessMap.dispose();

    // Dispose of the material itself
    material.dispose();
    console.log('[Tiramisu] Material disposed');
  }
}

/**
 * Dispose of an Object3D recursively (geometry, materials, textures, children)
 * @param {THREE.Object3D} object - The object to dispose
 */
export function disposeObject3D(object) {
  if (!object) return;

  // Dispose geometry
  if (object.geometry) {
    object.geometry.dispose();
  }

  // Dispose material(s)
  if (object.material) {
    if (Array.isArray(object.material)) {
      object.material.forEach(material => disposeMaterial(material));
    } else {
      disposeMaterial(object.material);
    }
  }

  // Recursively dispose children
  if (object.children) {
    for (const child of object.children) {
      disposeObject3D(child);
    }
  }

  console.log('[Tiramisu] Object3D disposed');
}

// ============================================================================
// AUDIO PLAYBACK - Audio control now managed in Gleam
// ============================================================================

// Audio playback control has been migrated to Gleam (see tiramisu/internal/audio_manager.gleam).
// Audio state is now managed immutably in Gleam, with only pure Three.js API calls via threejs.ffi.mjs.
// The renderer (renderer.gleam) handles audio patching and state transitions.

// Runtime guards namespace - prevents duplicate event listeners across hot reloads
// Stored on window object to persist across module reloads
if (typeof window !== 'undefined' && !window.__tiramisu) {
  window.__tiramisu = {
    initialized: {
      resizeListener: false,
    },
    // Singleton instances
    inputManager: null,
  };
}

// ============================================================================
// PHYSICS - Rapier physics engine integration
// ============================================================================

import RAPIER from '@dimforge/rapier3d-compat';

// Initialize Rapier (must be called before creating world)
await RAPIER.init();

// ============================================================================
// DEBUG VISUALIZATION - Debug rendering and performance monitoring
// ============================================================================

import * as DEBUG_GLEAM from './tiramisu/debug.mjs';

// --- Debug Rendering Helpers ---

/// Create a wireframe box between min and max points
export function createDebugBox(min, max, color) {
  const geometry = new THREE.BoxGeometry(
    max.x - min.x,
    max.y - min.y,
    max.z - min.z
  );
  const edges = new THREE.EdgesGeometry(geometry);
  const material = new THREE.LineBasicMaterial({ color: color });
  const wireframe = new THREE.LineSegments(edges, material);

  // Position at center of box
  wireframe.position.set(
    (min.x + max.x) / 2,
    (min.y + max.y) / 2,
    (min.z + max.z) / 2
  );

  geometry.dispose(); // Clean up the geometry since we only need edges
  return wireframe;
}

/// Create a wireframe sphere
export function createDebugSphere(center, radius, color) {
  const geometry = new THREE.SphereGeometry(radius, 16, 12);
  const edges = new THREE.EdgesGeometry(geometry);
  const material = new THREE.LineBasicMaterial({ color: color });
  const wireframe = new THREE.LineSegments(edges, material);

  wireframe.position.set(center.x, center.y, center.z);

  geometry.dispose();
  return wireframe;
}

/// Create a line between two points
export function createDebugLine(from, to, color) {
  const points = [
    new THREE.Vector3(from.x, from.y, from.z),
    new THREE.Vector3(to.x, to.y, to.z)
  ];
  const geometry = new THREE.BufferGeometry().setFromPoints(points);
  const material = new THREE.LineBasicMaterial({ color: color });
  return new THREE.Line(geometry, material);
}

/// Create coordinate axes (X=red, Y=green, Z=blue)
export function createDebugAxes(origin, size) {
  const axes = new THREE.Group();

  // X axis (red)
  const xPoints = [
    new THREE.Vector3(origin.x, origin.y, origin.z),
    new THREE.Vector3(origin.x + size, origin.y, origin.z)
  ];
  const xGeometry = new THREE.BufferGeometry().setFromPoints(xPoints);
  const xMaterial = new THREE.LineBasicMaterial({ color: 0xff0000 });
  axes.add(new THREE.Line(xGeometry, xMaterial));

  // Y axis (green)
  const yPoints = [
    new THREE.Vector3(origin.x, origin.y, origin.z),
    new THREE.Vector3(origin.x, origin.y + size, origin.z)
  ];
  const yGeometry = new THREE.BufferGeometry().setFromPoints(yPoints);
  const yMaterial = new THREE.LineBasicMaterial({ color: 0x00ff00 });
  axes.add(new THREE.Line(yGeometry, yMaterial));

  // Z axis (blue)
  const zPoints = [
    new THREE.Vector3(origin.x, origin.y, origin.z),
    new THREE.Vector3(origin.x, origin.y, origin.z + size)
  ];
  const zGeometry = new THREE.BufferGeometry().setFromPoints(zPoints);
  const zMaterial = new THREE.LineBasicMaterial({ color: 0x0000ff });
  axes.add(new THREE.Line(zGeometry, zMaterial));

  return axes;
}

/// Create a grid on the XZ plane
export function createDebugGrid(size, divisions, color) {
  return new THREE.GridHelper(size, divisions, color, color);
}

/// Create a point marker (small sphere)
export function createDebugPoint(position, size, color) {
  const geometry = new THREE.SphereGeometry(size, 8, 6);
  const material = new THREE.MeshBasicMaterial({ color: color });
  const sphere = new THREE.Mesh(geometry, material);

  sphere.position.set(position.x, position.y, position.z);

  return sphere;
}

// --- Performance Monitoring ---

/**
 * PerformanceManager - Manages performance monitoring state
 * Encapsulates FPS tracking, frame time averaging, and render statistics
 */
export class PerformanceManager {
  constructor() {
    this.stats = {
      fps: 0,
      frameTime: 0,
      drawCalls: 0,
      triangles: 0,
      memoryMB: 0,
    };
    this.frameCount = 0;
    this.lastFpsUpdate = performance.now();
    this.frameTimes = [];
  }

  /**
   * Reset performance monitoring state
   * @returns {void}
   */
  start() {
    this.frameCount = 0;
    this.lastFpsUpdate = performance.now();
    this.frameTimes = [];
  }

  /**
   * Update performance statistics with new frame data
   * @param {number} deltaTime - Frame delta time in seconds
   * @returns {void}
   */
  update(deltaTime) {
    this.frameCount++;
    this.frameTimes.push(deltaTime * 1000); // Convert to ms

    // Keep only last 60 frames
    if (this.frameTimes.length > 60) {
      this.frameTimes.shift();
    }

    // Update FPS every second
    const now = performance.now();
    if (now - this.lastFpsUpdate >= 1000) {
      this.stats.fps = this.frameCount;
      this.frameCount = 0;
      this.lastFpsUpdate = now;
    }

    // Average frame time
    const avgFrameTime = this.frameTimes.reduce((a, b) => a + b, 0) / this.frameTimes.length;
    this.stats.frameTime = avgFrameTime;

    // Memory (if available)
    if (performance.memory) {
      this.stats.memoryMB = performance.memory.usedJSHeapSize / (1024 * 1024);
    }
  }

  /**
   * Get current performance statistics as Gleam type
   * @returns {DEBUG_GLEAM.PerformanceStats}
   */
  getStats() {
    return new DEBUG_GLEAM.PerformanceStats(
      this.stats.fps,
      this.stats.frameTime,
      this.stats.drawCalls,
      this.stats.triangles,
      this.stats.memoryMB
    );
  }

  /**
   * Update render statistics from WebGL renderer info
   * @param {Object} info - THREE.WebGLRenderer.info object
   * @returns {void}
   */
  setRenderStats(info) {
    if (info) {
      this.stats.drawCalls = info.render?.calls || 0;
      this.stats.triangles = info.render?.triangles || 0;
    }
  }
}

// FFI wrapper functions for backwards compatibility
export function createPerformanceManager() {
  return new PerformanceManager();
}

export function performanceManagerStart(manager) {
  manager.start();
}

export function performanceManagerUpdate(manager, deltaTime) {
  manager.update(deltaTime);
}

export function performanceManagerGetStats(manager) {
  return manager.getStats();
}

export function performanceManagerSetRenderStats(manager, info) {
  manager.setRenderStats(info);
}

// --- PERFORMANCE STATS ACCESS ---

/**
 * Current performance manager instance (set by startLoop)
 * @type {PerformanceManager | null}
 */
let currentPerformanceManager = null;

/**
 * Get current performance stats
 * @returns {DEBUG_GLEAM.PerformanceStats} Current performance statistics
 */
export function getPerformanceStats() {
  if (currentPerformanceManager) {
    return currentPerformanceManager.getStats();
  }
  // Return default stats if no manager is active
  return new DEBUG_GLEAM.PerformanceStats(0, 0, 0, 0, 0);
}

/**
 * DebugManager - Manages physics debug visualization state
 * Encapsulates collider debug mesh and physics world reference
 */
export class DebugManager {
  /**
   * Create a new DebugManager
   */
  constructor() {
    /**
     * Three.js mesh for rendering physics colliders
     * @type {THREE.LineSegments | null}
     */
    this.colliderDebugMesh = null;

    /**
     * Reference to the current Rapier physics world
     * @type {RAPIER.World | null}
     */
    this.currentPhysicsWorld = null;

    /**
     * Flag indicating if collider debugging is enabled
     * @type {boolean}
     */
    this.enabled = false;
  }

  /**
   * Enable collider visualization for a physics world
   * @param {Object} physicsWorld - Gleam PhysicsWorld object
   * @returns {void}
   */
  enableColliders(physicsWorld) {
    if (!physicsWorld) {
      console.warn('[Tiramisu Debug] No physics world provided to enableColliders');
      return;
    }

    // Extract the Rapier world from the Gleam PhysicsWorld
    const rapierWorld = physicsWorld.world;
    if (!rapierWorld) {
      console.warn('[Tiramisu Debug] PhysicsWorld has no Rapier world');
      return;
    }

    // Create debug mesh if it doesn't exist
    if (!this.colliderDebugMesh) {
      const geometry = new THREE.BufferGeometry();
      const material = new THREE.LineBasicMaterial({
        color: 0xffffff,
        vertexColors: true,
        linewidth: 1
      });
      this.colliderDebugMesh = new THREE.LineSegments(geometry, material);
      this.colliderDebugMesh.name = 'RapierDebugRenderer';
      this.colliderDebugMesh.frustumCulled = false; // Always render debug visualization
    }

    // Store current physics world reference and enable flag
    this.currentPhysicsWorld = rapierWorld;
    this.enabled = true;

    console.log('[Tiramisu Debug] Collider visualization enabled');
  }

  /**
   * Disable collider visualization
   * @returns {void}
   */
  disableColliders() {
    this.enabled = false;
    this.currentPhysicsWorld = null;

    console.log('[Tiramisu Debug] Collider visualization disabled');
  }

  /**
   * Update collider debug mesh with latest physics data
   * Should be called each frame when debug rendering is enabled
   * @param {THREE.Scene} scene - The Three.js scene to add/remove debug mesh from
   * @returns {void}
   */
  update(scene) {
    if (!scene) {
      return;
    }

    // Handle enabling/disabling based on current state
    if (this.enabled && this.colliderDebugMesh) {
      // Add to scene if not already added
      if (!this.colliderDebugMesh.parent) {
        scene.add(this.colliderDebugMesh);
      }

      // Update the debug visualization if we have a physics world
      if (this.currentPhysicsWorld) {
        try {
          // Get debug render data from Rapier
          const buffers = this.currentPhysicsWorld.debugRender();

          // Update geometry attributes
          this.colliderDebugMesh.geometry.setAttribute(
            'position',
            new THREE.BufferAttribute(buffers.vertices, 3)
          );
          this.colliderDebugMesh.geometry.setAttribute(
            'color',
            new THREE.BufferAttribute(buffers.colors, 4)
          );

          // Mark as needing update
          this.colliderDebugMesh.geometry.attributes.position.needsUpdate = true;
          this.colliderDebugMesh.geometry.attributes.color.needsUpdate = true;
        } catch (error) {
          console.error('[Tiramisu Debug] Error updating collider debug mesh:', error);
        }
      }
    } else {
      // Disabled: remove from scene if present
      if (this.colliderDebugMesh && this.colliderDebugMesh.parent) {
        scene.remove(this.colliderDebugMesh);
      }
    }
  }

  /**
   * Check if collider debugging is currently enabled
   * @returns {boolean} True if debugging is enabled
   */
  isEnabled() {
    return this.enabled;
  }
}

// FFI wrapper functions for Gleam to call DebugManager methods

/**
 * Create a new DebugManager instance
 * @returns {DebugManager} New DebugManager instance
 */
export function createDebugManager() {
  return new DebugManager();
}

/**
 * Enable collider visualization via the manager
 * @param {DebugManager} manager - The DebugManager instance
 * @param {Object} physicsWorld - Gleam PhysicsWorld object
 * @returns {void}
 */
export function debugManagerEnableColliders(manager, physicsWorld) {
  manager.enableColliders(physicsWorld);
}

/**
 * Disable collider visualization via the manager
 * @param {DebugManager} manager - The DebugManager instance
 * @returns {void}
 */
export function debugManagerDisableColliders(manager) {
  manager.disableColliders();
}

/**
 * Update debug visualization via the manager
 * @param {DebugManager} manager - The DebugManager instance
 * @param {THREE.Scene} scene - The Three.js scene
 * @returns {void}
 */
export function debugManagerUpdate(manager, scene) {
  manager.update(scene);
}

/**
 * Check if debugging is enabled via the manager
 * @param {DebugManager} manager - The DebugManager instance
 * @returns {boolean} True if debugging is enabled
 */
export function debugManagerIsEnabled(manager) {
  return manager.isEnabled();
}

// --- Physics Debugger ---

/**
 * Current debug manager instance (set by startLoop)
 * @type {DebugManager | null}
 */
let currentDebugManager = null;

/**
 * Enable/disable collision shape visualization for a specific physics world
 * @param {PhysicsWorld} physicsWorld - The Gleam PhysicsWorld
 * @param {boolean} enabled - Whether to enable or disable visualization
 * @returns {void}
 */
export function showColliders(physicsWorld, enabled) {
  if (!currentDebugManager) {
    console.warn('[Tiramisu Debug] No debug manager active');
    return;
  }

  if (enabled) {
    currentDebugManager.enableColliders(physicsWorld);
  } else {
    currentDebugManager.disableColliders();
  }
}

// ============================================================================
// MAIN GAME LOOP FUNCTIONS
// ============================================================================

/**
 * Append renderer canvas to DOM
 */
export function appendToDom(canvas) {
  document.body.appendChild(canvas);
}

/**
 * Start the immutable game loop with effect system
 */
export function startLoop(
  state,
  prevNodes,
  effect,
  context,
  rendererState,
  update,
  view,
) {
  let currentState = state;
  let currentNodes = prevNodes;
  let currentRendererState = rendererState;
  let messageQueue = [];

  // Extract renderer and scene from RendererState
  const renderer = SCENE.get_renderer(currentRendererState);
  const scene = SCENE.get_scene(currentRendererState);

  // Initialize CSS2DRenderer for UI overlays
  const css2dRenderer = new CSS2DRenderer();
  const canvas = renderer.domElement;
  const canvasWidth = canvas.clientWidth || window.innerWidth;
  const canvasHeight = canvas.clientHeight || window.innerHeight;
  css2dRenderer.setSize(canvasWidth, canvasHeight);
  css2dRenderer.domElement.style.position = 'absolute';
  css2dRenderer.domElement.style.top = '0';
  css2dRenderer.domElement.style.left = '0';
  css2dRenderer.domElement.style.pointerEvents = 'none';

  if (canvas.parentElement) {
    canvas.parentElement.appendChild(css2dRenderer.domElement);
  } else {
    document.body.appendChild(css2dRenderer.domElement);
  }

  // Initialize CSS3DRenderer for 3D UI elements
  const css3dRenderer = new CSS3DRenderer();
  css3dRenderer.setSize(canvasWidth, canvasHeight);
  css3dRenderer.domElement.style.position = 'absolute';
  css3dRenderer.domElement.style.top = '0';
  css3dRenderer.domElement.style.left = '0';
  css3dRenderer.domElement.style.pointerEvents = 'none';

  if (canvas.parentElement) {
    canvas.parentElement.appendChild(css3dRenderer.domElement);
  } else {
    document.body.appendChild(css3dRenderer.domElement);
  }

  // Dispatch function for effects
  const dispatch = (msg) => {
    messageQueue.push(msg);
  };

  // Create manager instances
  const performanceManager = new PerformanceManager();
  performanceManager.start();

  const debugManager = new DebugManager();

  // Register game dispatch with the module-level dispatch manager
  dispatchManager.registerGame(dispatch);

  // Make managers accessible for FFI wrapper functions
  currentPerformanceManager = performanceManager;
  currentDebugManager = debugManager;

  // Run initial effect
  runEffect(effect, dispatch);

  createInputManager(renderer.domElement)

  // Setup AudioContext resume on first user interaction (guarded to prevent duplicates)
  // Browsers require user interaction before playing audio
  if (typeof window !== 'undefined' && window.__tiramisu) {
    // Initialize flag if not present
    if (window.__tiramisu.audioContextSetup === undefined) {
      window.__tiramisu.audioContextSetup = false;
    }

    // Only setup listeners once across all hot reloads
    if (!window.__tiramisu.audioContextSetup) {
      window.__tiramisu.audioContextSetup = true;

      const resumeAudioContext = async () => {
        // Let Gleam audio manager handle everything (resume context + play pending audio)
        currentRendererState = SCENE.resume_audio_context(currentRendererState);

        // Remove event listeners after first interaction
        document.removeEventListener('click', resumeAudioContext);
        document.removeEventListener('touchstart', resumeAudioContext);
        document.removeEventListener('keydown', resumeAudioContext);

        // Reset flag so future startLoop calls can set it up again if needed
        if (window.__tiramisu) {
          window.__tiramisu.audioContextSetup = false;
        }
      };

      // Listen for first user interaction
      document.addEventListener('click', resumeAudioContext);
      document.addEventListener('touchstart', resumeAudioContext);
      document.addEventListener('keydown', resumeAudioContext);
    }
  }

  let lastTime = performance.now();

  function gameLoop() {
    const currentTime = performance.now();
    const deltaTime = currentTime - lastTime
    lastTime = currentTime;

    // Capture input state snapshot
    const inputState = window.__tiramisu.inputManager.captureState();

    // Get canvas dimensions (use CSS display size for coordinate conversion)
    // Touch/mouse coordinates are in CSS pixels, so we need CSS dimensions
    const canvas = renderer.domElement;
    const canvasWidth = canvas.clientWidth;
    const canvasHeight = canvas.clientHeight;

    // Update context with new delta, input, and canvas dimensions
    // Keep the physics_world from the original context
    let newContext = {
      ...context,
      delta_time: deltaTime,
      input: inputState,
      canvas_width: canvasWidth,
      canvas_height: canvasHeight,
    };

    // Process all messages in queue
    while (messageQueue.length > 0) {
      const msg = messageQueue.shift();
      const [newState, newEffect, newPhysicsWorld] = update(currentState, msg, newContext);
      currentState = newState;

      // Update context and renderer state with new physics_world if it changed
      if (newPhysicsWorld) {
        newContext = {
          ...newContext,
          physics_world: newPhysicsWorld,
        };
        // Update renderer state with new physics world
        currentRendererState = SCENE.set_physics_world(currentRendererState, newPhysicsWorld);
      }

      runEffect(newEffect, dispatch);
    }

    // Generate new scene nodes - pass context to view
    const newNodes = view(currentState, newContext);

    // Dirty flagging optimization: skip diff if scene hasn't changed (referential equality)
    // This is extremely fast for static scenes where view() returns the same list reference
    if (currentNodes !== newNodes) {
      // Diff and patch using Gleam renderer
      const patches = SCENE.diff(currentNodes, newNodes);
      currentRendererState = SCENE.apply_patches(currentRendererState, patches);

      // Extract updated physics world from renderer (it may have new bodies)
      const updatedPhysicsWorld = SCENE.get_physics_world(currentRendererState);
      if (updatedPhysicsWorld) {
        newContext = {
          ...newContext,
          physics_world: updatedPhysicsWorld,
        };
      }

      currentNodes = newNodes;
    }
    // else: Scene unchanged, skip diff entirely (massive speedup for static/paused scenes)

    // Note: Physics stepping is handled in the Gleam update function via physics.step()

    // Sync physics body transforms to Three.js objects
    const physicsWorldOption = newContext.physics_world;
    if (physicsWorldOption && physicsWorldOption[0]) {
      SCENE.sync_physics_transforms(currentRendererState);
    }

    // Update animation mixers
    SCENE.update_mixers(currentRendererState, deltaTime);

    // Update particle systems (returns updated renderer state)
    currentRendererState = SCENE.update_particle_systems(currentRendererState, deltaTime);

    // Update physics debug visualization (pass scene from renderer state)
    debugManager.update(scene);

    // Clear the entire canvas first
    renderer.setScissorTest(false);
    renderer.clear();

    // Render main camera (active camera without viewport)
    const activeCamera = getActiveCamera();
    if (activeCamera) {
      renderer.setScissorTest(false);
      renderer.setViewport(0, 0, canvasWidth, canvasHeight);
      renderer.render(scene, activeCamera);
    } else {
      console.warn('[Game] No active camera found. Add a Camera scene node with active=True.');
    }

    // Render viewport cameras (picture-in-picture)
    const viewportCamerasList = SCENE.get_cameras_with_viewports(currentRendererState);
    const viewportCameras = viewportCamerasList.toArray(); // Convert Gleam list to JS array
    if (viewportCameras.length > 0) {
      renderer.setScissorTest(true);
      for (const entry of viewportCameras) {
        // Entry is a tuple [camera, viewport]
        const camera = entry[0];
        const viewport = entry[1];
        // viewport is a Viewport object {x, y, width, height}
        renderer.setViewport(viewport.x, viewport.y, viewport.width, viewport.height);
        renderer.setScissor(viewport.x, viewport.y, viewport.width, viewport.height);
        renderer.render(scene, camera);
      }
      renderer.setScissorTest(false);
    }

    // Render CSS2D UI overlays
    if (activeCamera) {
      css2dRenderer.render(scene, activeCamera);
    }

    // Render CSS3D UI elements
    if (activeCamera) {
      css3dRenderer.render(scene, activeCamera);
    }

    // Update performance stats
    performanceManager.update(deltaTime);
    performanceManager.setRenderStats(renderer.info);

    // Clear per-frame input state
    window.__tiramisu.inputManager.clearFrameState();

    // Update closure context for next frame (preserve updated physics world)
    context = newContext;

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

/**
 * Get the current canvas dimensions from the renderer
 * @param {THREE.WebGLRenderer} renderer
 * @returns {[number, number]} [width, height] as a tuple
 */
export function getCanvasDimensions(renderer) {
  const canvas = renderer.domElement;
  return [canvas.clientWidth, canvas.clientHeight];
}

/**
 * Identity function for type conversion (pass-through)
 * @param {any} value - Any value
 * @returns {any} The same value
 */
export function identity(value) {
  return value;
}

// ============================================================================
// UI INTEGRATION - Bidirectional messaging between Lustre and Tiramisu
// ============================================================================

/**
 * DispatchManager - Manages dispatch functions for UI and game messaging
 * Encapsulates bidirectional communication between Lustre UI and Tiramisu game
 */
export class DispatchManager {
  /**
   * Create a new DispatchManager
   */
  constructor() {
    /**
     * Lustre UI dispatch function
     * @type {Function | null}
     */
    this.uiDispatch = null;

    /**
     * Game dispatch function
     * @type {Function | null}
     */
    this.gameDispatch = null;
  }

  /**
   * Register Lustre UI dispatch function
   * @param {Function} dispatch - Lustre's dispatch function
   * @returns {void}
   */
  registerUI(dispatch) {
    this.uiDispatch = dispatch;
  }

  /**
   * Register game dispatch function
   * @param {Function} dispatch - Game's dispatch function
   * @returns {void}
   */
  registerGame(dispatch) {
    this.gameDispatch = dispatch;
  }

  /**
   * Dispatch a message from game to UI
   * @param {any} msg - The message to dispatch
   * @returns {void}
   */
  dispatchToUI(msg) {
    if (this.uiDispatch) {
      this.uiDispatch(msg);
    } else {
      console.warn('[Tiramisu] UI dispatch not registered. Call ui.register_lustre() in your Lustre init.');
    }
  }

  /**
   * Dispatch a message from UI to game
   * @param {any} msg - The message to dispatch
   * @returns {void}
   */
  dispatchToGame(msg) {
    if (this.gameDispatch) {
      this.gameDispatch(msg);
    } else {
      console.warn('[Tiramisu] Game dispatch not registered yet.');
    }
  }
}

/**
 * Module-level dispatch manager instance (singleton)
 * Created immediately so it's available when Lustre init calls registerUI
 * @type {DispatchManager}
 */
const dispatchManager = new DispatchManager();

// FFI functions for Gleam that use the dispatch manager

/**
 * Register Lustre UI to receive messages from Tiramisu game
 * @param {Function} dispatch - Lustre's dispatch function
 */
export function registerUI(dispatch) {
  dispatchManager.registerUI(dispatch);
}

/**
 * Dispatch a message from Tiramisu game to Lustre UI
 * @param {any} msg - The message to dispatch to UI
 */
export function dispatchToUI(msg) {
  dispatchManager.dispatchToUI(msg);
}

/**
 * Dispatch a message from Lustre UI to Tiramisu game
 * @param {any} msg - The message to dispatch to game
 */
export function dispatchToGame(msg) {
  dispatchManager.dispatchToGame(msg);
}

// ============================================================================
// TIME & ANIMATION EFFECTS - Timing and animation helpers
// ============================================================================

/**
 * Timer manager to handle pausing/resuming timers when page visibility changes
 * This prevents timer callbacks from accumulating when the user tabs out
 */
class TimerManager {
  constructor() {
    /** @type {Map<number, {type: 'timeout'|'interval', callback: Function, delay: number, remaining: number, lastPause: number, timerId: number|null}>} */
    this.timers = new Map();
    this.nextId = 1;
    this.isPaused = false;

    // Listen for page visibility changes
    if (typeof document !== 'undefined') {
      document.addEventListener('visibilitychange', () => {
        if (document.hidden) {
          this.pauseAll();
        } else {
          this.resumeAll();
        }
      });
    }
  }

  /**
   * Create a managed timeout
   * @param {Function} callback - Callback to execute after delay
   * @param {number} delay - Delay in milliseconds
   * @returns {number} Timer ID
   */
  setTimeout(callback, delay) {
    const id = this.nextId++;
    const timerId = setTimeout(() => {
      callback();
      this.timers.delete(id);
    }, delay);

    this.timers.set(id, {
      type: 'timeout',
      callback,
      delay,
      remaining: delay,
      lastPause: 0,
      timerId,
    });

    return id;
  }

  /**
   * Create a managed interval
   * @param {Function} callback - Callback to execute on each interval
   * @param {number} delay - Interval duration in milliseconds
   * @returns {number} Timer ID
   */
  setInterval(callback, delay) {
    const id = this.nextId++;
    const timerId = setInterval(callback, delay);

    this.timers.set(id, {
      type: 'interval',
      callback,
      delay,
      remaining: delay,
      lastPause: 0,
      timerId,
    });

    return id;
  }

  /**
   * Clear a managed timer
   * @param {number} id - Timer ID
   */
  clearTimer(id) {
    const timer = this.timers.get(id);
    if (timer) {
      if (timer.timerId !== null) {
        if (timer.type === 'timeout') {
          clearTimeout(timer.timerId);
        } else {
          clearInterval(timer.timerId);
        }
      }
      this.timers.delete(id);
    }
  }

  /**
   * Pause all active timers
   */
  pauseAll() {
    if (this.isPaused) return;
    this.isPaused = true;

    const now = performance.now();
    for (const [id, timer] of this.timers.entries()) {
      if (timer.timerId !== null) {
        // Clear the native timer
        if (timer.type === 'timeout') {
          clearTimeout(timer.timerId);
          // Calculate remaining time for timeouts
          timer.remaining = timer.remaining - (now - (timer.lastPause || now));
        } else {
          clearInterval(timer.timerId);
        }
        timer.timerId = null;
        timer.lastPause = now;
      }
    }
  }

  /**
   * Resume all paused timers
   */
  resumeAll() {
    if (!this.isPaused) return;
    this.isPaused = false;

    const now = performance.now();
    for (const [id, timer] of this.timers.entries()) {
      if (timer.timerId === null) {
        if (timer.type === 'timeout') {
          // Resume timeout with remaining time
          const remaining = Math.max(0, timer.remaining);
          timer.timerId = setTimeout(() => {
            timer.callback();
            this.timers.delete(id);
          }, remaining);
          timer.lastPause = now;
        } else {
          // Resume interval
          timer.timerId = setInterval(timer.callback, timer.delay);
        }
      }
    }
  }
}

// Global timer manager instance
const timerManager = new TimerManager();

/**
 * Delay execution by a specified duration
 * @param {number} milliseconds - Delay in milliseconds
 * @param {Function} callback - Callback to execute after delay
 */
export function delay(milliseconds, callback) {
  timerManager.setTimeout(callback, milliseconds);
}

/**
 * Create a recurring interval (returns interval ID)
 * @param {number} milliseconds - Interval duration in milliseconds
 * @param {Function} callback - Callback to execute on each interval
 * @returns {number} Timer ID (managed by TimerManager)
 */
export function interval(milliseconds, callback) {
  return timerManager.setInterval(callback, milliseconds);
}

/**
 * Cancel a recurring interval by its ID
 * @param {number} intervalId - Timer ID (from interval function)
 */
export function cancelInterval(intervalId) {
  timerManager.clearTimer(intervalId);
}

// Easing functions for tweening
const easingFunctions = {
  linear: (t) => t,
  easeInQuad: (t) => t * t,
  easeOutQuad: (t) => t * (2 - t),
  easeInOutQuad: (t) => (t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t),
  easeInCubic: (t) => t * t * t,
  easeOutCubic: (t) => (--t) * t * t + 1,
  easeInOutCubic: (t) => (t < 0.5 ? 4 * t * t * t : (t - 1) * (2 * t - 2) * (2 * t - 2) + 1),
};

/**
 * Animate a value from start to end with easing
 * @param {number} start - Starting value
 * @param {number} end - Ending value
 * @param {number} durationMs - Animation duration in milliseconds
 * @param {string} easingName - Name of easing function
 * @param {Function} onUpdate - Callback with interpolated value
 * @param {Function} onComplete - Callback when animation completes
 */
export function tween(start, end, durationMs, easingName, onUpdate, onComplete) {
  const easing = easingFunctions[easingName] || easingFunctions.linear;
  const startTime = performance.now();

  function animate() {
    const elapsed = performance.now() - startTime;
    const progress = Math.min(elapsed / durationMs, 1.0);
    const easedProgress = easing(progress);
    const value = start + (end - start) * easedProgress;

    onUpdate(value);

    if (progress < 1.0) {
      requestAnimationFrame(animate);
    } else {
      onComplete();
    }
  }

  requestAnimationFrame(animate);
}

// ============================================================================
// SYSTEM & BROWSER EFFECTS - Browser API interactions
// ============================================================================

/**
 * Request fullscreen mode for the canvas
 * @returns {Promise<Result<Nil, String>>}
 */
export async function requestFullscreen() {
  const canvas = document.querySelector('canvas');
  if (!canvas) {
    return Promise.resolve(new GLEAM.Error('No canvas element found'));
  }

  // Try different vendor-specific APIs
  const requestFn =
    canvas.requestFullscreen ||
    canvas.webkitRequestFullscreen ||
    canvas.mozRequestFullScreen ||
    canvas.msRequestFullscreen;

  if (!requestFn) {
    return Promise.resolve(new GLEAM.Error('Fullscreen API not supported'));
  }

  return requestFn
    .call(canvas)
    .then(() => new GLEAM.Ok(null))
    .catch((error) => new GLEAM.Error(error.message || 'Failed to enter fullscreen'));
}

/**
 * Exit fullscreen mode
 */
export function exitFullscreen() {
  // Try different vendor-specific APIs
  const exitFn =
    document.exitFullscreen ||
    document.webkitExitFullscreen ||
    document.mozCancelFullScreen ||
    document.msExitFullscreen;

  if (exitFn) {
    exitFn.call(document);
  }
}

/**
 * Request pointer lock for the canvas
 * @returns {Promise<Result<Nil, String>>}
 */
export function requestPointerLock() {
  const canvas = document.querySelector('canvas');
  if (!canvas) {
    return Promise.resolve(new GLEAM.Error('No canvas element found'));
  }

  // Try different vendor-specific APIs
  const requestFn =
    canvas.requestPointerLock ||
    canvas.webkitRequestPointerLock ||
    canvas.mozRequestPointerLock;

  if (!requestFn) {
    return Promise.resolve(new GLEAM.Error('Pointer Lock API not supported'));
  }

  return new Promise((resolve) => {
    const onLockChange = () => {
      // Check if pointer is locked to our canvas
      const lockedElement =
        document.pointerLockElement ||
        document.webkitPointerLockElement ||
        document.mozPointerLockElement;

      document.removeEventListener('pointerlockchange', onLockChange);
      document.removeEventListener('webkitpointerlockchange', onLockChange);
      document.removeEventListener('mozpointerlockchange', onLockChange);

      if (lockedElement === canvas) {
        resolve(new GLEAM.Ok(null));
      } else {
        resolve(new GLEAM.Error('Failed to lock pointer'));
      }
    };

    document.addEventListener('pointerlockchange', onLockChange);
    document.addEventListener('webkitpointerlockchange', onLockChange);
    document.addEventListener('mozpointerlockchange', onLockChange);

    requestFn.call(canvas);

    // Timeout after 2 seconds
    setTimeout(() => {
      document.removeEventListener('pointerlockchange', onLockChange);
      document.removeEventListener('webkitpointerlockchange', onLockChange);
      document.removeEventListener('mozpointerlockchange', onLockChange);
      resolve(new GLEAM.Error('Pointer lock request timed out'));
    }, 2000);
  });
}

/**
 * Exit pointer lock mode
 */
export function exitPointerLock() {
  // Try different vendor-specific APIs
  const exitFn =
    document.exitPointerLock ||
    document.webkitExitPointerLock ||
    document.mozExitPointerLock;

  if (exitFn) {
    exitFn.call(document);
  }
}

/**
 * Trigger device vibration
 * @param {Array<number>} pattern - Array of vibration durations in milliseconds
 */
export function vibrate(pattern) {
  if ('vibrate' in navigator) {
    // Convert Gleam list to JavaScript array
    const patternArray = pattern.toArray ? pattern.toArray() : pattern;
    navigator.vibrate(patternArray);
  } else {
    console.warn('[Tiramisu] Vibration API not supported');
  }
}

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

/**
 * Write text to clipboard
 * @param {string} text - Text to write
 * @returns {Promise<Result<Nil, String>>}
 */
export async function clipboardWrite(text) {
  if (!navigator.clipboard) {
    return Promise.resolve(new GLEAM.Error('Clipboard API not supported'));
  }

  return navigator.clipboard
    .writeText(text)
    .then(() => new GLEAM.Ok(null))
    .catch((error) => new GLEAM.Error(error.message || 'Failed to write to clipboard'));
}

/**
 * Read text from clipboard
 * @returns {Promise<Result<String, String>>}
 */
export async function clipboardRead() {
  if (!navigator.clipboard) {
    return Promise.resolve(new GLEAM.Error('Clipboard API not supported'));
  }

  return navigator.clipboard
    .readText()
    .then((text) => new GLEAM.Ok(text))
    .catch((error) => new GLEAM.Error(error.message || 'Failed to read from clipboard'));
}

/**
 * Get geometries array from mesh/material pairs object
 * Converts JavaScript array to Gleam list
 * @param {object} pairs - {geometries: Array, materials: Array}
 * @returns {List}
 */
export function getPairsGeometries(pairs) {
  // Convert JavaScript array to Gleam list format
  return GLEAM.toList(pairs.geometries);
}

/**
 * Get materials array from mesh/material pairs object
 * Converts JavaScript array to Gleam list
 * @param {object} pairs - {geometries: Array, materials: Array}
 * @returns {List}
 */
export function getPairsMaterials(pairs) {
  // Convert JavaScript array to Gleam list format
  return GLEAM.toList(pairs.materials);
}

/**
 * Generate a unique ID for an instance's physics body
 * Combines base ID with instance index
 * @param {*} baseId - The base ID (can be any type)
 * @param {number} index - The instance index
 * @returns {*} - Composite ID (tuple of base + index)
 */
export function generateInstanceId(baseId, index) {
  // Return a tuple that combines the base ID with the index
  // This creates unique IDs for each instance's physics body
  return [baseId, index];
}
