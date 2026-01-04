/**
 * Simulate FFI - Headless testing support for Tiramisu
 *
 * Provides mock/headless versions of Three.js objects for testing
 * without requiring WebGL or a browser environment.
 */

import * as THREE from 'three';
import { Vec2$Vec2 } from '../../vec/vec/vec2.mjs';

/**
 * Create a real Three.js Scene (works in Node.js without WebGL)
 * @returns {THREE.Scene}
 */
export function createHeadlessScene() {
  return new THREE.Scene();
}

/**
 * Create a mock renderer for testing (no WebGL required)
 * Provides the same interface as THREE.WebGLRenderer but does nothing
 * @param {number} width - Canvas width
 * @param {number} height - Canvas height
 * @returns {Object} Mock renderer object
 */
export function createHeadlessRenderer(width, height) {
  // Create a mock that satisfies the Renderer interface
  const mockRenderer = {
    // Mock canvas element
    domElement: {
      clientWidth: width,
      clientHeight: height,
      width: width,
      height: height,
      style: {},
      getContext: () => null,
      addEventListener: () => { },
      removeEventListener: () => { },
      getBoundingClientRect: () => ({
        left: 0,
        top: 0,
        width: width,
        height: height,
      }),
    },

    // Size management
    _width: width,
    _height: height,

    setSize(w, h) {
      this._width = w;
      this._height = h;
      this.domElement.clientWidth = w;
      this.domElement.clientHeight = h;
      this.domElement.width = w;
      this.domElement.height = h;
    },

    getSize(target) {
      if (target) {
        target.x = this._width;
        target.y = this._height;
        return target;
      }
      return { x: this._width, y: this._height };
    },

    // Rendering methods (no-ops)
    render: () => { },
    setViewport: () => { },
    setScissor: () => { },
    setScissorTest: () => { },
    clear: () => { },
    clearColor: () => { },
    clearDepth: () => { },
    clearStencil: () => { },

    // Configuration methods (no-ops)
    setPixelRatio: () => { },
    setClearColor: () => { },
    setRenderTarget: () => { },

    // Properties
    shadowMap: {
      enabled: false,
      type: 0,
      autoUpdate: true,
    },

    capabilities: {
      isWebGL2: false,
      maxTextures: 16,
      maxVertexTextures: 16,
      maxTextureSize: 4096,
      maxCubemapSize: 4096,
    },

    info: {
      memory: { geometries: 0, textures: 0 },
      render: { calls: 0, triangles: 0, points: 0, lines: 0, frame: 0 },
      programs: null,
    },

    // For compatibility
    outputColorSpace: 'srgb',
    toneMapping: 0,
    toneMappingExposure: 1,

    dispose: () => { },
  };

  return mockRenderer;
}

/**
 * Create a mock audio listener for testing
 * @returns {Object} Mock AudioListener object
 */
export function createMockAudioListener() {
  // Create a minimal mock that satisfies the AudioListener interface
  return {
    context: null,
    gain: { gain: { value: 1 } },
    filter: null,
    timeDelta: 0,

    // Methods
    getInput: () => null,
    removeFilter: () => { },
    setFilter: () => { },
    getFilter: () => null,
    setMasterVolume: () => { },
    getMasterVolume: () => 1,

    // Object3D properties (AudioListener extends Object3D)
    uuid: THREE.MathUtils.generateUUID(),
    name: '',
    type: 'AudioListener',
    parent: null,
    children: [],
    up: new THREE.Vector3(0, 1, 0),
    position: new THREE.Vector3(),
    rotation: new THREE.Euler(),
    quaternion: new THREE.Quaternion(),
    scale: new THREE.Vector3(1, 1, 1),
    matrix: new THREE.Matrix4(),
    matrixWorld: new THREE.Matrix4(),
    matrixAutoUpdate: true,
    matrixWorldNeedsUpdate: false,
    visible: true,

    // Object3D methods we might need
    updateMatrix: () => { },
    updateMatrixWorld: () => { },
    add: () => { },
    remove: () => { },
    getWorldPosition: (target) => target || new THREE.Vector3(),
    getWorldQuaternion: (target) => target || new THREE.Quaternion(),
  };
}

/**
 * Get canvas dimensions from headless renderer
 * Compatible with savoiardi's getCanvasDimensions
 * @param {Object} renderer - Mock or real renderer
 * @returns {Vec2} Vec2(width, height)
 */
export function getHeadlessCanvasDimensions(renderer) {
  const canvas = renderer.domElement;
  return Vec2$Vec2(canvas.clientWidth, canvas.clientHeight);
}

// ============================================================================
// MUTABLE LIST FOR EFFECT CAPTURE
// ============================================================================

/**
 * Push a value to a mutable list
 * @param {Array} list
 * @param {*} value
 */
export function pushToMutableList(list, value) {
  list.push(value);
}

// ============================================================================
// INPUT STATE ACCESSORS
// ============================================================================

/**
 * Get pressed keys from InputState
 * @param {Object} input - InputState record
 * @returns {Set} pressed_keys set
 */
export function getPressedKeys(input) {
  return input.keyboard.pressed_keys;
}

/**
 * Get just pressed keys from InputState
 * @param {Object} input - InputState record
 * @returns {Set} just_pressed_keys set
 */
export function getJustPressedKeys(input) {
  return input.keyboard.just_pressed_keys;
}

/**
 * Get just released keys from InputState
 * @param {Object} input - InputState record
 * @returns {Set} just_released_keys set
 */
export function getJustReleasedKeys(input) {
  return input.keyboard.just_released_keys;
}

/**
 * Get mouse state from InputState
 * @param {Object} input - InputState record
 * @returns {Object} MouseStateRecord
 */
export function getMouseState(input) {
  const m = input.mouse;
  return {
    x: m.x,
    y: m.y,
    delta_x: m.delta_x,
    delta_y: m.delta_y,
    wheel_delta: m.wheel_delta,
    left_button: m.left_button,
    middle_button: m.middle_button,
    right_button: m.right_button,
  };
}

/**
 * Get keyboard state from InputState
 * @param {Object} input - InputState record
 * @returns {Object} KeyboardState
 */
export function getKeyboardState(input) {
  return input.keyboard;
}

/**
 * Get touch state from InputState
 * @param {Object} input - InputState record
 * @returns {Object} TouchState
 */
export function getTouchState(input) {
  return input.touch;
}

/**
 * Get gamepads from InputState
 * @param {Object} input - InputState record
 * @returns {List} List of GamepadState
 */
export function getGamepads(input) {
  return input.gamepad;
}

/**
 * Get active touches from InputState
 * @param {Object} input - InputState record
 * @returns {List} List of Touch
 */
export function getActiveTouches(input) {
  return input.touch.touches;
}
