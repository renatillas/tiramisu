// Tiramisu FFI - Game engine specific functionality
// Contains functions that are NOT pure Three.js bindings
import * as THREE from 'three';

// ============================================================================
// PHYSICS - Rapier physics engine integration
// ============================================================================

import RAPIER from '@dimforge/rapier3d-compat';

// Initialize Rapier (must be called before creating world)
await RAPIER.init();

// ============================================================================
// CANVAS/PAINT INTEGRATION
// These functions integrate with the paint library for 2D canvas rendering
// ============================================================================

/**
 * Create a canvas texture from a paint.Picture (encoded as string)
 * Uses the paint library's global rendering function set up by define_web_component()
 * @param {string} encodedPicture - Encoded paint.Picture string (from paint/encode.to_string)
 * @param {number} width - Canvas width in pixels
 * @param {number} height - Canvas height in pixels
 * @returns {THREE.CanvasTexture}
 */
export function createCanvasTextureFromPicture(encodedPicture, width, height) {
  const canvas = document.createElement('canvas');
  canvas.width = width;
  canvas.height = height;
  const ctx = canvas.getContext('2d');

  // Clear canvas with transparent background
  ctx.clearRect(0, 0, width, height);

  // Use paint's global rendering function
  // This is set up by paint's define_web_component() call
  const display = window.PAINT_STATE?.["display_on_rendering_context_with_default_drawing_state"];

  if (!display) {
    console.error('Paint library not initialized. Make sure to call canvas.define_web_component() before rendering sprites.');
    return new THREE.CanvasTexture(canvas);
  }

  try {
    display(encodedPicture, ctx);
  } catch (error) {
    console.error('Failed to render paint.Picture:', error);
  }

  // Create texture
  const texture = new THREE.CanvasTexture(canvas);
  texture.minFilter = THREE.LinearFilter;
  texture.magFilter = THREE.LinearFilter;
  texture.needsUpdate = true;

  return texture;
}

/**
 * Create a plane mesh with texture for canvas drawing
 * @param {THREE.Texture} texture
 * @param {number} width - World space width
 * @param {number} height - World space height
 * @returns {THREE.Mesh}
 */
export function createCanvasPlane(texture, width, height) {
  const geometry = new THREE.PlaneGeometry(width, height);
  const material = new THREE.MeshBasicMaterial({
    map: texture,
    transparent: true,
    side: THREE.DoubleSide,
    depthWrite: true,
    alphaTest: 0.1,  // Discard fully transparent pixels to avoid z-fighting
  });
  const mesh = new THREE.Mesh(geometry, material);
  return mesh;
}

/**
 * Update canvas texture
 * @param {THREE.Object3D} object
 * @param {THREE.Texture} texture
 */
export function updateCanvasTexture(object, texture) {
  if (object.material && object.material.map) {
    object.material.map.dispose();
    object.material.map = texture;
    object.material.needsUpdate = true;
  }
}

/**
 * Update canvas plane size
 * @param {THREE.Object3D} object
 * @param {number} width
 * @param {number} height
 */
export function updateCanvasSize(object, width, height) {
  if (object.geometry) {
    object.geometry.dispose();
    object.geometry = new THREE.PlaneGeometry(width, height);
  }
}

/**
 * Get cached encoded picture from canvas object's userData
 * Performance optimization: avoid recreating textures when picture hasn't changed
 * @param {THREE.Object3D} object
 * @returns {string} Cached encoded picture, or empty string if not cached
 */
export function getCanvasCachedPicture(object) {
  return object.userData?.cachedEncodedPicture || '';
}

/**
 * Store encoded picture in canvas object's userData for comparison next frame
 * Performance optimization: tracks picture changes to skip texture recreation
 * @param {THREE.Object3D} object
 * @param {string} encodedPicture
 */
export function setCanvasCachedPicture(object, encodedPicture) {
  if (!object.userData) {
    object.userData = {};
  }
  object.userData.cachedEncodedPicture = encodedPicture;
}

// ============================================================================
// DEBUG VISUALIZATION HELPERS
// ============================================================================

/**
 * Create a wireframe box debug visualization
 * @param {Object} min - Minimum corner {x, y, z}
 * @param {Object} max - Maximum corner {x, y, z}
 * @param {number} color - Hex color
 * @returns {THREE.LineSegments} Wireframe box
 */
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

/**
 * Create a wireframe sphere debug visualization
 * @param {Object} center - Center position {x, y, z}
 * @param {number} radius - Sphere radius
 * @param {number} color - Hex color
 * @returns {THREE.LineSegments} Wireframe sphere
 */
export function createDebugSphere(center, radius, color) {
  const geometry = new THREE.SphereGeometry(radius, 16, 12);
  const edges = new THREE.EdgesGeometry(geometry);
  const material = new THREE.LineBasicMaterial({ color: color });
  const wireframe = new THREE.LineSegments(edges, material);

  wireframe.position.set(center.x, center.y, center.z);

  geometry.dispose();
  return wireframe;
}

/**
 * Create a line between two points
 * @param {Object} from - Start position {x, y, z}
 * @param {Object} to - End position {x, y, z}
 * @param {number} color - Hex color
 * @returns {THREE.Line} Debug line
 */
export function createDebugLine(from, to, color) {
  const points = [
    new THREE.Vector3(from.x, from.y, from.z),
    new THREE.Vector3(to.x, to.y, to.z)
  ];
  const geometry = new THREE.BufferGeometry().setFromPoints(points);
  const material = new THREE.LineBasicMaterial({ color: color });
  return new THREE.Line(geometry, material);
}

/**
 * Create coordinate axes (X=red, Y=green, Z=blue)
 * @param {Object} origin - Origin position {x, y, z}
 * @param {number} size - Length of each axis
 * @returns {THREE.Group} Group containing three axes
 */
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

/**
 * Create a grid on the XZ plane
 * @param {number} size - Total size of the grid
 * @param {number} divisions - Number of grid divisions
 * @param {number} color - Hex color
 * @returns {THREE.GridHelper} Grid helper
 */
export function createDebugGrid(size, divisions, color) {
  return new THREE.GridHelper(size, divisions, color, color);
}

/**
 * Create a point marker (small sphere)
 * @param {Object} position - Position {x, y, z}
 * @param {number} size - Size of the point marker
 * @param {number} color - Hex color
 * @returns {THREE.Mesh} Point mesh
 */
export function createDebugPoint(position, size, color) {
  const geometry = new THREE.SphereGeometry(size, 8, 6);
  const material = new THREE.MeshBasicMaterial({ color: color });
  const sphere = new THREE.Mesh(geometry, material);
  sphere.position.set(position.x, position.y, position.z);
  return sphere;
}

// ============================================================================
// AUDIO FADE FUNCTIONS
// ============================================================================

/**
 * Play audio with fade in effect
 * @param {THREE.Audio} audio
 * @param {number} fadeDurationMs - Fade duration in milliseconds
 * @param {number} targetVolume - Target volume (0.0 to 1.0)
 */
export function playAudioWithFadeIn(audio, fadeDurationMs, targetVolume) {
  if (!audio.buffer) {
    console.warn('[Tiramisu] Cannot play audio without buffer');
    return;
  }

  audio.setVolume(0.0);
  if (!audio.isPlaying) {
    try {
      audio.play();
    } catch (error) {
      console.error('[Tiramisu] Failed to play audio:', error);
      return;
    }
  }

  const startTime = Date.now();
  const fadeInterval = setInterval(() => {
    const elapsed = Date.now() - startTime;
    const progress = Math.min(elapsed / fadeDurationMs, 1.0);
    const currentVolume = progress * targetVolume;
    audio.setVolume(currentVolume);

    if (progress >= 1.0) {
      clearInterval(fadeInterval);
    }
  }, 16); // ~60fps
}

/**
 * Stop audio with fade out effect
 * @param {THREE.Audio} audio
 * @param {number} fadeDurationMs - Fade duration in milliseconds
 * @param {boolean} pauseInsteadOfStop - If true, pause instead of stop
 */
export function stopAudioWithFadeOut(audio, fadeDurationMs, pauseInsteadOfStop) {
  if (!audio.isPlaying) return;

  const startVolume = audio.getVolume();
  const startTime = Date.now();

  const fadeInterval = setInterval(() => {
    const elapsed = Date.now() - startTime;
    const progress = Math.min(elapsed / fadeDurationMs, 1.0);
    const currentVolume = startVolume * (1.0 - progress);
    audio.setVolume(currentVolume);

    if (progress >= 1.0) {
      clearInterval(fadeInterval);
      if (pauseInsteadOfStop) {
        audio.pause();
      } else {
        audio.stop();
      }
      audio.setVolume(startVolume);
    }
  }, 16); // ~60fps
}

// ============================================================================
// AUDIO CONTEXT MANAGEMENT
// These functions use the game's actual AudioListener instead of creating new ones
// ============================================================================

/**
 * Get AudioContext state from AudioListener
 * @param {THREE.AudioListener} audioListener - The game's audio listener
 * @returns {string} - 'suspended', 'running', or 'closed'
 */
export function getAudioContextStateFromListener(audioListener) {
  return audioListener.context.state;
}

/**
 * Resume AudioContext from AudioListener
 * @param {THREE.AudioListener} audioListener - The game's audio listener
 */
export function resumeAudioContextFromListener(audioListener) {
  if (audioListener.context.state === 'suspended') {
    audioListener.context.resume();
  }
}

// ============================================================================
// CSS2D RENDERER HELPERS
// ============================================================================

/**
 * Append an element to a container
 * Used to add CSS2D renderer's DOM element to the game container
 * @param {HTMLElement} container - The container element
 * @param {HTMLElement} element - The element to append
 */
export function appendElementToContainer(container, element) {
  // Ensure container has relative positioning for absolute children
  const containerPosition = window.getComputedStyle(container).position;
  if (containerPosition === 'static') {
    container.style.position = 'relative';
  }

  // Style the CSS2D renderer element to overlay the canvas
  element.style.position = 'absolute';
  element.style.top = '0';
  element.style.left = '0';
  element.style.pointerEvents = 'none';
  container.appendChild(element);
}
