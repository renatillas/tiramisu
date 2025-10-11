import * as THREE from 'three';

let currentCamera = null;

export function setCamera(camera) {
  currentCamera = camera;
}

export function getCamera() {
  return currentCamera;
}

/**
 * Create Three.js camera from projection config
 * Used by renderer.mjs when creating Camera scene nodes
 * @param {Object} projection - Camera projection configuration
 * @param {Array|null} viewport - Optional viewport [x, y, width, height]
 * @param {HTMLCanvasElement|null} canvas - Canvas element for aspect ratio calculation
 */
export function createThreeCamera(projection, viewport = null, canvas = null) {
  if (projection.fov !== undefined) {
    // Perspective camera - calculate aspect ratio from viewport, canvas, or window
    let aspect;
    if (viewport && viewport[2] && viewport[3]) {
      // Use viewport dimensions if specified
      aspect = viewport[2] / viewport[3];
    } else if (canvas) {
      // Use canvas CSS dimensions (clientWidth/Height) for correct aspect ratio
      aspect = canvas.clientWidth / canvas.clientHeight;
    } else {
      // Fallback to window dimensions
      aspect = window.innerWidth / window.innerHeight;
    }

    return new THREE.PerspectiveCamera(
      projection.fov,
      aspect,
      projection.near,
      projection.far
    );
  } else {
    // Orthographic camera
    return new THREE.OrthographicCamera(
      projection.left,
      projection.right,
      projection.top,
      projection.bottom,
      projection.near,
      projection.far
    );
  }
}
