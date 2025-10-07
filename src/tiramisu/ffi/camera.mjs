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
 */
export function createThreeCamera(projection, viewport = null) {
  if (projection.fov !== undefined) {
    // Perspective camera - calculate aspect ratio from viewport or window
    let aspect;
    if (viewport && viewport[2] && viewport[3]) {
      // Use viewport dimensions if specified
      aspect = viewport[2] / viewport[3];
    } else {
      // Use window dimensions (for fullscreen or main camera)
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
