import * as THREE from 'three';

/**
 * Create Three.js camera from projection config
 * Used by renderer.mjs when creating Camera scene nodes
 */
export function createThreeCamera(projection) {
  if (projection.fov !== undefined) {
    // Perspective camera
    return new THREE.PerspectiveCamera(
      projection.fov,
      projection.aspect,
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
