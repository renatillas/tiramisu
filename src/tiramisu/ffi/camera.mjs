import * as THREE from 'three';

// Internal mutable Three.js camera
let internalCamera = null;

/**
 * Update the internal Three.js camera based on the immutable camera config
 */
export function updateCamera(cameraConfig) {
  const { position, look_at_target, projection } = cameraConfig;

  // Create or recreate camera if projection changed
  if (!internalCamera || needsRecreation(internalCamera, projection)) {
    internalCamera = createThreeCamera(projection);
  }

  // Update position
  const x = position.x
  const y = position.y
  const z = position.z
  internalCamera.position.set(x, y, z);

  // Update look at
  const lx = look_at_target.x
  const ly = look_at_target.y
  const lz = look_at_target.z
  internalCamera.lookAt(lx, ly, lz);

  // Update projection matrix
  internalCamera.updateProjectionMatrix();
}

/**
 * Get the internal Three.js camera for rendering
 */
export function getInternalCamera() {
  return internalCamera;
}

/**
 * Create Three.js camera from projection config
 */
function createThreeCamera(projection) {
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

/**
 * Check if camera needs to be recreated due to projection change
 */
function needsRecreation(camera, projection) {
  const isPerspective = projection.fov !== undefined;
  const cameraIsPerspective = camera instanceof THREE.PerspectiveCamera;

  if (isPerspective !== cameraIsPerspective) {
    return true;
  }

  if (isPerspective) {
    return camera.fov !== projection.fov ||
           camera.aspect !== projection.aspect ||
           camera.near !== projection.near ||
           camera.far !== projection.far;
  } else {
    return camera.left !== projection.left ||
           camera.right !== projection.right ||
           camera.top !== projection.top ||
           camera.bottom !== projection.bottom ||
           camera.near !== projection.near ||
           camera.far !== projection.far;
  }
}
