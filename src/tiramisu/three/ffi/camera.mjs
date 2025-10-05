import * as THREE from 'three';

export function createPerspectiveCamera(fov, aspect, near, far) {
  return new THREE.PerspectiveCamera(fov, aspect, near, far);
}

export function createOrthographicCamera(left, right, top, bottom, near, far) {
  return new THREE.OrthographicCamera(left, right, top, bottom, near, far);
}

export function setCameraPosition(camera, x, y, z) {
  console.log('[Tiramisu] Setting camera position to:', x, y, z);
  camera.position.set(x, y, z);
  console.log('[Tiramisu] Camera position after set:', camera.position);
  return camera;
}

export function lookAt(camera, x, y, z) {
  console.log('[Tiramisu] Camera looking at:', x, y, z);
  camera.lookAt(x, y, z);
  return camera;
}

export function updateProjectionMatrix(camera) {
  camera.updateProjectionMatrix();
  return camera;
}
