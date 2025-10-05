import * as THREE from 'three';

export function createMesh(geometry, material) {
  return new THREE.Mesh(geometry, material);
}

export function setPosition(mesh, x, y, z) {
  mesh.position.set(x, y, z);
  return mesh;
}

export function setRotation(mesh, x, y, z) {
  mesh.rotation.set(x, y, z);
  return mesh;
}

export function setScale(mesh, x, y, z) {
  mesh.scale.set(x, y, z);
  return mesh;
}

export function rotateX(mesh, angle) {
  mesh.rotation.x += angle;
  return mesh;
}

export function rotateY(mesh, angle) {
  mesh.rotation.y += angle;
  return mesh;
}

export function rotateZ(mesh, angle) {
  mesh.rotation.z += angle;
  return mesh;
}
