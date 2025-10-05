import * as THREE from 'three';

export function createScene() {
  return new THREE.Scene();
}

export function addToScene(scene, object) {
  scene.add(object);
  return scene;
}

export function removeFromScene(scene, object) {
  scene.remove(object);
  return scene;
}

export function setBackground(scene, color) {
  scene.background = new THREE.Color(color);
  return scene;
}
