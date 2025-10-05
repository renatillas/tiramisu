import * as THREE from 'three';

export function createRenderer(options) {
  const renderer = new THREE.WebGLRenderer({
    antialias: options.antialias,
    alpha: options.alpha,
  });
  renderer.setSize(options.width, options.height);
  renderer.setPixelRatio(window.devicePixelRatio);
  return renderer;
}

export function render(renderer, scene, camera) {
  renderer.render(scene, camera);
}

export function setSize(renderer, width, height) {
  renderer.setSize(width, height);
  return renderer;
}

export function getDomElement(renderer) {
  return renderer.domElement;
}

export function setClearColor(renderer, color, alpha) {
  renderer.setClearColor(color, alpha);
  return renderer;
}
