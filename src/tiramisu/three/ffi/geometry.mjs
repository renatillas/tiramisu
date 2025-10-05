import * as THREE from 'three';

export function createBoxGeometry(width, height, depth) {
  return new THREE.BoxGeometry(width, height, depth);
}

export function createPlaneGeometry(width, height) {
  return new THREE.PlaneGeometry(width, height);
}

export function createSphereGeometry(radius, widthSegments, heightSegments) {
  return new THREE.SphereGeometry(radius, widthSegments, heightSegments);
}

export function createCircleGeometry(radius, segments) {
  return new THREE.CircleGeometry(radius, segments);
}

export function createConeGeometry(radius, height, segments) {
  return new THREE.ConeGeometry(radius, height, segments);
}
