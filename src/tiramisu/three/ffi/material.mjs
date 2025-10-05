import * as THREE from 'three';

export function createBasicMaterial(color, transparent, opacity) {
  return new THREE.MeshBasicMaterial({
    color: color,
    transparent: transparent,
    opacity: opacity,
  });
}

export function createBasicMaterialWithTexture(texture, transparent, opacity) {
  return new THREE.MeshBasicMaterial({
    map: texture,
    transparent: transparent,
    opacity: opacity,
  });
}

export function createStandardMaterial(color, metalness, roughness) {
  return new THREE.MeshStandardMaterial({
    color: color,
    metalness: metalness,
    roughness: roughness,
  });
}

export function createPhongMaterial(color, shininess) {
  return new THREE.MeshPhongMaterial({
    color: color,
    shininess: shininess,
  });
}

export function createSpriteMaterial(texture) {
  return new THREE.SpriteMaterial({
    map: texture,
  });
}
