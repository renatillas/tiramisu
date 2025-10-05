import * as THREE from 'three';
import { Ok, Error } from '../../gleam.mjs';

// Define error constructors to match Gleam types
class LoadError {
  constructor(message) {
    this.message = message;
  }
}

class InvalidUrl {
  constructor(url) {
    this.url = url;
  }
}

// Promise-based texture loading
export function loadTextureAsync(url) {
  return new Promise((resolve) => {
    if (!url || url.trim() === '') {
      resolve(new Error(new InvalidUrl(url)));
      return;
    }

    const loader = new THREE.TextureLoader();

    loader.load(
      url,
      (texture) => {
        resolve(new Ok(texture));
      },
      undefined,
      (error) => {
        resolve(new Error(new LoadError(error.message || 'Failed to load texture')));
      }
    );
  });
}
