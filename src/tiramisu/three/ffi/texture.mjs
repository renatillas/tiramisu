import * as THREE from 'three';
import { Ok, Error } from '../../../gleam.mjs';

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

export function loadTexture(url, onSuccess, onError) {
  const loader = new THREE.TextureLoader();

  loader.load(
    url,
    (texture) => {
      onSuccess(texture);
    },
    undefined,
    (error) => {
      onError(error.message || 'Failed to load texture');
    }
  );
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

// Helper to convert Gleam TextureWrap enum to THREE.js constant
function getWrapping(wrapEnum) {
  // wrapEnum is a Gleam type: ClampToEdge, Repeat, or MirroredRepeat
  if (wrapEnum.constructor.name === 'ClampToEdge') {
    return THREE.ClampToEdgeWrapping;
  } else if (wrapEnum.constructor.name === 'Repeat') {
    return THREE.RepeatWrapping;
  } else if (wrapEnum.constructor.name === 'MirroredRepeat') {
    return THREE.MirroredRepeatWrapping;
  }
  return THREE.RepeatWrapping; // default
}

// Helper to convert Gleam TextureFilter enum to THREE.js constant
function getFilter(filterEnum) {
  if (filterEnum.constructor.name === 'Nearest') {
    return THREE.NearestFilter;
  } else if (filterEnum.constructor.name === 'Linear') {
    return THREE.LinearFilter;
  }
  return THREE.LinearFilter; // default
}

// Load texture with custom options
export function loadTextureWithOptions(url, options) {
  return new Promise((resolve) => {
    if (!url || url.trim() === '') {
      resolve(new Error(new InvalidUrl(url)));
      return;
    }

    const loader = new THREE.TextureLoader();

    loader.load(
      url,
      (texture) => {
        // Apply options
        texture.wrapS = getWrapping(options.wrap_s);
        texture.wrapT = getWrapping(options.wrap_t);
        texture.minFilter = getFilter(options.min_filter);
        texture.magFilter = getFilter(options.mag_filter);
        texture.needsUpdate = true;

        resolve(new Ok(texture));
      },
      undefined,
      (error) => {
        resolve(new Error(new LoadError(error.message || 'Failed to load texture')));
      }
    );
  });
}

export function createTexture(image) {
  return new THREE.Texture(image);
}

export function setTextureWrapS(texture, wrap) {
  texture.wrapS = wrap;
  return texture;
}

export function setTextureWrapT(texture, wrap) {
  texture.wrapT = wrap;
  return texture;
}

export function setTextureFilter(texture, minFilter, magFilter) {
  texture.minFilter = minFilter;
  texture.magFilter = magFilter;
  return texture;
}

export function updateTexture(texture) {
  texture.needsUpdate = true;
  return texture;
}

// Texture wrap constants
export const ClampToEdgeWrapping = THREE.ClampToEdgeWrapping;
export const RepeatWrapping = THREE.RepeatWrapping;
export const MirroredRepeatWrapping = THREE.MirroredRepeatWrapping;

// Texture filter constants
export const NearestFilter = THREE.NearestFilter;
export const LinearFilter = THREE.LinearFilter;
