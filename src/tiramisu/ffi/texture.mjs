import * as THREE from 'three';
import * as GLEAM from '../../gleam.mjs';
import * as ASSETS_GLEAM from '../asset.mjs';

// Promise-based texture loading
export function loadTextureAsync(url) {
  return new Promise((resolve) => {
    if (!url || url.trim() === '') {
      resolve(new GLEAM.Error(new ASSETS_GLEAM.InvalidUrl(url)));
      return;
    }

    const loader = new THREE.TextureLoader();

    loader.load(
      url,
      (texture) => {
        resolve(new GLEAM.Ok(texture));
      },
      undefined,
      (error) => {
        resolve(new GLEAM.Error(new ASSETS_GLEAM.LoadError(error.message || 'Failed to load texture')));
      }
    );
  });
}
