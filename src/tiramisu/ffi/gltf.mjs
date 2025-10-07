import * as THREE from 'three';
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';
import * as GLEAM from '../../gleam.mjs';
import * as ASSETS_GLEAM from '../assets.mjs';

// Define GLTFData constructor to match Gleam type
class GLTFData {
  constructor(scene, animations) {
    this.scene = scene;
    this.animations = animations;
  }
}

// Promise-based GLTF loading
export function loadGLTFAsync(url) {
  return new Promise((resolve) => {
    if (!url || url.trim() === '') {
      resolve(new GLEAM.Error(new ASSETS_GLEAM.InvalidUrl(url)));
      return;
    }

    console.log('[GLTF Loader] Loading from URL:', url);

    const loader = new GLTFLoader();

    loader.load(
      url,
      // Success callback
      (gltf) => {
        console.log('[GLTF Loader] ✅ Successfully loaded model');
        console.log('[GLTF Loader] Animations:', gltf.animations.length);
        console.log('[GLTF Loader] Scene children:', gltf.scene.children.length);

        // Log animation names if any
        if (gltf.animations.length > 0) {
          console.log('[GLTF Loader] Animation names:', gltf.animations.map(clip => clip.name));
        }

        // Convert animations array to Gleam list
        const animationsList = GLEAM.toList(gltf.animations);
        const data = new GLTFData(gltf.scene, animationsList);
        resolve(new GLEAM.Ok(data));
      },
      // Progress callback
      (progress) => {
        if (progress.lengthComputable) {
          const percentComplete = (progress.loaded / progress.total) * 100;
          console.log('[GLTF Loader] Loading progress:', percentComplete.toFixed(2) + '%');
        }
      },
      // Error callback
      (error) => {
        console.error('[GLTF Loader] ❌ Error:', error);

        if (error.message && error.message.includes('404')) {
          resolve(new GLEAM.Error(new ASSETS_GLEAM.LoadError('File not found: ' + url)));
        } else if (error.message && (error.message.includes('Invalid') || error.message.includes('parse'))) {
          resolve(new GLEAM.Error(new ASSETS_GLEAM.ParseError(error.message)));
        } else {
          resolve(new GLEAM.Error(new ASSETS_GLEAM.LoadError(error.message || 'Failed to load GLTF')));
        }
      }
    );
  });
}
