import * as THREE from 'three';
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';
import { Ok, Error, toList } from '../../gleam.mjs';

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

class ParseError {
  constructor(message) {
    this.message = message;
  }
}

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
      resolve(new Error(new InvalidUrl(url)));
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
        const animationsList = toList(gltf.animations);
        const data = new GLTFData(gltf.scene, animationsList);
        resolve(new Ok(data));
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
          resolve(new Error(new LoadError('File not found: ' + url)));
        } else if (error.message && (error.message.includes('Invalid') || error.message.includes('parse'))) {
          resolve(new Error(new ParseError(error.message)));
        } else {
          resolve(new Error(new LoadError(error.message || 'Failed to load GLTF')));
        }
      }
    );
  });
}
