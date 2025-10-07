/// Asset Management FFI
///
/// Handles audio loading and batch asset loading with progress tracking.
import * as THREE from 'three';
import * as GLEAM from '../../../gleam_stdlib/gleam.mjs';
import * as DICT from '../../../gleam_stdlib/gleam/dict.mjs';
import * as ASSETS_GLEAM from '../asset.mjs';
import * as GLTF from './gltf.mjs';
import * as TEXTURE from './texture.mjs';
import * as STL from './stl.mjs';

// Wrapper functions for Gleam types
function LoadProgress(loaded, total, current_url) {
  return new ASSETS_GLEAM.LoadProgress(loaded, total, current_url);
}

function BatchLoadResult(cache, errors) {
  return new ASSETS_GLEAM.BatchLoadResult(cache, errors);
}

function AssetCache(assetsDict) {
  // assetsDict is already a Gleam Dict with url -> LoadedAsset
  // We need to wrap each LoadedAsset in a CacheEntry
  let cacheDict = DICT.new$();

  // Use fold to iterate through the dict and wrap entries
  cacheDict = DICT.fold(
    assetsDict,
    cacheDict,
    (acc, url, loadedAsset) => {
      // CacheEntry is a private type, but we can use ASSETS_GLEAM to access it
      // Import the internal class structure from the compiled Gleam code
      const CacheEntryClass = Object.getPrototypeOf(
        Object.values(DICT.to_list(DICT.new$()))[0] || {}
      ).constructor;

      // Create CacheEntry manually matching the compiled structure
      const cacheEntry = Object.create(Object.getPrototypeOf({}));
      cacheEntry.asset = loadedAsset;
      cacheEntry.last_accessed = Date.now();

      return DICT.insert(acc, url, cacheEntry);
    }
  );

  // Create CacheConfig manually
  const cacheConfig = Object.create(Object.getPrototypeOf({}));
  cacheConfig.max_size = 100;
  cacheConfig.current_time = Date.now();

  // Create AssetCache manually
  const assetCache = Object.create(Object.getPrototypeOf({}));
  assetCache.asset = cacheDict;
  assetCache.config = cacheConfig;

  return assetCache;
}

function AssetLoadError(url, reason) {
  return ASSETS_GLEAM.AssetError$AssetLoadError(url, reason);
}

// Audio loader singleton
let audioLoader = null;
let audioListener = null;

function getAudioLoader() {
  if (!audioLoader) {
    audioLoader = new THREE.AudioLoader();
  }
  return audioLoader;
}

/**
 * Load an audio file and return an AudioBuffer
 * @param {string} url - URL of the audio file
 * @returns {Promise<Result<AudioBuffer, string>>}
 */
export function loadAudio(url) {
  return new Promise((resolve) => {
    const loader = getAudioLoader();

    loader.load(
      url,
      // Success
      (audioBuffer) => {
        console.log(`[Tiramisu] Audio loaded: ${url}`);
        resolve(new GLEAM.Ok(audioBuffer));
      },
      // Progress (optional)
      undefined,
      // Error
      (error) => {
        console.error(`[Tiramisu] Audio load failed: ${url}`, error);
        resolve(new GLEAM.Error(error.message || 'Failed to load audio'));
      }
    );
  });
}

/**
 * Load multiple assets with progress tracking
 * @param {Array} assets - JavaScript Array of AssetType (converted from Gleam List)
 * @param {Function} onProgress - Progress callback
 * @returns {Promise<BatchLoadResult>}
 */
export function loadBatch(assets, onProgress) {
  const total = assets.length;
  let loaded = 0;
  let loadedAssetsDict = DICT.new$(); // Create Gleam Dict
  const loadedResults = []; // Track loaded assets to insert into dict later
  const errors = [];

  if (total === 0) {
    // Empty batch, return immediately
    return Promise.resolve(
      BatchLoadResult(
        AssetCache(DICT.new$()),
        GLEAM.toList([]) // Convert empty array to Gleam List
      )
    );
  }

  // Create promises for each asset
  const promises = assets.map((asset, index) => {
    const url = getAssetUrl(asset);

    // Report initial progress
    if (index === 0) {
      onProgress(LoadProgress(0, total, url));
    }

    return loadAssetByType(asset)
      .then((result) => {
        loaded++;

        // Store result
        if (result.isOk()) {
          loadedResults.push({ url, asset: result[0] });
        } else {
          errors.push(AssetLoadError(url, result[0]));
        }

        // Report progress
        onProgress(LoadProgress(loaded, total, url));

        return result;
      })
      .catch((error) => {
        loaded++;
        const errorMsg = error.message || 'Unknown error';
        errors.push(AssetLoadError(url, errorMsg));
        onProgress(LoadProgress(loaded, total, url));
        return new GLEAM.Error(errorMsg);
      });
  });

  // Wait for all assets to finish (success or failure)
  return Promise.all(promises).then(() => {
    // Build Gleam Dict from loaded results
    for (const { url, asset } of loadedResults) {
      loadedAssetsDict = DICT.insert(loadedAssetsDict, url, asset);
    }

    console.log(`[Tiramisu] Batch load complete: ${loadedResults.length}/${total} succeeded, ${errors.length} failed`);

    return BatchLoadResult(
      AssetCache(loadedAssetsDict),
      GLEAM.toList(errors) // Convert JavaScript array to Gleam List
    );
  });
}

/**
 * Get URL from AssetType
 */
function getAssetUrl(asset) {
  // AssetType variants: ModelAsset, TextureAsset, AudioAsset, STLAsset
  // Each has a 'url' field
  return asset.url;
}

/**
 * Load an asset based on its type
 */
function loadAssetByType(asset) {
  const url = asset.url;

  // Check asset type by constructor name or type field
  const typeName = asset.constructor.name;

  if (typeName === 'ModelAsset') {
    // Load GLTF model
    return GLTF.loadGLTFAsync(url).then(result => {
      if (result.isOk()) {
        return new GLEAM.Ok(ASSETS_GLEAM.loaded_model(result[0]));
      }
      return result;
    });
  } else if (typeName === 'TextureAsset') {
    // Load texture
    return TEXTURE.loadTextureAsync(url).then(result => {
      if (result.isOk()) {
        return new GLEAM.Ok(ASSETS_GLEAM.loaded_texture(result[0]));
      }
      return result;
    });
  } else if (typeName === 'AudioAsset') {
    // Load audio
    return loadAudio(url).then(result => {
      if (result.isOk()) {
        return new GLEAM.Ok(ASSETS_GLEAM.loaded_audio(result[0]));
      }
      return result;
    });
  } else if (typeName === 'STLAsset') {
    // Load STL
    return STL.loadSTLAsync(url).then(result => {
      if (result.isOk()) {
        return new GLEAM.Ok(ASSETS_GLEAM.loaded_stl(result[0]));
      }
      return result;
    });
  } else {
    return Promise.resolve(new GLEAM.Error(`Unknown asset type: ${typeName}`));
  }
}

/**
 * Get or create the audio listener
 * This is attached to the camera and is needed for positional audio
 */
export function getAudioListener() {
  if (!audioListener) {
    audioListener = new THREE.AudioListener();
  }
  return audioListener;
}

/**
 * Dispose of a Three.js texture and free GPU memory
 * @param {THREE.Texture} texture - The texture to dispose
 */
export function disposeTexture(texture) {
  if (texture && texture.dispose) {
    texture.dispose();
    console.log('[Tiramisu] Texture disposed');
  }
}

/**
 * Dispose of a Three.js BufferGeometry and free GPU memory
 * @param {THREE.BufferGeometry} geometry - The geometry to dispose
 */
export function disposeGeometry(geometry) {
  if (geometry && geometry.dispose) {
    geometry.dispose();
    console.log('[Tiramisu] Geometry disposed');
  }
}

/**
 * Dispose of a Three.js Material and free GPU memory
 * @param {THREE.Material} material - The material to dispose
 */
export function disposeMaterial(material) {
  if (material) {
    // Dispose of material textures if they exist
    if (material.map) material.map.dispose();
    if (material.lightMap) material.lightMap.dispose();
    if (material.bumpMap) material.bumpMap.dispose();
    if (material.normalMap) material.normalMap.dispose();
    if (material.specularMap) material.specularMap.dispose();
    if (material.envMap) material.envMap.dispose();
    if (material.alphaMap) material.alphaMap.dispose();
    if (material.aoMap) material.aoMap.dispose();
    if (material.displacementMap) material.displacementMap.dispose();
    if (material.emissiveMap) material.emissiveMap.dispose();
    if (material.gradientMap) material.gradientMap.dispose();
    if (material.metalnessMap) material.metalnessMap.dispose();
    if (material.roughnessMap) material.roughnessMap.dispose();

    // Dispose of the material itself
    material.dispose();
    console.log('[Tiramisu] Material disposed');
  }
}

/**
 * Dispose of an Object3D recursively (geometry, materials, textures, children)
 * @param {THREE.Object3D} object - The object to dispose
 */
export function disposeObject3D(object) {
  if (!object) return;

  // Dispose geometry
  if (object.geometry) {
    object.geometry.dispose();
  }

  // Dispose material(s)
  if (object.material) {
    if (Array.isArray(object.material)) {
      object.material.forEach(material => disposeMaterial(material));
    } else {
      disposeMaterial(object.material);
    }
  }

  // Recursively dispose children
  if (object.children) {
    for (const child of object.children) {
      disposeObject3D(child);
    }
  }

  console.log('[Tiramisu] Object3D disposed');
}
