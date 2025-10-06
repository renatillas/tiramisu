/// Asset Management FFI
///
/// Handles audio loading and batch asset loading with progress tracking.
import * as THREE from 'three';
import { Ok, Error } from '../../../gleam_stdlib/gleam.mjs';
import { toList } from '../../../gleam_stdlib/gleam.mjs';
import { new$ as dictNew, insert as dictInsert } from '../../../gleam_stdlib/gleam/dict.mjs';
import {
  LoadProgress as GleamLoadProgress,
  BatchLoadResult as GleamBatchLoadResult,
  AssetError$AssetLoadError,
  loaded_model,
  loaded_texture,
  loaded_audio,
  loaded_stl
} from '../assets.mjs';
import { loadGLTFAsync } from './gltf.mjs';
import { loadTextureAsync } from './texture.mjs';
import { loadSTLAsync } from './stl.mjs';

// Wrapper functions for Gleam types
function LoadProgress(loaded, total, current_url) {
  return new GleamLoadProgress(loaded, total, current_url);
}

function BatchLoadResult(cache, errors) {
  return new GleamBatchLoadResult(cache, errors);
}

function AssetCache(assets) {
  return { assets };
}

function AssetLoadError(url, reason) {
  return AssetError$AssetLoadError(url, reason);
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
        resolve(new Ok(audioBuffer));
      },
      // Progress (optional)
      undefined,
      // Error
      (error) => {
        console.error(`[Tiramisu] Audio load failed: ${url}`, error);
        resolve(new Error(error.message || 'Failed to load audio'));
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
  let loadedAssetsDict = dictNew(); // Create Gleam Dict
  const loadedResults = []; // Track loaded assets to insert into dict later
  const errors = [];

  if (total === 0) {
    // Empty batch, return immediately
    return Promise.resolve(
      BatchLoadResult(
        AssetCache(dictNew()),
        toList([]) // Convert empty array to Gleam List
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
        return new Error(errorMsg);
      });
  });

  // Wait for all assets to finish (success or failure)
  return Promise.all(promises).then(() => {
    // Build Gleam Dict from loaded results
    for (const { url, asset } of loadedResults) {
      loadedAssetsDict = dictInsert(loadedAssetsDict, url, asset);
    }

    console.log(`[Tiramisu] Batch load complete: ${loadedResults.length}/${total} succeeded, ${errors.length} failed`);

    return BatchLoadResult(
      AssetCache(loadedAssetsDict),
      toList(errors) // Convert JavaScript array to Gleam List
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
    return loadGLTFAsync(url).then(result => {
      if (result.isOk()) {
        return new Ok(loaded_model(result[0]));
      }
      return result;
    });
  } else if (typeName === 'TextureAsset') {
    // Load texture
    return loadTextureAsync(url).then(result => {
      if (result.isOk()) {
        return new Ok(loaded_texture(result[0]));
      }
      return result;
    });
  } else if (typeName === 'AudioAsset') {
    // Load audio
    return loadAudio(url).then(result => {
      if (result.isOk()) {
        return new Ok(loaded_audio(result[0]));
      }
      return result;
    });
  } else if (typeName === 'STLAsset') {
    // Load STL
    return loadSTLAsync(url).then(result => {
      if (result.isOk()) {
        return new Ok(loaded_stl(result[0]));
      }
      return result;
    });
  } else {
    return Promise.resolve(new Error(`Unknown asset type: ${typeName}`));
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
