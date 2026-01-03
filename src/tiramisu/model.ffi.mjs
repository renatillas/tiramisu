// Model FFI - GLTF and FBX data extraction functions
import { toList } from '../../gleam_stdlib/gleam.mjs';

// ============================================================================
// GLTF DATA EXTRACTION
// ============================================================================

/**
 * Get the scene (root Object3D) from GLTF data
 * @param {Object} gltf - Loaded GLTF data from GLTFLoader
 * @returns {THREE.Object3D}
 */
export function getGLTFScene(gltf) {
  return gltf.scene;
}

/**
 * Get animation clips from GLTF data
 * @param {Object} gltf - Loaded GLTF data from GLTFLoader
 * @returns {List<THREE.AnimationClip>}
 */
export function getGLTFAnimations(gltf) {
  return toList(gltf.animations || []);
}

/**
 * Get cameras from GLTF data
 * @param {Object} gltf - Loaded GLTF data from GLTFLoader
 * @returns {List<THREE.Camera>}
 */
export function getGLTFCameras(gltf) {
  return toList(gltf.cameras || []);
}

// ============================================================================
// FBX DATA EXTRACTION
// ============================================================================

/**
 * Get the scene (root Object3D) from FBX data
 * FBX data is itself a THREE.Group, so we just return it
 * @param {THREE.Group} fbx - Loaded FBX data from FBXLoader
 * @returns {THREE.Object3D}
 */
export function getFBXScene(fbx) {
  return fbx;
}

/**
 * Get animation clips from FBX data
 * @param {THREE.Group} fbx - Loaded FBX data from FBXLoader
 * @returns {List<THREE.AnimationClip>}
 */
export function getFBXAnimations(fbx) {
  return toList(fbx.animations || []);
}
