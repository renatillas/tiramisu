/// Renderer module - applies patches to Three.js scene

import tiramisu/scene/diff.{type Patch}
import tiramisu/three/scene

/// Apply a list of patches to the Three.js scene
@external(javascript, "./ffi/renderer.mjs", "applyPatches")
pub fn apply_patches(scene: scene.Scene, patches: List(Patch)) -> Nil

/// Clear the object cache (useful for cleanup)
@external(javascript, "./ffi/renderer.mjs", "clearCache")
pub fn clear_cache() -> Nil
