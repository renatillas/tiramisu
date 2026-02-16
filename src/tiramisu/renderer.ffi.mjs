// FFI for the tiramisu-renderer web component.
// Only non-DOM utilities that can't be done in pure Gleam live here.

/**
 * Generate a unique scene ID for renderers without a user-set scene-id.
 */
let sceneCounter = 0;
export function generateSceneId() {
  sceneCounter += 1;
  return "scene_" + sceneCounter + "_" + Date.now().toString(36);
}
