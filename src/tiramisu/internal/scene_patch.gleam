//// Patch types produced by the scene diff algorithm.
////
//// Each variant represents a minimal mutation to apply to the Three.js
//// scene graph via runtime.gleam. Patches are designed to be granular
//// so only changed properties trigger JS work.

import tiramisu/transform.{type Transform}

/// A single patch operation on the scene graph.
pub type ScenePatch {
  // -- Creation ---------------------------------------------------------------
  CreateMesh(
    id: String,
    parent_id: String,
    geometry: String,
    src: String,
    color: String,
    metalness: Float,
    roughness: Float,
    opacity: Float,
    wireframe: Bool,
    transform: Transform,
    visible: Bool,
  )
  CreateCamera(
    id: String,
    parent_id: String,
    camera_type: String,
    fov: Float,
    near: Float,
    far: Float,
    transform: Transform,
    active: Bool,
  )
  CreateLight(
    id: String,
    parent_id: String,
    light_type: String,
    color: String,
    intensity: Float,
    transform: Transform,
    cast_shadow: Bool,
  )
  CreateGroup(
    id: String,
    parent_id: String,
    transform: Transform,
    visible: Bool,
  )
  CreateAudio(
    id: String,
    src: String,
    volume: Float,
    loop: Bool,
    playing: Bool,
    playback_rate: Float,
  )
  CreatePositionalAudio(
    id: String,
    parent_id: String,
    src: String,
    volume: Float,
    loop: Bool,
    playing: Bool,
    playback_rate: Float,
    transform: Transform,
    ref_distance: Float,
    max_distance: Float,
    rolloff_factor: Float,
  )
  // -- Updates (granular per-property to minimize JS work) --------------------
  UpdateTransform(id: String, transform: Transform)
  UpdateMeshGeometry(id: String, geometry: String)
  UpdateMeshSrc(id: String, src: String)
  UpdateMeshMaterial(
    id: String,
    color: String,
    metalness: Float,
    roughness: Float,
    opacity: Float,
    wireframe: Bool,
  )
  UpdateMeshVisibility(id: String, visible: Bool)
  UpdateCameraProps(
    id: String,
    camera_type: String,
    fov: Float,
    near: Float,
    far: Float,
  )
  UpdateCameraActive(id: String, active: Bool)
  UpdateLightProps(
    id: String,
    color: String,
    intensity: Float,
    cast_shadow: Bool,
  )
  UpdateAudioProps(
    id: String,
    src: String,
    volume: Float,
    loop: Bool,
    playing: Bool,
    playback_rate: Float,
  )
  UpdatePositionalAudio(
    id: String,
    src: String,
    volume: Float,
    loop: Bool,
    playing: Bool,
    playback_rate: Float,
    transform: Transform,
    ref_distance: Float,
    max_distance: Float,
    rolloff_factor: Float,
  )
  UpdateGroupVisibility(id: String, visible: Bool)
  // -- Structure --------------------------------------------------------------
  Remove(id: String)
  Reparent(id: String, new_parent_id: String)
}
