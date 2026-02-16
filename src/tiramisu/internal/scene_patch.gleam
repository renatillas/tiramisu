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
    material_type: String,
    color: String,
    metalness: Float,
    roughness: Float,
    opacity: Float,
    wireframe: Bool,
    emissive: String,
    emissive_intensity: Float,
    side: String,
    color_map: String,
    normal_map: String,
    ao_map: String,
    roughness_map: String,
    metalness_map: String,
    displacement_map: String,
    displacement_scale: Float,
    displacement_bias: Float,
    shininess: Float,
    alpha_test: Float,
    transparent: Bool,
    transform: Transform,
    visible: Bool,
    cast_shadow: Bool,
    receive_shadow: Bool,
    distance: Float,
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
    detune: Float,
  )
  CreatePositionalAudio(
    id: String,
    parent_id: String,
    src: String,
    volume: Float,
    loop: Bool,
    playing: Bool,
    playback_rate: Float,
    detune: Float,
    transform: Transform,
    ref_distance: Float,
    max_distance: Float,
    rolloff_factor: Float,
  )
  CreateDebug(
    id: String,
    parent_id: String,
    debug_type: String,
    size: Float,
    divisions: Int,
    color: String,
    transform: Transform,
  )
  CreateLod(id: String, parent_id: String, transform: Transform)
  CreateInstancedMesh(
    id: String,
    parent_id: String,
    geometry: String,
    material_type: String,
    color: String,
    metalness: Float,
    roughness: Float,
    opacity: Float,
    wireframe: Bool,
    transparent: Bool,
    instances: String,
    transform: Transform,
    visible: Bool,
    cast_shadow: Bool,
    receive_shadow: Bool,
  )
  // -- Updates (granular per-property to minimize JS work) --------------------
  UpdateTransform(id: String, transform: Transform)
  UpdateMeshGeometry(id: String, geometry: String)
  UpdateMeshSrc(id: String, src: String)
  UpdateMeshMaterial(
    id: String,
    material_type: String,
    color: String,
    metalness: Float,
    roughness: Float,
    opacity: Float,
    wireframe: Bool,
    emissive: String,
    emissive_intensity: Float,
    side: String,
    color_map: String,
    normal_map: String,
    ao_map: String,
    roughness_map: String,
    metalness_map: String,
    displacement_map: String,
    displacement_scale: Float,
    displacement_bias: Float,
    shininess: Float,
    alpha_test: Float,
    transparent: Bool,
  )
  UpdateMeshShadow(id: String, cast_shadow: Bool, receive_shadow: Bool)
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
    detune: Float,
  )
  UpdatePositionalAudio(
    id: String,
    src: String,
    volume: Float,
    loop: Bool,
    playing: Bool,
    playback_rate: Float,
    detune: Float,
    transform: Transform,
    ref_distance: Float,
    max_distance: Float,
    rolloff_factor: Float,
  )
  UpdateGroupVisibility(id: String, visible: Bool)
  UpdateInstancedMeshInstances(id: String, instances: String)
  UpdateInstancedMeshMaterial(
    id: String,
    material_type: String,
    color: String,
    metalness: Float,
    roughness: Float,
    opacity: Float,
    wireframe: Bool,
    transparent: Bool,
  )
  UpdateInstancedMeshVisibility(id: String, visible: Bool)
  UpdateInstancedMeshShadow(id: String, cast_shadow: Bool, receive_shadow: Bool)
  // -- Structure --------------------------------------------------------------
  Remove(id: String)
  Reparent(id: String, new_parent_id: String)
}
