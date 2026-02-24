//// Patch types produced by the scene diff algorithm.
////
//// Each variant represents a minimal mutation to apply to the Three.js
//// scene graph via runtime.gleam. Patches are designed to be granular
//// so only changed properties trigger JS work.

import gleam/option.{type Option}
import tiramisu/transform.{type Transform}

/// A single patch operation on the scene graph.
pub type ScenePatch {
  // -- Creation ---------------------------------------------------------------
  CreateMesh(
    id: String,
    transform: Transform,
    parent_id: String,
    geometry: Option(String),
    src: Option(String),
    material_type: Option(String),
    color: Option(String),
    metalness: Option(Float),
    roughness: Option(Float),
    opacity: Option(Float),
    wireframe: Option(Bool),
    emissive: Option(String),
    emissive_intensity: Option(Float),
    side: Option(String),
    color_map: Option(String),
    normal_map: Option(String),
    ao_map: Option(String),
    roughness_map: Option(String),
    metalness_map: Option(String),
    displacement_map: Option(String),
    displacement_scale: Option(Float),
    displacement_bias: Option(Float),
    shininess: Option(Float),
    alpha_test: Option(Float),
    transparent: Option(Bool),
    visible: Option(Bool),
    cast_shadow: Option(Bool),
    receive_shadow: Option(Bool),
  )
  CreateCamera(
    id: String,
    parent_id: String,
    transform: Transform,
    camera_type: Option(String),
    fov: Option(Float),
    near: Option(Float),
    far: Option(Float),
    active: Option(Bool),
  )
  CreateLight(
    id: String,
    parent_id: String,
    light_type: Option(String),
    color: Option(String),
    intensity: Option(Float),
    transform: Transform,
    cast_shadow: Option(Bool),
  )
  CreateGroup(
    id: String,
    parent_id: String,
    transform: Transform,
    visible: Option(Bool),
  )
  CreateAudio(
    id: String,
    parent_id: String,
    src: Option(String),
    volume: Option(Float),
    loop: Option(Bool),
    playing: Option(Bool),
    playback_rate: Option(Float),
    detune: Option(Float),
  )
  CreatePositionalAudio(
    id: String,
    parent_id: String,
    transform: Transform,
    src: Option(String),
    volume: Option(Float),
    loop: Option(Bool),
    playing: Option(Bool),
    playback_rate: Option(Float),
    detune: Option(Float),
    ref_distance: Option(Float),
    max_distance: Option(Float),
    rolloff_factor: Option(Float),
  )
  CreateDebug(
    id: String,
    parent_id: String,
    transform: Transform,
    debug_type: Option(String),
    size: Option(Float),
    divisions: Option(Int),
    color: Option(String),
  )
  CreateInstancedMesh(
    id: String,
    parent_id: String,
    transform: Transform,
    geometry: Option(String),
    material_type: Option(String),
    color: Option(String),
    metalness: Option(Float),
    roughness: Option(Float),
    opacity: Option(Float),
    wireframe: Option(Bool),
    transparent: Option(Bool),
    instances: Option(String),
    visible: Option(Bool),
    cast_shadow: Option(Bool),
    receive_shadow: Option(Bool),
  )
  // -- Updates (granular per-property to minimize JS work) --------------------
  UpdateTransform(id: String, transform: Transform)
  UpdateMeshGeometry(id: String, geometry: Option(String))
  UpdateMeshSrc(id: String, src: Option(String))
  UpdateMeshMaterial(
    id: String,
    material_type: Option(String),
    color: Option(String),
    metalness: Option(Float),
    roughness: Option(Float),
    opacity: Option(Float),
    wireframe: Option(Bool),
    emissive: Option(String),
    emissive_intensity: Option(Float),
    side: Option(String),
    color_map: Option(String),
    normal_map: Option(String),
    ao_map: Option(String),
    roughness_map: Option(String),
    metalness_map: Option(String),
    displacement_map: Option(String),
    displacement_scale: Option(Float),
    displacement_bias: Option(Float),
    shininess: Option(Float),
    alpha_test: Option(Float),
    transparent: Option(Bool),
  )
  UpdateMeshShadow(
    id: String,
    cast_shadow: Option(Bool),
    receive_shadow: Option(Bool),
  )
  UpdateMeshVisibility(id: String, visible: Option(Bool))
  UpdateCameraProps(
    id: String,
    camera_type: Option(String),
    fov: Option(Float),
    near: Option(Float),
    far: Option(Float),
  )
  UpdateCameraActive(id: String, active: Option(Bool))
  UpdateLightProps(
    id: String,
    color: Option(String),
    intensity: Option(Float),
    cast_shadow: Option(Bool),
  )
  UpdateAudioProps(
    id: String,
    src: Option(String),
    volume: Option(Float),
    loop: Option(Bool),
    playing: Option(Bool),
    playback_rate: Option(Float),
    detune: Option(Float),
  )
  UpdatePositionalAudio(
    id: String,
    src: Option(String),
    volume: Option(Float),
    loop: Option(Bool),
    playing: Option(Bool),
    playback_rate: Option(Float),
    detune: Option(Float),
    transform: Transform,
    ref_distance: Option(Float),
    max_distance: Option(Float),
    rolloff_factor: Option(Float),
  )
  UpdateGroupVisibility(id: String, visible: Option(Bool))
  UpdateInstancedMeshInstances(id: String, instances: Option(String))
  UpdateInstancedMeshMaterial(
    id: String,
    material_type: Option(String),
    color: Option(String),
    metalness: Option(Float),
    roughness: Option(Float),
    opacity: Option(Float),
    wireframe: Option(Bool),
    transparent: Option(Bool),
  )
  UpdateInstancedMeshVisibility(id: String, visible: Option(Bool))
  UpdateInstancedMeshShadow(
    id: String,
    cast_shadow: Option(Bool),
    receive_shadow: Option(Bool),
  )
  // -- Structure --------------------------------------------------------------
  Remove(id: String)
  Reparent(id: String, new_parent_id: String)
}
