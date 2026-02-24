//// Scene node types for the Tiramisu scene graph.
////
//// These types represent the declarative scene description parsed from
//// the light DOM. The renderer diffs successive scene descriptions to
//// produce minimal patches applied via runtime.gleam.
////
//// Each variant corresponds to a DOM element tag name. Every node carries
//// a `key` (the element's `id` attribute) used for O(1) lookups during
//// diffing.

import gleam/option.{type Option}
import tiramisu/audio
import tiramisu/camera
import tiramisu/debug
import tiramisu/empty
import tiramisu/instanced_mesh
import tiramisu/light
import tiramisu/mesh
import tiramisu/transform.{type Transform}

// TYPES -----------------------------------------------------------------------

/// A node in the scene description tree.
pub type SceneNode {
  MeshNode(
    key: String,
    transform: Transform,
    children: List(SceneNode),
    geometry: Option(String),
    src: Option(String),
    // Material properties
    material_type: Option(String),
    color: Option(String),
    metalness: Option(Float),
    roughness: Option(Float),
    opacity: Option(Float),
    wireframe: Option(Bool),
    emissive: Option(String),
    emissive_intensity: Option(Float),
    side: Option(String),
    // Texture map URLs
    color_map: Option(String),
    normal_map: Option(String),
    ao_map: Option(String),
    roughness_map: Option(String),
    metalness_map: Option(String),
    displacement_map: Option(String),
    displacement_scale: Option(Float),
    displacement_bias: Option(Float),
    // Additional material properties
    shininess: Option(Float),
    alpha_test: Option(Float),
    transparent: Option(Bool),
    // Transform & state
    visible: Option(Bool),
    cast_shadow: Option(Bool),
    receive_shadow: Option(Bool),
    physics_controlled: Option(Bool),
  )
  CameraNode(
    key: String,
    transform: Transform,
    children: List(SceneNode),
    camera_type: Option(String),
    fov: Option(Float),
    near: Option(Float),
    far: Option(Float),
    active: Option(Bool),
  )
  LightNode(
    key: String,
    transform: Transform,
    children: List(SceneNode),
    light_type: Option(String),
    color: Option(String),
    intensity: Option(Float),
    cast_shadow: Option(Bool),
  )
  EmptyNode(
    key: String,
    transform: Transform,
    children: List(SceneNode),
    visible: Option(Bool),
  )
  AudioNode(
    key: String,
    transform: Transform,
    children: List(SceneNode),
    src: Option(String),
    volume: Option(Float),
    loop: Option(Bool),
    playing: Option(Bool),
    playback_rate: Option(Float),
    detune: Option(Float),
  )
  PositionalAudioNode(
    key: String,
    transform: Transform,
    children: List(SceneNode),
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
  DebugNode(
    key: String,
    transform: Transform,
    children: List(SceneNode),
    debug_type: Option(String),
    size: Option(Float),
    divisions: Option(Int),
    color: Option(String),
  )
  InstancedMeshNode(
    key: String,
    transform: Transform,
    children: List(SceneNode),
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
  /// Unknown elements (cocoa physics components, etc.) — preserved as pass-through.
  /// The renderer ignores these but keeps them in the tree for child traversal.
  UnknownNode(
    key: String,
    transform: Transform,
    children: List(SceneNode),
    tag: String,
  )
}

/// Extract the tag name string for a scene node (used in diff for type comparison).
pub fn tag(node: SceneNode) -> String {
  case node {
    MeshNode(..) -> mesh.tag
    CameraNode(..) -> camera.tag
    LightNode(..) -> light.tag
    EmptyNode(..) -> empty.tag
    AudioNode(..) -> audio.global_tag
    PositionalAudioNode(..) -> audio.positional_tag
    DebugNode(..) -> debug.tag
    InstancedMeshNode(..) -> instanced_mesh.tag
    UnknownNode(tag:, ..) -> tag
  }
}
