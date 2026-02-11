//// Scene node types for the Tiramisu scene graph.
////
//// These types represent the declarative scene description parsed from
//// the light DOM. The renderer diffs successive scene descriptions to
//// produce minimal patches applied via runtime.gleam.
////
//// Each variant corresponds to a DOM element tag name. Every node carries
//// a `key` (the element's `id` attribute) used for O(1) lookups during
//// diffing.

import tiramisu/transform.{type Transform}

// TYPES -----------------------------------------------------------------------

/// A node in the scene description tree.
pub type SceneNode {
  MeshNode(
    key: String,
    geometry: String,
    src: String,
    color: String,
    metalness: Float,
    roughness: Float,
    opacity: Float,
    wireframe: Bool,
    transform: Transform,
    visible: Bool,
    physics_controlled: Bool,
    children: List(SceneNode),
  )
  CameraNode(
    key: String,
    camera_type: String,
    fov: Float,
    near: Float,
    far: Float,
    transform: Transform,
    active: Bool,
  )
  LightNode(
    key: String,
    light_type: String,
    color: String,
    intensity: Float,
    transform: Transform,
    cast_shadow: Bool,
  )
  EmptyNode(
    key: String,
    transform: Transform,
    visible: Bool,
    children: List(SceneNode),
  )
  AudioNode(
    key: String,
    src: String,
    volume: Float,
    loop: Bool,
    playing: Bool,
    playback_rate: Float,
  )
  PositionalAudioNode(
    key: String,
    src: String,
    volume: Float,
    loop: Bool,
    playing: Bool,
    playback_rate: Float,
    transform: Transform,
    ref_distance: Float,
    max_distance: Float,
    rolloff_factor: Float,
    children: List(SceneNode),
  )
  /// Unknown elements (cocoa physics components, etc.) — preserved as pass-through.
  /// The renderer ignores these but keeps them in the tree for child traversal.
  UnknownNode(key: String, tag: String, children: List(SceneNode))
}

// ACCESSORS -------------------------------------------------------------------

/// Extract the key (id) from any scene node.
pub fn key(node: SceneNode) -> String {
  case node {
    MeshNode(key:, ..) -> key
    CameraNode(key:, ..) -> key
    LightNode(key:, ..) -> key
    EmptyNode(key:, ..) -> key
    AudioNode(key:, ..) -> key
    PositionalAudioNode(key:, ..) -> key
    UnknownNode(key:, ..) -> key
  }
}

/// Extract children from any scene node.
pub fn children(node: SceneNode) -> List(SceneNode) {
  case node {
    MeshNode(children:, ..) -> children
    EmptyNode(children:, ..) -> children
    PositionalAudioNode(children:, ..) -> children
    UnknownNode(children:, ..) -> children
    _ -> []
  }
}

/// Extract the tag name string for a scene node (used in diff for type comparison).
pub fn tag(node: SceneNode) -> String {
  case node {
    MeshNode(..) -> "tiramisu-mesh"
    CameraNode(..) -> "tiramisu-camera"
    LightNode(..) -> "tiramisu-light"
    EmptyNode(..) -> "tiramisu-empty"
    AudioNode(..) -> "tiramisu-audio"
    PositionalAudioNode(..) -> "tiramisu-audio-positional"
    UnknownNode(tag:, ..) -> tag
  }
}
