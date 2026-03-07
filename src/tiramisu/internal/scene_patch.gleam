//// Patch types produced by the scene diff algorithm.
////
//// Four generic variants replace the previous type-specific variants.
//// Handlers in scene_apply.gleam interpret the attrs dict for each tag.

import gleam/dict.{type Dict}
import gleam/set.{type Set}

/// A single patch operation on the scene graph.
pub type ScenePatch {
  /// Create a new Three.js object for the given element.
  CreateNode(
    id: String,
    parent_id: String,
    tag: String,
    attributes: Dict(String, String),
  )
  /// Update an existing Three.js object.
  UpdateNode(
    id: String,
    parent_id: String,
    tag: String,
    attributes: Dict(String, String),
    changed_attributes: Set(String),
  )
  /// Remove an existing Three.js object from the scene.
  Remove(id: String)
  /// Move an existing object to a new parent.
  Reparent(id: String, new_parent_id: String)
}
