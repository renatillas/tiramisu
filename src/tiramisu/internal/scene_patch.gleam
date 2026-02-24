//// Patch types produced by the scene diff algorithm.
////
//// Four generic variants replace the previous type-specific variants.
//// Handlers in scene_apply.gleam interpret the attrs dict for each tag.

import gleam/dict.{type Dict}
import tiramisu/transform.{type Transform}

/// A single patch operation on the scene graph.
pub type ScenePatch {
  /// Create a new Three.js object for the given element.
  CreateNode(
    id: String,
    parent_id: String,
    tag: String,
    attrs: Dict(String, String),
    transform: Transform,
  )
  /// Update an existing Three.js object.
  /// Both old_attrs and new_attrs are supplied so handlers can diff at the
  /// property level if they choose to avoid unnecessary Three.js work.
  UpdateNode(
    id: String,
    tag: String,
    old_attrs: Dict(String, String),
    new_attrs: Dict(String, String),
    transform: Transform,
  )
  /// Remove an existing Three.js object from the scene.
  Remove(id: String)
  /// Move an existing object to a new parent.
  Reparent(id: String, new_parent_id: String)
}
