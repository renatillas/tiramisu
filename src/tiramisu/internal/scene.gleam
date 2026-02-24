//// Scene node type for the Tiramisu scene graph.
////
//// A single generic SceneNode holds all element-specific data in an attrs
//// dict. Handlers in scene_apply.gleam interpret the attrs for each tag.
//// The renderer diffs successive scene descriptions to produce minimal
//// patches applied via scene_apply.gleam.
////
//// Every node carries a `key` (the element's `id` attribute) used for
//// O(1) lookups during diffing. All element-specific attributes except
//// `id` and `transform` live in the `attrs` dict.

import gleam/dict.{type Dict}
import tiramisu/transform.{type Transform}

// TYPES -----------------------------------------------------------------------

/// A node in the scene description tree.
pub type Node {
  SceneNode(
    key: String,
    tag: String,
    transform: Transform,
    children: List(Node),
    /// All element attributes except `id` and `transform`.
    attrs: Dict(String, String),
  )
}
