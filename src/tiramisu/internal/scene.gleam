//// Scene node type for the Tiramisu scene graph.
////
//// A single generic SceneNode holds all element-specific data in an attrs
//// dict. Handlers in scene_apply.gleam interpret the attrs for each tag.
//// The renderer diffs successive scene descriptions to produce minimal
//// patches applied via scene_apply.gleam.
////

import gleam/dict.{type Dict}

// TYPES -----------------------------------------------------------------------

/// A node in the scene description tree.
pub type Node {
  Node(
    key: String,
    tag: String,
    children: List(Node),
    /// All element attributes except `id`.
    attributes: Dict(String, String),
  )
}
