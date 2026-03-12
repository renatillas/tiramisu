//// Structural scene root for Tiramisu.
////
//// A scene lives inside a `tiramisu.renderer` and owns the scene-node subtree
//// that will be parsed and reconciled into Three.js objects.
////
//// Scene roots are structural rather than imperative: application code declares
//// the scene subtree, and Tiramisu reconciles it into the runtime.

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

/// The custom element tag used for scene roots.
@internal
pub const tag = "tiramisu-scene"

/// Create a `tiramisu-scene` element.
pub fn scene(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag, [attribute.id(id), ..attributes], children)
}
