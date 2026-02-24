import gleam/json
import lustre/attribute.{type Attribute}

@internal
pub const tag = "tiramisu-empty"

/// Set the element's visibility.
///
/// Works with meshes, empties, and instanced meshes.
///
pub fn visible(visible: Bool) -> Attribute(msg) {
  case visible {
    True -> attribute.attribute("visible", "")
    False -> attribute.property("visible", json.bool(False))
  }
}
