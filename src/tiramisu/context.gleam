//// Context types for Tiramisu web component communication.
////
//// This module defines the context types used by Tiramisu web components
//// to communicate scene/renderer information from parent to child components.
//// The renderer component provides a SceneContext that child components
//// (mesh, camera, light, empty) subscribe to.
////
//// ## Architecture
////
//// ```
//// <tiramisu-renderer>  (provides SceneContext)
////     +-- <tiramisu-mesh>   (subscribes to SceneContext)
////     +-- <tiramisu-camera> (subscribes to SceneContext)
////     +-- <tiramisu-light>  (subscribes to SceneContext)
////     +-- <tiramisu-empty>  (subscribes + provides GroupContext)
////         +-- <tiramisu-mesh> (subscribes to GroupContext)
//// ```

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}

// CONTEXT KEYS ----------------------------------------------------------------

/// Context key for the main scene context provided by tiramisu-renderer.
/// Child components subscribe to this key to receive scene information.
pub const scene_context_key = "tiramisu:scene"

/// Context key for group context provided by tiramisu-empty.
/// Used for nested hierarchy where children should be added to a Group.
pub const group_context_key = "tiramisu:group"

// TYPES -----------------------------------------------------------------------

/// Scene context provided by the tiramisu-renderer component.
/// Contains a unique ID for looking up the scene in the JavaScript registry.
pub type SceneContext {
  SceneContext(
    /// Unique ID for this scene instance, used to look up
    /// the Three.js Scene object in the JavaScript registry.
    scene_id: String,
  )
}

/// Group context provided by tiramisu-empty components.
/// Allows nested children to be added to a parent Group instead of the scene root.
pub type GroupContext {
  GroupContext(
    /// ID of the scene this group belongs to.
    scene_id: String,
    /// Unique ID for this group instance, used to look up
    /// the Three.js Group object in the JavaScript registry.
    group_id: String,
  )
}

/// Combined context that a child component may receive.
/// If inside a tiramisu-empty, it receives GroupContext.
/// Otherwise, it receives SceneContext from the renderer.
pub type ParentContext {
  /// Direct child of tiramisu-renderer
  SceneParent(scene_id: String)
  /// Nested inside a tiramisu-empty
  GroupParent(scene_id: String, group_id: String)
}

// ENCODING --------------------------------------------------------------------

/// Encode a SceneContext to JSON for context provision.
pub fn encode_scene_context(ctx: SceneContext) -> Json {
  json.object([#("scene_id", json.string(ctx.scene_id))])
}

/// Encode a GroupContext to JSON for context provision.
pub fn encode_group_context(ctx: GroupContext) -> Json {
  json.object([
    #("scene_id", json.string(ctx.scene_id)),
    #("group_id", json.string(ctx.group_id)),
  ])
}

// DECODING --------------------------------------------------------------------

/// Decoder for SceneContext from JSON.
pub fn scene_context_decoder() -> Decoder(SceneContext) {
  use scene_id <- decode.field("scene_id", decode.string)
  decode.success(SceneContext(scene_id:))
}

/// Decoder for GroupContext from JSON.
pub fn group_context_decoder() -> Decoder(GroupContext) {
  use scene_id <- decode.field("scene_id", decode.string)
  use group_id <- decode.field("group_id", decode.string)
  decode.success(GroupContext(scene_id:, group_id:))
}

/// Resolve the parent context from optional scene and group contexts.
/// Prefers group context if available (meaning we're nested in an empty).
pub fn resolve_parent_context(
  scene_ctx: Option(SceneContext),
  group_ctx: Option(GroupContext),
) -> Option(ParentContext) {
  case group_ctx, scene_ctx {
    // Prefer group context when available
    Some(GroupContext(scene_id:, group_id:)), _ ->
      Some(GroupParent(scene_id:, group_id:))
    // Fall back to scene context
    None, Some(SceneContext(scene_id:)) -> Some(SceneParent(scene_id:))
    // No context available
    None, None -> None
  }
}

/// Get the scene ID from a parent context.
pub fn scene_id(ctx: ParentContext) -> String {
  case ctx {
    SceneParent(scene_id:) -> scene_id
    GroupParent(scene_id:, ..) -> scene_id
  }
}

/// Get the parent ID where objects should be added.
/// For SceneParent, this is the scene ID.
/// For GroupParent, this is the group ID.
pub fn parent_id(ctx: ParentContext) -> String {
  case ctx {
    SceneParent(scene_id:) -> scene_id
    GroupParent(group_id:, ..) -> group_id
  }
}
