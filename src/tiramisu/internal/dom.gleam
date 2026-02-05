//// Shared DOM operations for Tiramisu web components.
////
//// This module provides browser-specific DOM operations that cannot be
//// implemented in pure Gleam. These include:
//// - Finding parent elements in the DOM tree
//// - Setting up event listeners
//// - Dispatching custom events
////
//// All components (mesh, camera, light, empty) use these functions to
//// discover the parent renderer and receive scene context.

import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

/// Find the parent tiramisu-renderer element and get its scene ID.
/// Walks up the DOM tree from the shadow root's host element.
/// Returns Some(scene_id) if found, None otherwise.
@external(javascript, "./dom.ffi.mjs", "findParentSceneId")
pub fn find_parent_scene_id(shadow_root: Dynamic) -> Option(String)

/// Set up a listener for the tiramisu:scene-ready custom event.
/// The callback is called with the scene ID when the renderer initializes.
/// This is used when the renderer hasn't been initialized yet at the time
/// a child component mounts.
@external(javascript, "./dom.ffi.mjs", "listenForSceneReady")
pub fn listen_for_scene_ready(
  shadow_root: Dynamic,
  callback: fn(String) -> Nil,
) -> Nil

/// Set an attribute on the host element of a shadow root.
@external(javascript, "./dom.ffi.mjs", "setHostAttribute")
pub fn set_host_attribute(
  shadow_root: Dynamic,
  name: String,
  value: String,
) -> Nil

/// Get an attribute from the host element of a shadow root.
@external(javascript, "./dom.ffi.mjs", "getHostAttribute")
pub fn get_host_attribute(shadow_root: Dynamic, name: String) -> Option(String)

/// Dispatch a custom event from the host element.
/// The event bubbles and is composed (crosses shadow DOM boundaries).
@external(javascript, "./dom.ffi.mjs", "dispatchCustomEvent")
pub fn dispatch_custom_event(
  shadow_root: Dynamic,
  event_name: String,
  detail: a,
) -> Nil

/// Find the immediate parent tiramisu element (mesh, empty, camera, etc.)
/// and return its ID. Returns None if no parent object is found before
/// reaching the renderer. This enables proper hierarchical transforms.
@external(javascript, "./dom.ffi.mjs", "findParentObjectId")
pub fn find_parent_object_id(shadow_root: Dynamic) -> Option(String)
