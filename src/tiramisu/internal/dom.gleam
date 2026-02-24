//// Centralized DOM operations for Tiramisu web components.
////
//// This module is the single entry point for all DOM manipulation. It
//// provides an opaque Element type and combines pure Gleam logic with
//// FFI primitives for operations that require direct JS interop.
////
//// Other tiramisu modules should import this module instead of using
//// their own DOM FFI.

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result

// ============================================================================
// ELEMENT TYPE
// ============================================================================

/// Opaque type representing a real DOM Element.
/// This is distinct from Lustre's virtual DOM Element(msg).
pub type Element

// ============================================================================
// SHADOW ROOT → HOST
// ============================================================================

/// Get the host element from a shadow root.
/// Lustre's after_paint passes the shadow root as Dynamic — this extracts
/// the host element (the <tiramisu-renderer> custom element itself).
@external(javascript, "./dom.ffi.mjs", "shadowRootHost")
pub fn shadow_root_host(shadow_root: Dynamic) -> Element

// ============================================================================
// ATTRIBUTE ACCESS
// ============================================================================

/// Get an attribute from a DOM element.
@external(javascript, "./dom.ffi.mjs", "getAttribute")
pub fn get_attribute(el: Element, name: String) -> Result(String, Nil)

/// Set an attribute on a DOM element.
@external(javascript, "./dom.ffi.mjs", "setAttribute")
pub fn set_attribute(el: Element, name: String, value: String) -> Nil

/// Find the closest ancestor matching a CSS selector.
@external(javascript, "./dom.ffi.mjs", "closest")
pub fn closest(el: Element, selector: String) -> Result(Element, Nil)

// ============================================================================
// ELEMENT INTROSPECTION
// ============================================================================

/// Get an element's tag name (e.g. "TIRAMISU-MESH").
@external(javascript, "./dom.ffi.mjs", "tagName")
pub fn tag_name(el: Element) -> String

/// Get an element's direct children as a Gleam List.
@external(javascript, "./dom.ffi.mjs", "children")
pub fn children(el: Element) -> List(Element)

/// Get an element's innerHTML.
@external(javascript, "./dom.ffi.mjs", "innerHTML")
pub fn inner_html(el: Element) -> String

/// Get an element's parent element.
@external(javascript, "./dom.ffi.mjs", "parentElement")
pub fn parent_element(el: Element) -> Result(Element, Nil)

// ============================================================================
// CUSTOM EVENTS
// ============================================================================

/// Dispatch a custom event from an element.
/// The event bubbles and is composed (crosses shadow DOM boundaries).
@external(javascript, "./dom.ffi.mjs", "dispatchCustomEvent")
pub fn dispatch_custom_event(el: Element, event_name: String, detail: a) -> Nil

// ============================================================================
// JS PROPERTY ACCESS (for Three.js object storage on DOM elements)
// ============================================================================

/// Set an arbitrary JavaScript property on an element.
@external(javascript, "./dom.ffi.mjs", "setProperty")
pub fn set_property(el: Element, name: String, value: a) -> Nil

/// Delete an arbitrary JavaScript property from an element.
@external(javascript, "./dom.ffi.mjs", "deleteProperty")
pub fn delete_property(el: Element, name: String) -> Nil

// ============================================================================
// DOCUMENT LOOKUP
// ============================================================================

/// Look up a DOM element by its ID.
@external(javascript, "./dom.ffi.mjs", "getElementById")
pub fn get_element_by_id(id: String) -> Result(Element, Nil)

/// Append a child element to a parent element.
@external(javascript, "./dom.ffi.mjs", "appendChild")
pub fn append_child(parent: Element, child: Element) -> Nil

// ============================================================================
// MUTATION OBSERVER
// ============================================================================

/// Set up a MutationObserver on the host element's light DOM children.
/// Uses queueMicrotask to batch rapid DOM mutations into a single callback.
/// `additional_observed_attrs` is prepended to the built-in attribute filter.
@external(javascript, "./dom.ffi.mjs", "setupMutationObserver")
pub fn setup_mutation_observer(
  host: Element,
  observed_attrs: List(String),
  callback: fn() -> Nil,
) -> Nil

// ============================================================================
// DOM TREE WALKING
// ============================================================================

/// Find the immediate parent tiramisu element (mesh, empty, camera, light, audio)
/// and return its ID. Returns None if no parent object is found before
/// reaching the renderer. This enables proper hierarchical transforms.
@external(javascript, "./dom.ffi.mjs", "findParentObjectId")
pub fn find_parent_object_id(host: Element) -> Result(String, Nil)

// ============================================================================
// ATTRIBUTE INTROSPECTION
// ============================================================================

/// Get all attributes on an element as a Dict(String, String).
pub fn get_all_attributes(el: Element) -> Dict(String, String) {
  get_all_attributes_list(el)
  |> list.fold(dict.new(), fn(acc, pair) { dict.insert(acc, pair.0, pair.1) })
}

@external(javascript, "./dom.ffi.mjs", "getAllAttributesList")
fn get_all_attributes_list(el: Element) -> List(#(String, String))

// ============================================================================
// COMPOSITE OPERATIONS (pure Gleam, composing primitives)
// ============================================================================

/// Find the parent tiramisu-renderer element and get its scene ID.
pub fn find_parent_scene_id(host: Element) -> Result(String, Nil) {
  case closest(host, "tiramisu-renderer") {
    Ok(renderer) ->
      case get_attribute(renderer, "data-scene-id") {
        Ok(scene_id) -> Ok(scene_id)
        Error(Nil) -> Error(Nil)
      }
    Error(Nil) -> Error(Nil)
  }
}

/// Get the scene-id attribute from a host element.
pub fn get_scene_id_from_host(host: Element) -> Result(String, Nil) {
  get_attribute(host, "scene-id")
}

/// Get the renderer configuration from a host element's attributes.
pub fn get_renderer_config(host: Element) -> RendererConfig {
  let width = case get_attribute(host, "width") {
    Ok(w) ->
      case int.parse(w) {
        Ok(n) -> option.Some(n)
        Error(Nil) -> option.None
      }
    Error(Nil) -> option.None
  }
  let height = case get_attribute(host, "height") {
    Ok(h) ->
      case int.parse(h) {
        Ok(n) -> option.Some(n)
        Error(Nil) -> option.None
      }
    Error(Nil) -> option.None
  }
  let background =
    get_attribute(host, "background")
    |> result.unwrap("#000000")
  let antialias = case get_attribute(host, "antialias") {
    Ok("false") -> False
    _ -> True
  }
  let alpha = case get_attribute(host, "alpha") {
    Ok("true") -> True
    _ -> False
  }
  RendererConfig(width:, height:, background:, antialias:, alpha:)
}

/// Renderer configuration parsed from host element attributes.
pub type RendererConfig {
  RendererConfig(
    width: Option(Int),
    height: Option(Int),
    background: String,
    antialias: Bool,
    alpha: Bool,
  )
}

/// Store a Three.js object reference on its corresponding DOM element.
/// This enables external integrations (cacao physics, etc.) to find
/// the Three.js object via document.getElementById(id)._object3d.
pub fn store_object_on_dom(id: String, object: a) -> Nil {
  case get_element_by_id(id) {
    Ok(el) -> set_property(el, "_object3d", object)
    Error(Nil) -> Nil
  }
}

/// Clear a Three.js object reference from a DOM element.
pub fn clear_object_from_dom(id: String) -> Nil {
  case get_element_by_id(id) {
    Ok(el) -> delete_property(el, "_object3d")
    Error(Nil) -> Nil
  }
}

/// Dispatch a custom event on a DOM element found by ID.
/// Used to notify Lustre apps about model load status.
pub fn dispatch_mesh_event(mesh_id: String, event_name: String) -> Nil {
  case get_element_by_id(mesh_id) {
    Ok(el) ->
      dispatch_custom_event(el, event_name, create_mesh_event_detail(mesh_id))
    Error(Nil) -> Nil
  }
}

/// Create a mesh event detail with the right JS key format.
@external(javascript, "./dom.ffi.mjs", "createMeshEventDetail")
fn create_mesh_event_detail(id: String) -> Dynamic

/// Append a canvas to a container (shadow root div).
/// Also sets display:block on the canvas.
/// Uses generic types because both container (shadow root) and canvas
/// (savoiardi.Canvas) are opaque types that are DOM elements at runtime.
@external(javascript, "./dom.ffi.mjs", "appendCanvasToContainer")
pub fn append_canvas_to_container(container: a, canvas: b) -> Nil
