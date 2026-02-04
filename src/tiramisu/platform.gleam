//// Platform implementation for Tiramisu using lustre_platform.
////
//// This module implements the Platform abstraction that allows Lustre's reconciler
//// to render to Three.js scenes instead of the DOM. It provides all the low-level
//// mutation methods needed to create, modify, and remove Three.js objects in response
//// to virtual DOM changes.
////
//// ## Architecture
////
//// The platform wraps Three.js Object3D instances in an opaque type and implements
//// operations like create_element, set_attribute, and insert_before using Three.js APIs.
//// Tags like "mesh", "camera", "light" map to corresponding Three.js object types.
////
//// ## Usage
////
//// ```gleam
//// import lustre
//// import tiramisu/platform
////
//// pub fn main() {
////   let assert Ok(platform) = platform.platform("#game")
////
////   lustre.application(init, update, view)
////   |> lustre.start(platform, Nil)
//// }
//// ```

// IMPORTS ---------------------------------------------------------------------

import gleam/option
import gleam/result
import lustre/element.{type Element}
import lustre/platform.{type Platform, type PlatformError}
import plinth/browser/document
import savoiardi.{type Renderer, type Scene}
import tiramisu
import vec/vec2.{type Vec2}

// TYPES -----------------------------------------------------------------------

/// A node in the Three.js scene graph.
///
/// We use plain JavaScript objects to store both DOM elements and Three.js objects.
/// This allows lustre_platform's runtime to call DOM methods directly on the root.
/// Three.js objects are stored in a registry and associated with placeholder DOM elements.
pub type ThreeJSNode

/// A DOM element type (from plinth/browser)
pub type DomElement

/// Platform target containing the renderer and root scene.
pub opaque type ThreeJSTarget {
  ThreeJSTarget(
    selector: String,
    container: DomElement,
    renderer: Renderer,
    scene: Scene,
    canvas_size: Vec2(Float),
  )
}

// PLATFORM CONSTRUCTOR --------------------------------------------------------

/// Create a Three.js rendering platform for the given CSS selector.
///
/// This initializes a WebGL renderer, creates a root scene, and returns a Platform
/// configured to render Lustre elements as Three.js objects.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(platform) = platform.platform("#game")
/// ```
///
pub fn platform(
  selector: String,
) -> Result(
  Platform(ThreeJSNode, ThreeJSTarget, ThreeJSNode, Nil, msg),
  PlatformError,
) {
  // Check if running in browser
  case platform.is_browser() {
    False -> Error(platform.NotABrowser)
    True -> {
      // Find container element
      use container <- result.try(
        document.query_selector(selector)
        |> result.replace_error(platform.ElementNotFound(selector)),
      )

      // Create renderer with options
      let renderer =
        savoiardi.create_renderer(savoiardi.RendererOptions(
          antialias: True,
          alpha: False,
          dimensions: option.None,
        ))

      // Create scene
      let scene = savoiardi.create_scene()

      // Get canvas dimensions
      let canvas_size = savoiardi.get_canvas_dimensions(renderer)

      // Create target (store container for Lustre runtime)
      let target =
        ThreeJSTarget(
          selector:,
          container: unsafe_coerce(container),
          renderer:,
          scene:,
          canvas_size:,
        )

      // Create platform with all operations
      Ok(
        platform.new(
          target: target,
          mount: mount,
          create_element: create_element,
          create_text_node: create_text_node,
          create_fragment: create_fragment,
          create_comment: create_comment,
          insert_before: insert_before,
          move_before: move_before,
          remove_child: remove_child,
          next_sibling: next_sibling,
          get_attribute: get_attribute,
          set_attribute: set_attribute,
          remove_attribute: remove_attribute,
          set_property: set_property,
          set_text: set_text,
          set_raw_content: set_raw_content,
          add_event_listener: add_event_listener,
          remove_event_listener: remove_event_listener,
          schedule_render: schedule_render,
          after_render: after_render,
        ),
      )
    }
  }
}

// PLATFORM OPERATIONS ---------------------------------------------------------

/// Mount the platform and return the root node and initial virtual DOM.
///
/// IMPORTANT: We return the container DOM element directly as the root node.
/// This allows lustre_platform's runtime to call addEventListener and other DOM methods.
/// Three.js objects are created and managed separately through our platform operations.
fn mount(target: ThreeJSTarget) -> #(ThreeJSNode, Element(msg)) {
  // Store the scene reference globally so our insert/remove operations can access it
  set_global_scene_ffi(target.scene)

  // Add the canvas to the body (not the container) to prevent reconciler issues
  let canvas = savoiardi.get_renderer_dom_element(target.renderer)
  append_canvas_to_body_ffi(canvas)

  // Use the actual container as the root and ensure it's empty
  clear_container_ffi(target.container)
  let root = unsafe_coerce(target.container)

  // CRITICAL: Initialize reconciler metadata on the root
  // This is what opentui does - without this, the reconciler fails!
  init_reconciler_metadata_ffi(root)

  // Return element.none() as initial vdom (like opentui does)
  let initial_vdom = element.none()

  // Set the renderer and scene for the subscription system to use
  // The subscription will handle rendering after each tick
  tiramisu.set_renderer_and_scene(target.renderer, target.scene)

  #(root, initial_vdom)
}

/// Create an element by namespace and tag.
///
/// Tags map to Three.js object types. We call FFI which handles all creation logic.
fn create_element(namespace: String, tag: String) -> ThreeJSNode {
  create_element_ffi(namespace, tag)
}

/// Create a text node (used for CSS2D/CSS3D labels)
fn create_text_node(content: String) -> ThreeJSNode {
  create_text_node_ffi(content)
}

/// Create a document fragment (virtual node)
fn create_fragment() -> ThreeJSNode {
  create_fragment_ffi()
}

/// Create a comment node (virtual node)
fn create_comment(text: String) -> ThreeJSNode {
  create_comment_ffi(text)
}

/// Insert a node before a reference node in the scene graph.
fn insert_before(
  parent: ThreeJSNode,
  node: ThreeJSNode,
  reference: Result(ThreeJSNode, Nil),
) -> Nil {
  insert_before_ffi(parent, node, reference)
}

/// Move a node before a reference node.
fn move_before(
  parent: ThreeJSNode,
  node: ThreeJSNode,
  reference: Result(ThreeJSNode, Nil),
) -> Nil {
  move_before_ffi(parent, node, reference)
}

/// Remove a child from a parent node.
fn remove_child(parent: ThreeJSNode, child: ThreeJSNode) -> Nil {
  remove_child_ffi(parent, child)
}

/// Get the next sibling of a node.
fn next_sibling(node: ThreeJSNode) -> Result(ThreeJSNode, Nil) {
  next_sibling_ffi(node)
}

/// Get an attribute value from a node.
fn get_attribute(node: ThreeJSNode, name: String) -> Result(String, Nil) {
  get_attribute_ffi(node, name)
}

/// Set an attribute on a node.
fn set_attribute(node: ThreeJSNode, name: String, value: String) -> Nil {
  set_attribute_ffi(node, name, value)
}

/// Remove an attribute from a node.
fn remove_attribute(node: ThreeJSNode, name: String) -> Nil {
  remove_attribute_ffi(node, name)
}

/// Set a property on a node (direct property access).
fn set_property(node: ThreeJSNode, name: String, value: ThreeJSNode) -> Nil {
  set_property_ffi(node, name, value)
}

/// Set text content (for CSS2D/CSS3D labels).
fn set_text(node: ThreeJSNode, content: String) -> Nil {
  set_text_ffi(node, content)
}

/// Set raw HTML content (for CSS2D/CSS3D elements).
fn set_raw_content(node: ThreeJSNode, content: String) -> Nil {
  set_raw_content_ffi(node, content)
}

/// Add an event listener to a node.
fn add_event_listener(
  node: ThreeJSNode,
  event_name: String,
  handler: fn(Nil) -> Nil,
  passive: Bool,
) -> Nil {
  add_event_listener_ffi(node, event_name, handler, passive)
}

/// Remove an event listener from a node.
fn remove_event_listener(
  node: ThreeJSNode,
  event_name: String,
  handler: fn(Nil) -> Nil,
) -> Nil {
  remove_event_listener_ffi(node, event_name, handler)
}

/// Schedule a render callback (integrated with requestAnimationFrame).
fn schedule_render(callback: fn() -> Nil) -> fn() -> Nil {
  schedule_render_ffi(callback)
}

/// Called after each render (can be used for cleanup).
fn after_render() -> Nil {
  Nil
}

// FFI DECLARATIONS ------------------------------------------------------------

// DOM manipulation
@external(javascript, "./platform.ffi.mjs", "appendCanvasToBody")
fn append_canvas_to_body_ffi(canvas: a) -> Nil

// Node creation
@external(javascript, "./platform.ffi.mjs", "createElement")
fn create_element_ffi(namespace: String, tag: String) -> ThreeJSNode

@external(javascript, "./platform.ffi.mjs", "createTextNode")
fn create_text_node_ffi(content: String) -> ThreeJSNode

@external(javascript, "./platform.ffi.mjs", "createFragment")
fn create_fragment_ffi() -> ThreeJSNode

@external(javascript, "./platform.ffi.mjs", "createComment")
fn create_comment_ffi(text: String) -> ThreeJSNode

// Scene graph manipulation
@external(javascript, "./platform.ffi.mjs", "insertBefore")
fn insert_before_ffi(
  parent: ThreeJSNode,
  node: ThreeJSNode,
  reference: Result(ThreeJSNode, Nil),
) -> Nil

@external(javascript, "./platform.ffi.mjs", "moveBefore")
fn move_before_ffi(
  parent: ThreeJSNode,
  node: ThreeJSNode,
  reference: Result(ThreeJSNode, Nil),
) -> Nil

@external(javascript, "./platform.ffi.mjs", "removeChild")
fn remove_child_ffi(parent: ThreeJSNode, child: ThreeJSNode) -> Nil

@external(javascript, "./platform.ffi.mjs", "nextSibling")
fn next_sibling_ffi(node: ThreeJSNode) -> Result(ThreeJSNode, Nil)

// Attributes
@external(javascript, "./platform.ffi.mjs", "getAttribute")
fn get_attribute_ffi(node: ThreeJSNode, name: String) -> Result(String, Nil)

@external(javascript, "./platform.ffi.mjs", "setAttribute")
fn set_attribute_ffi(node: ThreeJSNode, name: String, value: String) -> Nil

@external(javascript, "./platform.ffi.mjs", "removeAttribute")
fn remove_attribute_ffi(node: ThreeJSNode, name: String) -> Nil

@external(javascript, "./platform.ffi.mjs", "setProperty")
fn set_property_ffi(node: ThreeJSNode, name: String, value: ThreeJSNode) -> Nil

@external(javascript, "./platform.ffi.mjs", "setText")
fn set_text_ffi(node: ThreeJSNode, content: String) -> Nil

@external(javascript, "./platform.ffi.mjs", "setRawContent")
fn set_raw_content_ffi(node: ThreeJSNode, content: String) -> Nil

// Events
@external(javascript, "./platform.ffi.mjs", "addEventListener")
fn add_event_listener_ffi(
  node: ThreeJSNode,
  event_name: String,
  handler: fn(Nil) -> Nil,
  passive: Bool,
) -> Nil

@external(javascript, "./platform.ffi.mjs", "removeEventListener")
fn remove_event_listener_ffi(
  node: ThreeJSNode,
  event_name: String,
  handler: fn(Nil) -> Nil,
) -> Nil

// Scheduling
@external(javascript, "./platform.ffi.mjs", "scheduleRender")
fn schedule_render_ffi(callback: fn() -> Nil) -> fn() -> Nil

// Global scene management
@external(javascript, "./platform.ffi.mjs", "setGlobalScene")
fn set_global_scene_ffi(scene: Scene) -> Nil

@external(javascript, "./platform.ffi.mjs", "clearContainer")
fn clear_container_ffi(container: a) -> Nil

@external(javascript, "./platform.ffi.mjs", "initReconcilerMetadata")
fn init_reconciler_metadata_ffi(root: ThreeJSNode) -> Nil

// Type coercion
@external(javascript, "../../gleam_stdlib/gleam/function.mjs", "identity")
fn unsafe_coerce(a: a) -> b
