//// Public extension API for Tiramisu.
////
//// External libraries register extensions at startup via
//// `tiramisu.register([my_library.extension()])`.
////
//// `Attribute` hooks into the lifecycle of any tiramisu-managed node.
//// `Node` handles a custom HTML element tag with full lifecycle.

import gleam/dict.{type Dict}
import gleam/javascript/promise
import gleam/list
import gleam/option.{type Option}
import gleam/set.{type Set}
import savoiardi.{type Object3D}
import tiramisu/dev/registry.{type Registry}
import tiramisu/dev/render_loop.{type RenderLoop}

// NODE APPLY CONTEXT ----------------------------------------------------------

/// Context passed to all node extension handlers.
/// Handlers receive the context, perform their work, and return an updated one.
pub type Context {
  Context(
    registry: Registry,
    loop: RenderLoop,
    on_async: fn(promise.Promise(fn(Registry) -> Registry)) -> Nil,
    /// Called by async model loaders (mesh src=) after the model registers in
    /// the registry, so attribute extensions can react to the resolved object.
    /// Arguments: `(tag, id, object)`.
    on_object_resolved: fn(String, String, Object3D) -> Nil,
  )
}

// NODE EXTENSION --------------------------------------------------------------

/// Extension that handles a custom HTML element tag.
///
/// Built-in node types (mesh, camera, light, etc.) are implemented as `Node`
/// extensions pre-registered at register() time. External packages can define
/// their own by passing a `NodeExtension` variant to `tiramisu.register`.
pub type Node {
  Node(
    tag: String,
    /// Attribute names this node type observes (added to MutationObserver filter).
    observed_attributes: set.Set(String),
    /// Called when a new element with this tag appears in the scene.
    /// The order of arguments is as follows: 
    /// - `context: Context
    /// - `element_id: String`
    /// - `parent_id: String`
    /// - `attributes: Dict(String, String)`
    create: fn(Context, String, String, Dict(String, String)) -> Context,
    /// Called when an existing element's attrs or transform change.
    /// The order of arguments is as follows:
    /// - `context: Context`
    /// - `element_id: String`
    /// - `parent_id: String`
    /// - `object: Option(Object3D)`
    /// - `attributes: Dict(String, String)`
    /// - `changed_attributes: Set(String)`
    update: fn(
      Context,
      String,
      String,
      Option(Object3D),
      Dict(String, String),
      Set(String),
    ) ->
      Context,
    /// Called when an element is removed from the scene.
    /// The handler is responsible for calling `registry.remove_object`.
    remove: fn(Context, String, String, Object3D) -> Context,
  )
}

// ATTRIBUTE EXTENSION ---------------------------------------------------------

/// Extension that hooks into the lifecycle of any tiramisu-managed node.
///
/// Use this for side-effects such as physics or positional audio on existing
/// elements. The hook fires for every node — use `attrs` and the presence of
/// your custom attribute (e.g. `data-physics-body`) to decide whether to act.
pub type Attribute {
  Attribute(
    /// Attribute names to observe on tiramisu elements.
    /// Merged into the MutationObserver filter so DOM changes to these
    /// attributes trigger a scene re-parse.
    observed_attributes: set.Set(String),
    /// Fires when any tiramisu node is created.
    ///
    /// - `tag`: the element HTML tag (e.g. `"tiramisu-mesh"`)
    /// - `id`: the element's id attribute
    /// - `object`: `None` for async `src=` meshes until the model loads
    /// - `attrs`: all element attributes except id and transform
    on_create: fn(
      Context,
      String,
      String,
      Option(Object3D),
      Dict(String, String),
    ) ->
      Nil,
    /// Fires when any tiramisu node's attributes or transform change.
    ///
    /// - `tag`: the element HTML tag
    /// - `id`: the element's id attribute
    /// - `object`: `None` for async `src=` meshes until the model loads
    /// - `attrs`: all element attributes except id and transform
    /// - `changed_attributes`: the set of attribute names that changed in this update
    on_update: fn(
      Context,
      String,
      String,
      Option(Object3D),
      Dict(String, String),
      Set(String),
    ) ->
      Nil,
    on_remove: fn(Context, String, String, Object3D) -> Nil,
    on_object_resolved: fn(
      Context,
      String,
      String,
      Object3D,
      Dict(String, String),
    ) ->
      Nil,
  )
}

// EXTENSION UNION -------------------------------------------------------------

/// A tiramisu extension registered at startup via `tiramisu.register/1`.
pub type Extension {
  /// Handle a custom HTML element tag with full create/update/remove lifecycle.
  NodeExtension(Node)
  /// Hook into the lifecycle of any tiramisu-managed node for side-effects.
  AttributeExtension(Attribute)
}

// COMPILED EXTENSIONS ---------------------------------------------------------

/// Compiled lookup structure. One per renderer instance, threaded through
/// the apply pipeline as a pure immutable value. No global state.
pub type Extensions =
  #(Dict(String, Node), List(Attribute))

/// Compile a list of extensions into fast lookup structures.
pub fn from_list(extensions: List(Extension)) -> Extensions {
  use #(nodes, attributes), ext <- list.fold(extensions, #(dict.new(), []))
  case ext {
    NodeExtension(handler) -> #(
      dict.insert(nodes, handler.tag, handler),
      attributes,
    )
    AttributeExtension(handler) -> #(nodes, [handler, ..attributes])
  }
}

/// Get the `Node` handler for a given tag, if one is registered.
pub fn get_node(exts: Extensions, tag: String) -> Result(Node, Nil) {
  dict.get(exts.0, tag)
}

/// Get all registered attribute extension hooks.
pub fn attribute_hooks(exts: Extensions) -> List(Attribute) {
  exts.1
}

/// Collect all `observed_attributes` from all extensions for the MutationObserver.
pub fn all_observed_attributes(exts: Extensions) -> set.Set(String) {
  let node_attrs =
    dict.fold(exts.0, set.new(), fn(acc, _, handler) {
      set.union(acc, handler.observed_attributes)
    })
  let attr_attrs =
    list.fold(exts.1, set.new(), fn(acc, handler) {
      set.union(acc, handler.observed_attributes)
    })
  set.union(node_attrs, attr_attrs)
}
