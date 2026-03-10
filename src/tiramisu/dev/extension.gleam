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
import tiramisu/dev/loop.{type RenderLoop}
import tiramisu/dev/registry.{type Registry}

// NODE APPLY CONTEXT ----------------------------------------------------------

/// Context passed to all node extension handlers.
/// Handlers receive the context, perform their work, and return an updated one.
pub type Context {
  Context(
    registry: Registry,
    loop: RenderLoop,
    /// TODO: DOCS
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
    observed_attributes: List(String),
    /// Called when a new element with this tag appears in the scene.
    /// The order of arguments is as follows: 
    /// - `context: Context`
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
    /// - `context: Context`
    /// - `id: String`
    /// - `parent_id: String`
    /// - `object: Object3D`
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
    observed_attributes: List(String),
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
pub type Extensions {
  Extensions(
    nodes: Dict(String, Node),
    attributes: List(Attribute),
    observed_attributes: List(String),
  )
}

/// Compile a list of extensions into fast lookup structures.
pub fn from_list(extensions: List(Extension)) -> Extensions {
  use Extensions(nodes, attributes, observed_attributes), ext <- list.fold(
    extensions,
    Extensions(dict.new(), [], []),
  )
  case ext {
    NodeExtension(handler) ->
      Extensions(
        dict.insert(nodes, handler.tag, handler),
        attributes,
        list.append(handler.observed_attributes, observed_attributes),
      )
    AttributeExtension(handler) ->
      Extensions(
        nodes,
        [handler, ..attributes],
        list.append(handler.observed_attributes, observed_attributes),
      )
  }
}

/// Get the `Node` handler for a given tag, if one is registered.
pub fn get_node(exts: Extensions, tag: String) -> Result(Node, Nil) {
  dict.get(exts.nodes, tag)
}

/// Get all registered attribute extension hooks.
pub fn attribute_hooks(exts: Extensions) -> List(Attribute) {
  exts.attributes
}
