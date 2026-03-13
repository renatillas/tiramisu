//// Public extension API for Tiramisu.
////
//// External libraries register extensions at startup via
//// `tiramisu.register([my_library.extension()])`.
////
//// `Attribute` hooks into the lifecycle of any tiramisu-managed node.
//// `Node` handles a custom HTML element tag with full lifecycle.
////
//// Most users do not need this module. It exists for extension authors that
//// want to define custom nodes or cross-cutting attribute behaviour.

import gleam/dict.{type Dict}
import gleam/javascript/promise
import gleam/list
import gleam/option.{type Option}
import savoiardi.{type CubeTexture, type Object3D, type Texture}
import tiramisu/dev/runtime.{type Runtime}

// NODE APPLY CONTEXT ----------------------------------------------------------

pub type AsyncEffect {
  NoOp
  RegisterObject(id: String, parent_id: String, tag: String, object: Object3D)
  ReplaceObject(id: String, object: Object3D)
  SetBackgroundTexture(texture: Texture)
  SetBackgroundCubeTexture(texture: CubeTexture)
}

pub type AsyncScope {
  SceneScope
  NodeScope(String)
  CustomScope(String)
}

pub opaque type AsyncKey {
  AsyncKey(String)
}

pub fn async_key(value: String) -> AsyncKey {
  AsyncKey(value)
}

pub fn async_key_to_string(key: AsyncKey) -> String {
  let AsyncKey(value) = key
  value
}

/// Context passed to all node extension handlers.
/// Handlers receive the context, perform their work, and return an updated one.
pub type Context {
  Context(
    runtime: Runtime,
    /// TODO: DOCS
    spawn: fn(AsyncScope, AsyncKey, promise.Promise(AsyncEffect)) ->
      promise.Promise(Nil),
    /// Called by async model loaders (mesh src=) after the model registers in
    /// the registry, so attribute extensions can react to the resolved object.
    /// Arguments: `(tag, id, object)`.
    on_object_resolved: fn(String, String, Object3D) -> Nil,
  )
}

pub type AttributeChange {
  Added(String)
  Removed
  Updated(String)
}

/// Semantic attribute changes for one node update.
pub type AttributeChanges =
  Dict(String, AttributeChange)

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
    /// - `changed_attributes: AttributeChanges`
    update: fn(
      Context,
      String,
      String,
      Option(Object3D),
      Dict(String, String),
      AttributeChanges,
    ) ->
      Context,
    /// Called when an element is removed from the scene.
    /// The handler is responsible for calling `runtime.remove_object`.
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
      promise.Promise(Nil),
    /// Fires when any tiramisu node's attributes or transform change.
    ///
    /// - `tag`: the element HTML tag
    /// - `id`: the element's id attribute
    /// - `object`: `None` for async `src=` meshes until the model loads
    /// - `attrs`: all element attributes except id and transform
    /// - `changed_attributes`: semantic attribute changes for this update
    on_update: fn(
      Context,
      String,
      String,
      Option(Object3D),
      Dict(String, String),
      AttributeChanges,
    ) ->
      promise.Promise(Nil),
    on_remove: fn(Context, String, String, Object3D) -> promise.Promise(Nil),
    on_object_resolved: fn(
      Runtime,
      fn(AsyncScope, AsyncKey, promise.Promise(AsyncEffect)) ->
        promise.Promise(Nil),
      String,
      String,
      Object3D,
      Dict(String, String),
    ) ->
      promise.Promise(Nil),
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
@internal
pub type Extensions {
  Extensions(
    nodes: Dict(String, Node),
    attributes: List(Attribute),
    observed_attributes: List(String),
  )
}

/// Compile a list of extensions into fast lookup structures.
@internal
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
@internal
pub fn get_node(exts: Extensions, tag: String) -> Result(Node, Nil) {
  dict.get(exts.nodes, tag)
}

/// Get all registered attribute extension hooks.
@internal
pub fn attribute_hooks(exts: Extensions) -> List(Attribute) {
  exts.attributes
}

/// Check whether a given attribute changed in any way.
pub fn has_change(changes: AttributeChanges, key: String) -> Bool {
  case dict.get(changes, key) {
    Ok(_) -> True
    Error(Nil) -> False
  }
}

/// Get the semantic change for a specific attribute.
pub fn change(
  changes: AttributeChanges,
  key: String,
) -> Result(AttributeChange, Nil) {
  dict.get(changes, key)
}

/// Check whether an attribute was removed.
pub fn was_removed(changes: AttributeChanges, key: String) -> Bool {
  case dict.get(changes, key) {
    Ok(Removed) -> True
    _ -> False
  }
}

/// Check whether any of the given attributes changed.
pub fn has_any_change(changes: AttributeChanges, keys: List(String)) -> Bool {
  list.any(keys, has_change(changes, _))
}
