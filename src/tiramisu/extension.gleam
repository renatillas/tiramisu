//// Public extension API for Tiramisu.
////
//// External libraries register extensions at startup via
//// `tiramisu.register([my_library.extension()])`.
////
//// `Attribute` hooks into the lifecycle of any tiramisu-managed node.
//// `Node` handles a custom HTML element tag with full lifecycle.

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import savoiardi.{type Object3D}
import tiramisu/internal/registry.{type Registry}
import tiramisu/internal/render_loop.{type RenderLoop}
import tiramisu/transform.{type Transform}

// NODE APPLY CONTEXT ----------------------------------------------------------

/// Context passed to all node extension handlers.
/// Handlers receive the context, perform their work, and return an updated one.
pub type Context {
  Context(
    registry: Registry,
    loop: RenderLoop,
    on_async: fn(fn(Registry) -> Registry) -> Nil,
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
    /// `id` is the element id; `parent_id` is the Three.js parent id.
    create: fn(Context, String, String, Dict(String, String), Transform) ->
      Context,
    /// Called when an existing element's attrs or transform change.
    /// `old_attrs` is the previous snapshot; `new_attrs` is the current one.
    update: fn(
      Context,
      String,
      Dict(String, String),
      Dict(String, String),
      Transform,
    ) ->
      Context,
    /// Called when an element is removed from the scene.
    /// The handler is responsible for calling `registry.remove_object`.
    remove: fn(Context, String) -> Context,
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
    on_create: fn(String, String, Option(Object3D), Dict(String, String)) -> Nil,
    /// Fires when any tiramisu node's attributes or transform change.
    on_update: fn(String, String, Option(Object3D), Dict(String, String)) -> Nil,
    /// Fires when any tiramisu node is removed from the scene.
    on_remove: fn(String, String) -> Nil,
    /// Fires specifically when an async GLB / GLTF / FBX model finishes loading.
    /// Use this to refine physics collider shapes from the loaded geometry.
    on_object_resolved: fn(String, String, Object3D) -> Nil,
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
pub fn all_observed_attributes(exts: Extensions) -> List(String) {
  let node_attrs =
    dict.fold(exts.0, [], fn(acc, _, handler) {
      list.append(acc, handler.observed_attributes)
    })
  let attr_attrs =
    list.fold(exts.1, [], fn(acc, handler) {
      list.append(acc, handler.observed_attributes)
    })
  list.append(node_attrs, attr_attrs)
}
