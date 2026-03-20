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
import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/list
import gleam/option.{type Option}
import gleam/result
import lustre/effect
import savoiardi/object.{type Object3D}
import savoiardi/scene
import savoiardi/texture.{type CubeTexture, type Texture}
import tiramisu/dev/runtime

// NODE APPLY CONTEXT ----------------------------------------------------------

/// A deferred runtime mutation produced by an extension.
///
/// Runtime actions are returned from async work such as model or texture
/// loading, then applied later by the renderer once the owning node still
/// exists and the request is still current.
pub opaque type RuntimeAction {
  RuntimeAction(fn(runtime.Runtime) -> #(runtime.Runtime, effect.Effect(Msg)))
}

/// Wrap a runtime mutation as a `RuntimeAction`.
pub fn action(
  run: fn(runtime.Runtime) -> #(runtime.Runtime, effect.Effect(Msg)),
) -> RuntimeAction {
  RuntimeAction(run)
}

/// Register a newly created object in the runtime and emit `NotifyResolved`.
///
/// Extension authors typically return this from async loaders once an object is
/// ready to be inserted into the scene graph.
pub fn register_object(
  id: String,
  parent_id: String,
  tag: String,
  object: Object3D,
) -> RuntimeAction {
  RuntimeAction(fn(rt) {
    let runtime = runtime.add_object(rt, id, object, parent_id, tag)
    #(
      runtime,
      effect.from(fn(dispatch) { dispatch(NotifyResolved(tag, id, object)) }),
    )
  })
}

/// Replace an existing runtime object and emit `NotifyResolved` for the new one.
pub fn replace_object(id: String, object: Object3D) -> RuntimeAction {
  RuntimeAction(fn(rt) {
    let runtime = runtime.replace_object(rt, id, object)
    let tag = case runtime.find_entry(runtime, id) {
      Ok(runtime.ObjectEntry(tag:, ..)) -> tag
      Error(Nil) -> ""
    }
    #(
      runtime,
      effect.from(fn(dispatch) { dispatch(NotifyResolved(tag, id, object)) }),
    )
  })
}

/// Remove an object from the runtime.
pub fn remove_object(
  id: String,
  parent_id: String,
  object: Object3D,
) -> RuntimeAction {
  RuntimeAction(fn(rt) {
    #(runtime.remove_object(rt, id, parent_id, object), effect.none())
  })
}

/// Set the scene background to a 2D texture.
pub fn set_background_texture(texture: Texture) -> RuntimeAction {
  RuntimeAction(fn(rt) {
    let _ =
      scene.set_background(runtime.scene(rt), scene.background_texture(texture))
    #(rt, effect.none())
  })
}

/// Set the scene background to a cube texture.
pub fn set_background_cube_texture(texture: CubeTexture) -> RuntimeAction {
  RuntimeAction(fn(rt) {
    let _ =
      scene.set_background(
        runtime.scene(rt),
        scene.background_cube_texture(texture),
      )
    #(rt, effect.none())
  })
}

@internal
pub fn run_action(
  action: RuntimeAction,
  runtime: runtime.Runtime,
) -> #(runtime.Runtime, effect.Effect(Msg)) {
  let RuntimeAction(run) = action
  run(runtime)
}

/// Identifies which part of the scene owns an async request.
///
/// Tiramisu uses this to decide whether async results should still be applied
/// after later reconciliations.
pub type RequestOwner {
  SceneOwner
  NodeOwner(String)
  CustomOwner(String)
}

pub opaque type RequestKey {
  RequestKey(String)
}

/// Construct a logical key for an async request.
///
/// Owners can have multiple independent in-flight requests as long as they use
/// different keys.
pub fn request_key(value: String) -> RequestKey {
  RequestKey(value)
}

/// Convert a request key back into its raw string representation.
pub fn request_key_to_string(key: RequestKey) -> String {
  let RequestKey(value) = key
  value
}

@internal
pub fn request_owner_to_string(owner: RequestOwner) -> String {
  case owner {
    SceneOwner -> "scene"
    NodeOwner(id) -> "node:" <> id
    CustomOwner(value) -> "custom:" <> value
  }
}

/// Messages understood by the renderer's extension runtime.
///
/// Extension authors usually interact with this type indirectly through
/// `request/3` and runtime actions instead of constructing messages manually.
pub type Msg {
  NotifyResolved(String, String, Object3D)
  Spawn(
    owner: RequestOwner,
    key: RequestKey,
    task: promise.Promise(List(RuntimeAction)),
  )
}

/// Schedule async work that will eventually yield runtime actions.
///
/// The `owner` and `key` pair lets Tiramisu drop stale results after later
/// reconciliations or attribute changes.
pub fn request(
  owner: RequestOwner,
  key: RequestKey,
  task: promise.Promise(List(RuntimeAction)),
) -> effect.Effect(Msg) {
  effect.from(fn(dispatch) { dispatch(Spawn(owner:, key:, task:)) })
}

/// The semantic change recorded for one attribute between two reconciliations.
pub type AttributeChange {
  Added(String)
  Removed
  Updated(String)
}

/// Semantic attribute changes for one node update.
pub type AttributeChanges =
  Dict(String, AttributeChange)

// NODE EXTENSION --------------------------------------------------------------

/// Construct a custom node extension.
///
/// Use node extensions when you want to introduce a new scene element tag with
/// full create, update, and remove lifecycle hooks.
pub fn node_extension(
  tag tag: String,
  observed_attributes observed_attributes: List(String),
  create create: fn(runtime.Runtime, String, String, Dict(String, String)) ->
    #(runtime.Runtime, effect.Effect(Msg)),
  update update: fn(
    runtime.Runtime,
    String,
    String,
    Option(Object3D),
    Dict(String, String),
    Dict(String, AttributeChange),
  ) ->
    #(runtime.Runtime, effect.Effect(Msg)),
  remove remove: fn(runtime.Runtime, String, String, Object3D) ->
    #(runtime.Runtime, effect.Effect(Msg)),
) -> Extension {
  NodeExtension(Node(tag:, observed_attributes:, create:, update:, remove:))
}

/// Construct a cross-cutting attribute extension.
///
/// Attribute extensions observe attributes across existing node types and react
/// when matching attributes are created, updated, removed, or resolved.
pub fn attribute_extension(
  observed_attributes observed_attributes: List(String),
  on_create on_create: fn(
    runtime.Runtime,
    String,
    String,
    Option(Object3D),
    Dict(String, String),
  ) ->
    effect.Effect(Msg),
  on_update on_update: fn(
    runtime.Runtime,
    String,
    String,
    Option(Object3D),
    Dict(String, String),
    Dict(String, AttributeChange),
  ) ->
    effect.Effect(Msg),
  on_remove on_remove: fn(runtime.Runtime, String, String, Object3D) ->
    effect.Effect(Msg),
  on_resolved on_resolved: fn(
    runtime.Runtime,
    String,
    String,
    Object3D,
    Dict(String, String),
  ) ->
    effect.Effect(Msg),
) -> Extension {
  scoped_attribute_extension(
    applies_to: AllTags,
    observed_attributes: observed_attributes,
    on_create: on_create,
    on_update: on_update,
    on_remove: on_remove,
    on_resolved: on_resolved,
  )
}

/// Construct an attribute extension scoped to specific node tags.
pub fn scoped_attribute_extension(
  applies_to applies_to: TagScope,
  observed_attributes observed_attributes: List(String),
  on_create on_create: fn(
    runtime.Runtime,
    String,
    String,
    Option(Object3D),
    Dict(String, String),
  ) ->
    effect.Effect(Msg),
  on_update on_update: fn(
    runtime.Runtime,
    String,
    String,
    Option(Object3D),
    Dict(String, String),
    Dict(String, AttributeChange),
  ) ->
    effect.Effect(Msg),
  on_remove on_remove: fn(runtime.Runtime, String, String, Object3D) ->
    effect.Effect(Msg),
  on_resolved on_resolved: fn(
    runtime.Runtime,
    String,
    String,
    Object3D,
    Dict(String, String),
  ) ->
    effect.Effect(Msg),
) -> Extension {
  AttributeExtension(Attribute(
    applies_to:,
    observed_attributes:,
    on_create:,
    on_update:,
    on_remove:,
    on_resolved:,
  ))
}

@internal
pub type Node {
  Node(
    tag: String,
    observed_attributes: List(String),
    create: fn(runtime.Runtime, String, String, Dict(String, String)) ->
      #(runtime.Runtime, effect.Effect(Msg)),
    update: fn(
      runtime.Runtime,
      String,
      String,
      Option(Object3D),
      Dict(String, String),
      AttributeChanges,
    ) ->
      #(runtime.Runtime, effect.Effect(Msg)),
    remove: fn(runtime.Runtime, String, String, Object3D) ->
      #(runtime.Runtime, effect.Effect(Msg)),
  )
}

// ATTRIBUTE EXTENSION ---------------------------------------------------------

/// Select which node tags a cross-cutting attribute extension applies to.
pub type TagScope {
  AllTags
  OnlyTags(List(String))
}

@internal
pub type Attribute {
  Attribute(
    applies_to: TagScope,
    observed_attributes: List(String),
    on_create: fn(
      runtime.Runtime,
      String,
      String,
      Option(Object3D),
      Dict(String, String),
    ) ->
      effect.Effect(Msg),
    on_update: fn(
      runtime.Runtime,
      String,
      String,
      Option(Object3D),
      Dict(String, String),
      AttributeChanges,
    ) ->
      effect.Effect(Msg),
    on_remove: fn(runtime.Runtime, String, String, Object3D) ->
      effect.Effect(Msg),
    on_resolved: fn(
      runtime.Runtime,
      String,
      String,
      Object3D,
      Dict(String, String),
    ) ->
      effect.Effect(Msg),
  )
}

/// A compiled extension definition.
///
/// Extensions are either custom nodes or cross-cutting attribute handlers.
pub opaque type Extension {
  NodeExtension(Node)
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

@internal
pub fn attribute_hook_applies_to_tag(hook: Attribute, tag: String) -> Bool {
  case hook.applies_to {
    AllTags -> True
    OnlyTags(tags) -> list.contains(tags, tag)
  }
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

/// Check whether a boolean-style attribute is currently present.
///
/// This is useful for attributes encoded using HTML presence semantics such as
/// `hidden`, `active`, or `cast-shadow`.
pub fn get_bool(attributes: Dict(String, String), key: String) -> Bool {
  dict.get(attributes, key) |> result.is_ok
}

/// Interpret an attribute change as a boolean toggle.
///
/// Removed attributes become `False`, while added or updated attributes become
/// `True`.
pub fn bool_change(changes: AttributeChanges, key: String) -> Result(Bool, Nil) {
  case change(changes, key) {
    Ok(Added(_)) | Ok(Updated(_)) -> Ok(True)
    Ok(Removed) -> Ok(False)
    Error(Nil) -> Error(Nil)
  }
}

/// Read and parse an attribute, falling back to a default value on failure.
pub fn get(
  attributes attributes: Dict(String, String),
  key key: String,
  default default: a,
  parse_fn parse_fn: fn(String) -> Result(a, Nil),
) -> a {
  dict.get(attributes, key)
  |> result.try(parse_fn)
  |> result.unwrap(default)
}

/// Parse a number attribute as either a float literal or an integer literal.
pub fn parse_number(number: String) -> Result(Float, Nil) {
  case float.parse(number) {
    Ok(float) -> Ok(float)
    Error(Nil) -> result.map(int.parse(number), int.to_float)
  }
}
