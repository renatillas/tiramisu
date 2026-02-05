//// The tiramisu-empty web component.
////
//// This component creates a Three.js Group for hierarchical organization.
//// It provides its own context so nested children are added to the group.
////
//// ## Usage
////
//// ```html
//// <tiramisu-empty id="enemies" transform="pos:0,0,-10">
////   <tiramisu-mesh id="enemy1" geometry="box:1,1,1" transform="pos:-2,0,0"></tiramisu-mesh>
////   <tiramisu-mesh id="enemy2" geometry="box:1,1,1" transform="pos:2,0,0"></tiramisu-mesh>
//// </tiramisu-empty>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier for the group (required)
//// - `transform`: Transform as "pos:x,y,z quat:x,y,z,w scale:x,y,z" (default: origin)
//// - `visible`: Visibility (default: "true")

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/float
import gleam/option.{type Option, None, Some}
import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import tiramisu/context.{type SceneContext, GroupContext, SceneContext}
import tiramisu/internal/dom
import tiramisu/internal/runtime.{type ObjectRef}
import tiramisu/transform.{type Transform}
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// The model for the empty component.
pub type Model {
  Model(
    /// The group ID
    id: String,
    /// Reference to the created group object
    object_ref: Option(ObjectRef),
    /// Scene context from parent renderer
    scene_context: Option(SceneContext),
    /// Transform (position, rotation, scale)
    transform: Transform,
    /// Visibility
    visible: Bool,
  )
}

/// Messages for the empty component.
pub type Msg {
  /// Scene ID found via DOM traversal
  SceneIdFound(String)
  /// Scene context received from parent (kept for compatibility)
  SceneContextReceived(SceneContext)
  /// Group was created
  GroupCreated(ObjectRef)
  /// ID attribute changed
  IdChanged(String)
  /// Transform attribute changed
  TransformChanged(Transform)
  /// Visible attribute changed
  VisibleChanged(Bool)
}

// COMPONENT -------------------------------------------------------------------

/// The tag name for the empty component.
pub const tag_name = "tiramisu-empty"

/// Register the tiramisu-empty component as a custom element.
pub fn register() -> Result(Nil, lustre.Error) {
  let app =
    lustre.component(init, update, view, [
      component.on_attribute_change("id", fn(v) { Ok(IdChanged(v)) }),
      component.on_attribute_change("transform", fn(v) {
        Ok(TransformChanged(transform.parse(v)))
      }),
      component.on_attribute_change("visible", parse_bool_attr(VisibleChanged)),
    ])

  lustre.register(app, tag_name)
}

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-empty element (group).
///
/// Empty nodes are invisible groups used for hierarchical organization.
/// Children of an empty are transformed relative to it.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/empty
/// import tiramisu/mesh
///
/// empty.empty("enemies", [], [
///   mesh.mesh("enemy1", [...], []),
///   mesh.mesh("enemy2", [...], []),
/// ])
/// ```
///
pub fn empty(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

/// Alias for `empty` - creates a group node.
///
pub fn group(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  empty(id, attributes, children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the full transform of the group.
///
pub fn transform(t: Transform) -> Attribute(msg) {
  let vec3.Vec3(px, py, pz) = transform.position(t)
  let #(qx, qy, qz, qw) = transform.to_quaternion_xyzw(t)
  let vec3.Vec3(sx, sy, sz) = transform.scale(t)

  attribute.attribute(
    "transform",
    "pos:"
      <> float.to_string(px)
      <> ","
      <> float.to_string(py)
      <> ","
      <> float.to_string(pz)
      <> " quat:"
      <> float.to_string(qx)
      <> ","
      <> float.to_string(qy)
      <> ","
      <> float.to_string(qz)
      <> ","
      <> float.to_string(qw)
      <> " scale:"
      <> float.to_string(sx)
      <> ","
      <> float.to_string(sy)
      <> ","
      <> float.to_string(sz),
  )
}

/// Set the group's visibility.
///
pub fn visible(is_visible: Bool) -> Attribute(msg) {
  attribute.attribute("visible", case is_visible {
    True -> "true"
    False -> "false"
  })
}

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      id: "",
      object_ref: None,
      scene_context: None,
      transform: transform.identity,
      visible: True,
    )

  // Use after_paint to find the parent renderer via DOM traversal
  #(model, effect.after_paint(discover_scene_id))
}

fn discover_scene_id(dispatch: fn(Msg) -> Nil, root: dynamic.Dynamic) -> Nil {
  // Try to find the parent renderer's scene ID
  case dom.find_parent_scene_id(root) {
    Some(scene_id) -> dispatch(SceneIdFound(scene_id))
    None ->
      // Scene ID not ready yet, listen for the event
      dom.listen_for_scene_ready(root, fn(scene_id) {
        dispatch(SceneIdFound(scene_id))
      })
  }
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    SceneIdFound(scene_id) -> {
      let ctx = SceneContext(scene_id:)
      let new_model = Model(..model, scene_context: Some(ctx))
      // Create group if we have an ID
      case model.object_ref, model.id {
        None, id if id != "" -> #(
          new_model,
          create_group_effect(new_model, ctx),
        )
        _, _ -> #(new_model, effect.none())
      }
    }

    IdChanged(id) -> {
      let new_model = Model(..model, id:)
      // If we have context but no group yet, create it now
      case model.scene_context, model.object_ref {
        Some(ctx), None -> #(new_model, create_group_effect(new_model, ctx))
        _, _ -> #(new_model, effect.none())
      }
    }

    SceneContextReceived(ctx) -> {
      let new_model = Model(..model, scene_context: Some(ctx))
      // Only create group if we have an ID
      case model.object_ref, model.id {
        None, id if id != "" -> #(
          new_model,
          create_group_effect(new_model, ctx),
        )
        _, _ -> #(new_model, effect.none())
      }
    }

    GroupCreated(ref) -> {
      let new_model = Model(..model, object_ref: Some(ref))
      // Provide group context to children
      case model.scene_context {
        Some(ctx) -> {
          let group_ctx = GroupContext(scene_id: ctx.scene_id, group_id: ref.id)
          let provide_effect =
            effect.provide(
              context.group_context_key,
              context.encode_group_context(group_ctx),
            )
          #(new_model, provide_effect)
        }
        None -> #(new_model, effect.none())
      }
    }

    TransformChanged(new_transform) -> {
      let new_model = Model(..model, transform: new_transform)
      case model.object_ref {
        Some(ref) -> #(new_model, apply_transform_effect(ref, new_transform))
        None -> #(new_model, effect.none())
      }
    }

    VisibleChanged(visible) -> {
      let new_model = Model(..model, visible:)
      case model.object_ref {
        Some(ref) -> #(
          new_model,
          effect.from(fn(_) { runtime.set_visible(ref, visible) }),
        )
        None -> #(new_model, effect.none())
      }
    }
  }
}

fn create_group_effect(model: Model, ctx: SceneContext) -> Effect(Msg) {
  effect.from(fn(dispatch) {
    let scene_ref = runtime.SceneRef(ctx.scene_id)
    let object_ref = runtime.create_group(scene_ref, ctx.scene_id, model.id)

    // Apply initial transform
    apply_transform(object_ref, model.transform)
    runtime.set_visible(object_ref, model.visible)

    dispatch(GroupCreated(object_ref))
  })
}

fn apply_transform_effect(ref: ObjectRef, t: Transform) -> Effect(Msg) {
  effect.from(fn(_) { apply_transform(ref, t) })
}

fn apply_transform(ref: ObjectRef, t: Transform) -> Nil {
  let vec3.Vec3(px, py, pz) = transform.position(t)
  let #(qx, qy, qz, qw) = transform.to_quaternion_xyzw(t)
  let vec3.Vec3(sx, sy, sz) = transform.scale(t)

  runtime.set_position(ref, px, py, pz)
  runtime.set_quaternion(ref, qx, qy, qz, qw)
  runtime.set_scale(ref, sx, sy, sz)
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  // Group component provides a slot for nested children
  html.div([], [component.default_slot([], [])])
}

// HELPERS ---------------------------------------------------------------------

fn parse_bool_attr(to_msg: fn(Bool) -> Msg) -> fn(String) -> Result(Msg, Nil) {
  fn(v) {
    case v {
      "true" | "1" | "" -> Ok(to_msg(True))
      "false" | "0" -> Ok(to_msg(False))
      _ -> Error(Nil)
    }
  }
}
// FFI DECLARATIONS ------------------------------------------------------------
// DOM operations (findParentSceneId, listenForSceneReady) are now in
// tiramisu/internal/dom module. This component has no additional FFI.
