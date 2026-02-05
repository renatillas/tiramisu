//// The tiramisu-camera web component.
////
//// This component creates a Three.js camera and adds it to the scene.
//// Mark a camera as active to use it for rendering.
////
//// ## Usage
////
//// ```html
//// <tiramisu-camera
////   id="main"
////   type="perspective"
////   fov="75"
////   near="0.1"
////   far="1000"
////   transform="pos:0,5,10"
////   active="true"
//// ></tiramisu-camera>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier for the camera (required)
//// - `type`: Camera type - "perspective" or "orthographic" (default: "perspective")
//// - `fov`: Field of view in degrees (perspective only, default: 75)
//// - `near`: Near clipping plane (default: 0.1)
//// - `far`: Far clipping plane (default: 1000)
//// - `transform`: Transform as "pos:x,y,z quat:x,y,z,w scale:x,y,z" (default: pos:0,0,5)
//// - `active`: Whether this camera is used for rendering (default: "false")

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import savoiardi
import tiramisu/context.{type SceneContext, SceneContext}
import tiramisu/internal/dom
import tiramisu/internal/runtime.{type CameraRef}
import tiramisu/transform.{type Transform}
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// Camera type.
pub type CameraType {
  Perspective
  Orthographic
}

/// The model for the camera component.
pub type Model {
  Model(
    /// The camera ID
    id: String,
    /// Reference to the created camera object
    camera_ref: Option(CameraRef),
    /// Scene context from parent renderer
    scene_context: Option(SceneContext),
    /// Parent object ID for hierarchical transforms (None = scene root)
    parent_id: Option(String),
    /// Camera type
    camera_type: CameraType,
    /// Field of view (perspective only)
    fov: Float,
    /// Near clipping plane
    near: Float,
    /// Far clipping plane
    far: Float,
    /// Transform (position, rotation, scale)
    transform: Transform,
    /// Whether this camera is active
    active: Bool,
  )
}

/// Messages for the camera component.
pub type Msg {
  /// Scene ID and optional parent object ID found via DOM traversal
  SceneIdFound(scene_id: String, parent_id: Option(String))
  /// Scene context received from parent (kept for compatibility)
  SceneContextReceived(SceneContext)
  /// Camera was created
  CameraCreated(CameraRef)
  /// ID attribute changed
  IdChanged(String)
  /// Type attribute changed
  TypeChanged(CameraType)
  /// FOV attribute changed
  FovChanged(Float)
  /// Near attribute changed
  NearChanged(Float)
  /// Far attribute changed
  FarChanged(Float)
  /// Transform attribute changed
  TransformChanged(Transform)
  /// Active attribute changed
  ActiveChanged(Bool)
}

// COMPONENT -------------------------------------------------------------------

/// The tag name for the camera component.
pub const tag_name = "tiramisu-camera"

/// Register the tiramisu-camera component as a custom element.
pub fn register() -> Result(Nil, lustre.Error) {
  let app =
    lustre.component(
      init,
      update,
      view,
      [
        component.on_attribute_change("id", fn(v) { Ok(IdChanged(v)) }),
        component.on_attribute_change("type", fn(v) {
          case v {
            "perspective" -> Ok(TypeChanged(Perspective))
            "orthographic" -> Ok(TypeChanged(Orthographic))
            _ -> Error(Nil)
          }
        }),
        component.on_attribute_change("fov", parse_float_attr(FovChanged)),
        component.on_attribute_change("near", parse_float_attr(NearChanged)),
        component.on_attribute_change("far", parse_float_attr(FarChanged)),
        component.on_attribute_change("transform", fn(v) {
          Ok(TransformChanged(transform.parse(v)))
        }),
        component.on_attribute_change("active", parse_bool_attr(ActiveChanged)),
      ],
    )

  lustre.register(app, tag_name)
}

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-camera element.
///
/// Cameras define viewpoints into the scene. Mark one camera as `active`
/// to use it for rendering.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/camera
///
/// camera.camera("main", [
///   camera.fov(75.0),
///   camera.active(True),
/// ], [])
/// ```
///
pub fn camera(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the camera type ("perspective" or "orthographic").
///
pub fn camera_type(type_: String) -> Attribute(msg) {
  attribute.attribute("type", type_)
}

/// Set the field of view for a perspective camera (in degrees).
///
pub fn fov(degrees: Float) -> Attribute(msg) {
  attribute.attribute("fov", float.to_string(degrees))
}

/// Set the near clipping plane distance.
///
pub fn near(distance: Float) -> Attribute(msg) {
  attribute.attribute("near", float.to_string(distance))
}

/// Set the far clipping plane distance.
///
pub fn far(distance: Float) -> Attribute(msg) {
  attribute.attribute("far", float.to_string(distance))
}

/// Mark a camera as active (only active cameras render).
///
pub fn active(is_active: Bool) -> Attribute(msg) {
  attribute.attribute("active", case is_active {
    True -> "true"
    False -> "false"
  })
}

/// Set the full transform of the camera.
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

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  // Default camera at z=5 looking toward origin
  let default_transform = transform.at(position: vec3.Vec3(0.0, 0.0, 5.0))

  let model =
    Model(
      id: "",
      camera_ref: None,
      scene_context: None,
      parent_id: None,
      camera_type: Perspective,
      fov: 75.0,
      near: 0.1,
      far: 1000.0,
      transform: default_transform,
      active: False,
    )

  // Use after_paint to find the parent renderer via DOM traversal
  #(model, effect.after_paint(discover_scene_and_parent))
}

fn discover_scene_and_parent(
  dispatch: fn(Msg) -> Nil,
  root: dynamic.Dynamic,
) -> Nil {
  // Find both the scene ID and the immediate parent object ID
  let parent_id = dom.find_parent_object_id(root)

  case dom.find_parent_scene_id(root) {
    Some(scene_id) -> dispatch(SceneIdFound(scene_id, parent_id))
    None ->
      // Scene ID not ready yet, listen for the event
      dom.listen_for_scene_ready(root, fn(scene_id) {
        dispatch(SceneIdFound(scene_id, parent_id))
      })
  }
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    SceneIdFound(scene_id, parent_id) -> {
      let ctx = SceneContext(scene_id:)
      let new_model = Model(..model, scene_context: Some(ctx), parent_id:)
      // Create camera if we have an ID
      case model.camera_ref, model.id {
        None, id if id != "" -> #(new_model, create_camera_effect(new_model, ctx))
        _, _ -> #(new_model, effect.none())
      }
    }

    IdChanged(id) -> {
      let new_model = Model(..model, id:)
      // If we have context but no camera yet, create it now
      case model.scene_context, model.camera_ref {
        Some(ctx), None -> #(new_model, create_camera_effect(new_model, ctx))
        _, _ -> #(new_model, effect.none())
      }
    }

    SceneContextReceived(ctx) -> {
      let new_model = Model(..model, scene_context: Some(ctx))
      // Only create camera if we have an ID
      case model.camera_ref, model.id {
        None, id if id != "" -> #(new_model, create_camera_effect(new_model, ctx))
        _, _ -> #(new_model, effect.none())
      }
    }

    CameraCreated(ref) -> #(Model(..model, camera_ref: Some(ref)), effect.none())

    TypeChanged(camera_type) -> {
      // Type can only be changed before creation
      #(Model(..model, camera_type:), effect.none())
    }

    FovChanged(fov) -> {
      let new_model = Model(..model, fov:)
      case model.camera_ref {
        Some(ref) -> #(new_model, update_camera_effect(ref, new_model))
        None -> #(new_model, effect.none())
      }
    }

    NearChanged(near) -> {
      let new_model = Model(..model, near:)
      case model.camera_ref {
        Some(ref) -> #(new_model, update_camera_effect(ref, new_model))
        None -> #(new_model, effect.none())
      }
    }

    FarChanged(far) -> {
      let new_model = Model(..model, far:)
      case model.camera_ref {
        Some(ref) -> #(new_model, update_camera_effect(ref, new_model))
        None -> #(new_model, effect.none())
      }
    }

    TransformChanged(new_transform) -> {
      let new_model = Model(..model, transform: new_transform)
      case model.camera_ref {
        Some(ref) -> {
          let obj_ref = runtime.ObjectRef(ref.id)
          #(new_model, apply_transform_effect(obj_ref, new_transform))
        }
        None -> #(new_model, effect.none())
      }
    }

    ActiveChanged(active) -> {
      let new_model = Model(..model, active:)
      case model.camera_ref {
        Some(ref) -> #(new_model, effect.from(fn(_) {
          runtime.set_camera_active(ref, active)
        }))
        None -> #(new_model, effect.none())
      }
    }
  }
}

fn create_camera_effect(model: Model, ctx: SceneContext) -> Effect(Msg) {
  effect.from(fn(dispatch) {
    let scene_ref = runtime.SceneRef(ctx.scene_id)
    // Use parent_id if set, otherwise add to scene root
    let parent = case model.parent_id {
      Some(pid) -> pid
      None -> ctx.scene_id
    }

    let camera_ref = case model.camera_type {
      Perspective ->
        runtime.create_perspective_camera(
          scene_ref,
          parent,
          model.id,
          model.fov,
          16.0 /. 9.0,
          // Aspect ratio, will be updated by renderer
          model.near,
          model.far,
          model.active,
        )
      Orthographic ->
        runtime.create_orthographic_camera(
          scene_ref,
          parent,
          model.id,
          -10.0,
          // left
          10.0,
          // right
          10.0,
          // top
          -10.0,
          // bottom
          model.near,
          model.far,
          model.active,
        )
    }

    // Apply initial transform
    let obj_ref = runtime.ObjectRef(camera_ref.id)
    apply_transform(obj_ref, model.transform)

    dispatch(CameraCreated(camera_ref))
  })
}

fn apply_transform_effect(ref: runtime.ObjectRef, t: Transform) -> Effect(Msg) {
  effect.from(fn(_) { apply_transform(ref, t) })
}

fn apply_transform(ref: runtime.ObjectRef, t: Transform) -> Nil {
  let vec3.Vec3(px, py, pz) = transform.position(t)
  let #(qx, qy, qz, qw) = transform.to_quaternion_xyzw(t)
  let vec3.Vec3(sx, sy, sz) = transform.scale(t)

  runtime.set_position(ref, px, py, pz)
  runtime.set_quaternion(ref, qx, qy, qz, qw)
  runtime.set_scale(ref, sx, sy, sz)
}

fn update_camera_effect(ref: CameraRef, model: Model) -> Effect(Msg) {
  effect.from(fn(_) {
    case runtime.get_camera(ref) {
      Some(camera) -> {
        // Use savoiardi to update camera params
        // Note: aspect ratio is managed by the renderer on resize
        savoiardi.set_perspective_camera_params(
          camera,
          model.fov,
          16.0 /. 9.0,  // Default aspect, will be corrected by renderer
          model.near,
          model.far,
        )
        savoiardi.update_camera_projection_matrix(camera)
      }
      None -> Nil
    }
  })
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  html.div([], [])
}

// ATTRIBUTE PARSERS -----------------------------------------------------------

fn parse_float_attr(to_msg: fn(Float) -> Msg) -> fn(String) -> Result(Msg, Nil) {
  fn(v) {
    case float.parse(v) {
      Ok(f) -> Ok(to_msg(f))
      Error(_) -> {
        case int.parse(v) {
          Ok(i) -> Ok(to_msg(int.to_float(i)))
          Error(_) -> Error(Nil)
        }
      }
    }
  }
}

fn parse_bool_attr(to_msg: fn(Bool) -> Msg) -> fn(String) -> Result(Msg, Nil) {
  fn(v) {
    case v {
      "true" | "1" | "" -> Ok(to_msg(True))
      "false" | "0" -> Ok(to_msg(False))
      _ -> Error(Nil)
    }
  }
}

