//// The tiramisu-light web component.
////
//// This component creates a Three.js light and adds it to the scene.
////
//// ## Usage
////
//// ```html
//// <tiramisu-light
////   id="sun"
////   type="directional"
////   color="#ffffff"
////   intensity="1"
////   transform="pos:10,10,10"
////   cast-shadow="true"
//// ></tiramisu-light>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier for the light (required)
//// - `type`: Light type - "ambient", "directional", "point", "spot" (default: "ambient")
//// - `color`: Light color as hex string (default: "#ffffff")
//// - `intensity`: Light intensity (default: 1.0)
//// - `transform`: Transform as "pos:x,y,z quat:x,y,z,w scale:x,y,z" (default: origin)
//// - `cast-shadow`: Whether light casts shadows (default: "false")

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import savoiardi
import tiramisu/context.{type SceneContext, SceneContext}
import tiramisu/internal/dom
import tiramisu/internal/runtime.{type ObjectRef}
import tiramisu/transform.{type Transform}
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// Light type.
pub type LightType {
  Ambient
  Directional
  Point
  Spot
}

/// The model for the light component.
pub type Model {
  Model(
    /// The light ID
    id: String,
    /// Reference to the created light object
    object_ref: Option(ObjectRef),
    /// Scene context from parent renderer
    scene_context: Option(SceneContext),
    /// Parent object ID for hierarchical transforms (None = scene root)
    parent_id: Option(String),
    /// Light type
    light_type: LightType,
    /// Color as hex string
    color: String,
    /// Light intensity
    intensity: Float,
    /// Transform (position, rotation, scale)
    transform: Transform,
    /// Whether light casts shadows
    cast_shadow: Bool,
  )
}

/// Messages for the light component.
pub type Msg {
  /// Scene ID and optional parent object ID found via DOM traversal
  SceneIdFound(scene_id: String, parent_id: Option(String))
  /// Scene context received from parent (kept for compatibility)
  SceneContextReceived(SceneContext)
  /// Light was created
  LightCreated(ObjectRef)
  /// ID attribute changed
  IdChanged(String)
  /// Type attribute changed
  TypeChanged(LightType)
  /// Color attribute changed
  ColorChanged(String)
  /// Intensity attribute changed
  IntensityChanged(Float)
  /// Transform attribute changed
  TransformChanged(Transform)
  /// Cast shadow attribute changed
  CastShadowChanged(Bool)
}

// COMPONENT -------------------------------------------------------------------

/// The tag name for the light component.
pub const tag_name = "tiramisu-light"

/// Register the tiramisu-light component as a custom element.
pub fn register() -> Result(Nil, lustre.Error) {
  let app =
    lustre.component(init, update, view, [
      component.on_attribute_change("id", fn(v) { Ok(IdChanged(v)) }),
      component.on_attribute_change("type", fn(v) {
        case v {
          "ambient" -> Ok(TypeChanged(Ambient))
          "directional" -> Ok(TypeChanged(Directional))
          "point" -> Ok(TypeChanged(Point))
          "spot" -> Ok(TypeChanged(Spot))
          _ -> Error(Nil)
        }
      }),
      component.on_attribute_change("color", fn(v) { Ok(ColorChanged(v)) }),
      component.on_attribute_change(
        "intensity",
        parse_float_attr(IntensityChanged),
      ),
      component.on_attribute_change("transform", fn(v) {
        Ok(TransformChanged(transform.parse(v)))
      }),
      component.on_attribute_change(
        "cast-shadow",
        parse_bool_attr(CastShadowChanged),
      ),
    ])

  lustre.register(app, tag_name)
}

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-light element.
///
/// Lights illuminate the scene. Different types create different
/// lighting effects.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/light
///
/// light.light("sun", [
///   light.light_type("directional"),
///   light.intensity(1.5),
///   light.cast_shadow(True),
/// ], [])
/// ```
///
pub fn light(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the light type (ambient, directional, point, spot).
///
pub fn light_type(type_: String) -> Attribute(msg) {
  attribute.attribute("type", type_)
}

/// Set the light color (as hex integer).
///
pub fn color(hex: Int) -> Attribute(msg) {
  attribute.attribute("color", "#" <> int.to_base16(hex))
}

/// Set the light color (as hex string).
///
pub fn color_string(hex: String) -> Attribute(msg) {
  attribute.attribute("color", hex)
}

/// Set the intensity of the light.
///
pub fn intensity(value: Float) -> Attribute(msg) {
  attribute.attribute("intensity", float.to_string(value))
}

/// Set whether the light casts shadows.
///
pub fn cast_shadow(enabled: Bool) -> Attribute(msg) {
  attribute.attribute("cast-shadow", case enabled {
    True -> "true"
    False -> "false"
  })
}

/// Set the full transform of the light.
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
  let model =
    Model(
      id: "",
      object_ref: None,
      scene_context: None,
      parent_id: None,
      light_type: Ambient,
      color: "#ffffff",
      intensity: 1.0,
      transform: transform.identity,
      cast_shadow: False,
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
      // Create light if we have an ID
      case model.object_ref, model.id {
        None, id if id != "" -> #(
          new_model,
          create_light_effect(new_model, ctx),
        )
        _, _ -> #(new_model, effect.none())
      }
    }

    IdChanged(id) -> {
      let new_model = Model(..model, id:)
      // If we have context but no light yet, create it now
      case model.scene_context, model.object_ref {
        Some(ctx), None -> #(new_model, create_light_effect(new_model, ctx))
        _, _ -> #(new_model, effect.none())
      }
    }

    SceneContextReceived(ctx) -> {
      let new_model = Model(..model, scene_context: Some(ctx))
      // Only create light if we have an ID
      case model.object_ref, model.id {
        None, id if id != "" -> #(
          new_model,
          create_light_effect(new_model, ctx),
        )
        _, _ -> #(new_model, effect.none())
      }
    }

    LightCreated(ref) -> #(Model(..model, object_ref: Some(ref)), effect.none())

    TypeChanged(light_type) -> {
      // Type can only be changed before creation
      #(Model(..model, light_type:), effect.none())
    }

    ColorChanged(color) -> {
      let new_model = Model(..model, color:)
      case model.object_ref {
        Some(ref) -> #(new_model, update_light_effect(ref, new_model))
        None -> #(new_model, effect.none())
      }
    }

    IntensityChanged(intensity) -> {
      let new_model = Model(..model, intensity:)
      case model.object_ref {
        Some(ref) -> #(new_model, update_light_effect(ref, new_model))
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

    CastShadowChanged(cast_shadow) -> {
      // Cast shadow can only be changed at creation
      #(Model(..model, cast_shadow:), effect.none())
    }
  }
}

fn create_light_effect(model: Model, ctx: SceneContext) -> Effect(Msg) {
  effect.from(fn(dispatch) {
    let scene_ref = runtime.SceneRef(ctx.scene_id)
    let color_int = parse_color(model.color)
    // Use parent_id if set, otherwise add to scene root
    let parent = case model.parent_id {
      Some(pid) -> pid
      None -> ctx.scene_id
    }

    let object_ref = case model.light_type {
      Ambient ->
        runtime.create_ambient_light(
          scene_ref,
          parent,
          model.id,
          color_int,
          model.intensity,
        )
      Directional ->
        runtime.create_directional_light(
          scene_ref,
          parent,
          model.id,
          color_int,
          model.intensity,
          model.cast_shadow,
        )
      Point ->
        runtime.create_point_light(
          scene_ref,
          parent,
          model.id,
          color_int,
          model.intensity,
          0.0,
          // distance (0 = infinite)
          model.cast_shadow,
        )
      Spot ->
        // For spot lights, use point light as fallback (spot needs more params)
        runtime.create_point_light(
          scene_ref,
          parent,
          model.id,
          color_int,
          model.intensity,
          0.0,
          model.cast_shadow,
        )
    }

    // Apply initial transform
    apply_transform(object_ref, model.transform)

    dispatch(LightCreated(object_ref))
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

fn update_light_effect(ref: ObjectRef, model: Model) -> Effect(Msg) {
  effect.from(fn(_) {
    case runtime.get_light(ref) {
      Some(light) -> {
        let color_int = parse_color(model.color)
        savoiardi.update_light_color(light, color_int)
        savoiardi.update_light_intensity(light, model.intensity)
      }
      None -> Nil
    }
  })
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  html.div([], [])
}

// HELPERS ---------------------------------------------------------------------

fn parse_color(hex: String) -> Int {
  let clean = string.replace(hex, "#", "")
  case int.base_parse(clean, 16) {
    Ok(n) -> n
    Error(_) -> 0xffffff
  }
}

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
