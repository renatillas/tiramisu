//// The tiramisu-mesh web component.
////
//// This component creates a Three.js mesh and adds it to the scene.
//// It subscribes to the scene context provided by tiramisu-renderer.
////
//// ## Usage
////
//// ```html
//// <tiramisu-mesh
////   id="cube"
////   geometry="box:2,2,2"
////   color="#ff6b6b"
////   metalness="0.5"
////   roughness="0.5"
////   transform="pos:0,1,0"
//// ></tiramisu-mesh>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier for the mesh (required)
//// - `geometry`: Geometry specification (e.g., "box:1,1,1", "sphere:0.5,32,16")
//// - `color`: Hex color (e.g., "#ff0000")
//// - `metalness`: Material metalness 0.0-1.0 (default: 0.0)
//// - `roughness`: Material roughness 0.0-1.0 (default: 0.5)
//// - `opacity`: Material opacity 0.0-1.0 (default: 1.0)
//// - `wireframe`: Enable wireframe mode (default: "false")
//// - `transform`: Transform as "pos:x,y,z quat:x,y,z,w scale:x,y,z" (parts optional)
//// - `visible`: Visibility (default: "true")

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/list
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

import vec/vec2
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// The model for the mesh component.
pub type Model {
  Model(
    /// The mesh ID
    id: String,
    /// Reference to the created mesh object
    object_ref: Option(ObjectRef),
    /// Scene context from parent renderer
    scene_context: Option(SceneContext),
    /// Parent object ID for hierarchical transforms (None = scene root)
    parent_id: Option(String),
    /// Geometry specification (for primitive meshes)
    geometry: String,
    /// Source URL for external models (GLTF/GLB/FBX)
    src: Option(String),
    /// Whether we're currently loading a model
    loading: Bool,
    /// Color hex string
    color: String,
    /// Material metalness
    metalness: Float,
    /// Material roughness
    roughness: Float,
    /// Material opacity
    opacity: Float,
    /// Wireframe mode
    wireframe: Bool,
    /// Transform (position, rotation, scale)
    transform: Transform,
    /// Visibility
    visible: Bool,
    /// When true, transform is driven by physics — skip apply_transform_effect
    physics_controlled: Bool,
  )
}

/// Messages for the mesh component.
pub type Msg {
  /// Scene ID and optional parent object ID found via DOM traversal
  SceneIdFound(scene_id: String, parent_id: Option(String))
  /// Scene context received from parent (kept for compatibility)
  SceneContextReceived(SceneContext)
  /// Mesh was created
  MeshCreated(ObjectRef)
  /// ID attribute changed
  IdChanged(String)
  /// Geometry attribute changed
  GeometryChanged(String)
  /// Source URL attribute changed (for external models)
  SrcChanged(String)
  /// External model loaded successfully
  ModelLoaded(savoiardi.Object3D)
  /// External model failed to load
  ModelLoadError
  /// Color attribute changed
  ColorChanged(String)
  /// Metalness attribute changed
  MetalnessChanged(Float)
  /// Roughness attribute changed
  RoughnessChanged(Float)
  /// Opacity attribute changed
  OpacityChanged(Float)
  /// Wireframe attribute changed
  WireframeChanged(Bool)
  /// Transform attribute changed
  TransformChanged(Transform)
  /// Visible attribute changed
  VisibleChanged(Bool)
  /// Physics-controlled attribute changed
  PhysicsControlledChanged(Bool)
}

// COMPONENT -------------------------------------------------------------------

/// The tag name for the mesh component.
pub const tag_name = "tiramisu-mesh"

/// Register the tiramisu-mesh component as a custom element.
pub fn register() -> Result(Nil, lustre.Error) {
  let app =
    lustre.component(init, update, view, [
      // Attribute handlers
      component.on_attribute_change("id", fn(v) { Ok(IdChanged(v)) }),
      component.on_attribute_change("geometry", fn(v) { Ok(GeometryChanged(v)) }),
      component.on_attribute_change("src", fn(v) { Ok(SrcChanged(v)) }),
      component.on_attribute_change("color", fn(v) { Ok(ColorChanged(v)) }),
      component.on_attribute_change(
        "metalness",
        parse_float_attr(MetalnessChanged),
      ),
      component.on_attribute_change(
        "roughness",
        parse_float_attr(RoughnessChanged),
      ),
      component.on_attribute_change("opacity", parse_float_attr(OpacityChanged)),
      component.on_attribute_change(
        "wireframe",
        parse_bool_attr(WireframeChanged),
      ),
      component.on_attribute_change("transform", fn(v) {
        Ok(TransformChanged(transform.parse(v)))
      }),
      component.on_attribute_change("visible", parse_bool_attr(VisibleChanged)),
      component.on_attribute_change(
        "physics-controlled",
        parse_bool_attr(PhysicsControlledChanged),
      ),
    ])

  lustre.register(app, tag_name)
}

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-mesh element.
///
/// Meshes are 3D objects with geometry and material. Use attributes to
/// configure their shape, appearance, and transform.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/mesh
///
/// mesh.mesh("player", [
///   mesh.geometry_box(1.0, 2.0, 1.0),
///   mesh.color(0x00ff00),
/// ], [])
/// ```
///
pub fn mesh(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the source URL for an external 3D model (GLTF/GLB/FBX).
///
/// When src is set, the mesh will load the external model instead of
/// creating a primitive geometry. Supports GLTF, GLB, and FBX formats.
///
/// ## Example
///
/// ```gleam
/// mesh.mesh("character", [
///   mesh.src("models/character.glb"),
///   mesh.transform(transform.at(vec3.Vec3(0.0, 0.0, 0.0))),
/// ], [])
/// ```
///
pub fn src(url: String) -> Attribute(msg) {
  attribute.attribute("src", url)
}

/// Set a box geometry with width, height, and depth.
///
pub fn geometry_box(shape: vec3.Vec3(Float)) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "box:"
      <> float.to_string(shape.x)
      <> ","
      <> float.to_string(shape.y)
      <> ","
      <> float.to_string(shape.z),
  )
}

/// Set a sphere geometry with radius and segments.
///
pub fn geometry_sphere(
  radius: Float,
  segments: vec2.Vec2(Int),
) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "sphere:"
      <> float.to_string(radius)
      <> ","
      <> int.to_string(segments.x)
      <> ","
      <> int.to_string(segments.y),
  )
}

/// Set a sphere geometry with just radius (default segments).
///
pub fn geometry_sphere_simple(radius: Float) -> Attribute(msg) {
  attribute.attribute("geometry", "sphere:" <> float.to_string(radius))
}

/// Set a plane geometry with width and height.
///
pub fn geometry_plane(size: vec2.Vec2(Float)) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "plane:" <> float.to_string(size.x) <> "," <> float.to_string(size.y),
  )
}

/// Set a cylinder geometry.
///
pub fn geometry_cylinder(
  radius_top radius_top: Float,
  radius_bottom radius_bottom: Float,
  height height: Float,
  segments segments: Int,
) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "cylinder:"
      <> float.to_string(radius_top)
      <> ","
      <> float.to_string(radius_bottom)
      <> ","
      <> float.to_string(height)
      <> ","
      <> int.to_string(segments),
  )
}

/// Set a cylinder geometry with uniform radius.
///
pub fn geometry_cylinder_simple(radius: Float, height: Float) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "cylinder:" <> float.to_string(radius) <> "," <> float.to_string(height),
  )
}

/// Set a cone geometry.
///
pub fn geometry_cone(
  radius radius: Float,
  height height: Float,
  segments segments: Int,
) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "cone:"
      <> float.to_string(radius)
      <> ","
      <> float.to_string(height)
      <> ","
      <> int.to_string(segments),
  )
}

/// Set a cone geometry with default segments.
///
pub fn geometry_cone_simple(radius: Float, height: Float) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "cone:" <> float.to_string(radius) <> "," <> float.to_string(height),
  )
}

/// Set a torus geometry.
///
pub fn geometry_torus(
  radius radius: Float,
  tube tube: Float,
  radial_segments radial_segments: Int,
  tubular_segments tubular_segments: Int,
) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "torus:"
      <> float.to_string(radius)
      <> ","
      <> float.to_string(tube)
      <> ","
      <> int.to_string(radial_segments)
      <> ","
      <> int.to_string(tubular_segments),
  )
}

/// Set a torus geometry with default segments.
///
pub fn geometry_torus_simple(radius: Float, tube: Float) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "torus:" <> float.to_string(radius) <> "," <> float.to_string(tube),
  )
}

/// Set the base color of the material (as a hex integer).
///
/// ## Example
///
/// ```gleam
/// mesh.color(0xff0000)  // Red
/// ```
///
pub fn color(hex hex: Int) -> Attribute(msg) {
  attribute.attribute("color", "#" <> int.to_base16(hex))
}

/// Set the base color of the material (as a hex string).
///
pub fn color_string(hex hex: String) -> Attribute(msg) {
  attribute.attribute("color", hex)
}

/// Set the metalness of the material (0.0 = dielectric, 1.0 = metal).
///
pub fn metalness(m: Float) -> Attribute(msg) {
  attribute.attribute("metalness", float.to_string(m))
}

/// Set the roughness of the material (0.0 = smooth, 1.0 = rough).
///
pub fn roughness(r: Float) -> Attribute(msg) {
  attribute.attribute("roughness", float.to_string(r))
}

/// Set the opacity of the material (0.0 = transparent, 1.0 = opaque).
///
pub fn opacity(o: Float) -> Attribute(msg) {
  attribute.attribute("opacity", float.to_string(o))
}

/// Enable or disable wireframe rendering.
///
pub fn wireframe(enabled: Bool) -> Attribute(msg) {
  attribute.attribute("wireframe", case enabled {
    True -> "true"
    False -> "false"
  })
}

/// Set the full transform of the mesh.
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

/// Set the mesh's visibility.
///
pub fn visible(is_visible: Bool) -> Attribute(msg) {
  attribute.attribute("visible", case is_visible {
    True -> ""
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
      parent_id: None,
      geometry: "box:1,1,1",
      src: None,
      loading: False,
      color: "#ffffff",
      metalness: 0.0,
      roughness: 0.5,
      opacity: 1.0,
      wireframe: False,
      transform: transform.identity,
      visible: True,
      physics_controlled: False,
    )

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
      case model.object_ref, model.id {
        None, id if id != "" -> #(new_model, create_mesh_effect(new_model, ctx))
        _, _ -> #(new_model, effect.none())
      }
    }

    IdChanged(id) -> {
      let new_model = Model(..model, id:)
      case model.scene_context, model.object_ref {
        Some(ctx), None -> #(new_model, create_mesh_effect(new_model, ctx))
        _, _ -> #(new_model, effect.none())
      }
    }

    SceneContextReceived(ctx) -> {
      let new_model = Model(..model, scene_context: Some(ctx))
      case model.object_ref, model.id {
        None, id if id != "" -> #(new_model, create_mesh_effect(new_model, ctx))
        _, _ -> #(new_model, effect.none())
      }
    }

    MeshCreated(ref) -> {
      #(Model(..model, object_ref: Some(ref)), effect.none())
    }

    GeometryChanged(geometry) -> {
      let new_model = Model(..model, geometry:)
      case model.object_ref {
        Some(ref) -> #(new_model, update_geometry_effect(ref, geometry))
        None -> #(new_model, effect.none())
      }
    }

    SrcChanged(url) -> {
      let new_model = Model(..model, src: Some(url), loading: True)
      // If we have context, start loading the model
      case model.scene_context {
        Some(ctx) -> #(new_model, load_model_effect(url, ctx))
        None -> #(new_model, effect.none())
      }
    }

    ModelLoaded(object3d) -> {
      // Model loaded - add it to the scene
      case model.scene_context {
        Some(ctx) -> {
          let obj_ref = add_loaded_model_to_scene(object3d, model, ctx)
          let new_model =
            Model(..model, object_ref: Some(obj_ref), loading: False)
          #(new_model, effect.none())
        }
        None -> #(Model(..model, loading: False), effect.none())
      }
    }

    ModelLoadError -> {
      // Loading failed - keep the model in a non-loading state
      #(Model(..model, loading: False), effect.none())
    }

    ColorChanged(color) -> {
      let new_model = Model(..model, color:)
      case model.object_ref {
        Some(ref) -> #(new_model, update_material_effect(ref, new_model))
        None -> #(new_model, effect.none())
      }
    }

    MetalnessChanged(metalness) -> {
      let new_model = Model(..model, metalness:)
      case model.object_ref {
        Some(ref) -> #(new_model, update_material_effect(ref, new_model))
        None -> #(new_model, effect.none())
      }
    }

    RoughnessChanged(roughness) -> {
      let new_model = Model(..model, roughness:)
      case model.object_ref {
        Some(ref) -> #(new_model, update_material_effect(ref, new_model))
        None -> #(new_model, effect.none())
      }
    }

    OpacityChanged(opacity) -> {
      let new_model = Model(..model, opacity:)
      case model.object_ref {
        Some(ref) -> #(new_model, update_material_effect(ref, new_model))
        None -> #(new_model, effect.none())
      }
    }

    WireframeChanged(wireframe) -> {
      let new_model = Model(..model, wireframe:)
      case model.object_ref {
        Some(ref) -> #(new_model, update_material_effect(ref, new_model))
        None -> #(new_model, effect.none())
      }
    }

    TransformChanged(new_transform) -> {
      let new_model = Model(..model, transform: new_transform)
      // When physics controls the transform, the physics sync already
      // updated the Three.js object directly. Just keep the model in sync.
      case model.physics_controlled {
        True -> #(new_model, effect.none())
        False ->
          case model.object_ref {
            Some(ref) -> #(
              new_model,
              apply_transform_effect(ref, new_transform),
            )
            None -> #(new_model, effect.none())
          }
      }
    }

    PhysicsControlledChanged(controlled) -> {
      #(Model(..model, physics_controlled: controlled), effect.none())
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

fn create_mesh_effect(model: Model, ctx: SceneContext) -> Effect(Msg) {
  // If src is set, load external model instead of creating primitive
  case model.src {
    Some(url) -> load_model_effect(url, ctx)
    None -> create_primitive_mesh_effect(model, ctx)
  }
}

fn create_primitive_mesh_effect(model: Model, ctx: SceneContext) -> Effect(Msg) {
  effect.from(fn(dispatch) {
    // Parse geometry and create savoiardi geometry
    let geometry = parse_geometry(model.geometry)

    // Create savoiardi material
    let color_int = parse_color(model.color)
    let material =
      savoiardi.create_standard_material(
        color_int,
        model.metalness,
        model.roughness,
        model.opacity <. 1.0,
        // transparent
        model.opacity,
        option.None,
        // map
        option.None,
        // normal_map
        option.None,
        // roughness_map
        option.None,
        // metalness_map
        1.0,
        // env_map_intensity
        0.0,
        // emissive_intensity
        option.None,
        // emissive_map
        option.None,
        // ao_map
        0x000000,
        // emissive
        1.0,
        // ao_intensity
      )

    // Create mesh via runtime - use parent_id if set, otherwise add to scene root
    let scene_ref = runtime.SceneRef(ctx.scene_id)
    let parent = case model.parent_id {
      Some(pid) -> pid
      None -> ctx.scene_id
    }
    let obj_ref =
      runtime.create_mesh(scene_ref, parent, model.id, geometry, material)

    // Apply initial transform
    apply_transform(obj_ref, model.transform)
    runtime.set_visible(obj_ref, model.visible)

    dispatch(MeshCreated(obj_ref))
  })
}

fn load_model_effect(url: String, _ctx: SceneContext) -> Effect(Msg) {
  effect.from(fn(dispatch) {
    // Determine format from URL extension and load accordingly
    let is_fbx = string.ends_with(url, ".fbx") || string.ends_with(url, ".FBX")

    case is_fbx {
      True -> {
        savoiardi.load_fbx(url)
        |> promise.map(fn(result) {
          case result {
            Ok(fbx_data) -> {
              let scene = savoiardi.get_fbx_scene(fbx_data)
              dispatch(ModelLoaded(scene))
            }
            Error(_) -> dispatch(ModelLoadError)
          }
        })
        Nil
      }
      False -> {
        // Assume GLTF/GLB
        savoiardi.load_gltf(url)
        |> promise.map(fn(result) {
          case result {
            Ok(gltf_data) -> {
              let scene = savoiardi.get_gltf_scene(gltf_data)
              dispatch(ModelLoaded(scene))
            }
            Error(_) -> dispatch(ModelLoadError)
          }
        })
        Nil
      }
    }
  })
}

fn add_loaded_model_to_scene(
  object3d: savoiardi.Object3D,
  model: Model,
  ctx: SceneContext,
) -> ObjectRef {
  let scene_ref = runtime.SceneRef(ctx.scene_id)
  // Use parent_id if set, otherwise add to scene root
  let parent = case model.parent_id {
    Some(pid) -> pid
    None -> ctx.scene_id
  }
  let obj_ref =
    runtime.add_object_to_scene(scene_ref, parent, model.id, object3d)

  // Apply initial transform
  apply_transform(obj_ref, model.transform)
  runtime.set_visible(obj_ref, model.visible)

  obj_ref
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

fn update_geometry_effect(ref: ObjectRef, geometry_spec: String) -> Effect(Msg) {
  effect.from(fn(_) {
    // Use savoiardi to update geometry instead of FFI
    case runtime.get_object(ref) {
      Some(object) -> {
        // Dispose old geometry
        let old_geometry = savoiardi.get_object_geometry(object)
        savoiardi.dispose_geometry(old_geometry)
        // Create and apply new geometry
        let new_geometry = parse_geometry(geometry_spec)
        savoiardi.set_object_geometry(object, new_geometry)
      }
      None -> Nil
    }
  })
}

fn update_material_effect(ref: ObjectRef, model: Model) -> Effect(Msg) {
  effect.from(fn(_) {
    case runtime.get_object(ref) {
      Some(object) -> {
        let material = savoiardi.get_object_material(object)
        let color_int = parse_color(model.color)
        savoiardi.update_material_color(material, color_int)
        savoiardi.update_material_metalness(material, model.metalness)
        savoiardi.update_material_roughness(material, model.roughness)
        savoiardi.update_material_opacity(material, model.opacity)
        savoiardi.update_material_wireframe(material, model.wireframe)
      }
      None -> Nil
    }
  })
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  // Mesh component is invisible in DOM — projects children (e.g. rigidbody)
  html.div([], [component.default_slot([], [])])
}

// ATTRIBUTE PARSERS -----------------------------------------------------------

fn parse_float_attr(to_msg: fn(Float) -> Msg) -> fn(String) -> Result(Msg, Nil) {
  fn(v) {
    case float.parse(v) {
      Ok(f) -> Ok(to_msg(f))
      Error(Nil) -> {
        // Try parsing as int
        case int.parse(v) {
          Ok(i) -> Ok(to_msg(int.to_float(i)))
          Error(Nil) -> Error(Nil)
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

// GEOMETRY PARSING ------------------------------------------------------------

fn parse_geometry(spec: String) -> savoiardi.Geometry {
  case string.split(spec, ":") {
    [type_str, params_str] -> {
      let params =
        string.split(params_str, ",")
        |> list.map(string.trim)
        |> list.filter_map(float.parse)

      case type_str, params {
        "box", [w, h, d] -> savoiardi.create_box_geometry(w, h, d)
        "box", [s] -> savoiardi.create_box_geometry(s, s, s)
        "box", _ -> savoiardi.create_box_geometry(1.0, 1.0, 1.0)

        "sphere", [r, ws, hs] ->
          savoiardi.create_sphere_geometry(r, float.round(ws), float.round(hs))
        "sphere", [r] -> savoiardi.create_sphere_geometry(r, 32, 16)
        "sphere", _ -> savoiardi.create_sphere_geometry(1.0, 32, 16)

        "plane", [w, h] -> savoiardi.create_plane_geometry(w, h, 1, 1)
        "plane", _ -> savoiardi.create_plane_geometry(1.0, 1.0, 1, 1)

        "cylinder", [rt, rb, h, s] ->
          savoiardi.create_cylinder_geometry(rt, rb, h, float.round(s))
        "cylinder", [r, h] -> savoiardi.create_cylinder_geometry(r, r, h, 32)
        "cylinder", _ -> savoiardi.create_cylinder_geometry(1.0, 1.0, 1.0, 32)

        "cone", [r, h, s] ->
          savoiardi.create_cone_geometry(r, h, float.round(s))
        "cone", [r, h] -> savoiardi.create_cone_geometry(r, h, 32)
        "cone", _ -> savoiardi.create_cone_geometry(1.0, 1.0, 32)

        "torus", [r, t, rs, ts] ->
          savoiardi.create_torus_geometry(
            r,
            t,
            float.round(rs),
            float.round(ts),
          )
        "torus", [r, t] -> savoiardi.create_torus_geometry(r, t, 16, 48)
        "torus", _ -> savoiardi.create_torus_geometry(1.0, 0.4, 16, 48)

        _, _ -> savoiardi.create_box_geometry(1.0, 1.0, 1.0)
      }
    }
    [type_str] -> {
      case type_str {
        "box" -> savoiardi.create_box_geometry(1.0, 1.0, 1.0)
        "sphere" -> savoiardi.create_sphere_geometry(1.0, 32, 16)
        "plane" -> savoiardi.create_plane_geometry(1.0, 1.0, 1, 1)
        "cylinder" -> savoiardi.create_cylinder_geometry(1.0, 1.0, 1.0, 32)
        "cone" -> savoiardi.create_cone_geometry(1.0, 1.0, 32)
        "torus" -> savoiardi.create_torus_geometry(1.0, 0.4, 16, 48)
        _ -> savoiardi.create_box_geometry(1.0, 1.0, 1.0)
      }
    }
    _ -> savoiardi.create_box_geometry(1.0, 1.0, 1.0)
  }
}

fn parse_color(hex: String) -> Int {
  let clean = string.replace(hex, "#", "")
  case int.base_parse(clean, 16) {
    Ok(n) -> n
    Error(_) -> 0xffffff
  }
}
