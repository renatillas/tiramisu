import gleam/dict
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import savoiardi
import tiramisu/internal/node_utils
import tiramisu/internal/registry

import lustre/attribute.{type Attribute}
import lustre/event

import vec/vec2
import vec/vec3

import tiramisu/extension.{type Context, Context}
import tiramisu/transform.{type Transform}

@internal
pub const tag = "tiramisu-mesh"

// GEOMETRY ATTRIBUTES ---------------------------------------------------------

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
pub fn sphere(
  radius radius: Float,
  segments segments: vec2.Vec2(Int),
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

/// Set a plane geometry with width and height.
///
pub fn plane(size: vec2.Vec2(Float)) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "plane:" <> float.to_string(size.x) <> "," <> float.to_string(size.y),
  )
}

/// Set a cylinder geometry.
///
pub fn cylinder(
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

/// Set a cone geometry.
///
pub fn cone(
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

/// Set a torus geometry.
///
pub fn torus(
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

/// Set the base color as a hex integer.
///
/// ## Example
///
/// ```gleam
/// tiramisu.color(0xff0000)  // Red
/// ```
///
pub fn color(hex hex: Int) -> Attribute(msg) {
  attribute.attribute("color", "#" <> int.to_base16(hex))
}

// OTHER MESH ATTRIBUTES -------------------------------------------------------

/// Set the element's visibility.
///
/// Works with meshes, empties, and instanced meshes.
///
pub fn visible(is_visible: Bool) -> Attribute(msg) {
  case is_visible {
    True -> attribute.attribute("visible", "")
    False -> attribute.property("visible", json.bool(False))
  }
}

// EVENTS ----------------------------------------------------------------------

/// Listen for the model-loaded event.
///
/// This event fires when an external 3D model (set via `src`) has finished
/// loading. The handler receives the mesh ID.
///
/// ## Example
///
/// ```gleam
/// tiramisu.mesh("character", [
///   attribute.src("/models/character.glb"),
///   mesh.on_model_loaded(fn(id) { ModelLoaded(id) }),
/// ], [])
/// ```
///
pub fn on_model_loaded(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("tiramisu:model-loaded", {
    use id <- decode.subfield(["detail", "id"], decode.string)
    decode.success(handler(id))
  })
}

/// Listen for the model-error event.
///
/// This event fires when an external 3D model fails to load.
/// The handler receives the mesh ID.
///
pub fn on_model_error(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("tiramisu:model-error", {
    use id <- decode.subfield(["detail", "id"], decode.string)
    decode.success(handler(id))
  })
}

pub fn extension() -> extension.Extension {
  extension.NodeExtension(extension.Node(
    tag: "tiramisu-mesh",
    observed_attributes: [
      "transform", "geometry", "src", "material-type", "color", "metalness",
      "roughness", "opacity", "wireframe", "emissive", "emissive-intensity",
      "side", "color-map", "normal-map", "ao-map", "roughness-map",
      "metalness-map", "displacement-map", "displacement-scale",
      "displacement-bias", "shininess", "alpha-test", "transparent", "visible",
      "cast-shadow", "receive-shadow", "physics-controlled",
    ],
    create:,
    update:,
    remove: node_utils.default_remove,
  ))
}

fn create(
  ctx: Context,
  id: String,
  parent_id: String,
  attrs: dict.Dict(String, String),
  transform: Transform,
) -> Context {
  case dict.get(attrs, "src") {
    Ok(url) -> {
      node_utils.load_model(
        ctx.on_async,
        fn(i, obj) { ctx.on_object_resolved("tiramisu-mesh", i, obj) },
        id,
        parent_id,
        url,
        transform,
        node_utils.get_bool(attrs, "visible", True),
        node_utils.get_bool(attrs, "cast-shadow", False),
        node_utils.get_bool(attrs, "receive-shadow", False),
      )
      ctx
    }
    Error(Nil) -> {
      let color_int =
        dict.get(attrs, "color")
        |> result.try(node_utils.parse_color)
        |> result.unwrap(0xffffff)
      let emissive_int =
        dict.get(attrs, "emissive")
        |> result.try(node_utils.parse_color)
        |> result.unwrap(0x000000)
      case dict.get(attrs, "geometry") |> result.try(parse_geometry) {
        Ok(geometry) -> {
          let material =
            create_material_from_attrs(attrs, color_int, emissive_int)
          savoiardi.update_material_wireframe(
            material,
            node_utils.get_bool(attrs, "wireframe", False),
          )
          savoiardi.update_material_side(
            material,
            parse_material_side(node_utils.get_str(attrs, "side", "front")),
          )
          let mesh = savoiardi.create_mesh(geometry, material)
          let reg =
            registry.register_and_add_object(
              ctx.registry,
              id,
              mesh,
              parent_id,
              registry.ObjectKind("tiramisu-mesh"),
            )
          registry.set_transform(reg, id, transform)
          registry.set_visible(
            reg,
            id,
            node_utils.get_bool(attrs, "visible", True),
          )
          registry.set_mesh_shadow(
            reg,
            id,
            node_utils.get_bool(attrs, "cast-shadow", False),
            node_utils.get_bool(attrs, "receive-shadow", False),
          )
          load_material_textures(
            material,
            dict.get(attrs, "color-map") |> option.from_result,
            dict.get(attrs, "normal-map") |> option.from_result,
            dict.get(attrs, "ambient-occlusion-map") |> option.from_result,
            dict.get(attrs, "roughness-map") |> option.from_result,
            dict.get(attrs, "metalness-map") |> option.from_result,
            dict.get(attrs, "displacement-map") |> option.from_result,
          )
          Context(..ctx, registry: reg)
        }
        Error(Nil) -> ctx
      }
    }
  }
}

fn update(
  ctx: Context,
  id: String,
  old_attrs: dict.Dict(String, String),
  attrs: dict.Dict(String, String),
  transform: Transform,
) -> Context {
  // Skip transform if physics engine owns this object's position
  case node_utils.get_bool(attrs, "physics-controlled", False) {
    False -> registry.set_transform(ctx.registry, id, transform)
    True -> Nil
  }
  registry.set_visible(
    ctx.registry,
    id,
    node_utils.get_bool(attrs, "visible", True),
  )
  registry.set_mesh_shadow(
    ctx.registry,
    id,
    node_utils.get_bool(attrs, "cast-shadow", False),
    node_utils.get_bool(attrs, "receive-shadow", False),
  )
  case dict.get(attrs, "src") {
    Ok(url) -> {
      // Only reload when the src URL itself changed
      case dict.get(old_attrs, "src") == Ok(url) {
        True -> promise.resolve(Nil)
        False -> node_utils.set_model(ctx.on_async, id, url)
      }
    }
    Error(Nil) -> {
      // Update geometry
      let _ =
        registry.get_object(ctx.registry, id)
        |> result.map(fn(object) {
          case dict.get(attrs, "geometry") |> result.try(parse_geometry) {
            Ok(new_geom) -> {
              savoiardi.get_object_geometry(object)
              |> savoiardi.dispose_geometry
              savoiardi.set_object_geometry(object, new_geom)
            }
            Error(Nil) -> Nil
          }
        })
      use _ <- promise.tap(promise.resolve(Nil))
      registry.get_object(ctx.registry, id)
      |> result.map(fn(object) {
        let color_int =
          dict.get(attrs, "color")
          |> result.try(node_utils.parse_color)
          |> result.unwrap(0xffffff)
        let emissive_int =
          dict.get(attrs, "emissive")
          |> result.try(node_utils.parse_color)
          |> result.unwrap(0x000000)
        let new_material =
          create_material_from_attrs(attrs, color_int, emissive_int)
        savoiardi.update_material_wireframe(
          new_material,
          node_utils.get_bool(attrs, "wireframe", False),
        )
        savoiardi.update_material_side(
          new_material,
          parse_material_side(node_utils.get_str(attrs, "side", "front")),
        )
        let old_material = savoiardi.get_object_material(object)
        savoiardi.dispose_material(old_material)
        savoiardi.set_object_material(object, new_material)
        load_material_textures(
          new_material,
          dict.get(attrs, "color-map") |> option.from_result,
          dict.get(attrs, "normal-map") |> option.from_result,
          dict.get(attrs, "ao-map") |> option.from_result,
          dict.get(attrs, "roughness-map") |> option.from_result,
          dict.get(attrs, "metalness-map") |> option.from_result,
          dict.get(attrs, "displacement-map") |> option.from_result,
        )
      })
    }
  }
  ctx
}

pub fn parse_geometry(geometry: String) -> Result(savoiardi.Geometry, Nil) {
  case string.split(geometry, ":") {
    [type_str, params_str] -> {
      let params =
        string.split(params_str, ",")
        |> list.map(string.trim)
        |> list.filter_map(float.parse)

      case type_str, params {
        "box", [w, h, d] -> Ok(savoiardi.create_box_geometry(w, h, d))
        "box", [s] -> Ok(savoiardi.create_box_geometry(s, s, s))
        "box", _ -> Ok(savoiardi.create_box_geometry(1.0, 1.0, 1.0))

        "sphere", [r, ws, hs] ->
          Ok(savoiardi.create_sphere_geometry(
            r,
            float.round(ws),
            float.round(hs),
          ))
        "sphere", [r] -> Ok(savoiardi.create_sphere_geometry(r, 32, 16))
        "sphere", _ -> Ok(savoiardi.create_sphere_geometry(1.0, 32, 16))

        "plane", [w, h] -> Ok(savoiardi.create_plane_geometry(w, h, 1, 1))
        "plane", _ -> Ok(savoiardi.create_plane_geometry(1.0, 1.0, 1, 1))

        "cylinder", [rt, rb, h, s] ->
          Ok(savoiardi.create_cylinder_geometry(rt, rb, h, float.round(s)))
        "cylinder", [r, h] ->
          Ok(savoiardi.create_cylinder_geometry(r, r, h, 32))
        "cylinder", _ ->
          Ok(savoiardi.create_cylinder_geometry(1.0, 1.0, 1.0, 32))

        "cone", [r, h, s] ->
          Ok(savoiardi.create_cone_geometry(r, h, float.round(s)))
        "cone", [r, h] -> Ok(savoiardi.create_cone_geometry(r, h, 32))
        "cone", _ -> Ok(savoiardi.create_cone_geometry(1.0, 1.0, 32))

        "torus", [r, t, rs, ts] ->
          Ok(savoiardi.create_torus_geometry(
            r,
            t,
            float.round(rs),
            float.round(ts),
          ))
        "torus", [r, t] -> Ok(savoiardi.create_torus_geometry(r, t, 16, 48))
        "torus", _ -> Ok(savoiardi.create_torus_geometry(1.0, 0.4, 16, 48))

        _, _ -> Error(Nil)
      }
    }
    [type_str] -> {
      case type_str {
        "box" -> Ok(savoiardi.create_box_geometry(1.0, 1.0, 1.0))
        "sphere" -> Ok(savoiardi.create_sphere_geometry(1.0, 32, 16))
        "plane" -> Ok(savoiardi.create_plane_geometry(1.0, 1.0, 1, 1))
        "cylinder" -> Ok(savoiardi.create_cylinder_geometry(1.0, 1.0, 1.0, 32))
        "cone" -> Ok(savoiardi.create_cone_geometry(1.0, 1.0, 32))
        "torus" -> Ok(savoiardi.create_torus_geometry(1.0, 0.4, 16, 48))
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn create_material_from_attrs(
  attrs: dict.Dict(String, String),
  color_int: Int,
  emissive_int: Int,
) -> savoiardi.Material {
  let opacity_val = node_utils.get_float(attrs, "opacity", 1.0)
  let is_transparent =
    node_utils.get_bool(attrs, "transparent", False) || opacity_val <. 1.0
  case node_utils.get_str(attrs, "material-type", "standard") {
    "basic" ->
      savoiardi.create_basic_material(
        color_int,
        is_transparent,
        opacity_val,
        option.None,
        savoiardi.FrontSide,
        node_utils.get_float(attrs, "alpha-test", 0.0),
        True,
      )
    "phong" ->
      savoiardi.create_phong_material(
        color_int,
        node_utils.get_float(attrs, "shininess", 30.0),
        option.None,
        option.None,
        option.None,
        is_transparent,
        opacity_val,
        node_utils.get_float(attrs, "alpha-test", 0.0),
      )
    "lambert" ->
      savoiardi.create_lambert_material(
        color_int,
        option.None,
        option.None,
        option.None,
        is_transparent,
        opacity_val,
        node_utils.get_float(attrs, "alpha-test", 0.0),
        savoiardi.FrontSide,
      )
    "toon" ->
      savoiardi.create_toon_material(
        color_int,
        option.None,
        option.None,
        option.None,
        is_transparent,
        opacity_val,
        node_utils.get_float(attrs, "alpha-test", 0.0),
      )
    _ ->
      savoiardi.create_standard_material(
        color_int,
        node_utils.get_float(attrs, "metalness", 0.5),
        node_utils.get_float(attrs, "roughness", 0.5),
        is_transparent,
        opacity_val,
        option.None,
        option.None,
        option.None,
        option.None,
        node_utils.get_float(attrs, "displacement-scale", 1.0),
        node_utils.get_float(attrs, "displacement-bias", 0.0),
        option.None,
        option.None,
        emissive_int,
        node_utils.get_float(attrs, "emissive-intensity", 1.0),
        node_utils.get_float(attrs, "alpha-test", 0.0),
      )
  }
}

fn parse_material_side(side_str: String) -> savoiardi.MaterialSide {
  case side_str {
    "back" -> savoiardi.BackSide
    "double" -> savoiardi.DoubleSide
    _ -> savoiardi.FrontSide
  }
}

fn load_material_textures(
  material material: savoiardi.Material,
  color_map color_map: option.Option(String),
  normal_map normal_map: option.Option(String),
  ambient_occlusion_map ambient_occlusion_map: option.Option(String),
  roughness_map roughness_map: option.Option(String),
  metalness_map metalness_map: option.Option(String),
  displacement_map displacement_map: option.Option(String),
) -> promise.Promise(Result(Nil, Nil)) {
  [
    load_texture_to_material(material, color_map, "map"),
    load_texture_to_material(material, normal_map, "normalMap"),
    load_texture_to_material(material, ambient_occlusion_map, "aoMap"),
    load_texture_to_material(material, roughness_map, "roughnessMap"),
    load_texture_to_material(material, metalness_map, "metalnessMap"),
    load_texture_to_material(material, displacement_map, "displacementMap"),
  ]
  |> promise.await_list()
  |> promise.map(fn(results) {
    case list.any(results, result.is_error) {
      True -> Error(Nil)
      False -> Ok(Nil)
    }
  })
}

fn load_texture_to_material(
  material: savoiardi.Material,
  url: option.Option(String),
  property_name: String,
) -> promise.Promise(Result(Nil, Nil)) {
  case url {
    option.None -> promise.resolve(Error(Nil))
    option.Some(u) -> {
      use result <- promise.map(savoiardi.load_texture(u))
      use texture <- result.map(result)
      savoiardi.set_material_texture(material, property_name, texture)
    }
  }
}
