import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/set
import gleam/string
import tiramisu/internal/node

import lustre/attribute.{type Attribute}

import savoiardi.{type Object3D}

import tiramisu/dev/extension.{type Context}
import tiramisu/dev/registry

import vec/vec2
import vec/vec3

@internal
pub const tag: String = "tiramisu-primitive"

pub fn extension() {
  let observed_attributes = set.from_list(["geometry", "hidden"])
  extension.Node(tag:, observed_attributes:, create:, update:, remove:)
  |> extension.NodeExtension
}

pub fn box(shape: vec3.Vec3(Float)) -> Attribute(msg) {
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

pub fn plane(size: vec2.Vec2(Float)) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "plane:" <> float.to_string(size.x) <> "," <> float.to_string(size.y),
  )
}

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

fn create(
  context: Context,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> Context {
  let geometry = dict.get(attributes, "geometry") |> result.try(parse_geometry)
  let result = case geometry {
    Ok(geometry) -> {
      let object = savoiardi.create_mesh(geometry)
      savoiardi.set_object_visible(object, !node.get_bool(attributes, "hidden"))
      let registry =
        registry.add(context.registry, id, object:, parent_id:, tag:)
      Ok(extension.Context(..context, registry:))
    }
    Error(Nil) -> Error(Nil)
  }
  case result {
    Ok(context) -> context
    Error(Nil) -> context
  }
}

fn update(
  ctx: Context,
  _id: String,
  _parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: set.Set(String),
) -> Context {
  // If loading models, registry.get_object fails a few frames until load completes
  let _ = {
    use object <- result.map(object |> option.to_result(Nil))
    case set.contains("geometry", in: changed_attributes) {
      True -> {
        let _ =
          dict.get(attributes, "geometry")
          |> result.try(parse_geometry)
          |> result.map(savoiardi.set_object_geometry(object, _))
        Nil
      }
      False -> Nil
    }
    case set.contains("hidden", in: changed_attributes) {
      True ->
        savoiardi.set_object_visible(
          object,
          !node.get_bool(attributes, "hidden"),
        )
      False -> Nil
    }
  }
  ctx
}

fn parse_geometry(geometry: String) -> Result(savoiardi.Geometry, Nil) {
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

fn remove(
  context: Context,
  id: String,
  parent_id: String,
  object: Object3D,
) -> Context {
  let registry = registry.remove(context.registry, id, parent_id, object)
  extension.Context(..context, registry:)
}
