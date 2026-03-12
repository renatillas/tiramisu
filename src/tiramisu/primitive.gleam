//// Primitive mesh attributes.
////
//// Primitive nodes are the fastest way to put visible geometry in a scene
//// without relying on external assets.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import tiramisu/dev/extension
import tiramisu/internal/node

import lustre/attribute.{type Attribute}

import savoiardi.{type Object3D}

import tiramisu/dev/runtime

import vec/vec2
import vec/vec3

/// Hide or show the primitive object.
///
/// This controls Three.js visibility for the node rather than DOM layout.
pub const hidden = attribute.hidden

/// Create a box geometry.
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

/// Create a sphere geometry.
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

/// Create a plane geometry.
pub fn plane(size: vec2.Vec2(Float)) -> Attribute(msg) {
  attribute.attribute(
    "geometry",
    "plane:" <> float.to_string(size.x) <> "," <> float.to_string(size.y),
  )
}

/// Create a cylinder geometry.
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

/// Create a cone geometry.
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

/// Create a torus geometry.
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

/// Enable or disable shadow casting for the primitive.
pub fn cast_shadow(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("cast-shadow", "")
    False -> attribute.property("cast-shadow", json.bool(False))
  }
}

/// Enable or disable shadow receiving for the primitive.
pub fn receive_shadow(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("receive-shadow", "")
    False -> attribute.property("receive-shadow", json.bool(False))
  }
}

/// The custom element tag used for primitive nodes.
@internal
pub const tag: String = "tiramisu-primitive"

/// Internal node extension for primitive elements.
pub fn extension() -> extension.Extension {
  let observed_attributes = ["geometry", "hidden"]
  extension.Node(tag:, observed_attributes:, create:, update:, remove:)
  |> extension.NodeExtension
}

fn create(
  context: extension.Context,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> extension.Context {
  case dict.get(attributes, "geometry") |> result.try(parse_geometry) {
    Ok(geometry) -> {
      let object = savoiardi.create_mesh(geometry)
      apply_attributes(object, attributes)
      let next_runtime =
        runtime.add_object(context.runtime, id, object:, parent_id:, tag:)
      extension.Context(..context, runtime: next_runtime)
    }

    Error(Nil) -> context
  }
}

fn update(
  ctx: extension.Context,
  _id: String,
  _parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> extension.Context {
  case object {
    option.Some(object) -> {
      let _ = apply_geometry(object, attributes, changed_attributes)
      let _ = apply_visibility(object, attributes, changed_attributes)
      let _ = apply_shadows(object, attributes, changed_attributes)
      ctx
    }

    option.None -> ctx
  }
}

fn apply_attributes(object: Object3D, attributes: Dict(String, String)) -> Nil {
  savoiardi.set_object_visible(object, !node.get_bool(attributes, "hidden"))
  savoiardi.enable_shadows(
    object,
    cast_shadow: node.get_bool(attributes, "cast-shadow"),
    receive_shadow: node.get_bool(attributes, "receive-shadow"),
  )
}

fn apply_geometry(
  object: Object3D,
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Nil {
  case extension.has_change(changed_attributes, "geometry") {
    True -> {
      let _ =
        dict.get(attributes, "geometry")
        |> result.try(parse_geometry)
        |> result.map(savoiardi.set_object_geometry(object, _))
      Nil
    }

    False -> Nil
  }
}

fn apply_visibility(
  object: Object3D,
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Nil {
  case extension.has_change(changed_attributes, "hidden") {
    True ->
      savoiardi.set_object_visible(object, !node.get_bool(attributes, "hidden"))
    False -> Nil
  }
}

fn apply_shadows(
  object: Object3D,
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Nil {
  case
    extension.has_change(changed_attributes, "cast-shadow")
    || extension.has_change(changed_attributes, "receive-shadow")
  {
    True ->
      savoiardi.enable_shadows(
        object,
        cast_shadow: node.get_bool(attributes, "cast-shadow"),
        receive_shadow: node.get_bool(attributes, "receive-shadow"),
      )

    False -> Nil
  }
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
  context: extension.Context,
  id: String,
  parent_id: String,
  object: Object3D,
) -> extension.Context {
  let next_runtime =
    runtime.remove_object(context.runtime, id, parent_id, object)
  extension.Context(..context, runtime: next_runtime)
}
