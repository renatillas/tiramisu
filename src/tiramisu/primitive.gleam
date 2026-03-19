//// Primitive mesh attributes.
////
//// Primitive nodes are the fastest way to put visible geometry in a scene
//// without relying on external assets.

import gleam/bool
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import lustre/effect
import savoiardi/geometry.{type Geometry}
import savoiardi/material
import tiramisu/dev/extension

import lustre/attribute.{type Attribute}

import savoiardi/object.{type Object3D}

import tiramisu/dev/runtime.{type Runtime}

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

fn create(
  runtime: Runtime,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> #(runtime.Runtime, effect.Effect(extension.Msg)) {
  case dict.get(attributes, "geometry") |> result.try(parse_geometry) {
    Ok(geometry) -> {
      let object =
        object.mesh(geometry, material.basic(material.basic_options()))
      apply_attributes(object, attributes)
      let next_runtime =
        runtime.add_object(runtime, id, object:, parent_id:, tag:)
      #(next_runtime, effect.none())
    }

    Error(Nil) -> #(runtime, effect.none())
  }
}

fn update(
  ctx: runtime.Runtime,
  _id: String,
  _parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> #(runtime.Runtime, effect.Effect(extension.Msg)) {
  case object {
    option.Some(object) -> {
      let _ = apply_geometry(object, attributes, changed_attributes)
      let _ = apply_visibility(object, attributes, changed_attributes)
      let _ = apply_shadows(object, attributes, changed_attributes)
      #(ctx, effect.none())
    }

    option.None -> #(ctx, effect.none())
  }
}

fn apply_attributes(
  object: Object3D,
  attributes: Dict(String, String),
) -> Object3D {
  object
  |> object.set_visible(!extension.get_bool(attributes, "hidden"))
  |> object.set_cast_shadow(extension.get_bool(attributes, "cast-shadow"))
  |> object.set_receive_shadow(extension.get_bool(attributes, "receive-shadow"))
}

fn apply_geometry(
  object: Object3D,
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Nil {
  use <- bool.guard(
    when: !extension.has_change(changed_attributes, "geometry"),
    return: Nil,
  )
  let _ =
    dict.get(attributes, "geometry")
    |> result.try(parse_geometry)
    |> result.map(object.set_geometry(object, _))
  Nil
}

fn apply_visibility(
  object: Object3D,
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Object3D {
  use <- bool.guard(
    when: !extension.has_change(changed_attributes, "hidden"),
    return: object,
  )
  object.set_visible(object, !extension.get_bool(attributes, "hidden"))
}

fn apply_shadows(
  object: Object3D,
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Object3D {
  use <- bool.guard(
    when: !extension.has_change(changed_attributes, "cast-shadow")
      && !extension.has_change(changed_attributes, "receive-shadow"),
    return: object,
  )
  object
  |> object.set_cast_shadow(extension.get_bool(attributes, "cast-shadow"))
  |> object.set_receive_shadow(extension.get_bool(attributes, "receive-shadow"))
}

fn parse_geometry(geometry: String) -> Result(Geometry, Nil) {
  case string.split(geometry, ":") {
    [type_str, params_str] -> {
      let params =
        string.split(params_str, ",")
        |> list.map(string.trim)
        |> list.filter_map(float.parse)

      case type_str, params {
        "box", [w, h, d] -> Ok(geometry.box(w, h, d))
        "box", [s] -> Ok(geometry.box(s, s, s))
        "box", _ -> Ok(geometry.box(1.0, 1.0, 1.0))

        "sphere", [r, ws, hs] ->
          Ok(geometry.sphere(r, float.round(ws), float.round(hs)))
        "sphere", [r] -> Ok(geometry.sphere(r, 32, 16))
        "sphere", _ -> Ok(geometry.sphere(1.0, 32, 16))

        "plane", [w, h] -> Ok(geometry.plane(w, h, 1, 1))
        "plane", _ -> Ok(geometry.plane(1.0, 1.0, 1, 1))

        "cylinder", [rt, rb, h, s] ->
          Ok(geometry.cylinder(rt, rb, h, float.round(s)))
        "cylinder", [r, h] -> Ok(geometry.cylinder(r, r, h, 32))
        "cylinder", _ -> Ok(geometry.cylinder(1.0, 1.0, 1.0, 32))

        "cone", [r, h, s] -> Ok(geometry.cone(r, h, float.round(s)))
        "cone", [r, h] -> Ok(geometry.cone(r, h, 32))
        "cone", _ -> Ok(geometry.cone(1.0, 1.0, 32))

        "torus", [r, t, rs, ts] ->
          Ok(geometry.torus(r, t, float.round(rs), float.round(ts)))
        "torus", [r, t] -> Ok(geometry.torus(r, t, 16, 48))
        "torus", _ -> Ok(geometry.torus(1.0, 0.4, 16, 48))

        _, _ -> Error(Nil)
      }
    }
    [type_str] -> {
      case type_str {
        "box" -> Ok(geometry.box(1.0, 1.0, 1.0))
        "sphere" -> Ok(geometry.sphere(1.0, 32, 16))
        "plane" -> Ok(geometry.plane(1.0, 1.0, 1, 1))
        "cylinder" -> Ok(geometry.cylinder(1.0, 1.0, 1.0, 32))
        "cone" -> Ok(geometry.cone(1.0, 1.0, 32))
        "torus" -> Ok(geometry.torus(1.0, 0.4, 16, 48))
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn remove(
  runtime: runtime.Runtime,
  id: String,
  parent_id: String,
  object: Object3D,
) -> #(runtime.Runtime, effect.Effect(extension.Msg)) {
  let next_runtime = runtime.remove_object(runtime, id, parent_id, object)
  #(next_runtime, effect.none())
}

/// Internal node extension for primitive elements.
pub fn ext() -> extension.Extension {
  let observed_attributes = ["geometry", "hidden"]
  extension.node_extension(
    tag:,
    observed_attributes:,
    create:,
    update:,
    remove:,
  )
}
