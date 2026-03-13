//// Material attributes shared by Tiramisu mesh and primitive nodes.
////
//// Material attributes are implemented as a cross-cutting attribute extension,
//// so the same API works for both `tiramisu.primitive` and `tiramisu.mesh`.
////
//// ```gleam
//// tiramisu.primitive(
////   "cube",
////   [
////     primitive.box(vec3.Vec3(1.0, 1.0, 1.0)),
////     material.color(0xff8844),
////     material.metalness(0.7),
////     material.roughness(0.25),
////   ],
////   [],
//// )
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/json
import gleam/list
import gleam/option
import gleam/result

import lustre/attribute.{type Attribute}

import savoiardi
import tiramisu/dev/extension
import tiramisu/internal/element
import tiramisu/internal/node

// TYPES -----------------------------------------------------------------------

/// Which sides of geometry faces to render.
pub type Side {
  /// Render front faces only (default).
  Front
  /// Render back faces only.
  Back
  /// Render both front and back faces.
  Double
}

/// Use a standard physically based material.
pub fn standard() -> Attribute(msg) {
  attribute.attribute("type", "standard")
}

/// Use an unlit basic material.
pub fn basic() -> Attribute(msg) {
  attribute.attribute("type", "basic")
}

/// Use a Phong material.
pub fn phong() -> Attribute(msg) {
  attribute.attribute("type", "phong")
}

/// Use a Lambert material.
pub fn lambert() -> Attribute(msg) {
  attribute.attribute("type", "lambert")
}

/// Use a toon material.
pub fn toon() -> Attribute(msg) {
  attribute.attribute("type", "toon")
}

/// Set Phong shininess.
pub fn shininess(value: Float) -> Attribute(msg) {
  attribute.attribute("shininess", float.to_string(value))
}

/// Set alpha test threshold.
pub fn alpha_test(value: Float) -> Attribute(msg) {
  attribute.attribute("alpha-test", float.to_string(value))
}

/// Enable or disable material transparency.
pub fn transparent(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("transparent", "")
    False -> attribute.property("transparent", json.bool(False))
  }
}

/// Set emissive color.
pub fn emissive(hex: Int) -> Attribute(msg) {
  attribute.attribute("emissive", "#" <> int.to_base16(hex))
}

/// Set emissive intensity.
pub fn emissive_intensity(intensity: Float) -> Attribute(msg) {
  attribute.attribute("emissive-intensity", float.to_string(intensity))
}

/// Select which side of the geometry should render.
pub fn side(s: Side) -> Attribute(msg) {
  attribute.attribute("side", case s {
    Front -> "front"
    Back -> "back"
    Double -> "double"
  })
}

/// Set the base color.
pub fn color(hex hex: Int) -> Attribute(msg) {
  attribute.attribute("color", "#" <> int.to_base16(hex))
}

/// Set the base color texture.
pub fn color_map(url: String) -> Attribute(msg) {
  attribute.attribute("color-map", url)
}

/// Set the normal map texture.
pub fn normal_map(url: String) -> Attribute(msg) {
  attribute.attribute("normal-map", url)
}

/// Set the ambient occlusion map texture.
pub fn ambient_occlusion_map(url: String) -> Attribute(msg) {
  attribute.attribute("ambient-occlusion-map", url)
}

/// Set the roughness map texture.
pub fn roughness_map(url: String) -> Attribute(msg) {
  attribute.attribute("roughness-map", url)
}

/// Set the metalness map texture.
pub fn metalness_map(url: String) -> Attribute(msg) {
  attribute.attribute("metalness-map", url)
}

/// Set the displacement map texture.
pub fn displacement_map(url: String) -> Attribute(msg) {
  attribute.attribute("displacement-map", url)
}

/// Set displacement scale.
pub fn displacement_scale(scale: Float) -> Attribute(msg) {
  attribute.attribute("displacement-scale", float.to_string(scale))
}

/// Set displacement bias.
pub fn displacement_bias(bias: Float) -> Attribute(msg) {
  attribute.attribute("displacement-bias", float.to_string(bias))
}

/// Set metalness amount.
pub fn metalness(m: Float) -> Attribute(msg) {
  attribute.attribute("metalness", float.to_string(m))
}

/// Set roughness amount.
pub fn roughness(r: Float) -> Attribute(msg) {
  attribute.attribute("roughness", float.to_string(r))
}

/// Set opacity.
pub fn opacity(o: Float) -> Attribute(msg) {
  attribute.attribute("opacity", float.to_string(o))
}

/// Enable or disable wireframe rendering.
pub fn wireframe(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("wireframe", "")
    False -> attribute.property("wireframe", json.bool(False))
  }
}

fn observed_attributes() {
  [
    "type", "color", "metalness", "roughness", "opacity", "wireframe",
    "emissive", "emissive-intensity", "side", "color-map", "normal-map",
    "ambient-occlusion-map", "roughness-map", "metalness-map",
    "displacement-map", "displacement-scale", "displacement-bias", "shininess",
    "alpha-test", "transparent",
  ]
}

/// Attribute extension that handles material updates for all tiramisu mesh nodes.
///
/// Centralises material parsing and disposal logic that would otherwise be
/// duplicated across primitive and mesh node handlers.
pub fn extension() -> extension.Extension {
  extension.AttributeExtension(extension.Attribute(
    observed_attributes: observed_attributes(),
    on_create: fn(context, _tag, id, object, attributes) {
      case object {
        // On objects that dont register themselves upon creation, we do nothing (yet)
        option.None -> promise.resolve(Nil)
        option.Some(object) ->
          apply_material(context.spawn, id, object, attributes)
      }
    },
    on_update: fn(context, _tag, id, object, attrs, changed_attributes) {
      case object {
        option.None -> promise.resolve(Nil)
        option.Some(obj) ->
          // Only act when at least one material attribute changed.
          case
            extension.has_any_change(changed_attributes, observed_attributes())
          {
            True -> replace_material(context.spawn, id, obj, attrs)
            False -> promise.resolve(Nil)
          }
      }
    },
    on_remove: fn(_context, _id, _parent_id, object) {
      object
      |> savoiardi.get_object_material
      |> savoiardi.dispose_material
      |> promise.resolve
    },
    // Once the object has been resolved we can set the material
    on_object_resolved: fn(_runtime, on_async, _tag, id, object, attributes) {
      apply_material(on_async, id, object, attributes)
    },
  ))
}

fn apply_material(
  on_async,
  id: String,
  object: savoiardi.Object3D,
  attributes: Dict(String, String),
) -> promise.Promise(Nil) {
  let material = parse_material(attributes)
  savoiardi.set_object_material(object, material)
  load_material_textures(on_async, id, material, attributes)
}

fn replace_material(
  on_async,
  id: String,
  object: savoiardi.Object3D,
  attributes: Dict(String, String),
) -> promise.Promise(Nil) {
  object
  |> savoiardi.get_object_material
  |> savoiardi.dispose_material

  apply_material(on_async, id, object, attributes)
}

fn load_material_textures(
  spawn,
  id: String,
  material: savoiardi.Material,
  attributes: Dict(String, String),
) -> promise.Promise(Nil) {
  spawn(
    extension.NodeScope(id),
    extension.async_key("material:textures"),
    promise.map(
      promise.await_list(parse_apply_textures_async(id, material, attributes)),
      fn(_) { extension.NoOp },
    ),
  )
}

// INTERNAL MATERIAL PARSING ---------------------------------------------------

/// Parse a material from node attributes.
///
/// This is public so custom extensions can reuse Tiramisu's material parsing
/// logic when creating their own nodes.
@internal
pub fn parse_material(attrs: Dict(String, String)) -> savoiardi.Material {
  let color = node.get(attrs, "color", 0xffffff, node.parse_color)
  let emissive = node.get(attrs, "emissive", 0x000000, node.parse_color)
  let opacity = node.get(attrs, "opacity", 1.0, node.parse_number)
  let transparent = node.get_bool(attrs, "transparent") || opacity <. 1.0
  let material_type = dict.get(attrs, "type") |> result.unwrap("standard")

  let mat = case material_type {
    "basic" -> {
      let alpha_test = node.get(attrs, "alpha-test", 0.0, node.parse_number)
      savoiardi.create_basic_material(
        color:,
        transparent:,
        opacity:,
        color_map: option.None,
        side: savoiardi.FrontSide,
        alpha_test:,
        depth_write: True,
      )
    }
    "phong" -> {
      let shininess = node.get(attrs, "shininess", 30.0, node.parse_number)
      let alpha_test = node.get(attrs, "alpha-test", 0.0, node.parse_number)
      savoiardi.create_phong_material(
        color:,
        shininess:,
        color_map: option.None,
        normal_map: option.None,
        ambient_occlusion_map: option.None,
        transparent:,
        opacity:,
        alpha_test:,
      )
    }
    "lambert" -> {
      let alpha_test = node.get(attrs, "alpha-test", 0.0, node.parse_number)
      savoiardi.create_lambert_material(
        color:,
        color_map: option.None,
        normal_map: option.None,
        ambient_occlusion_map: option.None,
        transparent:,
        opacity:,
        alpha_test:,
      )
    }
    "toon" -> {
      let alpha_test = node.get(attrs, "alpha-test", 0.0, node.parse_number)
      savoiardi.create_toon_material(
        color:,
        color_map: option.None,
        normal_map: option.None,
        ambient_occlusion_map: option.None,
        transparent:,
        opacity:,
        alpha_test:,
      )
    }
    _ -> {
      let metalness = node.get(attrs, "metalness", 0.5, node.parse_number)
      let roughness = node.get(attrs, "roughness", 0.5, node.parse_number)
      let displacement_scale =
        node.get(attrs, "displacement-scale", 1.0, node.parse_number)
      let displacement_bias =
        node.get(attrs, "displacement-bias", 0.0, node.parse_number)
      let emissive_intensity =
        node.get(attrs, "emissive-intensity", 1.0, node.parse_number)
      let alpha_test = node.get(attrs, "alpha-test", 0.0, node.parse_number)

      savoiardi.create_standard_material(
        color:,
        metalness:,
        roughness:,
        transparent:,
        opacity:,
        color_map: option.None,
        normal_map: option.None,
        ambient_occlusion_map: option.None,
        displacement_map: option.None,
        displacement_scale:,
        displacement_bias:,
        roughness_map: option.None,
        metalness_map: option.None,
        emissive:,
        emissive_intensity:,
        alpha_test:,
      )
    }
  }
  savoiardi.update_material_wireframe(mat, node.get_bool(attrs, "wireframe"))
  savoiardi.update_material_side(
    mat,
    parse_material_side(dict.get(attrs, "side") |> result.unwrap("front")),
  )
  mat
}

/// Parse texture attributes and return the async texture-loading work needed to
/// apply them to a material.
@internal
pub fn parse_apply_textures_async(
  id: String,
  mat: savoiardi.Material,
  attrs: Dict(String, String),
) -> List(promise.Promise(Result(Nil, Nil))) {
  use #(texture_name, texture_src) <- list.map([
    #("map", dict.get(attrs, "color-map")),
    #("normalMap", dict.get(attrs, "normal-map")),
    #("aoMap", dict.get(attrs, "ambient-occlusion-map")),
    #("roughnessMap", dict.get(attrs, "roughness-map")),
    #("metalnessMap", dict.get(attrs, "metalness-map")),
    #("displacementMap", dict.get(attrs, "displacement-map")),
  ])
  case texture_src {
    Error(Nil) -> {
      promise.resolve(Ok(Nil))
    }
    Ok(url) -> {
      use result <- promise.map(savoiardi.load_texture(url))
      case result {
        Ok(texture) ->
          Ok(savoiardi.set_material_texture(mat, texture_name, texture))
        Error(_) -> {
          element.dispatch_event(
            id,
            "tiramisu:load-texture-error",
            json.object([
              #("id", json.string(id)),
              #("src", json.string(url)),
            ]),
          )
          Error(Nil)
        }
      }
    }
  }
}

fn parse_material_side(side_str: String) -> savoiardi.MaterialSide {
  case side_str {
    "back" -> savoiardi.BackSide
    "double" -> savoiardi.DoubleSide
    _ -> savoiardi.FrontSide
  }
}
