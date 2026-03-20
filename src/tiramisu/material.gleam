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
import lustre/effect
import lustre/event

import savoiardi/loader
import savoiardi/material.{type Material}
import savoiardi/object.{type Object3D}
import savoiardi/texture.{type Texture}

import tiramisu/dev/extension
import tiramisu/dev/runtime

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

/// Use a standard physically based material.extension
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
  attribute.attribute("emissive", int.to_base16(hex))
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
  attribute.attribute("color", int.to_base16(hex))
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

fn on_resolved(
  _runtime: runtime.Runtime,
  _tag: String,
  id: String,
  object: Object3D,
  attributes: Dict(String, String),
) -> effect.Effect(extension.Msg) {
  apply_material(id, object, attributes)
}

fn on_remove(
  _runtime: runtime.Runtime,
  _id: String,
  _parent_id: String,
  object: Object3D,
) -> effect.Effect(extension.Msg) {
  object
  |> object.get_material
  |> material.dispose
  effect.none()
}

fn on_update(
  _runtime: runtime.Runtime,
  _string: String,
  id: String,
  object: option.Option(Object3D),
  attrs: Dict(String, String),
  changed_attributes: Dict(String, extension.AttributeChange),
) -> effect.Effect(extension.Msg) {
  case object {
    option.None -> effect.none()
    option.Some(obj) ->
      // Only act when at least one material attribute changed.
      case extension.has_any_change(changed_attributes, observed_attributes()) {
        True -> replace_material(id, obj, attrs)
        False -> effect.none()
      }
  }
}

fn on_create(
  _runtime: runtime.Runtime,
  _string: String,
  id: String,
  object: option.Option(Object3D),
  attributes: Dict(String, String),
) -> effect.Effect(extension.Msg) {
  case object {
    // On objects that dont register themselves upon creation, we do nothing (yet)
    option.None -> effect.none()
    option.Some(object) -> apply_material(id, object, attributes)
  }
}

fn apply_material(
  id: String,
  object: Object3D,
  attributes: Dict(String, String),
) -> effect.Effect(extension.Msg) {
  let material = parse_material(attributes)
  object.set_material(object, material)
  load_material_textures(id, material, attributes)
}

fn replace_material(
  id: String,
  object: Object3D,
  attributes: Dict(String, String),
) -> effect.Effect(extension.Msg) {
  object
  |> object.get_material
  |> material.dispose

  apply_material(id, object, attributes)
}

fn load_material_textures(
  id: String,
  material: Material,
  attributes: Dict(String, String),
) -> effect.Effect(extension.Msg) {
  extension.request(
    extension.NodeOwner(id),
    extension.request_key("material:textures"),
    promise.map(
      promise.await_list(parse_apply_textures_async(id, material, attributes)),
      list.flatten,
    ),
  )
}

// INTERNAL MATERIAL PARSING ---------------------------------------------------

/// Parse a material from node attributes.
///
/// This is public so custom extensions can reuse Tiramisu's material parsing
/// logic when creating their own nodes.
@internal
pub fn parse_material(attrs: Dict(String, String)) -> Material {
  let color = extension.get(attrs, "color", 0xffffff, int.base_parse(_, 16))
  let emissive =
    extension.get(attrs, "emissive", 0x000000, int.base_parse(_, 16))
  let opacity = extension.get(attrs, "opacity", 1.0, extension.parse_number)
  let transparent = extension.get_bool(attrs, "transparent") || opacity <. 1.0
  let material_type = dict.get(attrs, "type") |> result.unwrap("standard")

  let mat = case material_type {
    "basic" -> {
      let alpha_test =
        extension.get(attrs, "alpha-test", 0.0, extension.parse_number)
      material.basic(material.BasicOptions(
        color:,
        transparent:,
        opacity:,
        color_map: option.None,
        side: material.FrontSide,
        alpha_test:,
        depth_write: True,
      ))
    }
    "phong" -> {
      let shininess =
        extension.get(attrs, "shininess", 30.0, extension.parse_number)
      let alpha_test =
        extension.get(attrs, "alpha-test", 0.0, extension.parse_number)
      material.phong(material.PhongOptions(
        color:,
        shininess:,
        color_map: option.None,
        normal_map: option.None,
        ambient_occlusion_map: option.None,
        transparent:,
        opacity:,
        alpha_test:,
      ))
    }
    "lambert" -> {
      let alpha_test =
        extension.get(attrs, "alpha-test", 0.0, extension.parse_number)
      material.lambert(material.LambertOptions(
        color:,
        color_map: option.None,
        normal_map: option.None,
        ambient_occlusion_map: option.None,
        transparent:,
        opacity:,
        alpha_test:,
      ))
    }
    "toon" -> {
      let alpha_test =
        extension.get(attrs, "alpha-test", 0.0, extension.parse_number)
      material.toon(material.ToonOptions(
        color:,
        color_map: option.None,
        normal_map: option.None,
        ambient_occlusion_map: option.None,
        transparent:,
        opacity:,
        alpha_test:,
      ))
    }
    _ -> {
      let metalness =
        extension.get(attrs, "metalness", 0.5, extension.parse_number)
      let roughness =
        extension.get(attrs, "roughness", 0.5, extension.parse_number)
      let displacement_scale =
        extension.get(attrs, "displacement-scale", 1.0, extension.parse_number)
      let displacement_bias =
        extension.get(attrs, "displacement-bias", 0.0, extension.parse_number)
      let emissive_intensity =
        extension.get(attrs, "emissive-intensity", 1.0, extension.parse_number)
      let alpha_test =
        extension.get(attrs, "alpha-test", 0.0, extension.parse_number)

      material.standard(
        material.StandardOptions(
          ..material.standard_options(),
          color:,
          metalness:,
          roughness:,
          transparent:,
          opacity:,
          displacement_scale:,
          displacement_bias:,
          emissive:,
          emissive_intensity:,
          alpha_test:,
        ),
      )
    }
  }
  material.set_wireframe(mat, extension.get_bool(attrs, "wireframe"))
  material.set_side(
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
  _mat: Material,
  attrs: Dict(String, String),
) -> List(promise.Promise(List(extension.RuntimeAction))) {
  use #(texture_name, texture_src) <- list.map([
    #(material.set_env_map, dict.get(attrs, "color-map")),
    #(material.set_normal_map, dict.get(attrs, "normal-map")),
    #(
      material.set_ambient_occlusion_map,
      dict.get(attrs, "ambient-occlusion-map"),
    ),
    #(material.set_roughness_map, dict.get(attrs, "roughness-map")),
    #(material.set_metalness_map, dict.get(attrs, "metalness-map")),
    #(material.set_displacement_map, dict.get(attrs, "displacement-map")),
  ])
  case texture_src {
    Error(Nil) -> promise.resolve([])
    Ok(url) -> {
      let loader = loader.texture()
      use result <- promise.map(texture.load_async(loader, url))
      case result {
        Ok(texture) -> [apply_texture(id, texture_name, texture)]
        Error(_) -> [emit_texture_error(id, url)]
      }
    }
  }
}

fn apply_texture(
  id: String,
  apply_function,
  texture: Texture,
) -> extension.RuntimeAction {
  extension.action(fn(rt) {
    case runtime.object(rt, id) {
      Ok(object) -> {
        let material = object.get_material(object)
        let _ = apply_function(material, texture)
        #(rt, effect.none())
      }
      Error(Nil) -> #(rt, effect.none())
    }
  })
}

fn emit_texture_error(id: String, url: String) -> extension.RuntimeAction {
  extension.action(fn(rt) {
    #(
      rt,
      event.emit(
        "tiramisu:load-texture-error",
        json.object([
          #("id", json.string(id)),
          #("src", json.string(url)),
        ]),
      ),
    )
  })
}

fn parse_material_side(side_str: String) -> material.MaterialSide {
  case side_str {
    "back" -> material.BackSide
    "double" -> material.DoubleSide
    _ -> material.FrontSide
  }
}

/// Build the internal extension used to apply material attributes.
///
/// Most applications should not call this directly; use
/// `tiramisu.builtin_extensions()` instead.
pub fn ext() -> extension.Extension {
  extension.scoped_attribute_extension(
    applies_to: extension.OnlyTags(["tiramisu-primitive", "tiramisu-mesh"]),
    observed_attributes: observed_attributes(),
    on_create:,
    on_update:,
    on_remove:,
    on_resolved:,
  )
}
