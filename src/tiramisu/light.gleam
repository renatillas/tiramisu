import gleam/float
import gleam/int
import gleam/json
import gleam/option
import gleam/result
import savoiardi
import tiramisu/internal/registry

import lustre/attribute.{type Attribute}

import tiramisu/extension
import tiramisu/internal/node_utils

@internal
pub const tag = "tiramisu-light"

pub type Light {
  Ambient
  Directional
  Point
  Spot
}

// LIGHT ATTRIBUTES ------------------------------------------------------------

/// Set the light type ("ambient", "directional", "point", or "spot").
///
pub fn kind(kind: Light) -> Attribute(msg) {
  attribute.attribute("light-type", case kind {
    Ambient -> "ambient"
    Directional -> "directional"
    Point -> "point"
    Spot -> "spot"
  })
}

/// Set the intensity of the light.
///
pub fn intensity(value: Float) -> Attribute(msg) {
  attribute.attribute("intensity", float.to_string(value))
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

/// Enable shadow casting on the element.
///
/// For meshes and instanced meshes: the element will cast shadows.
/// For lights: the light will produce shadows.
/// Requires a light with shadow casting enabled.
///
pub fn cast_shadow(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("cast-shadow", "")
    False -> attribute.property("cast-shadow", json.bool(False))
  }
}

pub fn extension() -> extension.Extension {
  extension.NodeExtension(
    extension.Node(
      tag: "tiramisu-light",
      observed_attributes: [
        "transform",
        "light-type",
        "color",
        "intensity",
        "cast-shadow",
      ],
      create: fn(ctx, id, parent_id, attrs, transform) {
        case
          node_utils.parse_color(node_utils.get_str(attrs, "color", "#ffffff"))
        {
          Ok(color_int) -> {
            let intensity_val = node_utils.get_float(attrs, "intensity", 1.0)
            let casts_shadow = node_utils.get_bool(attrs, "cast-shadow", False)
            let light = case node_utils.get_str(attrs, "light-type", "point") {
              "directional" -> {
                let l =
                  savoiardi.create_directional_light(color_int, intensity_val)
                case casts_shadow {
                  True -> {
                    savoiardi.set_light_cast_shadow(l, True)
                    savoiardi.configure_shadow(
                      l,
                      savoiardi.ShadowConfig(
                        resolution: 1024,
                        bias: -0.0001,
                        normal_bias: 0.0,
                      ),
                    )
                    savoiardi.configure_directional_shadow_camera(
                      l,
                      savoiardi.DirectionalShadowConfig(
                        camera_left: -10.0,
                        camera_right: 10.0,
                        camera_top: 10.0,
                        camera_bottom: -10.0,
                        camera_near: 0.5,
                        camera_far: 500.0,
                      ),
                    )
                  }
                  False -> Nil
                }
                savoiardi.light_to_object3d(l)
              }
              "point" -> {
                let l =
                  savoiardi.create_point_light(color_int, intensity_val, 0.0)
                case casts_shadow {
                  True -> {
                    savoiardi.set_light_cast_shadow(l, True)
                    savoiardi.configure_shadow(
                      l,
                      savoiardi.ShadowConfig(
                        resolution: 1024,
                        bias: -0.0001,
                        normal_bias: 0.0,
                      ),
                    )
                  }
                  False -> Nil
                }
                savoiardi.light_to_object3d(l)
              }
              _ ->
                savoiardi.create_ambient_light(color_int, intensity_val)
                |> savoiardi.light_to_object3d
            }
            let reg =
              registry.register_and_add_object(
                ctx.registry,
                id,
                light,
                parent_id,
                registry.ObjectKind("tiramisu-light"),
              )
            registry.set_transform(reg, id, transform)
            extension.Context(..ctx, registry: reg)
          }
          Error(Nil) -> ctx
        }
      },
      update: fn(ctx, id, _old_attrs, new_attrs, transform) {
        registry.set_transform(ctx.registry, id, transform)
        let _ = {
          use light <- result.try(
            registry.get_light(ctx.registry, id) |> option.to_result(Nil),
          )
          use color_int <- result.try(
            node_utils.parse_color(node_utils.get_str(
              new_attrs,
              "color",
              "#ffffff",
            )),
          )
          savoiardi.update_light_color(light, color_int)
          savoiardi.update_light_intensity(
            light,
            node_utils.get_float(new_attrs, "intensity", 1.0),
          )
          Ok(Nil)
        }
        ctx
      },
      remove: fn(ctx, id) { node_utils.default_remove(ctx, id) },
    ),
  )
}
