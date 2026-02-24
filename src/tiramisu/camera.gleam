import gleam/float
import gleam/json
import gleam/result
import lustre/attribute.{type Attribute}
import savoiardi
import tiramisu/extension.{Context}
import tiramisu/internal/node_utils
import tiramisu/internal/registry

@internal
pub const tag = "tiramisu-camera"

pub type Camera {
  Perspective
  Orthographic
}

pub fn kind(kind: Camera) -> Attribute(msg) {
  attribute.attribute("camera-type", case kind {
    Perspective -> "perspective"
    Orthographic -> "orthographic"
  })
}

pub fn fov(degrees: Float) -> Attribute(msg) {
  attribute.attribute("fov", float.to_string(degrees))
}

pub fn near(distance: Float) -> Attribute(msg) {
  attribute.attribute("near", float.to_string(distance))
}

pub fn far(distance: Float) -> Attribute(msg) {
  attribute.attribute("far", float.to_string(distance))
}

pub fn active(is_active: Bool) -> Attribute(msg) {
  case is_active {
    True -> attribute.attribute("active", "")
    False -> attribute.property("active", json.bool(False))
  }
}

pub fn left(value: Float) -> Attribute(msg) {
  attribute.attribute("left", float.to_string(value))
}

pub fn right(value: Float) -> Attribute(msg) {
  attribute.attribute("right", float.to_string(value))
}

pub fn top(value: Float) -> Attribute(msg) {
  attribute.attribute("top", float.to_string(value))
}

pub fn bottom(value: Float) -> Attribute(msg) {
  attribute.attribute("bottom", float.to_string(value))
}

pub fn extension() {
  extension.NodeExtension(
    extension.Node(
      tag: "tiramisu-camera",
      observed_attributes: [
        "transform", "camera-type", "fov", "near", "far", "active", "left",
        "right", "top", "bottom",
      ],
      create: fn(ctx, id, parent_id, attrs, transform) {
        let near_val = node_utils.get_float(attrs, "near", 0.1)
        let far_val = node_utils.get_float(attrs, "far", 1000.0)
        let camera = case
          node_utils.get_str(attrs, "camera-type", "perspective")
        {
          "orthographic" ->
            savoiardi.create_orthographic_camera(
              node_utils.get_float(attrs, "left", -10.0),
              node_utils.get_float(attrs, "right", 10.0),
              node_utils.get_float(attrs, "top", 10.0),
              node_utils.get_float(attrs, "bottom", -10.0),
              near_val,
              far_val,
            )
          _ ->
            savoiardi.create_perspective_camera(
              node_utils.get_float(attrs, "fov", 75.0),
              registry.get_renderer_aspect_ratio(ctx.registry),
              near_val,
              far_val,
            )
        }
        let reg =
          registry.register_camera(
            ctx.registry,
            id,
            camera,
            parent_id,
            node_utils.get_bool(attrs, "active", False),
          )
        registry.set_transform(reg, id, transform)
        node_utils.sync_active_camera(reg, ctx.loop)
        Context(..ctx, registry: reg)
      },
      update: fn(ctx, id, _old_attrs, new_attrs, transform) {
        registry.set_transform(ctx.registry, id, transform)
        let _ =
          registry.get_camera(ctx.registry, id)
          |> result.map(fn(camera) {
            savoiardi.set_perspective_camera_params(
              camera,
              node_utils.get_float(new_attrs, "fov", 75.0),
              registry.get_renderer_aspect_ratio(ctx.registry),
              node_utils.get_float(new_attrs, "near", 0.1),
              node_utils.get_float(new_attrs, "far", 1000.0),
            )
            savoiardi.update_camera_projection_matrix(camera)
          })
        let reg =
          registry.set_camera_active(
            ctx.registry,
            id,
            node_utils.get_bool(new_attrs, "active", False),
          )
        node_utils.sync_active_camera(reg, ctx.loop)
        Context(..ctx, registry: reg)
      },
      remove: fn(ctx, id) {
        let reg = registry.remove_object(ctx.registry, id)
        node_utils.sync_active_camera(reg, ctx.loop)
        Context(..ctx, registry: reg)
      },
    ),
  )
}
