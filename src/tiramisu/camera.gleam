import gleam/dict.{type Dict}
import gleam/float
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/set.{type Set}
import lustre/attribute.{type Attribute}
import savoiardi.{type Object3D}

import tiramisu/dev/extension.{type Context, Context}
import tiramisu/dev/loop
import tiramisu/dev/registry
import tiramisu/internal/node
import vec/vec2

pub const tag = "tiramisu-camera"

pub fn extension() {
  let observed_attributes = [
    "active",
    "type",
    "fov",
    "near",
    "far",
    "left",
    "right",
    "top",
    "bottom",
  ]
  extension.Node(tag:, observed_attributes:, create:, update:, remove:)
  |> extension.NodeExtension
}

pub type Camera {
  Perspective
  Orthographic
}

pub fn kind(kind: Camera) -> Attribute(msg) {
  attribute.attribute("type", case kind {
    Perspective -> "perspective"
    Orthographic -> "orthographic"
  })
}

pub fn active(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("active", "")
    False -> attribute.property("active", json.bool(False))
  }
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

fn remove(
  ctx: Context,
  id: String,
  parent_id: String,
  object: Object3D,
) -> Context {
  let reg = registry.remove(ctx.registry, id, parent_id, object)
  Context(..ctx, registry: reg)
}

fn create(
  ctx: Context,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> Context {
  let near = node.get(attributes, "near", 0.1, node.parse_number)
  let far = node.get(attributes, "far", 1000.0, node.parse_number)
  let camera = case dict.get(attributes, "type") {
    Ok("orthographic") -> {
      let left = node.get(attributes, "left", -10.0, node.parse_number)
      let right = node.get(attributes, "right", 10.0, node.parse_number)
      let top = node.get(attributes, "top", 10.0, node.parse_number)
      let bottom = node.get(attributes, "bottom", -10.0, node.parse_number)
      savoiardi.create_orthographic_camera(
        left:,
        right:,
        top:,
        bottom:,
        near:,
        far:,
      )
    }
    _ -> {
      let fov = node.get(attributes, "fov", 75.0, node.parse_number)
      let aspect = get_canvas_ratio(ctx.registry)
      savoiardi.create_perspective_camera(fov:, aspect:, near:, far:)
    }
  }
  let object = savoiardi.camera_to_object3d(camera)
  let active = node.get_bool(attributes, "active")
  case active {
    True -> loop.set_active_camera(ctx.loop, camera)
    False -> Nil
  }
  let registry = registry.add(ctx.registry, id, object:, parent_id:, tag:)
  Context(..ctx, registry: registry)
}

fn update(
  context: Context,
  id: String,
  parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: Set(String),
) -> Context {
  let has_changed = set.contains("type", in: changed_attributes)
  let new_camera_type =
    dict.get(attributes, "type")
    |> result.unwrap("perspective")
  let result = case has_changed, new_camera_type {
    False, "perspective" ->
      update_perspective(context, object, attributes, changed_attributes)
    False, "orthographic" ->
      update_orthographic(context, object, attributes, changed_attributes)
    True, "perspective" | True, "orthographic" ->
      case object {
        option.Some(object) ->
          remove(context, id, parent_id, object)
          |> create(id, parent_id, attributes)
          |> Ok
        option.None -> Error(Nil)
      }
    _, _ -> Error(Nil)
  }

  case result {
    Error(Nil) -> context
    Ok(context) -> context
  }
}

fn update_orthographic(
  context: Context,
  object: Option(Object3D),
  new_attributes: Dict(String, String),
  changed_attributes: Set(String),
) -> Result(Context, Nil) {
  use camera <- result.map(object |> option.to_result(Nil))
  let camera = savoiardi.object3d_to_camera(camera)

  conditionally_set_orthographic_camera_params(
    new_attributes,
    changed_attributes,
    camera,
  )

  conditionally_set_active(context, camera, new_attributes, changed_attributes)

  context
}

fn conditionally_set_orthographic_camera_params(
  attributes: Dict(String, String),
  changed_attributes: Set(String),
  camera: savoiardi.Camera,
) {
  case
    list.any(
      [
        "near",
        "far",
        "left",
        "right",
        "top",
        "bottom",
      ],
      set.contains(changed_attributes, _),
    )
  {
    True -> {
      let near = get_near(attributes)
      let far = get_far(attributes)
      let left = get_left(attributes)
      let right = get_right(attributes)
      let top = get_top(attributes)
      let bottom = get_bottom(attributes)
      savoiardi.set_orthographic_camera_params(
        camera,
        left:,
        right:,
        top:,
        bottom:,
        near:,
        far:,
      )
      savoiardi.update_camera_projection_matrix(camera)
    }
    False -> Nil
  }
}

fn update_perspective(
  context: Context,
  object: Option(Object3D),
  new_attributes: Dict(String, String),
  changed_attributes: Set(String),
) -> Result(Context, Nil) {
  use object <- result.map(object |> option.to_result(Nil))
  let camera = savoiardi.object3d_to_camera(object)
  conditionally_set_perspective_camera_params(
    context,
    camera,
    changed_attributes,
    new_attributes,
  )
  conditionally_set_active(context, camera, new_attributes, changed_attributes)
  context
}

fn conditionally_set_active(
  context: Context,
  camera: savoiardi.Camera,
  new_attributes: Dict(String, String),
  changed_attributes: Set(String),
) -> Nil {
  let has_changed = set.contains("active", in: changed_attributes)
  let active = node.get_bool(new_attributes, "active")
  case has_changed, active {
    False, _ -> Nil
    _, True -> {
      loop.clear_active_camera(context.loop)
      loop.set_active_camera(context.loop, camera)
    }
    _, False -> loop.clear_active_camera(context.loop)
  }
}

fn conditionally_set_perspective_camera_params(
  context: Context,
  camera: savoiardi.Camera,
  changed_attributes: Set(String),
  attributes: Dict(String, String),
) -> Nil {
  case
    list.any(
      [
        "fov",
        "near",
        "far",
        "aspect",
      ],
      set.contains(changed_attributes, _),
    )
  {
    True -> {
      let fov = get_fov(attributes)
      let near = get_near(attributes)
      let far = get_far(attributes)
      let aspect = get_canvas_ratio(context.registry)
      savoiardi.set_perspective_camera_params(
        camera,
        fov:,
        aspect:,
        near:,
        far:,
      )
      savoiardi.update_camera_projection_matrix(camera)
    }
    False -> Nil
  }
}

fn get_fov(attributes: Dict(String, String)) -> Float {
  node.get(attributes:, key: "fov", default: 75.0, parse_fn: node.parse_number)
}

fn get_near(attributes: Dict(String, String)) -> Float {
  node.get(attributes:, key: "near", default: 0.1, parse_fn: node.parse_number)
}

fn get_far(attributes: Dict(String, String)) -> Float {
  node.get(
    attributes:,
    key: "far",
    default: 1000.0,
    parse_fn: node.parse_number,
  )
}

fn get_left(attributes: Dict(String, String)) -> Float {
  node.get(attributes:, key: "left", default: 10.0, parse_fn: node.parse_number)
}

fn get_right(attributes: Dict(String, String)) -> Float {
  node.get(
    attributes:,
    key: "right",
    default: 10.0,
    parse_fn: node.parse_number,
  )
}

fn get_top(attributes: Dict(String, String)) -> Float {
  node.get(attributes:, key: "top", default: 10.0, parse_fn: node.parse_number)
}

fn get_bottom(attributes: Dict(String, String)) -> Float {
  node.get(
    attributes:,
    key: "bottom",
    default: -10.0,
    parse_fn: node.parse_number,
  )
}

fn get_canvas_ratio(registry: registry.Registry) {
  let vec2.Vec2(x: width, y: height) =
    savoiardi.get_canvas_dimensions(registry.renderer)
  case height {
    0.0 -> 16.0 /. 9.0
    h -> width /. h
  }
}
