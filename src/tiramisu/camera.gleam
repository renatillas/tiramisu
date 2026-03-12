//// Camera node attributes and runtime integration.
////
//// Cameras are scene nodes, not renderer configuration. They can live anywhere
//// in the scene hierarchy and inherit parent transforms like other objects.
////
//// A typical setup looks like this:
////
//// ```gleam
//// tiramisu.camera(
////   "main-camera",
////   [
////     camera.active(True),
////     camera.fov(60.0),
////     transform.position(vec3.Vec3(0.0, 2.0, 6.0)),
////   ],
////   [],
//// )
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/json
import gleam/option.{type Option}
import gleam/result
import lustre/attribute.{type Attribute}
import savoiardi.{type Object3D}

import tiramisu/dev/extension
import tiramisu/dev/runtime
import tiramisu/internal/node
import vec/vec2

/// The custom element tag used for camera nodes.
pub const tag = "tiramisu-camera"

/// Internal node extension for camera elements.
pub fn extension() -> extension.Extension {
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

/// Supported camera kinds.
pub type Camera {
  Perspective
  Orthographic
}

/// Choose the camera type.
pub fn kind(kind: Camera) -> Attribute(msg) {
  attribute.attribute("type", case kind {
    Perspective -> "perspective"
    Orthographic -> "orthographic"
  })
}

/// Mark a camera as active.
///
/// A scene should have at most one active camera.
///
/// If multiple cameras are marked active the runtime will still pick one, but
/// that configuration is not considered part of the stable public API.
pub fn active(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("active", "")
    False -> attribute.property("active", json.bool(False))
  }
}

/// Set the field of view for perspective cameras.
pub fn fov(degrees: Float) -> Attribute(msg) {
  attribute.attribute("fov", float.to_string(degrees))
}

/// Set the near clipping plane.
pub fn near(distance: Float) -> Attribute(msg) {
  attribute.attribute("near", float.to_string(distance))
}

/// Set the far clipping plane.
pub fn far(distance: Float) -> Attribute(msg) {
  attribute.attribute("far", float.to_string(distance))
}

/// Set the left bound for orthographic cameras.
pub fn left(value: Float) -> Attribute(msg) {
  attribute.attribute("left", float.to_string(value))
}

/// Set the right bound for orthographic cameras.
pub fn right(value: Float) -> Attribute(msg) {
  attribute.attribute("right", float.to_string(value))
}

/// Set the top bound for orthographic cameras.
pub fn top(value: Float) -> Attribute(msg) {
  attribute.attribute("top", float.to_string(value))
}

/// Set the bottom bound for orthographic cameras.
pub fn bottom(value: Float) -> Attribute(msg) {
  attribute.attribute("bottom", float.to_string(value))
}

fn remove(
  ctx: extension.Context,
  id: String,
  parent_id: String,
  object: Object3D,
) -> extension.Context {
  let next_runtime =
    ctx.runtime
    |> runtime.remove_object(id, parent_id, object)
    |> runtime.deactivate_camera(id)
  extension.Context(..ctx, runtime: next_runtime)
}

fn create(
  ctx: extension.Context,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> extension.Context {
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
      let aspect = get_canvas_ratio(ctx.runtime)
      savoiardi.create_perspective_camera(fov:, aspect:, near:, far:)
    }
  }
  let object = savoiardi.camera_to_object3d(camera)
  let active = node.get_bool(attributes, "active")
  let next_runtime =
    ctx.runtime
    |> runtime.add_object(id, object:, parent_id:, tag:)
    |> when(active, do: runtime.activate_camera(_, id, camera))
  extension.Context(..ctx, runtime: next_runtime)
}

fn update(
  context: extension.Context,
  id: String,
  parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> extension.Context {
  case
    extension.has_change(changed_attributes, "type"),
    dict.get(attributes, "type") |> result.unwrap("perspective")
  {
    True, "perspective" | True, "orthographic" ->
      recreate_camera(context, id, parent_id, object, attributes)

    False, "perspective" ->
      update_perspective(context, id, object, attributes, changed_attributes)
      |> result.unwrap(context)

    False, "orthographic" ->
      update_orthographic(context, id, object, attributes, changed_attributes)
      |> result.unwrap(context)

    _, _ -> context
  }
}

fn recreate_camera(
  context: extension.Context,
  id: String,
  parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
) -> extension.Context {
  case object {
    option.Some(object) ->
      remove(context, id, parent_id, object)
      |> create(id, parent_id, attributes)
    option.None -> context
  }
}

fn update_orthographic(
  context: extension.Context,
  id: String,
  object: Option(Object3D),
  new_attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Result(extension.Context, Nil) {
  case object {
    option.Some(object) -> {
      let camera = savoiardi.object3d_to_camera(object)

      let _ = do_update_orthographic(new_attributes, changed_attributes, camera)

      conditionally_set_active(
        context,
        id,
        camera,
        new_attributes,
        changed_attributes,
      )
      |> Ok
    }

    option.None -> Error(Nil)
  }
}

fn do_update_orthographic(
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
  camera: savoiardi.Camera,
) -> Nil {
  case
    extension.has_any_change(changed_attributes, [
      "near",
      "far",
      "left",
      "right",
      "top",
      "bottom",
    ])
  {
    True -> {
      let near = get_near(attributes)
      let far = get_far(attributes)
      let left = get_left(attributes)
      let right = get_right(attributes)
      let top = get_top(attributes)
      let bottom = get_bottom(attributes)
      savoiardi.update_orthograpic(
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
  context: extension.Context,
  id: String,
  object: Option(Object3D),
  new_attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Result(extension.Context, Nil) {
  case object {
    option.Some(object) -> {
      let camera = savoiardi.object3d_to_camera(object)
      do_update_perspective(context, camera, changed_attributes, new_attributes)
      conditionally_set_active(
        context,
        id,
        camera,
        new_attributes,
        changed_attributes,
      )
      |> Ok
    }

    option.None -> Error(Nil)
  }
}

fn conditionally_set_active(
  context: extension.Context,
  id: String,
  camera: savoiardi.Camera,
  new_attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> extension.Context {
  case extension.change(changed_attributes, "active") {
    Ok(extension.Removed) ->
      extension.Context(
        ..context,
        runtime: runtime.deactivate_camera(context.runtime, id),
      )

    Ok(extension.Added(_)) | Ok(extension.Updated(_)) ->
      case node.get_bool(new_attributes, "active") {
        True ->
          extension.Context(
            ..context,
            runtime: runtime.activate_camera(context.runtime, id, camera),
          )

        False ->
          extension.Context(
            ..context,
            runtime: runtime.deactivate_camera(context.runtime, id),
          )
      }

    Error(Nil) -> context
  }
}

fn do_update_perspective(
  context: extension.Context,
  camera: savoiardi.Camera,
  changed_attributes: extension.AttributeChanges,
  attributes: Dict(String, String),
) -> Nil {
  case
    extension.has_any_change(changed_attributes, [
      "fov",
      "near",
      "far",
      "aspect",
    ])
  {
    True -> {
      let fov = get_fov(attributes)
      let near = get_near(attributes)
      let far = get_far(attributes)
      let aspect = get_canvas_ratio(context.runtime)
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

fn get_canvas_ratio(runtime: runtime.Runtime) -> Float {
  let vec2.Vec2(x: width, y: height) =
    savoiardi.get_canvas_dimensions(runtime.renderer(runtime))
  case height {
    0.0 -> 16.0 /. 9.0
    _ -> width /. height
  }
}

fn when(value: a, condition: Bool, do callback: fn(a) -> a) -> a {
  case condition {
    True -> callback(value)
    False -> value
  }
}
