//// Camera extension attributes and runtime integration.
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
import lustre/effect
import savoiardi/renderer
import tiramisu/internal/element

import savoiardi/camera
import savoiardi/object.{type Object3D}

import tiramisu/dev/extension
import tiramisu/dev/runtime.{type Runtime}
import vec/vec2

/// The custom element tag used for camera nodes.
pub const tag = "tiramisu-camera"

pub fn perspective() -> Attribute(msg) {
  attribute.attribute("type", "perspective")
}

pub fn orthographic() -> Attribute(msg) {
  attribute.attribute("type", "orthographic")
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
  runtime: Runtime,
  id: String,
  parent_id: String,
  object: Object3D,
) -> #(Runtime, effect.Effect(extension.Msg)) {
  let next_runtime =
    runtime
    |> runtime.remove_object(id, parent_id, object)
    |> runtime.deactivate_camera(id)
  #(next_runtime, effect.none())
}

fn create(
  runtime: Runtime,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> #(Runtime, effect.Effect(extension.Msg)) {
  let near = extension.get(attributes, "near", 0.1, extension.parse_number)
  let far = extension.get(attributes, "far", 1000.0, extension.parse_number)
  let camera = case dict.get(attributes, "type") {
    Ok("orthographic") -> {
      let left =
        extension.get(attributes, "left", -10.0, extension.parse_number)
      let right =
        extension.get(attributes, "right", 10.0, extension.parse_number)
      let top = extension.get(attributes, "top", 10.0, extension.parse_number)
      let bottom =
        extension.get(attributes, "bottom", -10.0, extension.parse_number)
      camera.orthographic(left:, right:, top:, bottom:, near:, far:)
      |> camera.from_orthographic
    }
    _ -> {
      let fov = extension.get(attributes, "fov", 75.0, extension.parse_number)
      let aspect = get_canvas_ratio(runtime)
      camera.perspective(fov:, aspect:, near:, far:)
      |> camera.from_perspective
    }
  }
  let object = camera.to_object3d(camera)
  let active = extension.get_bool(attributes, "active")
  let runtime =
    runtime
    |> runtime.add_object(id, object:, parent_id:, tag:)
    |> when(active, do: runtime.activate_camera(_, id, camera))

  #(runtime, effect.none())
}

fn update(
  runtime: Runtime,
  id: String,
  parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> #(Runtime, effect.Effect(extension.Msg)) {
  case
    extension.has_change(changed_attributes, "type"),
    dict.get(attributes, "type") |> result.unwrap("perspective")
  {
    True, "perspective" | True, "orthographic" ->
      recreate_camera(runtime, id, parent_id, object, attributes)

    False, "perspective" ->
      update_perspective(runtime, id, object, attributes, changed_attributes)
      |> result.unwrap(runtime)
      |> with_no_effect

    False, "orthographic" ->
      update_orthographic(runtime, id, object, attributes, changed_attributes)
      |> result.unwrap(runtime)
      |> with_no_effect

    _, _ -> #(runtime, effect.none())
  }
}

fn recreate_camera(
  context: Runtime,
  id: String,
  parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
) -> #(Runtime, effect.Effect(extension.Msg)) {
  case object {
    option.Some(object) -> {
      let #(context, remove_effect) = remove(context, id, parent_id, object)
      let #(context, create_effect) = create(context, id, parent_id, attributes)
      #(context, effect.batch([remove_effect, create_effect]))
    }
    option.None -> #(context, effect.none())
  }
}

fn with_no_effect(context: Runtime) -> #(Runtime, effect.Effect(extension.Msg)) {
  #(context, effect.none())
}

fn update_orthographic(
  context: Runtime,
  id: String,
  object: Option(Object3D),
  new_attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Result(Runtime, Nil) {
  case object {
    option.Some(object) -> {
      use camera <- result.try(camera.orthographic_from_object3d(object))

      let _ = do_update_orthographic(new_attributes, changed_attributes, camera)

      let camera = camera.from_orthographic(camera)

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
  camera: camera.Orthographic,
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
      camera.update_orthographic(
        camera,
        left:,
        right:,
        top:,
        bottom:,
        near:,
        far:,
      )
      |> camera.from_orthographic
      |> camera.update_projection_matrix
      Nil
    }
    False -> Nil
  }
}

fn update_perspective(
  context: Runtime,
  id: String,
  object: Option(Object3D),
  new_attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Result(Runtime, Nil) {
  case object {
    option.Some(object) -> {
      use camera <- result.try(camera.perspective_from_object3d(object))
      do_update_perspective(context, camera, changed_attributes, new_attributes)
      let camera = camera.from_perspective(camera)
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
  runtime: Runtime,
  id: String,
  camera: camera.Camera,
  _new_attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Runtime {
  case extension.bool_change(changed_attributes, "active") {
    Ok(True) -> runtime.activate_camera(runtime, id, camera)
    Ok(False) -> runtime.deactivate_camera(runtime, id)
    Error(Nil) -> runtime
  }
}

fn do_update_perspective(
  runtime: Runtime,
  camera: camera.Perspective,
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
      let aspect = get_canvas_ratio(runtime)
      camera.update_perspective(camera, fov:, aspect:, near:, far:)
      |> camera.from_perspective
      |> camera.update_projection_matrix
      Nil
    }
    False -> Nil
  }
}

fn get_fov(attributes: Dict(String, String)) -> Float {
  extension.get(
    attributes:,
    key: "fov",
    default: 75.0,
    parse_fn: extension.parse_number,
  )
}

fn get_near(attributes: Dict(String, String)) -> Float {
  extension.get(
    attributes:,
    key: "near",
    default: 0.1,
    parse_fn: extension.parse_number,
  )
}

fn get_far(attributes: Dict(String, String)) -> Float {
  extension.get(
    attributes:,
    key: "far",
    default: 1000.0,
    parse_fn: extension.parse_number,
  )
}

fn get_left(attributes: Dict(String, String)) -> Float {
  extension.get(
    attributes:,
    key: "left",
    default: 10.0,
    parse_fn: extension.parse_number,
  )
}

fn get_right(attributes: Dict(String, String)) -> Float {
  extension.get(
    attributes:,
    key: "right",
    default: 10.0,
    parse_fn: extension.parse_number,
  )
}

fn get_top(attributes: Dict(String, String)) -> Float {
  extension.get(
    attributes:,
    key: "top",
    default: 10.0,
    parse_fn: extension.parse_number,
  )
}

fn get_bottom(attributes: Dict(String, String)) -> Float {
  extension.get(
    attributes:,
    key: "bottom",
    default: -10.0,
    parse_fn: extension.parse_number,
  )
}

fn get_canvas_ratio(runtime: Runtime) -> Float {
  let vec2.Vec2(x: width, y: height) =
    renderer.canvas(runtime.threejs_renderer(runtime))
    |> element.canvas_size
  width /. height
}

fn when(value: a, condition: Bool, do callback: fn(a) -> a) -> a {
  case condition {
    True -> callback(value)
    False -> value
  }
}

/// Build the internal extension used to reconcile camera nodes.
///
/// Most applications should not call this directly; use
/// `tiramisu.builtin_extensions()` instead.
pub fn ext() -> extension.Extension {
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
  extension.node_extension(
    tag:,
    observed_attributes:,
    create:,
    update:,
    remove:,
  )
}
