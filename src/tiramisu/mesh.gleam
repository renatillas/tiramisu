//// Asset-backed mesh node attributes.
////
//// Mesh nodes represent externally loaded assets such as GLTF, GLB, OBJ, FBX,
//// and STL files. Loading is asynchronous and integrated into the scene
//// runtime, so the node can appear in the view immediately and resolve later.

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/javascript/promise
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import lustre/attribute.{type Attribute}
import lustre/effect
import lustre/event
import savoiardi/geometry
import savoiardi/loader
import savoiardi/material

import savoiardi/object.{type Object3D}

import tiramisu/dev/extension
import tiramisu/dev/runtime.{type Runtime}

/// The custom element tag used for asset-backed meshes.
@internal
pub const tag: String = "tiramisu-mesh"

/// Hide or show the mesh object.
///
/// This controls Three.js visibility for the node rather than DOM layout.
pub const hidden: fn(Bool) -> Attribute(a) = attribute.hidden

/// Set the asset URL to load for this mesh.
///
/// Tiramisu resolves supported formats such as GLTF, GLB, OBJ, FBX, and STL
/// through this attribute.
pub const src: fn(String) -> Attribute(a) = attribute.src

/// Listen for successful model loading on this mesh.
///
/// The handler receives the mesh node id from the event detail.
pub fn on_model_load(to_msg: fn(String) -> a) -> Attribute(a) {
  event.on("tiramisu:model-loaded", {
    use id <- decode.subfield(["detail", "id"], decode.string)
    decode.success(to_msg(id))
  })
}

/// Listen for model loading failures on this mesh.
///
/// The handler receives the mesh node id from the event detail.
pub fn on_model_error(to_msg: fn(String) -> a) -> Attribute(a) {
  event.on("tiramisu:model-error", {
    use id <- decode.subfield(["detail", "id"], decode.string)
    decode.success(to_msg(id))
  })
}

/// Center the loaded geometry around the origin when supported.
pub fn center(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("center", "")
    False -> attribute.property("center", json.bool(False))
  }
}

/// Enable or disable shadow casting for the mesh.
pub fn cast_shadow(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("cast-shadow", "")
    False -> attribute.property("cast-shadow", json.bool(False))
  }
}

/// Enable or disable shadow receiving for the mesh.
pub fn receive_shadow(bool: Bool) -> Attribute(msg) {
  case bool {
    True -> attribute.attribute("receive-shadow", "")
    False -> attribute.property("receive-shadow", json.bool(False))
  }
}

fn create(
  context: Runtime,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> #(Runtime, effect.Effect(extension.Msg)) {
  case dict.get(attributes, "src") {
    Ok(src) -> #(
      context,
      extension.request(
        extension.NodeOwner(id),
        extension.request_key("src"),
        register_model(id, parent_id, src, attributes, Register),
      ),
    )
    Error(Nil) -> #(context, effect.none())
  }
}

fn update(
  ctx: Runtime,
  id: String,
  parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> #(Runtime, effect.Effect(extension.Msg)) {
  case object {
    option.Some(object) -> {
      case
        reload_model_if_needed(
          ctx,
          id,
          parent_id,
          object,
          attributes,
          changed_attributes,
        )
      {
        Ok(next_context) -> next_context
        Error(Nil) -> {
          set_object_attributes(object, attributes)
          #(ctx, effect.none())
        }
      }
    }

    option.None -> #(ctx, effect.none())
  }
}

fn remove(
  runtime: Runtime,
  id: String,
  parent_id: String,
  object: Object3D,
) -> #(Runtime, effect.Effect(extension.Msg)) {
  let runtime = runtime.remove_object(runtime, id, parent_id, object)
  #(runtime, effect.none())
}

/// Internal async loading mode for mesh assets.
///
/// `Register` is used for first-time loads, while `Replace` swaps an already
/// loaded object after the `src` attribute changes.
type Mode {
  Register
  Replace
}

fn register_model(
  id: String,
  parent_id: String,
  src: String,
  attributes: Dict(String, String),
  mode: Mode,
) -> promise.Promise(List(extension.RuntimeAction)) {
  case source_extension(src) {
    "gltf" | "glb" -> create_gltf(src, id, parent_id, attributes, mode)
    "fbx" -> create_fbx(src, id, parent_id, attributes, mode)
    "obj" -> create_obj(src, id, parent_id, attributes, mode)
    "stl" -> create_stl(src, id, parent_id, attributes, mode)
    _ -> promise.resolve([])
  }
}

fn source_extension(src: String) -> String {
  src
  |> string.split("#")
  |> list.first
  |> result.unwrap(src)
  |> string.split("?")
  |> list.first
  |> result.unwrap(src)
  |> string.split(".")
  |> list.last
  |> result.unwrap("")
  |> string.lowercase
}

fn create_stl(
  src: String,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
  mode: Mode,
) -> promise.Promise(List(extension.RuntimeAction)) {
  use result <- promise.await(geometry.load_stl_async(loader.stl(), src))
  case result {
    Error(_) -> promise.resolve([emit_model_error(id)])
    Ok(geometry) -> {
      let geometry = case extension.get_bool(attributes, "center") {
        True -> geometry.center(geometry)
        False -> geometry
      }
      let object =
        object.mesh(geometry, material.standard(material.standard_options()))
      set_object_attributes(object, attributes)
      promise.resolve(build_runtime_actions(mode, id, parent_id, object))
    }
  }
}

fn create_obj(
  src: String,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
  mode: Mode,
) -> promise.Promise(List(extension.RuntimeAction)) {
  use result <- promise.await(object.load_obj_async(loader.obj(), src))
  case result {
    Error(_) -> promise.resolve([emit_model_error(id)])
    Ok(object) -> {
      set_object_attributes(object, attributes)
      promise.resolve(build_runtime_actions(mode, id, parent_id, object))
    }
  }
}

fn create_fbx(
  src: String,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
  mode: Mode,
) -> promise.Promise(List(extension.RuntimeAction)) {
  use result <- promise.await(object.load_fbx_async(loader.fbx(), src))
  case result {
    Error(_) -> promise.resolve([emit_model_error(id)])
    Ok(data) -> {
      let object = object.get_fbx_scene(data)
      set_object_attributes(object, attributes)
      promise.resolve(build_runtime_actions(mode, id, parent_id, object))
    }
  }
}

fn create_gltf(
  src: String,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
  mode: Mode,
) -> promise.Promise(List(extension.RuntimeAction)) {
  use result <- promise.await(object.load_gltf_async(loader.gltf(), src))
  case result {
    Error(_) -> promise.resolve([emit_model_error(id)])
    Ok(data) -> {
      let object = object.get_gltf_scene(data)
      set_object_attributes(object, attributes)
      promise.resolve(build_runtime_actions(mode, id, parent_id, object))
    }
  }
}

fn set_object_attributes(
  object: Object3D,
  attributes: Dict(String, String),
) -> Object3D {
  object
  |> object.set_visible(!extension.get_bool(attributes, "hidden"))
  |> object.set_cast_shadow(extension.get_bool(attributes, "cast-shadow"))
  |> object.set_receive_shadow(extension.get_bool(attributes, "receive-shadow"))
}

fn build_runtime_actions(
  mode: Mode,
  id: String,
  parent_id: String,
  object: Object3D,
) -> List(extension.RuntimeAction) {
  case mode {
    Register -> [
      extension.register_object(id, parent_id, tag, object),
      emit_model_loaded(id),
    ]
    Replace -> [extension.replace_object(id, object), emit_model_loaded(id)]
  }
}

fn emit_model_loaded(id: String) -> extension.RuntimeAction {
  extension.action(fn(runtime) {
    #(
      runtime,
      event.emit(
        "tiramisu:model-loaded",
        json.object([#("id", json.string(id))]),
      ),
    )
  })
}

fn emit_model_error(id: String) -> extension.RuntimeAction {
  extension.action(fn(runtime) {
    #(
      runtime,
      event.emit(
        "tiramisu:model-error",
        json.object([#("id", json.string(id))]),
      ),
    )
  })
}

fn reload_model_if_needed(
  runtime: Runtime,
  id: String,
  parent_id: String,
  object: Object3D,
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Result(#(Runtime, effect.Effect(extension.Msg)), Nil) {
  case extension.change(changed_attributes, "src") {
    Ok(extension.Removed) ->
      Ok(#(runtime.remove_object(runtime, id, parent_id, object), effect.none()))

    Ok(extension.Added(_)) | Ok(extension.Updated(_)) -> {
      use src <- result.map(dict.get(attributes, "src"))
      #(
        runtime,
        extension.request(
          extension.NodeOwner(id),
          extension.request_key("src"),
          register_model(id, parent_id, src, attributes, Replace),
        ),
      )
    }

    Error(Nil) -> Error(Nil)
  }
}

/// Internal node extension for mesh elements.
pub fn ext() -> extension.Extension {
  let observed_attributes = ["src", "hidden"]
  extension.node_extension(
    tag:,
    observed_attributes:,
    create:,
    update:,
    remove:,
  )
}
