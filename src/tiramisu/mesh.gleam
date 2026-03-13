//// Asset-backed mesh node attributes.
////
//// Mesh nodes represent externally loaded assets such as GLTF, GLB, OBJ, FBX,
//// and STL files. Loading is asynchronous and integrated into the scene
//// runtime, so the node can appear in the view immediately and resolve later.

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/javascript/promise
import gleam/json
import gleam/option.{type Option}
import lustre/attribute.{type Attribute}
import lustre/event

import savoiardi.{type Object3D}

import tiramisu/dev/extension
import tiramisu/dev/runtime
import tiramisu/internal/element
import tiramisu/internal/node

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

/// Internal node extension for mesh elements.
pub fn extension() -> extension.Extension {
  let observed_attributes = ["src", "hidden"]
  extension.Node(tag:, observed_attributes:, create:, update:, remove:)
  |> extension.NodeExtension
}

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
  context: extension.Context,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> extension.Context {
  case dict.get(attributes, "src") {
    Ok(src) -> {
      context.spawn(
        extension.NodeScope(id),
        extension.async_key("src"),
        register_model(context, id, parent_id, src, attributes, Register),
      )
      context
    }
    Error(Nil) -> context
  }
}

fn update(
  ctx: extension.Context,
  id: String,
  parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> extension.Context {
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
          ctx
        }
      }
    }

    option.None -> ctx
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

pub type Mode {
  Register
  Replace
}

fn register_model(
  context: extension.Context,
  id: String,
  parent_id: String,
  src: String,
  attributes: Dict(String, String),
  mode: Mode,
) -> promise.Promise(extension.AsyncEffect) {
  case src {
    "gltf" | "glb" -> create_gltf(src, id, parent_id, context, attributes, mode)
    "fbx" -> create_fbx(src, id, parent_id, context, attributes, mode)
    "obj" -> create_obj(src, id, parent_id, context, attributes, mode)
    "stl" -> create_stl(src, id, parent_id, context, attributes, mode)
    _ -> promise.resolve(extension.NoOp)
  }
}

fn create_stl(
  src: String,
  id: String,
  parent_id: String,
  _context: extension.Context,
  attributes: Dict(String, String),
  mode: Mode,
) -> promise.Promise(extension.AsyncEffect) {
  use result <- promise.await(savoiardi.load_stl(src))
  case result {
    Error(Nil) -> {
      element.dispatch_event(
        id,
        "tiramisu:model-error",
        json.object([#("id", json.string(id))]),
      )
      promise.resolve(extension.NoOp)
    }
    Ok(geometry) -> {
      let geometry = case node.get_bool(attributes, "center") {
        True -> savoiardi.center_geometry(geometry)
        False -> geometry
      }
      let object = savoiardi.create_mesh(geometry)
      set_object_attributes(object, attributes)
      promise.resolve(build_async_effect(mode, id, parent_id, object))
    }
  }
}

fn create_obj(
  src: String,
  id: String,
  parent_id: String,
  _context: extension.Context,
  attributes: Dict(String, String),
  mode: Mode,
) -> promise.Promise(extension.AsyncEffect) {
  use result <- promise.await(savoiardi.load_obj(src))
  case result {
    Error(_) -> {
      element.dispatch_event(
        id,
        "tiramisu:model-error",
        json.object([#("id", json.string(id))]),
      )
      promise.resolve(extension.NoOp)
    }
    Ok(object) -> {
      set_object_attributes(object, attributes)
      promise.resolve(build_async_effect(mode, id, parent_id, object))
    }
  }
}

fn create_fbx(
  src: String,
  id: String,
  parent_id: String,
  _context: extension.Context,
  attributes: Dict(String, String),
  mode: Mode,
) -> promise.Promise(extension.AsyncEffect) {
  use result <- promise.await(savoiardi.load_fbx(src))
  case result {
    Error(_) -> {
      element.dispatch_event(
        id,
        "tiramisu:model-error",
        json.object([#("id", json.string(id))]),
      )
      promise.resolve(extension.NoOp)
    }
    Ok(data) -> {
      let object = savoiardi.get_fbx_scene(data)
      set_object_attributes(object, attributes)
      promise.resolve(build_async_effect(mode, id, parent_id, object))
    }
  }
}

fn create_gltf(
  src: String,
  id: String,
  parent_id: String,
  _context: extension.Context,
  attributes: Dict(String, String),
  mode: Mode,
) -> promise.Promise(extension.AsyncEffect) {
  use result <- promise.await(savoiardi.load_gltf(src))
  case result {
    Error(_) -> {
      element.dispatch_event(
        id,
        "tiramisu:model-error",
        json.object([#("id", json.string(id))]),
      )
      promise.resolve(extension.NoOp)
    }
    Ok(data) -> {
      let object = savoiardi.get_gltf_scene(data)
      set_object_attributes(object, attributes)
      promise.resolve(build_async_effect(mode, id, parent_id, object))
    }
  }
}

fn set_object_attributes(
  object: Object3D,
  attributes: Dict(String, String),
) -> Nil {
  savoiardi.set_object_visible(object, !node.get_bool(attributes, "hidden"))
  savoiardi.enable_shadows(
    object,
    cast_shadow: node.get_bool(attributes, "cast-shadow"),
    receive_shadow: node.get_bool(attributes, "receive-shadow"),
  )
}

fn build_async_effect(
  mode: Mode,
  id: String,
  parent_id: String,
  object: Object3D,
) -> extension.AsyncEffect {
  element.dispatch_event(
    id,
    "tiramisu:model-loaded",
    json.object([#("id", json.string(id))]),
  )
  case mode {
    Register -> extension.RegisterObject(id:, parent_id:, tag:, object:)
    Replace -> extension.ReplaceObject(id:, object:)
  }
}

fn reload_model_if_needed(
  context: extension.Context,
  id: String,
  parent_id: String,
  object: Object3D,
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Result(extension.Context, Nil) {
  case extension.change(changed_attributes, "src") {
    Ok(extension.Removed) ->
      extension.Context(
        ..context,
        runtime: runtime.remove_object(context.runtime, id, parent_id, object),
      )
      |> Ok

    Ok(extension.Added(_)) | Ok(extension.Updated(_)) ->
      case dict.get(attributes, "src") {
        Ok(src) -> {
          context.spawn(
            extension.NodeScope(id),
            extension.async_key("src"),
            register_model(context, id, parent_id, src, attributes, Replace),
          )
          Error(Nil)
        }

        Error(Nil) -> Error(Nil)
      }

    Error(Nil) -> Error(Nil)
  }
}
