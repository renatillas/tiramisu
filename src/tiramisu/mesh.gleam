import gleam/dict.{type Dict}
import gleam/function
import gleam/javascript/promise
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/set.{type Set}
import gleam/string

import savoiardi.{type Object3D}

import tiramisu/dev/extension.{type Context}
import tiramisu/dev/registry
import tiramisu/internal/dom
import tiramisu/internal/node

@internal
pub const tag: String = "tiramisu-mesh"

pub fn extension() {
  let observed_attributes = set.from_list(["src", "hidden"])
  extension.Node(tag:, observed_attributes:, create:, update:, remove:)
  |> extension.NodeExtension
}

fn create(
  context: Context,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> Context {
  case dict.get(attributes, "src") {
    Ok(src) -> {
      context.on_async(
        register_model(
          context,
          id:,
          src:,
          attributes:,
          transformation: fn(registry, id, object) {
            registry.register_and_add_object(
              registry,
              id,
              object,
              parent_id:,
              tag:,
            )
          },
        ),
      )
      context
    }
    Error(Nil) -> context
  }
}

fn update(
  ctx: Context,
  id: String,
  _parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: Set(String),
) -> Context {
  // If loading models, registry.get_object fails a few frames until load completes
  let _ = {
    use object <- result.map(object |> option.to_result(Nil))
    case
      set.contains("src", in: changed_attributes),
      dict.get(attributes, "src")
    {
      True, Ok(src) ->
        ctx.on_async(register_model(
          ctx,
          id:,
          src:,
          attributes:,
          transformation: registry.replace_object_model,
        ))
      _, _ -> Nil
    }
    savoiardi.set_object_visible(object, !node.get_bool(attributes, "hidden"))
  }
  ctx
}

fn remove(
  context: Context,
  id: String,
  parent_id: String,
  object: Object3D,
) -> Context {
  let registry = registry.remove_object(context.registry, id, parent_id, object)
  extension.Context(..context, registry:)
}

fn register_model(
  context: Context,
  id id: String,
  src src: String,
  attributes attributes,
  transformation transformation: fn(registry.Registry, String, Object3D) ->
    registry.Registry,
) -> promise.Promise(fn(registry.Registry) -> registry.Registry) {
  case get_file_extension(src) {
    "gltf" | "glb" -> create_gltf(src, id, context, attributes, transformation)
    "fbx" -> create_fbx(src, id, context, attributes, transformation)
    "obj" -> create_obj(src, id, context, attributes, transformation)
    "stl" -> create_stl(src, id, context, attributes, transformation)
    _ -> promise.resolve(function.identity)
  }
}

fn create_stl(
  src: String,
  id: String,
  context: Context,
  attributes,
  registry_transformation: fn(registry.Registry, String, Object3D) ->
    registry.Registry,
) -> promise.Promise(fn(registry.Registry) -> registry.Registry) {
  use result <- promise.await(savoiardi.load_stl(src))
  case result {
    Error(Nil) -> {
      dom.dispatch_event(id, "tiramisu:model-error", id)
      promise.resolve(function.identity)
    }
    Ok(geometry) -> {
      let geometry = case node.get_bool(attributes, "center") {
        True -> savoiardi.center_geometry(geometry)
        False -> geometry
      }
      let object = savoiardi.create_mesh(geometry)
      set_object_attributes(object, attributes)
      update_registry(_, object, context, id, registry_transformation)
      |> promise.resolve
    }
  }
}

fn create_obj(
  src: String,
  id: String,
  context: Context,
  attributes,
  registry_transformation: fn(registry.Registry, String, Object3D) ->
    registry.Registry,
) -> promise.Promise(fn(registry.Registry) -> registry.Registry) {
  use result <- promise.map(savoiardi.load_obj(src))
  case result {
    Error(_) -> {
      dom.dispatch_event(id, "tiramisu:model-error", id)
      function.identity
    }
    Ok(object) -> {
      set_object_attributes(object, attributes)
      update_registry(_, object, context, id, registry_transformation)
    }
  }
}

fn create_fbx(
  src: String,
  id: String,
  context: Context,
  attributes,
  registry_transformation: fn(registry.Registry, String, Object3D) ->
    registry.Registry,
) -> promise.Promise(fn(registry.Registry) -> registry.Registry) {
  use result <- promise.map(savoiardi.load_fbx(src))
  case result {
    Error(_) -> {
      dom.dispatch_event(id, "tiramisu:model-error", id)
      function.identity
    }
    Ok(data) -> {
      let object = savoiardi.get_fbx_scene(data)
      set_object_attributes(object, attributes)
      update_registry(_, object, context, id, registry_transformation)
    }
  }
}

fn create_gltf(
  src: String,
  id: String,
  context: Context,
  attributes,
  registry_transformation: fn(registry.Registry, String, Object3D) ->
    registry.Registry,
) -> promise.Promise(fn(registry.Registry) -> registry.Registry) {
  use result <- promise.map(savoiardi.load_gltf(src))
  case result {
    Error(_) -> {
      dom.dispatch_event(
        id,
        "tiramisu:model-error",
        json.object([#("id", json.string(id))]),
      )
      function.identity
    }
    Ok(data) -> {
      let object = savoiardi.get_gltf_scene(data)
      set_object_attributes(object, attributes)
      update_registry(_, object, context, id, registry_transformation)
    }
  }
}

fn set_object_attributes(object: Object3D, attributes: Dict(String, String)) {
  savoiardi.set_object_visible(object, !node.get_bool(attributes, "hidden"))
}

fn update_registry(
  registry: registry.Registry,
  object: Object3D,
  context: Context,
  id: String,
  registry_transformation: fn(registry.Registry, String, Object3D) ->
    registry.Registry,
) -> registry.Registry {
  dom.dispatch_event(
    id,
    "tiramisu:model-loaded",
    json.object([#("id", json.string(id))]),
  )
  context.on_object_resolved(tag, id, object)
  registry_transformation(registry, id, object)
}

fn get_file_extension(url: String) -> String {
  url
  |> string.split("?")
  |> list.first
  |> result.unwrap(url)
  |> string.split(".")
  |> list.last
  |> result.unwrap("")
  |> string.lowercase
}
