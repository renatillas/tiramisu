import gleam/dict
import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import savoiardi.{type Object3D}
import tiramisu/extension.{type Context, Context}
import tiramisu/internal/registry.{type Registry}
import tiramisu/internal/render_loop
import tiramisu/transform.{type Transform}

pub fn default_remove(ctx: Context, id: String) -> Context {
  let reg = registry.remove_object(ctx.registry, id)
  sync_active_camera(reg, ctx.loop)
  Context(..ctx, registry: reg)
}

pub fn sync_active_camera(reg: Registry, loop: render_loop.RenderLoop) -> Nil {
  case registry.find_active_camera(reg) {
    option.Some(camera) -> render_loop.set_active_camera(loop, camera)
    option.None -> render_loop.clear_active_camera(loop)
  }
}

pub fn load_model(
  on_async: fn(fn(Registry) -> Registry) -> Nil,
  on_object_resolved: fn(String, Object3D) -> Nil,
  id: String,
  parent_id: String,
  src: String,
  transform: Transform,
  visible: Bool,
  cast_shadow: Bool,
  receive_shadow: Bool,
) -> Nil {
  let register_loaded =
    register_loaded_model(
      on_async,
      on_object_resolved,
      id,
      parent_id,
      transform,
      visible,
      cast_shadow,
      receive_shadow,
    )
  case get_file_extension(src) {
    "gltf" | "glb" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_gltf(src))
        case result {
          Ok(data) -> register_loaded(savoiardi.get_gltf_scene(data))
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    "fbx" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_fbx(src))
        case result {
          Ok(data) -> register_loaded(savoiardi.get_fbx_scene(data))
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    "obj" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_obj(src))
        case result {
          Ok(data) -> register_loaded(data)
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    "stl" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_stl(src))
        case result {
          Ok(data) -> {
            let centered = savoiardi.center_geometry(data)
            let material =
              savoiardi.create_standard_material(
                0xcccccc,
                0.5,
                0.5,
                False,
                1.0,
                option.None,
                option.None,
                option.None,
                option.None,
                1.0,
                0.0,
                option.None,
                option.None,
                0x000000,
                1.0,
                0.0,
              )
            register_loaded(savoiardi.create_mesh(centered, material))
          }
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    _ -> Nil
  }
}

fn register_loaded_model(
  on_async: fn(fn(Registry) -> Registry) -> Nil,
  on_object_resolved: fn(String, Object3D) -> Nil,
  id: String,
  parent_id: String,
  transform: Transform,
  visible: Bool,
  cast_shadow: Bool,
  receive_shadow: Bool,
) -> fn(Object3D) -> Nil {
  fn(object) {
    on_async(fn(reg) {
      let reg =
        registry.register_and_add_object(
          reg,
          id,
          object,
          parent_id,
          registry.ObjectKind("tiramisu-mesh"),
        )
      registry.set_transform(reg, id, transform)
      registry.set_visible(reg, id, visible)
      registry.set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
      reg
    })
    // Notify attribute hooks that the async model has resolved
    on_object_resolved(id, object)
    registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
  }
}

pub fn set_model(
  on_async: fn(fn(Registry) -> Registry) -> Nil,
  id: String,
  src: String,
) -> promise.Promise(Nil) {
  case get_file_extension(src) {
    "gltf" | "glb" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_gltf(src))
        case result {
          Ok(data) -> {
            let object = savoiardi.get_gltf_scene(data)
            on_async(fn(reg) { registry.replace_object_model(reg, id, object) })
            registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
          }
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
    }
    "fbx" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_fbx(src))
        case result {
          Ok(data) -> {
            let object = savoiardi.get_fbx_scene(data)
            on_async(fn(reg) { registry.replace_object_model(reg, id, object) })
            registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
          }
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
    }
    "obj" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_obj(src))
        case result {
          Ok(data) -> {
            on_async(fn(reg) { registry.replace_object_model(reg, id, data) })
            registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
          }
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
    }
    _ -> promise.resolve(Nil)
  }
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

pub fn parse_color(hex: String) -> Result(Int, Nil) {
  let clean = string.replace(hex, "#", "")
  int.base_parse(clean, 16)
}

pub fn get_float(
  attrs: dict.Dict(String, String),
  key: String,
  default: Float,
) -> Float {
  dict.get(attrs, key)
  |> result.try(float.parse)
  |> result.unwrap(default)
}

pub fn get_bool(
  attrs: dict.Dict(String, String),
  key: String,
  default: Bool,
) -> Bool {
  case dict.get(attrs, key) {
    // HTML boolean attributes: presence with empty string or "true" means true
    Ok("true") | Ok("") -> True
    Ok("false") -> False
    Ok(_) | Error(Nil) -> default
  }
}

pub fn get_str(
  attrs: dict.Dict(String, String),
  key: String,
  default: String,
) -> String {
  dict.get(attrs, key) |> result.unwrap(default)
}

pub fn get_int(
  attrs: dict.Dict(String, String),
  key: String,
  default: Int,
) -> Int {
  dict.get(attrs, key)
  |> result.try(int.parse)
  |> result.unwrap(default)
}

pub fn get_or_create_listener(
  reg: Registry,
) -> #(Registry, savoiardi.AudioListener) {
  case registry.get_audio_listener(reg) {
    Ok(listener) -> #(reg, listener)
    Error(Nil) -> {
      let listener = savoiardi.create_audio_listener()
      let reg = registry.store_audio_listener(reg, listener)
      registry.attach_listener_to_camera(reg, listener)
      #(reg, listener)
    }
  }
}
