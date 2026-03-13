import gleam/bool
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/javascript/promise.{type Promise}
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import savoiardi.{type Camera, type Object3D}
import tiramisu/dev/extension
import tiramisu/dev/runtime
import tiramisu/internal/element
import tiramisu/internal/renderer
import tiramisu/scene as public_scene

pub type Node {
  Node(
    key: String,
    tag: String,
    children: List(Node),
    attributes: dict.Dict(String, String),
  )
}

pub type Patch {
  CreateNode(
    id: String,
    parent_id: String,
    tag: String,
    attributes: dict.Dict(String, String),
  )
  UpdateNode(
    id: String,
    parent_id: String,
    tag: String,
    attributes: dict.Dict(String, String),
    changed_attributes: extension.AttributeChanges,
  )
  Remove(id: String)
  Reparent(id: String, new_parent_id: String)
}

pub type Runtime {
  Runtime(
    runtime: runtime.Runtime,
    scene_id: String,
    root: element.HtmlElement,
    previous_nodes: List(Node),
    extensions: extension.Extensions,
  )
}

pub fn initialize(
  shadow_root: Dynamic,
  host: element.HtmlElement,
  extensions: extension.Extensions,
  on_dom_mutated: fn() -> Nil,
  spawn: fn(
    extension.AsyncScope,
    extension.AsyncKey,
    Promise(extension.AsyncEffect),
  ) ->
    promise.Promise(Nil),
  on_tick: fn(Float) -> Nil,
) -> promise.Promise(Runtime) {
  let #(scene_id, root) = find_root(host)
  let renderer_runtime =
    renderer.initialize(shadow_root, host, scene_id, on_tick)

  element.observe_mutations(
    host,
    list.append(
      public_scene.observed_attributes(),
      extensions.observed_attributes,
    ),
    on_dom_mutated,
  )

  let renderer_runtime = apply_scene_attributes(renderer_runtime, root, spawn)

  let initial_nodes = parse(root, extensions)
  let runtime =
    reconcile_nodes(
      renderer_runtime,
      [],
      initial_nodes,
      scene_id,
      extensions,
      spawn,
    )
  use runtime <- promise.map(runtime)

  Runtime(
    runtime:,
    scene_id:,
    root:,
    previous_nodes: initial_nodes,
    extensions:,
  )
}

pub fn reconcile(
  rt: Runtime,
  spawn: fn(
    extension.AsyncScope,
    extension.AsyncKey,
    Promise(extension.AsyncEffect),
  ) ->
    promise.Promise(Nil),
) -> promise.Promise(Runtime) {
  let new_nodes = parse(rt.root, rt.extensions)
  let renderer_runtime = apply_scene_attributes(rt.runtime, rt.root, spawn)
  let runtime =
    reconcile_nodes(
      renderer_runtime,
      rt.previous_nodes,
      new_nodes,
      rt.scene_id,
      rt.extensions,
      spawn,
    )
  use runtime <- promise.map(runtime)

  Runtime(..rt, runtime:, previous_nodes: new_nodes)
}

pub fn apply_async_effect(rt: Runtime, effect: extension.AsyncEffect) -> Runtime {
  case effect {
    extension.NoOp -> rt

    extension.RegisterObject(id:, parent_id:, tag:, object:) -> {
      let runtime = runtime.add_object(rt.runtime, id, object, parent_id, tag)
      Runtime(..rt, runtime:)
    }

    extension.ReplaceObject(id:, object:) -> {
      let runtime = runtime.replace_object(rt.runtime, id, object)
      Runtime(..rt, runtime:)
    }

    extension.SetBackgroundTexture(texture:) -> {
      let _ =
        savoiardi.set_scene_background_texture(
          runtime.scene(rt.runtime),
          texture,
        )
      rt
    }

    extension.SetBackgroundCubeTexture(texture:) -> {
      let _ =
        savoiardi.set_scene_background_cube_texture(
          runtime.scene(rt.runtime),
          texture,
        )
      rt
    }
  }
}

pub fn has_scope(_rt: Runtime, scope: extension.AsyncScope) -> Bool {
  case scope {
    extension.SceneScope -> True
    extension.NodeScope(id) ->
      case element.find(id) {
        Ok(_) -> True
        Error(Nil) -> False
      }
    extension.CustomScope(_) -> True
  }
}

pub fn renderer(rt: Runtime) -> savoiardi.Renderer {
  rt.runtime |> runtime.threejs_renderer
}

pub fn root_scene(rt: Runtime) -> savoiardi.Scene {
  rt.runtime |> runtime.scene
}

pub fn active_camera(rt: Runtime) -> Result(Camera, Nil) {
  rt.runtime |> runtime.active_camera
}

pub fn dispatch_tick(
  runtime: Runtime,
  delta_ms: Float,
  timestamp_ms: Int,
) -> Nil {
  element.dispatch_tick(runtime.scene_id, delta_ms, timestamp_ms)
}

pub fn parse(
  root: element.HtmlElement,
  extensions: extension.Extensions,
) -> List(Node) {
  parse_children(root, extensions)
}

pub fn diff(
  old_nodes: List(Node),
  new_nodes: List(Node),
  scene_id: String,
) -> List(Patch) {
  let old_map = flatten(dict.new(), old_nodes, scene_id)
  let #(patches, remaining) = walk(new_nodes, scene_id, old_map, [])

  dict.fold(remaining, patches, fn(acc, id, _entry) { [Remove(id:), ..acc] })
  |> list.reverse
}

pub fn apply(
  patches: List(Patch),
  starting_context,
  extensions: extension.Extensions,
) -> promise.Promise(extension.Context) {
  use context, patch <- list.fold(patches, promise.resolve(starting_context))
  use context <- promise.await(context)
  apply_patch(context, patch, extensions)
}

fn reconcile_nodes(
  runtime: runtime.Runtime,
  previous_nodes: List(Node),
  next_nodes: List(Node),
  scene_id: String,
  extensions: extension.Extensions,
  spawn: fn(
    extension.AsyncScope,
    extension.AsyncKey,
    Promise(extension.AsyncEffect),
  ) ->
    promise.Promise(Nil),
) -> promise.Promise(runtime.Runtime) {
  let starting_context = create_context(runtime, extensions, spawn)

  let final_context =
    diff(previous_nodes, next_nodes, scene_id)
    |> apply(starting_context, extensions)

  use final_context <- promise.map(final_context)

  final_context.runtime
}

type Background {
  None
  Color(Int)
  Texture(String)
  Equirectangular(String)
  Cube(List(String))
}

type BackgroundColorSpace {
  SRGB
  LinearSRGB
}

type Fog {
  NoFog
  LinearFog(Int, Float, Float)
  Exp2Fog(Int, Float)
}

fn apply_scene_attributes(
  runtime: runtime.Runtime,
  root: element.HtmlElement,
  spawn: fn(
    extension.AsyncScope,
    extension.AsyncKey,
    Promise(extension.AsyncEffect),
  ) ->
    promise.Promise(Nil),
) -> runtime.Runtime {
  let background_color_space =
    parse_background_color_space(
      element.attribute(root, "background-color-space") |> result.unwrap("srgb"),
    )
  let runtime =
    runtime
    |> apply_background(
      element.attribute(root, "background") |> result.unwrap("000000"),
      background_color_space,
      spawn,
    )
    |> apply_fog(element.attribute(root, "fog") |> result.unwrap("none"))

  runtime
}

fn apply_background(
  runtime: runtime.Runtime,
  background: String,
  background_color_space: BackgroundColorSpace,
  spawn: fn(
    extension.AsyncScope,
    extension.AsyncKey,
    Promise(extension.AsyncEffect),
  ) ->
    promise.Promise(Nil),
) -> runtime.Runtime {
  case parse_background(background) {
    None -> {
      let _ = savoiardi.clear_scene_background(runtime.scene(runtime))
      runtime
    }

    Color(color) -> {
      let _ =
        savoiardi.set_scene_background_color(runtime.scene(runtime), color)
      runtime
    }

    Texture(url) -> {
      spawn(
        extension.SceneScope,
        extension.async_key("background"),
        load_background_texture(url, background_color_space),
      )
      runtime
    }

    Equirectangular(url) -> {
      spawn(
        extension.SceneScope,
        extension.async_key("background"),
        load_equirectangular_background(url, background_color_space),
      )
      runtime
    }

    Cube(urls) -> {
      spawn(
        extension.SceneScope,
        extension.async_key("background"),
        load_cube_background(urls),
      )
      runtime
    }
  }
}

fn load_background_texture(
  url: String,
  background_color_space: BackgroundColorSpace,
) -> promise.Promise(extension.AsyncEffect) {
  use result <- promise.await(savoiardi.load_texture(url))
  case result {
    Ok(texture) -> {
      savoiardi.set_texture_color_space(
        texture,
        to_savoiardi_color_space(background_color_space),
      )
      promise.resolve(extension.SetBackgroundTexture(texture))
    }
    Error(Nil) -> promise.resolve(extension.NoOp)
  }
}

fn load_equirectangular_background(
  url: String,
  background_color_space: BackgroundColorSpace,
) -> promise.Promise(extension.AsyncEffect) {
  use result <- promise.await(savoiardi.load_equirectangular_texture(url))
  case result {
    Ok(texture) -> {
      savoiardi.set_texture_color_space(
        texture,
        to_savoiardi_color_space(background_color_space),
      )
      promise.resolve(extension.SetBackgroundTexture(texture))
    }
    Error(Nil) -> promise.resolve(extension.NoOp)
  }
}

fn load_cube_background(
  urls: List(String),
) -> promise.Promise(extension.AsyncEffect) {
  use result <- promise.await(savoiardi.load_cube_texture(urls))
  case result {
    Ok(texture) -> promise.resolve(extension.SetBackgroundCubeTexture(texture))
    Error(Nil) -> promise.resolve(extension.NoOp)
  }
}

fn parse_background(background: String) -> Background {
  case background {
    "" | "none" -> None
    _ ->
      case string.starts_with(background, "texture:") {
        True -> Texture(string.drop_start(from: background, up_to: 8))
        False ->
          case string.starts_with(background, "equirectangular:") {
            True ->
              Equirectangular(string.drop_start(from: background, up_to: 16))

            False ->
              case string.starts_with(background, "cube:") {
                True ->
                  parse_cube_background(string.drop_start(
                    from: background,
                    up_to: 5,
                  ))

                False ->
                  case
                    background
                    |> int.base_parse(16)
                  {
                    Ok(color) -> Color(color)
                    Error(Nil) -> None
                  }
              }
          }
      }
  }
}

fn parse_cube_background(encoded: String) -> Background {
  let urls = string.split(encoded, "|")
  case list.length(urls) == 6 {
    True -> Cube(urls)
    False -> None
  }
}

fn parse_background_color_space(encoded: String) -> BackgroundColorSpace {
  case string.lowercase(encoded) {
    "linear-srgb" -> LinearSRGB
    _ -> SRGB
  }
}

fn to_savoiardi_color_space(space: BackgroundColorSpace) -> savoiardi.ColorSpace {
  case space {
    SRGB -> savoiardi.SRGBColorSpace
    LinearSRGB -> savoiardi.LinearSRGBColorSpace
  }
}

fn apply_fog(runtime: runtime.Runtime, fog: String) -> runtime.Runtime {
  case parse_fog(fog) {
    NoFog -> {
      let _ = savoiardi.clear_scene_fog(runtime.scene(runtime))
      runtime
    }

    LinearFog(color, near, far) -> {
      let _ = savoiardi.set_scene_fog(runtime.scene(runtime), color, near, far)
      runtime
    }

    Exp2Fog(color, density) -> {
      let _ =
        savoiardi.set_scene_fog_exp2(runtime.scene(runtime), color, density)
      runtime
    }
  }
}

fn parse_fog(fog: String) -> Fog {
  case fog {
    "" | "none" -> NoFog
    _ -> {
      let parts = string.split(fog, "|")
      case parts {
        ["linear:" <> color, near, far] -> {
          case int.base_parse(color, 16), float.parse(near), float.parse(far) {
            Ok(color), Ok(near), Ok(far) -> LinearFog(color, near, far)
            _, _, _ -> NoFog
          }
        }

        ["exp2:" <> color, density] -> {
          case int.base_parse(color, 16), float.parse(density) {
            Ok(color), Ok(density) -> Exp2Fog(color, density)
            _, _ -> NoFog
          }
        }

        _ -> NoFog
      }
    }
  }
}

fn create_context(
  runtime: runtime.Runtime,
  extensions: extension.Extensions,
  spawn: fn(
    extension.AsyncScope,
    extension.AsyncKey,
    Promise(extension.AsyncEffect),
  ) ->
    promise.Promise(Nil),
) -> extension.Context {
  extension.Context(runtime:, spawn:, on_object_resolved: fn(tag, id, object) {
    case element.find(id) {
      Ok(node) -> {
        let attributes = element.attributes(node)
        spawn(
          extension.NodeScope(id),
          extension.async_key("object-resolved"),
          promise.map(
            notify_object_resolved(
              extensions,
              runtime,
              spawn,
              tag,
              id,
              object,
              attributes,
            ),
            fn(_) { extension.NoOp },
          ),
        )
        Nil
      }

      Error(Nil) -> Nil
    }
  })
}

fn find_root(host: element.HtmlElement) -> #(String, element.HtmlElement) {
  case find_scene_element(element.children(host)) {
    Ok(root) -> {
      let scene_id = element.attribute(root, "id") |> result.unwrap("")
      #(scene_id, root)
    }

    Error(Nil) -> #("", host)
  }
}

fn find_scene_element(
  children: List(element.HtmlElement),
) -> Result(element.HtmlElement, Nil) {
  list.fold(children, Error(Nil), fn(found, child) {
    case found {
      Ok(_) -> found

      Error(Nil) ->
        case string.lowercase(element.tag(child)) == public_scene.tag {
          True -> Ok(child)
          False -> Error(Nil)
        }
    }
  })
}

fn parse_element(
  item: element.HtmlElement,
  extensions: extension.Extensions,
) -> Node {
  let tag = element.tag(item) |> string.lowercase
  let attributes = element.attributes(item)
  let key = dict.get(attributes, "id") |> result.unwrap("")
  let node_attributes = dict.delete(attributes, "id")

  case extension.get_node(extensions, tag) {
    Ok(_) ->
      Node(
        key:,
        tag:,
        children: parse_children(item, extensions),
        attributes: node_attributes,
      )

    Error(Nil) ->
      Node(
        key:,
        tag:,
        children: parse_children(item, extensions),
        attributes: dict.new(),
      )
  }
}

fn parse_children(
  root: element.HtmlElement,
  extensions: extension.Extensions,
) -> List(Node) {
  use child <- list.map(element.children(root))
  parse_element(child, extensions)
}

fn flatten(
  map: dict.Dict(String, #(Node, String)),
  nodes: List(Node),
  parent_id: String,
) -> dict.Dict(String, #(Node, String)) {
  list.fold(nodes, map, fn(acc, node) {
    case node.key {
      "" -> flatten(acc, node.children, parent_id)
      id -> {
        let acc = dict.insert(acc, id, #(node, parent_id))
        flatten(acc, node.children, id)
      }
    }
  })
}

fn walk(
  nodes: List(Node),
  parent_id: String,
  old_map: dict.Dict(String, #(Node, String)),
  patches: List(Patch),
) -> #(List(Patch), dict.Dict(String, #(Node, String))) {
  list.fold(nodes, #(patches, old_map), fn(acc, node) {
    let #(patches, old_map) = acc
    case node.key {
      "" -> walk(node.children, parent_id, old_map, patches)
      id -> {
        case dict.get(old_map, id) {
          Ok(#(old_node, old_parent_id)) -> {
            let old_map = dict.delete(old_map, id)
            let patches = case old_parent_id == parent_id {
              True -> patches
              False -> [Reparent(id:, new_parent_id: parent_id), ..patches]
            }
            let patches = diff_node(old_node, node, id, parent_id, patches)
            walk(node.children, id, old_map, patches)
          }

          Error(Nil) -> {
            let patches = [
              CreateNode(
                id:,
                parent_id:,
                tag: node.tag,
                attributes: node.attributes,
              ),
              ..patches
            ]
            walk(node.children, id, old_map, patches)
          }
        }
      }
    }
  })
}

fn diff_node(
  old: Node,
  new: Node,
  id: String,
  parent_id: String,
  patches: List(Patch),
) -> List(Patch) {
  use <- bool.guard(old.tag != new.tag, [Remove(id:), ..patches])
  use <- bool.guard(old.attributes == new.attributes, patches)

  [
    UpdateNode(
      id:,
      parent_id:,
      tag: new.tag,
      attributes: new.attributes,
      changed_attributes: difference_of_attributes(
        old.attributes,
        new.attributes,
      ),
    ),
    ..patches
  ]
}

fn difference_of_attributes(
  old_attributes: dict.Dict(String, String),
  new_attributes: dict.Dict(String, String),
) -> extension.AttributeChanges {
  let changes =
    dict.fold(old_attributes, dict.new(), fn(acc, key, old_value) {
      case dict.get(new_attributes, key) {
        Ok(new_value) if new_value == old_value -> acc
        Ok(new_value) -> dict.insert(acc, key, extension.Updated(new_value))
        Error(Nil) -> dict.insert(acc, key, extension.Removed)
      }
    })

  dict.fold(new_attributes, changes, fn(acc, key, new_value) {
    case dict.get(old_attributes, key) {
      Ok(_) -> acc
      Error(Nil) -> dict.insert(acc, key, extension.Added(new_value))
    }
  })
}

fn apply_patch(
  context: extension.Context,
  patch: Patch,
  extensions: extension.Extensions,
) -> promise.Promise(extension.Context) {
  case patch {
    CreateNode(id:, parent_id:, tag:, attributes:) -> {
      case extension.get_node(extensions, tag) {
        Ok(handler) -> {
          let new_context = handler.create(context, id, parent_id, attributes)
          let object = find_object(new_context.runtime, id)
          let promise =
            notify_create(extensions, context, tag, id, object, attributes)
          use _ <- promise.map(promise)
          new_context
        }

        Error(Nil) -> promise.resolve(context)
      }
    }

    UpdateNode(id:, parent_id:, tag:, attributes:, changed_attributes:) -> {
      case extension.get_node(extensions, tag) {
        Ok(handler) -> {
          let object = find_object(context.runtime, id)
          let new_context =
            handler.update(
              context,
              id,
              parent_id,
              object,
              attributes,
              changed_attributes,
            )
          let promise =
            notify_update(
              extensions,
              context,
              tag,
              id,
              object,
              attributes,
              changed_attributes,
            )
          use _ <- promise.await(promise)
          promise.resolve(new_context)
        }

        Error(Nil) -> promise.resolve(context)
      }
    }

    Remove(id:) -> {
      case dict.get(runtime.entries(context.runtime), id) {
        Ok(runtime.ObjectEntry(parent_id:, tag:, object:)) -> {
          let promise =
            notify_remove(extensions, context, id, parent_id, object)
          use _ <- promise.await(promise)
          case extension.get_node(extensions, tag) {
            Ok(handler) ->
              promise.resolve(handler.remove(context, id, parent_id, object))
            Error(Nil) -> promise.resolve(context)
          }
        }

        Error(Nil) -> promise.resolve(context)
      }
    }

    Reparent(id:, new_parent_id:) -> {
      let next_runtime =
        runtime.reparent_object(context.runtime, id, new_parent_id)
      promise.resolve(extension.Context(..context, runtime: next_runtime))
    }
  }
}

fn notify_create(
  extensions: extension.Extensions,
  context,
  tag: String,
  id: String,
  object: option.Option(Object3D),
  attributes: dict.Dict(String, String),
) -> promise.Promise(Nil) {
  list.map(extension.attribute_hooks(extensions), fn(hook) {
    hook.on_create(context, tag, id, object, attributes)
  })
  |> promise.await_list
  |> promise.map(fn(_) { Nil })
}

fn notify_update(
  extensions: extension.Extensions,
  context,
  tag: String,
  id: String,
  object: option.Option(Object3D),
  attributes: dict.Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> promise.Promise(Nil) {
  list.map(extension.attribute_hooks(extensions), fn(hook) {
    hook.on_update(context, tag, id, object, attributes, changed_attributes)
  })
  |> promise.await_list
  |> promise.map(fn(_) { Nil })
}

fn notify_remove(
  extensions: extension.Extensions,
  context: extension.Context,
  id: String,
  parent_id: String,
  object: Object3D,
) -> promise.Promise(Nil) {
  list.map(extension.attribute_hooks(extensions), fn(hook) {
    hook.on_remove(context, id, parent_id, object)
  })
  |> promise.await_list
  |> promise.map(fn(_) { Nil })
}

fn notify_object_resolved(
  extensions: extension.Extensions,
  runtime: runtime.Runtime,
  spawn: fn(
    extension.AsyncScope,
    extension.AsyncKey,
    Promise(extension.AsyncEffect),
  ) ->
    Promise(Nil),
  tag: String,
  id: String,
  object: Object3D,
  attributes: dict.Dict(String, String),
) -> promise.Promise(Nil) {
  list.map(extension.attribute_hooks(extensions), fn(hook) {
    hook.on_object_resolved(runtime, spawn, tag, id, object, attributes)
  })
  |> promise.await_list
  |> promise.map(fn(_) { Nil })
}

fn find_object(runtime: runtime.Runtime, id: String) -> option.Option(Object3D) {
  runtime.object(runtime, id)
  |> option.from_result
}
