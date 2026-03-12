import gleam/bool
import gleam/dict
import gleam/dynamic.{type Dynamic}
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
    renderer: renderer.Runtime,
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
  on_async: fn(Promise(fn(runtime.Runtime) -> runtime.Runtime)) -> Nil,
  on_tick: fn(Float, Int) -> Nil,
) -> Runtime {
  let #(scene_id, root) = find_root(host)
  let renderer_runtime =
    renderer.initialize(shadow_root, host, scene_id, on_async, on_tick)

  element.observe_mutations(host, extensions.observed_attributes, on_dom_mutated)

  let initial_nodes = parse(root, extensions)
  let renderer_runtime =
    reconcile_nodes(
      renderer_runtime,
      [],
      initial_nodes,
      scene_id,
      extensions,
      on_async,
    )

  Runtime(
    renderer: renderer_runtime,
    scene_id:,
    root:,
    previous_nodes: initial_nodes,
    extensions:,
  )
}

pub fn reconcile(
  runtime: Runtime,
  on_async: fn(Promise(fn(runtime.Runtime) -> runtime.Runtime)) -> Nil,
) -> Runtime {
  let new_nodes = parse(runtime.root, runtime.extensions)
  let renderer_runtime =
    reconcile_nodes(
      runtime.renderer,
      runtime.previous_nodes,
      new_nodes,
      runtime.scene_id,
      runtime.extensions,
      on_async,
    )

  Runtime(..runtime, renderer: renderer_runtime, previous_nodes: new_nodes)
}

pub fn apply_runtime_transform(
  runtime: Runtime,
  transform: fn(runtime.Runtime) -> runtime.Runtime,
) -> Runtime {
  let renderer_runtime = renderer.apply_transform(runtime.renderer, transform)
  Runtime(..runtime, renderer: renderer_runtime)
}

pub fn renderer(runtime: Runtime) -> savoiardi.Renderer {
  renderer.renderer(runtime.renderer)
}

pub fn root_scene(runtime: Runtime) -> savoiardi.Scene {
  renderer.scene(runtime.renderer)
}

pub fn active_camera(runtime: Runtime) -> Result(Camera, Nil) {
  renderer.runtime(runtime.renderer) |> runtime.active_camera
}

pub fn host(runtime: Runtime) -> element.HtmlElement {
  renderer.host(runtime.renderer)
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
  runtime: runtime.Runtime,
  patches: List(Patch),
  extensions: extension.Extensions,
  on_async: fn(Promise(fn(runtime.Runtime) -> runtime.Runtime)) -> Nil,
) -> runtime.Runtime {
  let context = patch_context(runtime, extensions, on_async)
  list.fold(patches, context, fn(context, patch) {
    apply_patch(context, patch, extensions)
  })
  .runtime
}

fn reconcile_nodes(
  renderer_runtime: renderer.Runtime,
  previous_nodes: List(Node),
  next_nodes: List(Node),
  scene_id: String,
  extensions: extension.Extensions,
  on_async: fn(Promise(fn(runtime.Runtime) -> runtime.Runtime)) -> Nil,
) -> renderer.Runtime {
  let patches = diff(previous_nodes, next_nodes, scene_id)
  let next_runtime =
    apply(
      renderer.runtime(renderer_runtime),
      patches,
      extensions,
      on_async,
    )

  renderer.with_runtime(renderer_runtime, next_runtime)
}

fn patch_context(
  runtime: runtime.Runtime,
  extensions: extension.Extensions,
  on_async: fn(Promise(fn(runtime.Runtime) -> runtime.Runtime)) -> Nil,
) -> extension.Context {
  let callback_context =
    extension.Context(
      runtime:,
      on_async:,
      on_object_resolved: fn(_, _, _) { Nil },
    )

  extension.Context(
    runtime:,
    on_async:,
    on_object_resolved: fn(tag, id, object) {
      case element.find(id) {
        Ok(node) -> {
          let attributes = element.attributes(node) |> dict.delete("id")
          notify_object_resolved(
            extensions,
            callback_context,
            tag,
            id,
            object,
            attributes,
          )
        }

        Error(Nil) -> Nil
      }
    },
  )
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
) -> extension.Context {
  case patch {
    CreateNode(id:, parent_id:, tag:, attributes:) -> {
      case extension.get_node(extensions, tag) {
        Ok(handler) -> {
          let new_context = handler.create(context, id, parent_id, attributes)
          let object = find_object(new_context.runtime, id)
          notify_create(extensions, context, tag, id, object, attributes)
          new_context
        }

        Error(Nil) -> context
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
          notify_update(
            extensions,
            context,
            tag,
            id,
            object,
            attributes,
            changed_attributes,
          )
          new_context
        }

        Error(Nil) -> context
      }
    }

    Remove(id:) -> {
      case dict.get(runtime.entries(context.runtime), id) {
        Ok(runtime.ObjectEntry(parent_id:, tag:, object:)) -> {
          notify_remove(extensions, context, id, parent_id, object)
          case extension.get_node(extensions, tag) {
            Ok(handler) -> handler.remove(context, id, parent_id, object)
            Error(Nil) -> context
          }
        }

        Error(Nil) -> context
      }
    }

    Reparent(id:, new_parent_id:) -> {
      let next_runtime = runtime.reparent_object(context.runtime, id, new_parent_id)
      extension.Context(..context, runtime: next_runtime)
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
) -> Nil {
  list.each(extension.attribute_hooks(extensions), fn(hook) {
    hook.on_create(context, tag, id, object, attributes)
  })
}

fn notify_update(
  extensions: extension.Extensions,
  context,
  tag: String,
  id: String,
  object: option.Option(Object3D),
  attributes: dict.Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> Nil {
  list.each(extension.attribute_hooks(extensions), fn(hook) {
    hook.on_update(
      context,
      tag,
      id,
      object,
      attributes,
      changed_attributes,
    )
  })
}

fn notify_remove(
  extensions: extension.Extensions,
  context: extension.Context,
  id: String,
  parent_id: String,
  object: Object3D,
) -> Nil {
  list.each(extension.attribute_hooks(extensions), fn(hook) {
    hook.on_remove(context, id, parent_id, object)
  })
}

fn notify_object_resolved(
  extensions: extension.Extensions,
  context: extension.Context,
  tag: String,
  id: String,
  object: Object3D,
  attributes: dict.Dict(String, String),
) -> Nil {
  list.each(extension.attribute_hooks(extensions), fn(hook) {
    hook.on_object_resolved(context, tag, id, object, attributes)
  })
}

fn find_object(
  runtime: runtime.Runtime,
  id: String,
) -> option.Option(Object3D) {
  runtime.object(runtime, id)
  |> option.from_result
}
