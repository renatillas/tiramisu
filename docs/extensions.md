# Writing Extensions

Tiramisu exposes an extension API for libraries and advanced applications that
need to add new scene nodes or react to attributes across existing nodes.

Most applications only need:

```gleam
let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
```

If you are building on top of Tiramisu, register the built-in extensions plus
your own:

```gleam
let assert Ok(_) =
  tiramisu.register([
    ..tiramisu.builtin_extensions(),
    my_extension(),
  ])
```

The public API for this lives in `tiramisu/dev/extension` and
`tiramisu/dev/runtime`.

## Two extension kinds

Tiramisu has two extension models:

- Node extensions add a new custom scene tag with full lifecycle hooks.
- Attribute extensions observe a set of attributes and react across existing
  node types.

Use a node extension when you want a new element such as
`<tiramisu-particle-system>`.

Use an attribute extension when you want behavior that can apply to multiple
existing nodes, similar to how `tiramisu/material` works for both primitives
and meshes.

## Node extensions

Create node extensions with `extension.node_extension`.

```gleam
pub fn ext() -> extension.Extension {
  extension.node_extension(
    tag: "tiramisu-empty",
    observed_attributes: ["hidden"],
    create: create,
    update: update,
    remove: remove,
  )
}
```

The hooks are:

- `create(runtime, id, parent_id, attributes)` for the first reconciliation.
- `update(runtime, id, parent_id, object, attributes, changed_attributes)` for
  later reconciliations.
- `remove(runtime, id, parent_id, object)` when the node leaves the scene.

`id` is the DOM node id. `parent_id` is the Tiramisu parent id. `attributes`
contains the current attribute map. `changed_attributes` is a semantic diff.
`object` is `Option(Object3D)` because some nodes may reconcile before an
underlying Three.js object has been registered.

In most `create` hooks you will:

1. Build an `Object3D`.
2. Apply initial attributes.
3. Register it with `runtime.add_object`.

Example:

```gleam
fn create(
  runtime: runtime.Runtime,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> #(runtime.Runtime, effect.Effect(extension.Msg)) {
  let object = object.group()
  let hidden = extension.get_bool(attributes, "hidden")
  let _ = object.set_visible(object, !hidden)
  let runtime =
    runtime.add_object(
      runtime,
      id,
      object: object,
      parent_id: parent_id,
      tag: "tiramisu-empty",
    )
  #(runtime, effect.none())
}
```

In `update`, check `changed_attributes` and mutate only what changed:

```gleam
fn update(
  runtime: runtime.Runtime,
  _id: String,
  _parent_id: String,
  object: Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> #(runtime.Runtime, effect.Effect(extension.Msg)) {
  case object {
    Some(object) if extension.has_change(changed_attributes, "hidden") -> {
      let _ = object.set_visible(object, !extension.get_bool(attributes, "hidden"))
      #(runtime, effect.none())
    }
    _ -> #(runtime, effect.none())
  }
}
```

In `remove`, clean up through `runtime.remove_object` unless you have a more
specialized removal flow.

## Attribute extensions

Create attribute extensions with `extension.attribute_extension`.

```gleam
pub fn ext() -> extension.Extension {
  extension.attribute_extension(
    observed_attributes: ["wireframe", "color"],
    on_create: on_create,
    on_update: on_update,
    on_remove: on_remove,
    on_resolved: on_resolved,
  )
}
```

The hooks are:

- `on_create(runtime, tag, id, object, attributes)` when a matching node is
  first seen.
- `on_update(runtime, tag, id, object, attributes, changed_attributes)` when an
  observed attribute changes.
- `on_remove(runtime, id, parent_id, object)` when the node is removed.
- `on_resolved(runtime, tag, id, object, attributes)` after async object
  registration or replacement succeeds.

`on_resolved` matters for nodes that do not have their final object during the
initial `on_create` call, such as asset-backed meshes.

`tiramisu/material` is the best in-tree example of this pattern. It observes
material attributes, reapplies them on updates, and uses `on_resolved` so
loaded meshes receive the material once their object exists.

## Attribute helpers

`tiramisu/dev/extension` includes helpers for common attribute work:

- `extension.has_change(changes, key)`
- `extension.has_any_change(changes, keys)`
- `extension.was_removed(changes, key)`
- `extension.change(changes, key)`
- `extension.bool_change(changes, key)`
- `extension.get_bool(attributes, key)`
- `extension.get(attributes, key, default, parse_fn)`
- `extension.parse_number(string)`

Use `get_bool` for presence-style HTML attributes such as `hidden`,
`wireframe`, or `active`.

Use `get` when an attribute needs parsing and a fallback:

```gleam
let opacity = extension.get(attributes, "opacity", 1.0, extension.parse_number)
```

## Runtime capabilities

Extension code should use `tiramisu/dev/runtime` instead of reaching into
internal renderer state.

The main functions are:

- `runtime.object(runtime, id)` to get a registered object.
- `runtime.add_object(runtime, id, object, parent_id, tag)` to register a new
  object and attach it to the scene graph.
- `runtime.replace_object(runtime, id, object)` to swap an existing object while
  preserving the node slot in the runtime.
- `runtime.remove_object(runtime, id, parent_id, object)` to remove and dispose
  an object.
- `runtime.reparent_object(runtime, id, parent_id)` to move an object in the
  scene graph.
- `runtime.active_camera(runtime)` to read the active camera.
- `runtime.scene(runtime)` and `runtime.threejs_renderer(runtime)` for direct
  access to the owning scene and renderer.

## Async work

If your extension loads models, textures, or any other async resource, return
an effect from `extension.request(owner, key, task)`.

```gleam
extension.request(
  extension.NodeOwner(id),
  extension.request_key("material:textures"),
  task,
)
```

The `owner` and `key` pair is how Tiramisu discards stale async results after a
later reconciliation. Use:

- `SceneOwner` for scene-level work.
- `NodeOwner(id)` for node-specific work.
- `CustomOwner(name)` for other library-managed scopes.

The promise must resolve to `List(extension.RuntimeAction)`. Build actions with
helpers such as:

- `extension.action(fn(runtime) { ... })`
- `extension.register_object(id, parent_id, tag, object)`
- `extension.replace_object(id, object)`
- `extension.remove_object(id, parent_id, object)`
- `extension.set_background_texture(texture)`
- `extension.set_background_cube_texture(texture)`

Typical async flow:

1. Start async work from `create`, `update`, or an attribute hook.
2. Return `extension.request(...)`.
3. Resolve the promise into runtime actions.
4. Let Tiramisu apply those actions only if the owner still exists and the
   request key is still current.

## Design guidance

- Register your extension alongside `tiramisu.builtin_extensions()`, not instead
  of it, unless you are deliberately replacing core behavior.
- Keep the boundary narrow. Parse attributes in the extension layer and work
  with validated values internally.
- Prefer explicit `observed_attributes` lists so updates stay cheap and
  predictable.
- Treat `update` and `on_update` as incremental reconciliation hooks. Avoid
  recreating objects unless the underlying resource truly changed.
- Use `on_resolved` for behavior that depends on async-created objects.
- Model library-specific failures with your own error types at the public API
  boundary and keep runtime actions focused on scene mutation.

## Existing examples

The built-in modules are the current reference implementations:

- `src/tiramisu/empty.gleam` for a minimal node extension.
- `src/tiramisu/material.gleam` for a cross-cutting attribute extension.
- `src/tiramisu/mesh.gleam` for async object loading.
