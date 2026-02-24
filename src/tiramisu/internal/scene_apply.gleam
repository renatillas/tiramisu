//// Scene patch applicator — dispatches patches to registered node handlers.
////
//// Built-in node types (mesh, camera, light, empty, audio, debug, instanced
//// mesh) are pre-registered as NodeExtensions via builtin_extensions().
//// External extensions are merged in at register() time.
////
//// apply_patches folds over the patch list, threading NodeApplyContext
//// through each handler call. No hardcoded tag dispatch — all routing goes
//// through the Extensions dict.
////
//// After each patch is applied, all registered AttributeHooks are notified
//// so that external libraries (e.g. physics) can react to the scene lifecycle
//// without scanning the DOM every frame.

import gleam/dict
import gleam/list
import gleam/option
import savoiardi.{type Object3D}
import tiramisu/internal/node_utils

import tiramisu/extension.{type Context, type Extensions}

import tiramisu/internal/registry.{type Registry}
import tiramisu/internal/render_loop.{type RenderLoop}
import tiramisu/internal/scene_patch.{type ScenePatch}

// APPLY -----------------------------------------------------------------------

/// Apply a list of patches to the Three.js scene graph.
/// Returns the updated Registry after all patches are applied.
pub fn apply_patches(
  registry: Registry,
  loop: RenderLoop,
  patches: List(ScenePatch),
  exts: Extensions,
  on_async: fn(fn(Registry) -> Registry) -> Nil,
) -> Registry {
  // Build the on_object_resolved callback that notifies all attribute hooks.
  // Receives the tag from the node handler so the correct tag is reported.
  let on_object_resolved = fn(tag: String, id: String, object: Object3D) {
    notify_resolved(exts, tag, id, object)
  }
  let ctx = extension.Context(registry:, loop:, on_async:, on_object_resolved:)
  let ctx =
    list.fold(patches, ctx, fn(ctx, patch) { apply_patch(ctx, patch, exts) })
  ctx.registry
}

fn apply_patch(
  ctx: Context,
  patch: ScenePatch,
  extensions: Extensions,
) -> Context {
  case patch {
    scene_patch.CreateNode(id:, parent_id:, tag:, attrs:, transform:) -> {
      case extension.get_node(extensions, tag) {
        Ok(handler) -> {
          let new_ctx = handler.create(ctx, id, parent_id, attrs, transform)
          // Notify attribute hooks — object is None for async src= meshes
          let obj =
            registry.get_object(new_ctx.registry, id) |> option.from_result
          notify_create(extensions, tag, id, obj, attrs)
          new_ctx
        }
        Error(Nil) -> ctx
      }
    }

    scene_patch.UpdateNode(id:, tag:, old_attrs:, new_attrs:, transform:) -> {
      case extension.get_node(extensions, tag) {
        Ok(handler) -> {
          let new_ctx = handler.update(ctx, id, old_attrs, new_attrs, transform)
          let obj =
            registry.get_object(new_ctx.registry, id) |> option.from_result
          notify_update(extensions, tag, id, obj, new_attrs)
          new_ctx
        }
        Error(Nil) -> ctx
      }
    }

    scene_patch.Remove(id:) -> {
      case dict.get(ctx.registry.objects, id) {
        Ok(registry.ObjectEntry(kind: registry.ObjectKind(tag:), ..)) -> {
          // Notify attribute hooks BEFORE removal so they can read the object
          notify_remove(extensions, tag, id)
          case extension.get_node(extensions, tag) {
            Ok(handler) -> handler.remove(ctx, id)
            Error(Nil) -> node_utils.default_remove(ctx, id)
          }
        }
        Error(Nil) -> ctx
      }
    }

    scene_patch.Reparent(id:, new_parent_id:) -> {
      let reg = registry.reparent_object(ctx.registry, id, new_parent_id)
      extension.Context(..ctx, registry: reg)
    }
  }
}

// ATTRIBUTE HOOK NOTIFICATIONS ------------------------------------------------

fn notify_create(
  exts: Extensions,
  tag: String,
  id: String,
  object: option.Option(Object3D),
  attrs: dict.Dict(String, String),
) -> Nil {
  list.each(extension.attribute_hooks(exts), fn(hook) {
    hook.on_create(tag, id, object, attrs)
  })
}

fn notify_update(
  exts: Extensions,
  tag: String,
  id: String,
  object: option.Option(Object3D),
  attrs: dict.Dict(String, String),
) -> Nil {
  list.each(extension.attribute_hooks(exts), fn(hook) {
    hook.on_update(tag, id, object, attrs)
  })
}

fn notify_remove(exts: Extensions, tag: String, id: String) -> Nil {
  list.each(extension.attribute_hooks(exts), fn(hook) {
    hook.on_remove(tag, id)
  })
}

fn notify_resolved(
  exts: Extensions,
  tag: String,
  id: String,
  object: Object3D,
) -> Nil {
  list.each(extension.attribute_hooks(exts), fn(hook) {
    hook.on_object_resolved(tag, id, object)
  })
}
