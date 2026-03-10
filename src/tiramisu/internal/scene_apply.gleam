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

import gleam/dict.{type Dict}
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import savoiardi.{type Object3D}

import tiramisu/dev/extension.{type Context, type Extensions}

import tiramisu/dev/loop.{type RenderLoop}
import tiramisu/dev/registry.{type Registry}
import tiramisu/internal/scene_patch.{type ScenePatch}

// APPLY -----------------------------------------------------------------------

/// Apply a list of patches to the Three.js scene graph.
/// Returns the updated Registry after all patches are applied.
pub fn apply_patches(
  registry: Registry,
  loop: RenderLoop,
  patches: List(ScenePatch),
  exts: Extensions,
  on_async: fn(promise.Promise(fn(Registry) -> Registry)) -> Nil,
) -> Registry {
  // Build the on_object_resolved callback that notifies all attribute hooks.
  let ctx =
    extension.Context(
      registry:,
      loop:,
      on_async:,
      on_object_resolved: fn(_, _, _) { Nil },
    )
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
    scene_patch.CreateNode(id:, parent_id:, tag:, attributes:) -> {
      case extension.get_node(extensions, tag) {
        Ok(handler) -> {
          let new_ctx = handler.create(ctx, id, parent_id, attributes)
          // Notify attribute hooks — object is None for async src= meshes
          let obj =
            registry.get(new_ctx.registry, id)
            |> result.map(fn(entry) { entry.object })
            |> option.from_result
          notify_create(extensions, ctx, tag, id, obj, attributes)
          new_ctx
        }
        Error(Nil) -> ctx
      }
    }

    scene_patch.UpdateNode(
      id:,
      parent_id:,
      tag:,
      attributes:,
      changed_attributes:,
    ) -> {
      case extension.get_node(extensions, tag) {
        Ok(handler) -> {
          let obj =
            registry.get(ctx.registry, id)
            |> result.map(fn(entry) { entry.object })
            |> option.from_result
          let new_ctx =
            handler.update(
              ctx,
              id,
              parent_id,
              obj,
              attributes,
              changed_attributes,
            )
          notify_update(
            extensions,
            ctx,
            tag,
            id,
            obj,
            attributes,
            changed_attributes,
          )
          new_ctx
        }
        Error(Nil) -> ctx
      }
    }

    scene_patch.Remove(id:) -> {
      case dict.get(ctx.registry.objects, id) {
        Ok(registry.ObjectEntry(parent_id:, tag:, object:)) -> {
          // Notify attribute hooks BEFORE removal so they can read the object
          notify_remove(extensions, ctx, id, parent_id, object)
          case extension.get_node(extensions, tag) {
            Ok(handler) -> handler.remove(ctx, id, parent_id, object)
            Error(Nil) -> ctx
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
  context,
  tag: String,
  id: String,
  object: option.Option(Object3D),
  attrs: Dict(String, String),
) -> Nil {
  list.each(extension.attribute_hooks(exts), fn(hook) {
    hook.on_create(context, tag, id, object, attrs)
  })
}

fn notify_update(
  exts: Extensions,
  context,
  tag: String,
  id: String,
  object: option.Option(Object3D),
  attrs: Dict(String, String),
  changed_attributes: set.Set(String),
) -> Nil {
  list.each(extension.attribute_hooks(exts), fn(hook) {
    hook.on_update(context, tag, id, object, attrs, changed_attributes)
  })
}

fn notify_remove(
  exts: Extensions,
  context: Context,
  id: String,
  parent_id: String,
  object: Object3D,
) -> Nil {
  list.each(extension.attribute_hooks(exts), fn(hook) {
    hook.on_remove(context, id, parent_id, object)
  })
}
