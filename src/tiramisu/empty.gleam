//// Empty scene node used for hierarchy and shared transforms.

import gleam/dict.{type Dict}
import gleam/option
import lustre/attribute
import lustre/effect
import savoiardi.{type Object3D}

import tiramisu/dev/extension
import tiramisu/dev/runtime.{type Runtime}

/// The custom element tag used for empty group nodes.
@internal
pub const tag = "tiramisu-empty"

/// Hide or show the group and its descendants.
///
/// This uses the standard HTML `hidden` attribute syntax, but Tiramisu applies
/// it to scene visibility rather than DOM layout.
pub const hidden = attribute.hidden

fn create(
  runtime: Runtime,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> #(Runtime, effect.Effect(extension.Msg)) {
  let object = savoiardi.create_group()
  set_hidden(object, attributes)
  let next_runtime = runtime.add_object(runtime, id, object:, parent_id:, tag:)
  #(next_runtime, effect.none())
}

fn set_hidden(group: Object3D, attributes: Dict(String, String)) -> Nil {
  let hide = extension.get_bool(attributes, "hidden")
  savoiardi.set_object_visible(group, !hide)
}

fn update(
  ctx: Runtime,
  _id: String,
  _parent_id: String,
  object: option.Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> #(Runtime, effect.Effect(extension.Msg)) {
  let _ = case object {
    option.Some(group) -> {
      case extension.has_change(changed_attributes, "hidden") {
        True -> set_hidden(group, attributes)
        False -> Nil
      }
    }
    option.None -> Nil
  }

  #(ctx, effect.none())
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

/// Build the internal extension used to reconcile empty group nodes.
///
/// Most applications should not call this directly; use
/// `tiramisu.builtin_extensions()` instead.
pub fn ext() -> extension.Extension {
  let observed_attributes = ["hidden"]
  extension.node_extension(
    tag:,
    observed_attributes:,
    create:,
    update:,
    remove:,
  )
}
