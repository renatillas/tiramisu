//// Empty scene node used for hierarchy and shared transforms.

import gleam/dict.{type Dict}
import gleam/option
import lustre/attribute
import savoiardi.{type Object3D}

import tiramisu/dev/extension
import tiramisu/dev/runtime
import tiramisu/internal/node

/// The custom element tag used for empty group nodes.
@internal
pub const tag = "tiramisu-empty"

/// Hide or show the group and its descendants.
///
/// This uses the standard HTML `hidden` attribute syntax, but Tiramisu applies
/// it to scene visibility rather than DOM layout.
pub const hidden = attribute.hidden

/// Internal node extension for empty elements.
pub fn extension() -> extension.Extension {
  let observed_attributes = ["hidden"]
  extension.Node(tag:, observed_attributes:, create:, update:, remove:)
  |> extension.NodeExtension
}

fn create(
  ctx: extension.Context,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> extension.Context {
  let object = savoiardi.create_group()
  set_hidden(object, attributes)
  let next_runtime =
    runtime.add_object(ctx.runtime, id, object:, parent_id:, tag:)
  extension.Context(..ctx, runtime: next_runtime)
}

fn set_hidden(group: Object3D, attributes: Dict(String, String)) -> Nil {
  let hide = node.get_bool(attributes, "hidden")
  savoiardi.set_object_visible(group, !hide)
}

fn update(
  ctx: extension.Context,
  _id: String,
  _parent_id: String,
  object: option.Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> extension.Context {
  case object {
    option.Some(group) -> {
      case extension.has_change(changed_attributes, "hidden") {
        True -> set_hidden(group, attributes)
        False -> Nil
      }
      ctx
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
