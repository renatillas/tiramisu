import gleam/dict.{type Dict}
import gleam/option
import gleam/result
import gleam/set.{type Set}
import lustre/attribute
import savoiardi.{type Object3D}
import tiramisu/transform

import tiramisu/dev/extension.{type Context}
import tiramisu/dev/registry
import tiramisu/internal/console
import tiramisu/internal/node

@internal
pub const tag = "tiramisu-empty"

pub fn extension() {
  let observed_attributes = set.from_list(["transform", "hidden"])
  extension.Node(tag:, observed_attributes:, create:, update:, remove:)
  |> extension.NodeExtension
}

pub fn transform(transform: transform.Transform) -> attribute.Attribute(msg) {
  attribute.attribute("transform", transform.to_string(transform))
}

fn create(
  ctx: Context,
  id: String,
  parent_id: String,
  attributes: Dict(String, String),
) -> Context {
  let group = savoiardi.create_group()
  case node.get_transform(attributes) {
    Ok(transform) -> node.set_transform(group, transform)
    Error(Nil) -> Nil
  }
  set_hidden(group, attributes)
  let registry =
    registry.register_and_add_object(ctx.registry, id, group, parent_id, tag)
  extension.Context(..ctx, registry:)
}

fn set_hidden(group: Object3D, attributes: Dict(String, String)) -> Nil {
  let hide = node.get_bool(attributes, "hidden")
  savoiardi.set_object_visible(group, !hide)
}

fn update(
  ctx: Context,
  _id: String,
  _parent_id: String,
  object: option.Option(Object3D),
  attributes: Dict(String, String),
  changed_attributes: Set(String),
) -> Context {
  let result = {
    use group <- result.map(object |> option.to_result(Nil))
    case set.contains("transform", in: changed_attributes) {
      True ->
        case node.get_transform(attributes) {
          Ok(transform) -> node.set_transform(group, transform)
          Error(Nil) -> node.set_transform(group, transform.identity)
        }

      False -> Nil
    }
    case set.contains("hidden", in: changed_attributes) {
      True -> set_hidden(group, attributes)
      False -> Nil
    }
    Nil
  }
  case result {
    Ok(Nil) -> Nil
    Error(Nil) ->
      console.error("Error updating empty mesh - Please open an issue!")
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
