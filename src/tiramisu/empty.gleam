import gleam/json
import lustre/attribute.{type Attribute}
import savoiardi
import tiramisu/extension
import tiramisu/internal/node_utils
import tiramisu/internal/registry

@internal
pub const tag = "tiramisu-empty"

pub fn visible(visible: Bool) -> Attribute(msg) {
  case visible {
    True -> attribute.attribute("visible", "")
    False -> attribute.property("visible", json.bool(False))
  }
}

pub fn extension() -> extension.Extension {
  extension.NodeExtension(
    extension.Node(
      tag: "tiramisu-empty",
      observed_attributes: ["transform", "visible"],
      create: fn(ctx, id, parent_id, attrs, transform) {
        let group = savoiardi.create_group()
        let reg =
          registry.register_and_add_object(
            ctx.registry,
            id,
            group,
            parent_id,
            registry.ObjectKind("tiramisu-empty"),
          )
        registry.set_transform(reg, id, transform)
        registry.set_visible(
          reg,
          id,
          node_utils.get_bool(attrs, "visible", True),
        )
        extension.Context(..ctx, registry: reg)
      },
      update: fn(ctx, id, _old_attrs, new_attrs, transform) {
        registry.set_transform(ctx.registry, id, transform)
        registry.set_visible(
          ctx.registry,
          id,
          node_utils.get_bool(new_attrs, "visible", True),
        )
        ctx
      },
      remove: fn(ctx, id) { node_utils.default_remove(ctx, id) },
    ),
  )
}
