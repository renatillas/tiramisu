import gleam/dynamic.{type Dynamic}
import lustre/effect.{type Effect}
import tiramisu/internal/element as dom_element

pub fn before_paint(
  run: fn(fn(message) -> Nil, Dynamic, dom_element.HtmlElement) -> Nil,
) -> Effect(message) {
  use dispatch, shadow_root <- effect.before_paint
  let component = dom_element.host_from_shadow_root(shadow_root)
  run(dispatch, shadow_root, component)
}
