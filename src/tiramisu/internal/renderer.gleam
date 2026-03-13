import gleam/dynamic.{type Dynamic}
import gleam/option
import savoiardi
import tiramisu/dev/runtime
import tiramisu/internal/element as dom_element

pub type Config {
  Config(width: Int, height: Int, antialias: Bool, alpha: Bool)
}

pub fn config(
  host: dom_element.HtmlElement,
  width fallback_width: Int,
  height fallback_height: Int,
) -> Config {
  let renderer_config =
    dom_element.renderer_config(
      host,
      width: fallback_width,
      height: fallback_height,
    )

  Config(
    width: renderer_config.width |> option.unwrap(fallback_width),
    height: renderer_config.height |> option.unwrap(fallback_height),
    antialias: renderer_config.antialias,
    alpha: renderer_config.alpha,
  )
}

pub fn create(config: Config) -> savoiardi.Renderer {
  let renderer =
    savoiardi.create_renderer(antialias: config.antialias, alpha: config.alpha)
  savoiardi.set_renderer_size(renderer, config.width, config.height)
  savoiardi.enable_renderer_shadow_map(renderer, True)
  renderer
}

pub fn initialize(
  shadow_root: Dynamic,
  host: dom_element.HtmlElement,
  scene_id: String,
  on_tick: fn(Float) -> Nil,
) -> runtime.Runtime {
  let config = host |> config(width: 1920, height: 1080)
  let scene = savoiardi.create_scene()
  let renderer = create(config)
  let canvas = savoiardi.get_renderer_dom_element(renderer)

  dom_element.append_canvas_to_container(shadow_root, canvas)
  savoiardi.set_animation_loop(renderer, on_tick)
  runtime.new(scene, scene_id, renderer)
}

pub fn resize(renderer: savoiardi.Renderer, width: Int, height: Int) -> Nil {
  savoiardi.set_renderer_size(renderer, width, height)
}

pub fn apply_transform(
  renderer: runtime.Runtime,
  transform: fn(runtime.Runtime) -> runtime.Runtime,
) -> runtime.Runtime {
  transform(renderer)
}
