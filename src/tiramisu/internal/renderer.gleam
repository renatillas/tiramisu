import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/javascript/promise.{type Promise}
import gleam/list
import gleam/option
import gleam/string
import savoiardi
import tiramisu/dev/runtime
import tiramisu/internal/element as dom_element
import tiramisu/internal/loop

pub type Config {
  Config(
    width: Int,
    height: Int,
    background: String,
    antialias: Bool,
    alpha: Bool,
  )
}

pub opaque type Runtime {
  Runtime(
    host: dom_element.HtmlElement,
    runtime: runtime.Runtime,
    loop: loop.Loop,
  )
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
    background: renderer_config.background,
    antialias: renderer_config.antialias,
    alpha: renderer_config.alpha,
  )
}

pub fn create(config: Config) -> savoiardi.Renderer {
  let renderer = savoiardi.create_renderer()
  savoiardi.set_renderer_size(renderer, config.width, config.height)
  savoiardi.enable_renderer_shadow_map(renderer, True)
  renderer
}

pub fn initialize(
  shadow_root: Dynamic,
  host: dom_element.HtmlElement,
  scene_id: String,
  on_async: fn(Promise(fn(runtime.Runtime) -> runtime.Runtime)) -> Nil,
  on_tick: fn(Float, Int) -> Nil,
) -> Runtime {
  let config = host |> config(width: 1920, height: 1080)
  let scene = savoiardi.create_scene()
  let renderer = create(config)
  let canvas = savoiardi.get_renderer_dom_element(renderer)

  dom_element.append_canvas_to_container(shadow_root, canvas)

  let runtime =
    runtime.new(scene, scene_id, renderer)
    |> apply_background(config.background, on_async)

  Runtime(
    host:,
    runtime:,
    loop: loop.start(on_tick),
  )
}

pub fn resize(renderer: savoiardi.Renderer, width: Int, height: Int) -> Nil {
  savoiardi.set_renderer_size(renderer, width, height)
}

type Background {
  None
  Color(Int)
  Texture(String)
  Equirectangular(String)
  Cube(List(String))
}

pub fn apply_background(
  runtime: runtime.Runtime,
  background: String,
  on_async: fn(Promise(fn(runtime.Runtime) -> runtime.Runtime)) -> Nil,
) -> runtime.Runtime {
  case parse_background(background) {
    None -> {
      let _ = savoiardi.clear_scene_background(runtime.scene(runtime))
      runtime
    }

    Color(color) -> {
      let _ = savoiardi.set_scene_background_color(runtime.scene(runtime), color)
      runtime
    }

    Texture(url) -> {
      on_async(
        savoiardi.load_texture(url)
        |> promise.map(apply_loaded_texture(_, fn(scene, texture) {
          let _ = savoiardi.set_scene_background_texture(scene, texture)
          Nil
        })),
      )
      runtime
    }

    Equirectangular(url) -> {
      on_async(
        savoiardi.load_equirectangular_texture(url)
        |> promise.map(apply_loaded_texture(_, fn(scene, texture) {
          let _ = savoiardi.set_scene_background_texture(scene, texture)
          Nil
        })),
      )
      runtime
    }

    Cube(urls) -> {
      on_async(
        savoiardi.load_cube_texture(urls)
        |> promise.map(fn(result) {
          fn(runtime) {
            case result {
              Ok(texture) -> {
                let _ =
                  savoiardi.set_scene_background_cube_texture(
                    runtime.scene(runtime),
                    texture,
                  )
                runtime
              }
              Error(Nil) -> runtime
            }
          }
        }),
      )
      runtime
    }
  }
}

fn apply_loaded_texture(
  result: Result(savoiardi.Texture, Nil),
  apply: fn(savoiardi.Scene, savoiardi.Texture) -> Nil,
) -> fn(runtime.Runtime) -> runtime.Runtime {
  fn(runtime) {
    case result {
      Ok(texture) -> {
        apply(runtime.scene(runtime), texture)
        runtime
      }
      Error(Nil) -> runtime
    }
  }
}

fn parse_background(background: String) -> Background {
  case background {
    "" | "none" -> None
    _ ->
      case string.starts_with(background, "texture:") {
        True -> Texture(string.drop_start(from: background, up_to: 8))
        False ->
          case string.starts_with(background, "equirectangular:") {
            True ->
              Equirectangular(
                string.drop_start(from: background, up_to: 16),
              )

            False ->
              case string.starts_with(background, "cube:") {
                True ->
                  parse_cube_background(
                    string.drop_start(from: background, up_to: 5),
                  )

                False ->
                  case
                    background
                    |> string.replace("#", "")
                    |> int.base_parse(16)
                  {
                    Ok(color) -> Color(color)
                    Error(Nil) -> None
                  }
              }
          }
      }
  }
}

fn parse_cube_background(encoded: String) -> Background {
  let urls = string.split(encoded, "|")
  case list.length(urls) == 6 {
    True -> Cube(urls)
    False -> None
  }
}

pub fn apply_transform(
  renderer: Runtime,
  transform: fn(runtime.Runtime) -> runtime.Runtime,
) -> Runtime {
  Runtime(..renderer, runtime: transform(renderer.runtime))
}

pub fn host(renderer: Runtime) -> dom_element.HtmlElement {
  renderer.host
}

pub fn runtime(renderer: Runtime) -> runtime.Runtime {
  renderer.runtime
}

pub fn scene(renderer: Runtime) -> savoiardi.Scene {
  renderer.runtime |> runtime.scene
}

pub fn renderer(renderer_runtime: Runtime) -> savoiardi.Renderer {
  renderer_runtime.runtime |> runtime.renderer
}

pub fn with_runtime(renderer: Runtime, runtime next: runtime.Runtime) -> Runtime {
  Runtime(..renderer, runtime: next)
}
