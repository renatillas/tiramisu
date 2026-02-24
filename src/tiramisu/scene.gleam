import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import vec/vec2

import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html

import tiramisu/extension
import tiramisu/internal/dom
import tiramisu/internal/registry.{type Registry}
import tiramisu/internal/render_loop.{type RenderLoop}
import tiramisu/internal/scene
import tiramisu/internal/scene_apply
import tiramisu/internal/scene_diff
import tiramisu/transform

import savoiardi

// TYPES -----------------------------------------------------------------------

/// The model for the renderer component.
pub type Model {
  Model(
    /// Instance-scoped registry (purely functional)
    registry: Option(Registry),
    /// Render loop (owns per-frame mutable state)
    render_loop: Option(RenderLoop),
    /// Scene ID for tick subscriptions and context
    scene_id: Option(String),
    /// Reference to the host element for DOM parsing
    host: Option(dom.Element),
    /// Previous scene tree for diffing
    previous_scene: List(scene.Node),
    /// Canvas width
    width: Int,
    /// Canvas height
    height: Int,
    /// Background color
    background: String,
    /// Antialiasing enabled
    antialias: Bool,
    /// Transparent background
    alpha: Bool,
    /// Compiled extensions (built-ins + user-provided)
    extensions: extension.Extensions,
  )
}

/// Messages for the renderer component.
pub type Msg {
  /// Scene and renderer have been initialized
  Initialized(
    registry: Registry,
    render_loop: RenderLoop,
    scene_id: String,
    host: dom.Element,
    initial_scene: List(scene.Node),
  )
  /// Light DOM children changed — re-parse, diff, and apply
  DomMutated
  /// Async registry mutation (model loading, etc.) — always applied to latest registry
  PatcherModifiedRegistry(fn(Registry) -> Registry)
  /// Width attribute changed
  WidthChanged(Int)
  /// Height attribute changed
  HeightChanged(Int)
  /// Background attribute changed
  BackgroundChanged(String)
  /// Antialias attribute changed
  AntialiasChanged(Bool)
  /// Alpha attribute changed
  AlphaChanged(Bool)
}

// COMPONENT -------------------------------------------------------------------

/// The tag name for the renderer component.
@internal
pub const tag = "tiramisu-scene"

@internal
pub fn register(
  extensions: List(extension.Extension),
) -> Result(Nil, lustre.Error) {
  // Merge built-ins first so built-in tags always take precedence
  let extensions = extension.from_list(extensions)

  let app =
    lustre.component(fn(_) { init(extensions) }, update, view, [
      component.on_attribute_change("width", fn(v) {
        case int.parse(v) {
          Ok(n) -> Ok(WidthChanged(n))
          Error(_) -> Error(Nil)
        }
      }),
      component.on_attribute_change("height", fn(v) {
        case int.parse(v) {
          Ok(n) -> Ok(HeightChanged(n))
          Error(_) -> Error(Nil)
        }
      }),
      component.on_attribute_change("background", fn(v) {
        Ok(BackgroundChanged(v))
      }),
      component.on_attribute_change("antialias", fn(v) {
        v |> parse_bool |> result.map(AntialiasChanged)
      }),
      component.on_attribute_change("alpha", fn(v) {
        v |> parse_bool |> result.map(AlphaChanged)
      }),
    ])

  lustre.register(app, tag)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the background color for the renderer (as hex int).
pub fn background_color(hex: Int) -> Attribute(msg) {
  attribute.attribute("background", "#" <> int.to_base16(hex))
}

/// Enable or disable antialiasing.
pub fn antialias(enabled: Bool) -> Attribute(msg) {
  case enabled {
    True -> attribute.attribute("antialias", "")
    False -> attribute.property("antialias", json.bool(False))
  }
}

/// Enable or disable transparent background (alpha).
pub fn alpha(enabled: Bool) -> Attribute(msg) {
  case enabled {
    True -> attribute.attribute("alpha", "")
    False -> attribute.property("alpha", json.bool(False))
  }
}

// INIT ------------------------------------------------------------------------

fn init(exts: extension.Extensions) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      registry: None,
      render_loop: None,
      scene_id: None,
      host: None,
      previous_scene: [],
      width: 1920,
      height: 1080,
      background: "#000000",
      antialias: True,
      alpha: False,
      extensions: exts,
    )

  #(model, effect.after_paint(do_init(exts)))
}

fn do_init(exts: extension.Extensions) -> fn(fn(Msg) -> Nil, Dynamic) -> Nil {
  fn(dispatch, root) {
    let host = dom.shadow_root_host(root)
    let scene_id = dom.get_scene_id_from_host(host) |> result.unwrap("")
    let gleam_scene = savoiardi.create_scene()
    let config = dom.get_renderer_config(host)
    let w = config.width |> option.unwrap(1920) |> int.to_float
    let h = config.height |> option.unwrap(1080) |> int.to_float

    let renderer =
      savoiardi.create_renderer(savoiardi.RendererOptions(
        antialias: config.antialias,
        alpha: config.alpha,
        dimensions: option.Some(vec2.Vec2(w, h)),
      ))
    savoiardi.enable_renderer_shadow_map(renderer, True)
    set_renderer_background(renderer, config.background)

    let canvas = savoiardi.get_renderer_dom_element(renderer)
    dom.append_canvas_to_container(root, canvas)

    let reg = registry.new(gleam_scene, scene_id, renderer)
    let render_loop = render_loop.start(gleam_scene, renderer, scene_id)

    // Set up MutationObserver with all extension-observed attributes
    let observed_attrs = extension.all_observed_attributes(exts)
    dom.setup_mutation_observer(host, observed_attrs, fn() {
      dispatch(DomMutated)
    })

    let on_async = fn(registry_transform) {
      dispatch(PatcherModifiedRegistry(registry_transform))
    }

    let initial_scene = parse_children(host, exts)
    let patches = scene_diff.diff([], initial_scene, scene_id)
    let registry =
      scene_apply.apply_patches(reg, render_loop, patches, exts, on_async)

    dispatch(Initialized(
      registry:,
      render_loop:,
      scene_id:,
      host:,
      initial_scene:,
    ))
  }
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Initialized(
      registry: reg,
      render_loop: loop,
      scene_id: sid,
      host:,
      initial_scene:,
    ) -> {
      #(
        Model(
          ..model,
          registry: Some(reg),
          render_loop: Some(loop),
          scene_id: Some(sid),
          host: Some(host),
          previous_scene: initial_scene,
        ),
        effect.none(),
      )
    }

    DomMutated -> {
      case model.scene_id, model.host, model.registry, model.render_loop {
        Some(sid), Some(host), Some(reg), Some(loop) -> {
          let new_scene = parse_children(host, model.extensions)
          let patches = scene_diff.diff(model.previous_scene, new_scene, sid)

          #(
            Model(..model, previous_scene: new_scene),
            effect.from(fn(dispatch) {
              let registry =
                scene_apply.apply_patches(
                  reg,
                  loop,
                  patches,
                  model.extensions,
                  fn(transform) { dispatch(PatcherModifiedRegistry(transform)) },
                )
              dispatch(PatcherModifiedRegistry(fn(_) { registry }))
            }),
          )
        }
        _, _, _, _ -> #(model, effect.none())
      }
    }

    PatcherModifiedRegistry(transform) -> {
      case model.registry {
        Some(reg) -> {
          let new_reg = transform(reg)
          #(Model(..model, registry: Some(new_reg)), effect.none())
        }
        None -> #(model, effect.none())
      }
    }

    WidthChanged(w) -> {
      let new_model = Model(..model, width: w)
      case model.registry {
        Some(reg) -> #(
          new_model,
          effect.from(fn(_) { registry.resize(reg, w, model.height) }),
        )
        _ -> #(new_model, effect.none())
      }
    }

    HeightChanged(h) -> {
      let new_model = Model(..model, height: h)
      case model.registry {
        Some(reg) -> #(
          new_model,
          effect.from(fn(_) { registry.resize(reg, model.width, h) }),
        )
        _ -> #(new_model, effect.none())
      }
    }

    BackgroundChanged(bg) -> {
      let new_model = Model(..model, background: bg)
      case model.registry {
        Some(reg) -> #(
          new_model,
          effect.from(fn(_) { set_renderer_background(reg.renderer, bg) }),
        )
        None -> #(new_model, effect.none())
      }
    }

    AntialiasChanged(aa) -> {
      #(Model(..model, antialias: aa), effect.none())
    }

    AlphaChanged(a) -> {
      #(Model(..model, alpha: a), effect.none())
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  html.div([], [])
}

// DOM PARSING -----------------------------------------------------------------

/// Parse a single DOM element into a SceneNode.
/// All attrs except `id` and `transform` are captured into the attrs dict.
fn parse_element(el: dom.Element, exts: extension.Extensions) -> scene.Node {
  let tag = dom.tag_name(el) |> string.lowercase
  let attrs = dom.get_all_attributes(el)
  let key = dict.get(attrs, "id") |> result.unwrap("")
  let transform =
    dict.get(attrs, "transform")
    |> result.map(transform.parse)
    |> result.unwrap(transform.identity)
  // Strip id and transform — they're stored separately on the node
  let node_attrs =
    attrs
    |> dict.delete("id")
    |> dict.delete("transform")

  // Only produce a scene node for known/registered tags — skip everything else
  case extension.get_node(exts, tag) {
    Ok(_) ->
      scene.SceneNode(
        key:,
        tag:,
        transform:,
        children: parse_children(el, exts),
        attrs: node_attrs,
      )
    Error(Nil) ->
      // Unknown tag — pass through as a transparent structural node
      // (no Three.js object, but children are still parsed)
      scene.SceneNode(
        key:,
        tag:,
        transform:,
        children: parse_children(el, exts),
        attrs: dict.new(),
      )
  }
}

fn parse_children(
  el: dom.Element,
  exts: extension.Extensions,
) -> List(scene.Node) {
  list.map(dom.children(el), fn(child) { parse_element(child, exts) })
}

// HELPERS ---------------------------------------------------------------------

fn set_renderer_background(renderer: savoiardi.Renderer, color: String) -> Nil {
  let color =
    color
    |> string.replace("#", "")
    |> int.base_parse(16)

  case color {
    Ok(n) -> savoiardi.set_renderer_clear_color(renderer, n)
    Error(Nil) -> Nil
  }
}

fn parse_bool(value: String) -> Result(Bool, Nil) {
  case value {
    "" | "true" -> Ok(True)
    "false" -> Ok(False)
    _ -> Error(Nil)
  }
}
