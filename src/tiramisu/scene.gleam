import gleam/dynamic.{type Dynamic}
import gleam/float
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

import tiramisu/internal/dom
import tiramisu/internal/registry.{type Registry}
import tiramisu/internal/render_loop.{type RenderLoop}
import tiramisu/internal/scene.{type SceneNode}
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
    previous_scene: List(SceneNode),
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
    initial_scene: List(SceneNode),
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
pub fn register() -> Result(Nil, lustre.Error) {
  let app =
    lustre.component(init, update, view, [
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
///
pub fn background_color(hex: Int) -> Attribute(msg) {
  attribute.attribute("background", "#" <> int.to_base16(hex))
}

/// Enable or disable antialiasing.
///
pub fn antialias(enabled: Bool) -> Attribute(msg) {
  case enabled {
    True -> attribute.attribute("antialias", "")
    False -> attribute.property("antialias", json.bool(False))
  }
}

/// Enable or disable transparent background (alpha).
///
pub fn alpha(enabled: Bool) -> Attribute(msg) {
  case enabled {
    True -> attribute.attribute("alpha", "")
    False -> attribute.property("alpha", json.bool(False))
  }
}

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
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
    )

  // Initialize the scene and renderer after the component is mounted
  #(model, effect.after_paint(do_init))
}

fn do_init(dispatch: fn(Msg) -> Nil, root: Dynamic) -> Nil {
  // Get host element from shadow root
  let host = dom.shadow_root_host(root)

  // Check for user-defined scene ID
  let scene_id = dom.get_scene_id_from_host(host) |> result.unwrap("")

  // Create scene via savoiardi
  let scene = savoiardi.create_scene()

  // Get config from host element attributes
  let config = dom.get_renderer_config(host)

  let w = config.width |> option.unwrap(1920) |> int.to_float
  let h = config.height |> option.unwrap(1080) |> int.to_float

  // Create renderer using savoiardi
  let renderer =
    savoiardi.create_renderer(savoiardi.RendererOptions(
      antialias: config.antialias,
      alpha: config.alpha,
      dimensions: option.Some(vec2.Vec2(w, h)),
    ))
  savoiardi.enable_renderer_shadow_map(renderer, True)

  // Set background color
  set_renderer_background(renderer, config.background)

  // Append canvas to container (shadow root div)
  let canvas = savoiardi.get_renderer_dom_element(renderer)
  dom.append_canvas_to_container(root, canvas)

  // Create instance-scoped registry
  let reg = registry.new(scene, scene_id, renderer)

  // Start the render loop (owns per-frame mutable state)
  let render_loop = render_loop.start(scene, renderer, scene_id)

  // Set up MutationObserver to trigger scene diff/patch on DOM changes.
  dom.setup_mutation_observer(host, fn() { dispatch(DomMutated) })

  // Do the initial scene parse and apply
  let initial_scene = parse_children(host)
  let patches = scene_diff.diff([], initial_scene, scene_id)
  let on_async = fn(registry_transform) {
    dispatch(PatcherModifiedRegistry(registry_transform))
  }
  let registry = scene_apply.apply_patches(reg, render_loop, patches, on_async)

  dispatch(Initialized(
    registry:,
    render_loop:,
    scene_id:,
    host:,
    initial_scene:,
  ))
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
        Some(sid), Some(host), Some(_reg), Some(loop) -> {
          // Parse the DOM, diff against previous scene
          let new_scene = parse_children(host)
          let patches = scene_diff.diff(model.previous_scene, new_scene, sid)
          // Apply patches via RegistryTransform to avoid stale registry races.
          // The transform is dispatched as a message and always applied to the
          // latest registry, ensuring async operations (model loading) don't
          // get overwritten.
          #(
            Model(..model, previous_scene: new_scene),
            // TODO: Weird hack: I don't like this
            effect.from(fn(dispatch) {
              dispatch(
                PatcherModifiedRegistry(fn(reg) {
                  scene_apply.apply_patches(reg, loop, patches, fn(transform) {
                    dispatch(PatcherModifiedRegistry(transform))
                  })
                }),
              )
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

// HELPERS ---------------------------------------------------------------------

/// Set background color on a renderer. Parses hex string to int.
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

fn parse_element(el: dom.Element) -> SceneNode {
  let tag = dom.tag_name(el) |> string.lowercase
  let key = dom.get_attribute(el, "id") |> result.unwrap("")
  case tag {
    "tiramisu-mesh" -> parse_mesh(el, key)
    "tiramisu-camera" -> parse_camera(el, key)
    "tiramisu-light" -> parse_light(el, key)
    "tiramisu-empty" -> parse_empty(el, key)
    "tiramisu-global-audio" -> parse_audio(el, key)
    "tiramisu-positional-audio" -> parse_positional_audio(el, key)
    "tiramisu-debug" -> parse_debug(el, key)
    "tiramisu-instanced-mesh" -> parse_instanced_mesh(el, key)
    _ ->
      scene.UnknownNode(
        key:,
        tag:,
        transform: transform_attribute(el),
        children: parse_children(el),
      )
  }
}

fn parse_mesh(el: dom.Element, key: String) -> SceneNode {
  scene.MeshNode(
    key:,
    geometry: optional_attribute(el, "geometry"),
    src: optional_attribute(el, "src"),
    material_type: optional_attribute(el, "material-type"),
    color: optional_attribute(el, "color"),
    metalness: optional_float_attribute(el, "metalness"),
    roughness: optional_float_attribute(el, "roughness"),
    opacity: optional_float_attribute(el, "opacity"),
    wireframe: optional_bool_attribute(el, "wireframe"),
    emissive: optional_attribute(el, "emissive"),
    emissive_intensity: optional_float_attribute(el, "emissive-intensity"),
    side: optional_attribute(el, "side"),
    color_map: optional_attribute(el, "color-map"),
    normal_map: optional_attribute(el, "normal-map"),
    ao_map: optional_attribute(el, "ambient-occlusion-map"),
    roughness_map: optional_attribute(el, "roughness-map"),
    metalness_map: optional_attribute(el, "metalness-map"),
    displacement_map: optional_attribute(el, "displacement-map"),
    displacement_scale: optional_float_attribute(el, "displacement-scale"),
    displacement_bias: optional_float_attribute(el, "displacement-bias"),
    shininess: optional_float_attribute(el, "shininess"),
    alpha_test: optional_float_attribute(el, "alpha-test"),
    transparent: optional_bool_attribute(el, "transparent"),
    transform: transform_attribute(el),
    visible: optional_bool_attribute(el, "visible"),
    cast_shadow: optional_bool_attribute(el, "cast-shadow"),
    receive_shadow: optional_bool_attribute(el, "receive-shadow"),
    physics_controlled: optional_bool_attribute(el, "physics-controlled"),
    children: parse_children(el),
  )
}

fn parse_camera(el: dom.Element, key: String) -> SceneNode {
  scene.CameraNode(
    key:,
    camera_type: optional_attribute(el, "camera-type"),
    fov: optional_float_attribute(el, "fov"),
    near: optional_float_attribute(el, "near"),
    far: optional_float_attribute(el, "far"),
    transform: transform_attribute(el),
    active: optional_bool_attribute(el, "active"),
    children: parse_children(el),
  )
}

fn parse_light(el: dom.Element, key: String) -> SceneNode {
  scene.LightNode(
    key:,
    light_type: optional_attribute(el, "light-type"),
    color: optional_attribute(el, "color"),
    intensity: optional_float_attribute(el, "intensity"),
    transform: transform_attribute(el),
    cast_shadow: optional_bool_attribute(el, "cast-shadow"),
    children: parse_children(el),
  )
}

fn parse_empty(el: dom.Element, key: String) -> SceneNode {
  scene.EmptyNode(
    key:,
    transform: transform_attribute(el),
    visible: optional_bool_attribute(el, "visible"),
    children: parse_children(el),
  )
}

fn parse_audio(el: dom.Element, key: String) -> SceneNode {
  scene.AudioNode(
    key:,
    src: optional_attribute(el, "src"),
    volume: optional_float_attribute(el, "volume"),
    loop: optional_bool_attribute(el, "loop"),
    playing: optional_bool_attribute(el, "playing"),
    playback_rate: optional_float_attribute(el, "playback-rate"),
    detune: optional_float_attribute(el, "detune"),
    transform: transform_attribute(el),
    children: parse_children(el),
  )
}

fn parse_positional_audio(el: dom.Element, key: String) -> SceneNode {
  scene.PositionalAudioNode(
    key:,
    src: optional_attribute(el, "src"),
    volume: optional_float_attribute(el, "volume"),
    loop: optional_bool_attribute(el, "loop"),
    playing: optional_bool_attribute(el, "playing"),
    playback_rate: optional_float_attribute(el, "playback-rate"),
    detune: optional_float_attribute(el, "detune"),
    transform: transform_attribute(el),
    ref_distance: optional_float_attribute(el, "ref-distance"),
    max_distance: optional_float_attribute(el, "max-distance"),
    rolloff_factor: optional_float_attribute(el, "rolloff-factor"),
    children: parse_children(el),
  )
}

fn parse_debug(el: dom.Element, key: String) -> SceneNode {
  scene.DebugNode(
    key:,
    debug_type: optional_attribute(el, "type"),
    size: optional_float_attribute(el, "size"),
    divisions: optional_int_attribute(el, "divisions"),
    color: optional_attribute(el, "color"),
    transform: transform_attribute(el),
    children: parse_children(el),
  )
}

fn parse_instanced_mesh(el: dom.Element, key: String) -> SceneNode {
  scene.InstancedMeshNode(
    key:,
    geometry: optional_attribute(el, "geometry"),
    material_type: optional_attribute(el, "material-type"),
    color: optional_attribute(el, "color"),
    metalness: optional_float_attribute(el, "metalness"),
    roughness: optional_float_attribute(el, "roughness"),
    opacity: optional_float_attribute(el, "opacity"),
    wireframe: optional_bool_attribute(el, "wireframe"),
    transparent: optional_bool_attribute(el, "transparent"),
    instances: optional_attribute(el, "instances"),
    transform: transform_attribute(el),
    visible: optional_bool_attribute(el, "visible"),
    cast_shadow: optional_bool_attribute(el, "cast-shadow"),
    receive_shadow: optional_bool_attribute(el, "receive-shadow"),
    children: parse_children(el),
  )
}

fn parse_bool(value: String) -> Result(Bool, Nil) {
  case value {
    "" | "true" -> Ok(True)
    "false" -> Ok(False)
    _ -> Error(Nil)
  }
}

fn parse_children(el: dom.Element) -> List(SceneNode) {
  list.map(dom.children(el), parse_element)
}

fn transform_attribute(el: dom.Element) -> transform.Transform {
  dom.get_attribute(el, "transform")
  |> result.map(transform.parse)
  |> result.unwrap(transform.identity)
}

/// Return `Some(value)` if the attribute exists, `None` otherwise.
fn optional_attribute(el: dom.Element, name: String) -> Option(String) {
  dom.get_attribute(el, name) |> option.from_result
}

/// Return `Some(f)` if the attribute exists and parses as a float, `None` otherwise.
fn optional_float_attribute(el: dom.Element, name: String) -> Option(Float) {
  dom.get_attribute(el, name)
  |> result.try(float.parse)
  |> option.from_result
}

/// Return `Some(b)` if the attribute exists and parses as a bool, `None` otherwise.
fn optional_bool_attribute(el: dom.Element, name: String) -> Option(Bool) {
  dom.get_attribute(el, name)
  |> result.try(parse_bool)
  |> option.from_result
}

/// Return `Some(i)` if the attribute exists and parses as an int, `None` otherwise.
fn optional_int_attribute(el: dom.Element, name: String) -> Option(Int) {
  dom.get_attribute(el, name)
  |> result.try(int.parse)
  |> option.from_result
}
