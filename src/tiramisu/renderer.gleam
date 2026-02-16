//// The tiramisu-renderer web component.
////
//// This is the ONLY web component in Tiramisu. All other elements
//// (mesh, camera, light, empty, audio) are plain DOM elements in the
//// light DOM. The renderer uses a MutationObserver to parse its light
//// DOM children into a Gleam scene description, diffs it in pure Gleam,
//// and applies patches to Three.js via the instance-scoped registry.
////
//// ## Architecture
////
//// - **Registry** — purely functional, lives as `Option(Registry)` in
////   the Model. Updated through Lustre's message dispatch cycle.
//// - **RenderLoop** — opaque FFI type, owns minimal mutable state for
////   60fps rendering (active camera).
//// - **Async operations** — model loading dispatches `RegistryTransform`
////   messages, always applied to the latest registry.
////
//// ## Usage
////
//// ```html
//// <tiramisu-renderer width="800" height="600" background="#1a1a2e">
////   <tiramisu-camera id="main" transform="pos:0,5,10" active="true"></tiramisu-camera>
////   <tiramisu-mesh id="cube" geometry="box:2,2,2" color="#ff6b6b"></tiramisu-mesh>
////   <tiramisu-light id="sun" type="directional" intensity="1"></tiramisu-light>
//// </tiramisu-renderer>
//// ```
////
//// ## Attributes
////
//// - `scene-id`: Custom scene identifier for tick subscriptions (default: auto-generated)
//// - `width`: Canvas width in pixels (default: container width or 800)
//// - `height`: Canvas height in pixels (default: container height or 600)
//// - `background`: Background color (default: "#000000")
//// - `antialias`: Enable antialiasing (default: "true")
//// - `alpha`: Enable transparent background (default: "false")
////
//// ## Events
////
//// - `tiramisu:scene-ready`: Dispatched when the scene is initialized.
////   The event detail contains `{ sceneId: string }`.

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

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
  RegistryTransform(fn(Registry) -> Registry)
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
pub const tag_name = "tiramisu-renderer"

/// Register the tiramisu-renderer component as a custom element.
/// This is the only registration needed — all other elements are
/// plain DOM elements parsed from the light DOM.
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

  lustre.register(app, tag_name)
}

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-renderer element.
///
/// This is the root container for all 3D content. It creates a WebGL renderer
/// and scene, and parses light DOM children as the scene description.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/renderer
/// import tiramisu/mesh
/// import tiramisu/camera
///
/// renderer.renderer([renderer.background("#1a1a2e")], [
///   camera.camera("main", [...], []),
///   mesh.mesh("cube", [...], []),
/// ])
/// ```
///
pub fn renderer(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, attributes, children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the background color for the renderer (as hex string).
///
pub fn background(hex: String) -> Attribute(msg) {
  attribute.attribute("background", hex)
}

/// Set the background color for the renderer (as hex int).
///
pub fn background_color(hex: Int) -> Attribute(msg) {
  attribute.attribute("background", "#" <> int.to_base16(hex))
}

/// Set the canvas width.
///
pub fn width(pixels: Int) -> Attribute(msg) {
  attribute.attribute("width", int.to_string(pixels))
}

/// Set the canvas height.
///
pub fn height(pixels: Int) -> Attribute(msg) {
  attribute.attribute("height", int.to_string(pixels))
}

/// Enable or disable antialiasing.
///
pub fn antialias(enabled: Bool) -> Attribute(msg) {
  attribute.attribute("antialias", case enabled {
    True -> ""
    False -> "false"
  })
}

/// Enable or disable transparent background (alpha).
///
pub fn alpha(enabled: Bool) -> Attribute(msg) {
  attribute.attribute("alpha", case enabled {
    True -> ""
    False -> "false"
  })
}

/// Set a custom scene ID for this renderer.
///
/// This allows you to use a known scene ID for tick subscriptions instead
/// of relying on auto-generated IDs.
///
pub fn scene_id(id: String) -> Attribute(msg) {
  attribute.attribute("scene-id", id)
}

/// Listen for the scene-ready event.
///
/// This event fires when the renderer has initialized its scene.
///
pub fn on_scene_ready(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("tiramisu:scene-ready", {
    use sid <- decode.subfield(["detail", "sceneId"], decode.string)
    decode.success(handler(sid))
  })
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
      width: 800,
      height: 600,
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
  let user_set_scene_id = dom.get_scene_id_from_host(host) |> result.unwrap("")

  // Create scene via savoiardi
  let scene = savoiardi.create_scene()

  // Determine scene ID — use user-set or generate one
  let sid = case user_set_scene_id {
    "" -> generate_scene_id()
    _ -> user_set_scene_id
  }

  // Always set the data-scene-id attribute and dispatch the scene-ready event,
  // even when the user provided a scene-id attribute, so cocoa etc. can discover it.
  dom.set_scene_id_on_host(host, sid)

  // Get config from host element attributes
  let config = dom.get_renderer_config(host)

  // Create renderer using savoiardi
  let renderer =
    savoiardi.create_renderer(savoiardi.RendererOptions(
      antialias: config.antialias,
      alpha: config.alpha,
      dimensions: option.None,
    ))
  savoiardi.enable_renderer_shadow_map(renderer, True)

  // Set initial size
  let w = config.width |> option.unwrap(800)
  let h = config.height |> option.unwrap(600)
  savoiardi.set_renderer_size(renderer, w, h)
  savoiardi.set_renderer_pixel_ratio(
    renderer,
    registry.get_device_pixel_ratio(),
  )

  // Set background color
  set_renderer_background(renderer, config.background)

  // Append canvas to container (shadow root div)
  let canvas = savoiardi.get_renderer_dom_element(renderer)
  dom.append_canvas_to_container(root, canvas)

  // Create instance-scoped registry (purely functional)
  let reg = registry.new(scene, sid, renderer)

  // Start the render loop (owns per-frame mutable state)
  let loop = render_loop.start(scene, renderer, sid)

  // Set up MutationObserver to trigger scene diff/patch on DOM changes.
  dom.setup_mutation_observer(host, fn() { dispatch(DomMutated) })

  // Do the initial scene parse and apply
  let initial_scene = parse_children(host)
  let patches = scene_diff.diff([], initial_scene, sid)
  let on_async = fn(transform) { dispatch(RegistryTransform(transform)) }
  let reg = scene_apply.apply_patches(reg, loop, patches, on_async)

  dispatch(Initialized(
    registry: reg,
    render_loop: loop,
    scene_id: sid,
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
      let new_model =
        Model(
          ..model,
          registry: Some(reg),
          render_loop: Some(loop),
          scene_id: Some(sid),
          host: Some(host),
          previous_scene: initial_scene,
        )
      #(new_model, effect.none())
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
            effect.from(fn(dispatch) {
              let on_async = fn(transform) {
                dispatch(RegistryTransform(transform))
              }
              dispatch(RegistryTransform(fn(reg) {
                scene_apply.apply_patches(reg, loop, patches, on_async)
              }))
            }),
          )
        }
        _, _, _, _ -> #(model, effect.none())
      }
    }

    RegistryTransform(transform) -> {
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
          effect.from(fn(_) {
            registry.resize(reg, w, model.height)
          }),
        )
        _ -> #(new_model, effect.none())
      }
    }

    HeightChanged(h) -> {
      let new_model = Model(..model, height: h)
      case model.registry {
        Some(reg) -> #(
          new_model,
          effect.from(fn(_) {
            registry.resize(reg, model.width, h)
          }),
        )
        _ -> #(new_model, effect.none())
      }
    }

    BackgroundChanged(bg) -> {
      let new_model = Model(..model, background: bg)
      case model.registry {
        Some(reg) -> #(
          new_model,
          effect.from(fn(_) {
            set_renderer_background(reg.renderer, bg)
          }),
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
  // The renderer renders a container div for the WebGL canvas.
  // position:relative allows CSS renderer overlays to position absolutely within.
  html.div([attribute.style("position", "relative")], [])
}

// HELPERS ---------------------------------------------------------------------

/// Set background color on a renderer. Parses hex string to int.
fn set_renderer_background(renderer: savoiardi.Renderer, color: String) -> Nil {
  let clean = string.replace(color, "#", "")
  case int.base_parse(clean, 16) {
    Ok(n) -> savoiardi.set_renderer_clear_color(renderer, n)
    Error(_) -> Nil
  }
}

// FFI DECLARATIONS ------------------------------------------------------------

@external(javascript, "./renderer.ffi.mjs", "generateSceneId")
fn generate_scene_id() -> String

fn parse_element(el: dom.Element) -> SceneNode {
  let tag = dom.tag_name(el) |> string.lowercase
  let key = dom.get_attribute(el, "id") |> result.unwrap("")
  case tag {
    "tiramisu-mesh" -> parse_mesh(el, key)
    "tiramisu-camera" -> parse_camera(el, key)
    "tiramisu-light" -> parse_light(el, key)
    "tiramisu-empty" -> parse_empty(el, key)
    "tiramisu-audio" -> parse_audio(el, key)
    "tiramisu-audio-positional" -> parse_positional_audio(el, key)
    "tiramisu-debug" -> parse_debug(el, key)
    "tiramisu-lod" -> parse_lod(el, key)
    "tiramisu-instanced-mesh" -> parse_instanced_mesh(el, key)
    _ -> scene.UnknownNode(key:, tag:, children: parse_children(el))
  }
}

fn parse_mesh(el: dom.Element, key: String) -> SceneNode {
  scene.MeshNode(
    key:,
    geometry: attr(el, "geometry"),
    src: attr(el, "src"),
    material_type: dom.get_attribute(el, "material-type")
      |> result.unwrap("standard"),
    color: dom.get_attribute(el, "color") |> result.unwrap("#ffffff"),
    metalness: float_attr(el, "metalness", 0.5),
    roughness: float_attr(el, "roughness", 0.5),
    opacity: float_attr(el, "opacity", 1.0),
    wireframe: bool_attr(el, "wireframe", False),
    emissive: dom.get_attribute(el, "emissive") |> result.unwrap("#000000"),
    emissive_intensity: float_attr(el, "emissive-intensity", 0.0),
    side: dom.get_attribute(el, "side") |> result.unwrap("front"),
    color_map: attr(el, "color-map"),
    normal_map: attr(el, "normal-map"),
    ao_map: attr(el, "ao-map"),
    roughness_map: attr(el, "roughness-map"),
    metalness_map: attr(el, "metalness-map"),
    displacement_map: attr(el, "displacement-map"),
    displacement_scale: float_attr(el, "displacement-scale", 1.0),
    displacement_bias: float_attr(el, "displacement-bias", 0.0),
    shininess: float_attr(el, "shininess", 30.0),
    alpha_test: float_attr(el, "alpha-test", 0.0),
    transparent: bool_attr(el, "transparent", False),
    transform: transform_attr(el),
    visible: bool_attr(el, "visible", True),
    cast_shadow: bool_attr(el, "cast-shadow", False),
    receive_shadow: bool_attr(el, "receive-shadow", False),
    physics_controlled: bool_attr(el, "physics-controlled", False),
    distance: float_attr(el, "distance", 0.0),
    children: parse_children(el),
  )
}

fn parse_camera(el: dom.Element, key: String) -> SceneNode {
  scene.CameraNode(
    key:,
    camera_type: dom.get_attribute(el, "type")
      |> result.unwrap("perspective"),
    fov: float_attr(el, "fov", 75.0),
    near: float_attr(el, "near", 0.1),
    far: float_attr(el, "far", 1000.0),
    transform: transform_attr(el),
    active: bool_attr(el, "active", False),
  )
}

fn parse_light(el: dom.Element, key: String) -> SceneNode {
  scene.LightNode(
    key:,
    light_type: dom.get_attribute(el, "type") |> result.unwrap("point"),
    color: dom.get_attribute(el, "color") |> result.unwrap("#ffffff"),
    intensity: float_attr(el, "intensity", 1.0),
    transform: transform_attr(el),
    cast_shadow: bool_attr(el, "cast-shadow", False),
  )
}

fn parse_empty(el: dom.Element, key: String) -> SceneNode {
  scene.EmptyNode(
    key:,
    transform: transform_attr(el),
    visible: bool_attr(el, "visible", True),
    children: parse_children(el),
  )
}

fn parse_audio(el: dom.Element, key: String) -> SceneNode {
  scene.AudioNode(
    key:,
    src: attr(el, "src"),
    volume: float_attr(el, "volume", 1.0),
    loop: bool_attr(el, "loop", False),
    playing: bool_attr(el, "playing", False),
    playback_rate: float_attr(el, "playback-rate", 1.0),
    detune: float_attr(el, "detune", 0.0),
  )
}

fn parse_positional_audio(el: dom.Element, key: String) -> SceneNode {
  scene.PositionalAudioNode(
    key:,
    src: attr(el, "src"),
    volume: float_attr(el, "volume", 1.0),
    loop: bool_attr(el, "loop", False),
    playing: bool_attr(el, "playing", False),
    playback_rate: float_attr(el, "playback-rate", 1.0),
    detune: float_attr(el, "detune", 0.0),
    transform: transform_attr(el),
    ref_distance: float_attr(el, "ref-distance", 1.0),
    max_distance: float_attr(el, "max-distance", 100.0),
    rolloff_factor: float_attr(el, "rolloff-factor", 1.0),
    children: parse_children(el),
  )
}

fn parse_debug(el: dom.Element, key: String) -> SceneNode {
  scene.DebugNode(
    key:,
    debug_type: dom.get_attribute(el, "type") |> result.unwrap("axes"),
    size: float_attr(el, "size", 5.0),
    divisions: dom.get_attribute(el, "divisions")
      |> result.try(int.parse)
      |> result.unwrap(10),
    color: dom.get_attribute(el, "color") |> result.unwrap("#888888"),
    transform: transform_attr(el),
  )
}

fn parse_lod(el: dom.Element, key: String) -> SceneNode {
  scene.LodNode(
    key:,
    transform: transform_attr(el),
    children: parse_children(el),
  )
}

fn parse_instanced_mesh(el: dom.Element, key: String) -> SceneNode {
  scene.InstancedMeshNode(
    key:,
    geometry: attr(el, "geometry"),
    material_type: dom.get_attribute(el, "material-type")
      |> result.unwrap("standard"),
    color: dom.get_attribute(el, "color") |> result.unwrap("#ffffff"),
    metalness: float_attr(el, "metalness", 0.5),
    roughness: float_attr(el, "roughness", 0.5),
    opacity: float_attr(el, "opacity", 1.0),
    wireframe: bool_attr(el, "wireframe", False),
    transparent: bool_attr(el, "transparent", False),
    instances: attr(el, "instances"),
    transform: transform_attr(el),
    visible: bool_attr(el, "visible", True),
    cast_shadow: bool_attr(el, "cast-shadow", False),
    receive_shadow: bool_attr(el, "receive-shadow", False),
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

// Attribute parsing helpers — reduce repetition in parse functions

fn attr(el: dom.Element, name: String) -> String {
  dom.get_attribute(el, name) |> result.unwrap("")
}

fn float_attr(el: dom.Element, name: String, default: Float) -> Float {
  dom.get_attribute(el, name)
  |> result.try(float.parse)
  |> result.unwrap(default)
}

fn bool_attr(el: dom.Element, name: String, default: Bool) -> Bool {
  dom.get_attribute(el, name)
  |> result.try(parse_bool)
  |> result.unwrap(default)
}

fn transform_attr(el: dom.Element) -> transform.Transform {
  dom.get_attribute(el, "transform")
  |> result.map(transform.parse)
  |> result.unwrap(transform.identity)
}
