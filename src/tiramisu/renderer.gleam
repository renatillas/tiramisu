//// The tiramisu-renderer web component.
////
//// This is the ONLY web component in Tiramisu. All other elements
//// (mesh, camera, light, empty, audio) are plain DOM elements in the
//// light DOM. The renderer uses a MutationObserver to parse its light
//// DOM children into a Gleam scene description, diffs it in pure Gleam,
//// and applies patches to Three.js via runtime.gleam.
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
import tiramisu/transform

import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

import tiramisu/internal/runtime.{type RendererRef, type SceneRef}
import tiramisu/internal/scene.{type SceneNode}
import tiramisu/internal/scene_apply
import tiramisu/internal/scene_diff
import tiramisu/internal/utils

// TYPES -----------------------------------------------------------------------

/// The model for the renderer component.
pub type Model {
  Model(
    /// Reference to the registered renderer
    renderer_ref: Option(RendererRef),
    /// Reference to the registered scene
    scene_ref: Option(SceneRef),
    /// Scene ID for tick subscriptions and context
    scene_id: Option(String),
    /// Reference to the host element for DOM parsing
    host: Option(Element(Msg)),
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
    renderer_ref: RendererRef,
    scene_ref: SceneRef,
    scene_id: String,
    host: element.Element(Msg),
  )
  /// Light DOM children changed — re-parse, diff, and apply
  DomMutated
  /// Scene has been reconciled — update the previous_scene
  SceneReconciled(List(SceneNode))
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
      component.on_attribute_change("antialias", utils.set_changed(
        _,
        AntialiasChanged,
      )),
      component.on_attribute_change("alpha", utils.set_changed(_, AlphaChanged)),
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
      renderer_ref: None,
      scene_ref: None,
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
  // Check for user-defined scene ID, otherwise auto-generate one
  let host = get_host(root)
  let user_set_scene_id = get_scene_id(host) |> result.unwrap("")
  let scene_ref = runtime.create_scene_with_id(user_set_scene_id)
  let runtime.SceneRef(scene_id) = scene_ref

  // Set the scene ID on the host element for external discovery (cocoa etc.)
  case user_set_scene_id != scene_id {
    True -> set_scene_id(host, scene_id)
    False -> Nil
  }

  // Get config from host element attributes
  let config = get_renderer_config(host)

  // Create renderer using savoiardi via runtime module
  let renderer_ref = runtime.create_renderer(root, config)

  // Start the render loop
  runtime.start_render_loop(renderer_ref, scene_ref)

  // Get host element reference and set up MutationObserver
  let host = get_host(root)
  setup_mutation_observer(host, fn() { dispatch(DomMutated) })

  // Do the initial scene parse and apply
  let initial_scene = parse_children(host)
  let patches = scene_diff.diff([], initial_scene, scene_id)
  scene_apply.apply_patches(scene_id, patches)

  dispatch(Initialized(renderer_ref:, scene_ref:, scene_id:, host:))
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Initialized(renderer_ref:, scene_ref:, scene_id:, host:) -> {
      // Parse the current DOM to get the initial scene
      let initial_scene = parse_children(host)
      let new_model =
        Model(
          ..model,
          renderer_ref: Some(renderer_ref),
          scene_ref: Some(scene_ref),
          scene_id: Some(scene_id),
          host: Some(host),
          previous_scene: initial_scene,
        )
      #(new_model, effect.none())
    }

    DomMutated -> {
      case model.scene_id, model.host {
        Some(sid), Some(host) -> {
          // Parse the DOM, diff against previous scene, apply patches
          let new_scene = parse_children(host)
          let patches = scene_diff.diff(model.previous_scene, new_scene, sid)
          #(
            Model(..model, previous_scene: new_scene),
            effect.from(fn(_) { scene_apply.apply_patches(sid, patches) }),
          )
        }
        _, _ -> #(model, effect.none())
      }
    }

    SceneReconciled(new_scene) -> {
      #(Model(..model, previous_scene: new_scene), effect.none())
    }

    WidthChanged(w) -> {
      let new_model = Model(..model, width: w)
      case model.renderer_ref {
        Some(ref) -> #(
          new_model,
          effect.from(fn(_) { runtime.resize_renderer(ref, w, model.height) }),
        )
        None -> #(new_model, effect.none())
      }
    }

    HeightChanged(h) -> {
      let new_model = Model(..model, height: h)
      case model.renderer_ref {
        Some(ref) -> #(
          new_model,
          effect.from(fn(_) { runtime.resize_renderer(ref, model.width, h) }),
        )
        None -> #(new_model, effect.none())
      }
    }

    BackgroundChanged(bg) -> {
      let new_model = Model(..model, background: bg)
      case model.renderer_ref {
        Some(ref) -> #(
          new_model,
          effect.from(fn(_) { set_background_ffi(ref, bg) }),
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
  // No <slot> needed — children are in the light DOM and parsed directly.
  html.div([], [])
}

// FFI DECLARATIONS ------------------------------------------------------------

@external(javascript, "./renderer.ffi.mjs", "getRendererConfig")
fn get_renderer_config(host: Element(msg)) -> runtime.RendererConfig

@external(javascript, "./renderer.ffi.mjs", "getSceneIdFromHost")
fn get_scene_id(host: Element(msg)) -> Result(String, Nil)

@external(javascript, "./renderer.ffi.mjs", "setBackground")
fn set_background_ffi(renderer_ref: RendererRef, color: String) -> Nil

@external(javascript, "./renderer.ffi.mjs", "setSceneIdOnHost")
fn set_scene_id(host: Element(msg), scene_id: String) -> Nil

@external(javascript, "./renderer.ffi.mjs", "getHostElement")
fn get_host(root: Dynamic) -> Element(msg)

@external(javascript, "./renderer.ffi.mjs", "setupMutationObserver")
fn setup_mutation_observer(host: Element(msg), callback: fn() -> Nil) -> Nil

@external(javascript, "./renderer.ffi.mjs", "elementTagName")
pub fn element_tag_name(element: element.Element(msg)) -> String

fn parse_element(element: Element(a)) -> SceneNode {
  let tag = element_tag_name(element) |> string.lowercase
  let key = get_element_attribute(element, "id") |> result.unwrap("")
  case tag {
    "tiramisu-mesh" -> parse_mesh(element, key)
    "tiramisu-camera" -> parse_camera(element, key)
    "tiramisu-light" -> parse_light(element, key)
    "tiramisu-empty" -> parse_empty(element, key)
    "tiramisu-audio" -> parse_audio(element, key)
    "tiramisu-audio-positional" -> parse_positional_audio(element, key)
    _ -> scene.UnknownNode(key:, tag:, children: parse_children(element))
  }
}

fn parse_mesh(element, key) {
  scene.MeshNode(
    key:,
    geometry: get_element_attribute(element, "geometry") |> result.unwrap(""),
    src: get_element_attribute(element, "src") |> result.unwrap(""),
    color: get_element_attribute(element, "color") |> result.unwrap("#ffffff"),
    metalness: get_element_attribute(element, "metalness")
      |> result.try(float.parse)
      |> result.unwrap(0.5),
    roughness: get_element_attribute(element, "roughness")
      |> result.try(float.parse)
      |> result.unwrap(0.5),
    opacity: get_element_attribute(element, "opacity")
      |> result.try(float.parse)
      |> result.unwrap(1.0),
    wireframe: get_element_attribute(element, "wireframe")
      |> result.try(parse_bool)
      |> result.unwrap(False),
    transform: get_element_attribute(element, "transform")
      |> result.map(transform.parse)
      |> result.unwrap(transform.identity),
    visible: get_element_attribute(element, "visible")
      |> result.try(parse_bool)
      |> result.unwrap(True),
    physics_controlled: get_element_attribute(element, "physics-controlled")
      |> result.try(parse_bool)
      |> result.unwrap(False),
    children: parse_children(element),
  )
}

fn parse_camera(element, key) {
  scene.CameraNode(
    key:,
    camera_type: get_element_attribute(element, "type")
      |> result.unwrap("perspective"),
    fov: get_element_attribute(element, "fov")
      |> result.try(float.parse)
      |> result.unwrap(75.0),
    near: get_element_attribute(element, "near")
      |> result.try(float.parse)
      |> result.unwrap(0.1),
    far: get_element_attribute(element, "far")
      |> result.try(float.parse)
      |> result.unwrap(1000.0),
    transform: get_element_attribute(element, "transform")
      |> result.map(transform.parse)
      |> result.unwrap(transform.identity),
    active: get_element_attribute(element, "active")
      |> result.try(parse_bool)
      |> result.unwrap(False),
  )
}

fn parse_light(element, key) {
  scene.LightNode(
    key:,
    light_type: get_element_attribute(element, "type")
      |> result.unwrap("point"),
    color: get_element_attribute(element, "color") |> result.unwrap("#ffffff"),
    intensity: get_element_attribute(element, "intensity")
      |> result.try(float.parse)
      |> result.unwrap(1.0),
    transform: get_element_attribute(element, "transform")
      |> result.map(transform.parse)
      |> result.unwrap(transform.identity),
    cast_shadow: get_element_attribute(element, "cast-shadow")
      |> result.try(parse_bool)
      |> result.unwrap(False),
  )
}

fn parse_empty(element, key) {
  scene.EmptyNode(
    key:,
    transform: get_element_attribute(element, "transform")
      |> result.map(transform.parse)
      |> result.unwrap(transform.identity),
    visible: get_element_attribute(element, "visible")
      |> result.try(parse_bool)
      |> result.unwrap(True),
    children: parse_children(element),
  )
}

fn parse_audio(element, key) {
  scene.AudioNode(
    key:,
    src: get_element_attribute(element, "src") |> result.unwrap(""),
    volume: get_element_attribute(element, "volume")
      |> result.try(float.parse)
      |> result.unwrap(1.0),
    loop: get_element_attribute(element, "loop")
      |> result.try(parse_bool)
      |> result.unwrap(False),
    playing: get_element_attribute(element, "playing")
      |> result.try(parse_bool)
      |> result.unwrap(False),
    playback_rate: get_element_attribute(element, "playback-rate")
      |> result.try(float.parse)
      |> result.unwrap(1.0),
  )
}

fn parse_positional_audio(element, key) {
  scene.PositionalAudioNode(
    key:,
    src: get_element_attribute(element, "src") |> result.unwrap(""),
    volume: get_element_attribute(element, "volume")
      |> result.try(float.parse)
      |> result.unwrap(1.0),
    loop: get_element_attribute(element, "loop")
      |> result.try(parse_bool)
      |> result.unwrap(False),
    playing: get_element_attribute(element, "playing")
      |> result.try(parse_bool)
      |> result.unwrap(False),
    playback_rate: get_element_attribute(element, "playback-rate")
      |> result.try(float.parse)
      |> result.unwrap(1.0),
    transform: get_element_attribute(element, "transform")
      |> result.map(transform.parse)
      |> result.unwrap(transform.identity),
    ref_distance: get_element_attribute(element, "ref-distance")
      |> result.try(float.parse)
      |> result.unwrap(1.0),
    max_distance: get_element_attribute(element, "max-distance")
      |> result.try(float.parse)
      |> result.unwrap(100.0),
    rolloff_factor: get_element_attribute(element, "rolloff-factor")
      |> result.try(float.parse)
      |> result.unwrap(1.0),
    children: parse_children(element),
  )
}

@external(javascript, "./renderer.ffi.mjs", "elementAttribute")
fn get_element_attribute(
  element: Element(msg),
  attribute: String,
) -> Result(String, Nil)

fn parse_bool(value: String) -> Result(Bool, Nil) {
  case value {
    "" | "true" -> Ok(True)
    "false" -> Ok(False)
    _ -> Error(Nil)
  }
}

fn parse_children(element) {
  list.map(get_element_children(element), parse_element)
}

@external(javascript, "./renderer.ffi.mjs", "elementChildren")
fn get_element_children(element: Element(msg)) -> List(Element(msg))
