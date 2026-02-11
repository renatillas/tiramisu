//// The tiramisu-renderer web component.
////
//// This component owns the WebGL renderer and Three.js scene. It provides
//// scene context to all child components (mesh, camera, light, empty).
////
//// ## Usage
////
//// ```html
//// <tiramisu-renderer width="800" height="600" background="#1a1a2e">
////   <tiramisu-camera id="main" position="0,5,10" active="true"></tiramisu-camera>
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
////
//// ## Scene ID for Tick Subscriptions
////
//// You can either:
//// 1. Use a user-defined scene ID via the `scene-id` attribute:
////    ```gleam
////    renderer.renderer([renderer.scene_id("my-scene")], [...])
////    // Then subscribe with:
////    tick.subscribe("my-scene", Tick)
////    ```
////
//// 2. Listen for the `tiramisu:scene-ready` event to get the scene ID dynamically:
////    ```gleam
////    renderer.on_scene_ready(fn(scene_id) { SceneReady(scene_id) })
////    ```

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/option.{type Option, None, Some}

import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

import tiramisu/context.{SceneContext}
import tiramisu/internal/runtime.{type RendererRef, type SceneRef}

// TYPES -----------------------------------------------------------------------

/// The model for the renderer component.
pub type Model {
  Model(
    /// Reference to the registered renderer
    renderer_ref: Option(RendererRef),
    /// Reference to the registered scene
    scene_ref: Option(SceneRef),
    /// Scene ID for context provision
    scene_id: Option(String),
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

/// Tick context
/// Messages for the renderer component.
pub type Msg {
  /// Scene and renderer have been initialized
  Initialized(renderer_ref: RendererRef, scene_ref: SceneRef, scene_id: String)
  /// Canvas was resized
  Resized(width: Int, height: Int)
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
pub fn register() -> Result(Nil, lustre.Error) {
  let app =
    lustre.component(init, update, view, [
      // Attribute handlers
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
        case v {
          "true" | "1" | "" -> Ok(AntialiasChanged(True))
          "false" | "0" -> Ok(AntialiasChanged(False))
          _ -> Error(Nil)
        }
      }),
      component.on_attribute_change("alpha", fn(v) {
        case v {
          "true" | "1" | "" -> Ok(AlphaChanged(True))
          "false" | "0" -> Ok(AlphaChanged(False))
          _ -> Error(Nil)
        }
      }),
    ])

  lustre.register(app, tag_name)
}

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-renderer element.
///
/// This is the root container for all 3D content. It creates a WebGL renderer
/// and scene, and provides context to all child components.
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
/// ## Example
///
/// ```gleam
/// import tiramisu/renderer
/// import tiramisu/tick
///
/// // In your view:
/// renderer.renderer([renderer.scene_id("my-scene")], [...])
///
/// // In your init:
/// tick.subscribe("my-scene", Tick)
/// ```
///
pub fn scene_id(id: String) -> Attribute(msg) {
  attribute.attribute("scene-id", id)
}

/// Listen for the scene-ready event.
///
/// This event fires when the renderer has initialized its scene. The handler
/// receives the scene ID, which you can store in your model for later use
/// with `tick.subscribe`.
///
/// ## Example
///
/// ```gleam
/// pub type Msg {
///   SceneReady(String)
///   Tick(tick.TickContext)
/// }
///
/// fn view(model: Model) {
///   renderer.renderer(
///     [renderer.on_scene_ready(SceneReady)],
///     [...]
///   )
/// }
///
/// fn update(model: Model, msg: Msg) {
///   case msg {
///     SceneReady(scene_id) -> {
///       // Now subscribe to ticks for this specific scene
///       #(Model(..model, scene_id: Some(scene_id)), tick.subscribe(scene_id, Tick))
///     }
///     // ...
///   }
/// }
/// ```
///
pub fn on_scene_ready(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("tiramisu:scene-ready", {
    use scene_id <- decode.subfield(["detail", "sceneId"], decode.string)
    decode.success(handler(scene_id))
  })
}

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      renderer_ref: None,
      scene_ref: None,
      scene_id: None,
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
  let user_scene_id = get_scene_id_from_host_ffi(root)
  let scene_ref = runtime.create_scene_with_id(user_scene_id)
  let runtime.SceneRef(scene_id) = scene_ref

  // Get config from host element attributes
  let config = get_renderer_config_ffi(root)

  // Create renderer using savoiardi via runtime module
  let renderer_ref = runtime.create_renderer(root, config)

  // Start the render loop
  runtime.start_render_loop(renderer_ref, scene_ref)

  // Set the scene ID on the host element for child components to find
  // This is needed because Lustre's context system doesn't work across
  // separate web component instances
  set_scene_id_on_host_ffi(root, scene_id)

  dispatch(Initialized(renderer_ref:, scene_ref:, scene_id:))
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Initialized(renderer_ref:, scene_ref:, scene_id:) -> {
      let new_model =
        Model(
          ..model,
          renderer_ref: Some(renderer_ref),
          scene_ref: Some(scene_ref),
          scene_id: Some(scene_id),
        )
      // Provide scene context to children
      let provide_effect = provide_scene_context(scene_id)
      #(new_model, provide_effect)
    }

    Resized(width:, height:) -> {
      let new_model = Model(..model, width:, height:)
      let resize_effect = case model.renderer_ref {
        Some(ref) ->
          effect.from(fn(_) { runtime.resize_renderer(ref, width, height) })
        None -> effect.none()
      }
      #(new_model, resize_effect)
    }

    WidthChanged(width) -> {
      let new_model = Model(..model, width:)
      case model.renderer_ref {
        Some(ref) -> #(
          new_model,
          effect.from(fn(_) {
            runtime.resize_renderer(ref, width, model.height)
          }),
        )
        None -> #(new_model, effect.none())
      }
    }

    HeightChanged(height) -> {
      let new_model = Model(..model, height:)
      case model.renderer_ref {
        Some(ref) -> #(
          new_model,
          effect.from(fn(_) {
            runtime.resize_renderer(ref, model.width, height)
          }),
        )
        None -> #(new_model, effect.none())
      }
    }

    BackgroundChanged(background) -> {
      let new_model = Model(..model, background:)
      case model.renderer_ref {
        Some(ref) -> #(
          new_model,
          effect.from(fn(_) { set_background_ffi(ref, background) }),
        )
        None -> #(new_model, effect.none())
      }
    }

    AntialiasChanged(antialias) -> {
      // Antialiasing can only be set at renderer creation time
      #(Model(..model, antialias:), effect.none())
    }

    AlphaChanged(alpha) -> {
      // Alpha can only be set at renderer creation time
      #(Model(..model, alpha:), effect.none())
    }
  }
}

/// Provide scene context to child components.
fn provide_scene_context(scene_id: String) -> Effect(Msg) {
  let ctx = SceneContext(scene_id:)
  effect.provide(context.scene_context_key, context.encode_scene_context(ctx))
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  // The renderer component renders a container div with a slot for children.
  // The canvas is appended to the shadow root by the FFI during initialization.
  html.div([], [
    // Slot for child components (mesh, camera, light, empty)
    component.default_slot([], []),
  ])
}

// FFI DECLARATIONS ------------------------------------------------------------

@external(javascript, "./renderer.ffi.mjs", "getRendererConfig")
fn get_renderer_config_ffi(root: Dynamic) -> runtime.RendererConfig

@external(javascript, "./renderer.ffi.mjs", "getSceneIdFromHost")
fn get_scene_id_from_host_ffi(root: Dynamic) -> String

@external(javascript, "./renderer.ffi.mjs", "setBackground")
fn set_background_ffi(renderer_ref: RendererRef, color: String) -> Nil

@external(javascript, "./renderer.ffi.mjs", "setSceneIdOnHost")
fn set_scene_id_on_host_ffi(root: Dynamic, scene_id: String) -> Nil
