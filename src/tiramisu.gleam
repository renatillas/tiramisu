//// Tiramisu game engine main module - immutable game loop with effect system.
////
//// This module provides the core game loop following the Model-View-Update (MVU) architecture,
//// inspired by Lustre. Your game state is immutable, and updates return new state along with effects.
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu
//// import tiramisu/effect
//// import tiramisu/scene
//// import tiramisu/transform
////
//// type Model {
////   Model(rotation: Float)
//// }
////
//// type Msg {
////   Frame
//// }
////
//// pub fn main() {
////   tiramisu.run(
////     width: 800,
////     height: 600,
////     background: 0x111111,
////     init: init,
////     update: update,
////     view: view,
////   )
//// }
////
//// fn init(_ctx: tiramisu.Context) {
////   #(Model(rotation: 0.0), effect.none())
//// }
////
//// fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
////   case msg {
////     Frame -> {
////       let new_rotation = model.rotation +. ctx.delta_time
////       #(Model(rotation: new_rotation), effect.none())
////     }
////   }
//// }
////
//// fn view(model: Model) {
////   [
////     scene.Mesh(
////       id: "cube",
////       geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
////       material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
////       transform: transform.identity()
////         |> transform.set_rotation(vec3.Vec3(model.rotation, model.rotation, 0.0)),
////       physics: option.None,
////     ),
////   ]
//// }
//// ```

import tiramisu/effect
import tiramisu/input
import tiramisu/internal/renderer
import tiramisu/scene

/// Internal Three.js Scene type (opaque)
@internal
pub type Scene

/// Game context passed to init and update functions.
///
/// Contains timing information and input state for the current frame.
///
/// ## Fields
///
/// - `delta_time`: Time in seconds since the last frame (useful for frame-rate independent movement)
/// - `input`: Current input state (keyboard, mouse, touch)
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: Context) {
///   // Move player based on delta time for smooth motion
///   let speed = 5.0
///   let new_x = model.x +. speed *. ctx.delta_time
///
///   // Check if space key is pressed
///   case input.is_key_down(ctx.input, "Space") {
///     True -> jump(model)
///     False -> Model(..model, x: new_x)
///   }
/// }
/// ```
pub type Context {
  Context(delta_time: Float, input: input.InputState)
}

/// Initialize and run the game loop.
///
/// This is the main entry point for your game. It sets up the renderer, initializes your game state,
/// and starts the game loop. The loop will call your `update` and `view` functions each frame.
///
/// ## Parameters
///
/// - `width`: Canvas width in pixels
/// - `height`: Canvas height in pixels
/// - `background`: Background color as hex integer (e.g., 0x111111 for dark gray)
/// - `init`: Function to create initial game state and effect
/// - `update`: Function to update state based on messages
/// - `view`: Function to render your game state as scene nodes
///
/// ## Camera Setup
///
/// You must include a `Camera` scene node with `active: True` in your initial scene.
/// Use `effect.set_active_camera(id)` to switch between cameras at runtime.
///
/// ## Example
///
/// ```gleam
/// import tiramisu
/// import tiramisu/camera
/// import tiramisu/effect
/// import tiramisu/scene
/// import tiramisu/transform
/// import vec/vec3
///
/// type Model {
///   Model(rotation: Float)
/// }
///
/// type Msg {
///   Tick
/// }
///
/// pub fn main() {
///   tiramisu.run(
///     width: 800,
///     height: 600,
///     background: 0x111111,
///     init: fn(_ctx) {
///       #(Model(rotation: 0.0), effect.batch([
///         effect.on_animation_frame(Tick),
///       ]))
///     },
///     update: fn(model, msg, ctx) {
///       case msg {
///         Tick -> #(
///           Model(rotation: model.rotation +. ctx.delta_time),
///           effect.on_animation_frame(Tick),
///         )
///       }
///     },
///     view: fn(model) {
///       [
///         scene.Camera(
///           id: "main-camera",
///           camera: camera.perspective(fov: 75.0, near: 0.1, far: 1000.0),
///           transform: transform.at(vec3.Vec3(0.0, 0.0, 5.0)),
///           active: True,
///           viewport: option.None,
///         ),
///         scene.Mesh(
///           id: "cube",
///           geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
///           material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
///           transform: transform.identity()
///             |> transform.set_rotation(vec3.Vec3(model.rotation, model.rotation, 0.0)),
///           physics: option.None,
///         ),
///       ]
///     },
///   )
/// }
/// ```
pub fn run(
  width width: Int,
  height height: Int,
  background background: Int,
  init init: fn(Context) -> #(state, effect.Effect(msg)),
  update update: fn(state, msg, Context) -> #(state, effect.Effect(msg)),
  view view: fn(state) -> List(scene.SceneNode),
) -> Nil {
  // Create Three.js objects
  let renderer_obj =
    renderer.create(renderer.RendererOptions(
      antialias: True,
      alpha: False,
      width: width,
      height: height,
    ))

  let scene_obj = create_scene(background)

  // Initial context with empty input
  let initial_context = Context(delta_time: 0.0, input: input.new())

  // Initialize game state
  let #(initial_state, initial_effect) = init(initial_context)

  // Get initial scene nodes
  let initial_nodes = view(initial_state)

  // Append renderer to DOM and initialize input
  let canvas = renderer.get_dom_element(renderer_obj)
  append_to_dom(canvas)
  initialize_input_systems(canvas)

  // Apply initial scene (this will set up cameras from scene nodes)
  apply_initial_scene(scene_obj, initial_nodes)

  // Start game loop
  start_loop(
    initial_state,
    initial_nodes,
    initial_effect,
    initial_context,
    scene_obj,
    renderer_obj,
    update,
    view,
  )
}

// --- FFI Declarations ---

@external(javascript, "./tiramisu.ffi.mjs", "createScene")
fn create_scene(background: Int) -> Scene

@external(javascript, "./tiramisu.ffi.mjs", "appendToDom")
fn append_to_dom(element: renderer.DomElement) -> Nil

@external(javascript, "./tiramisu.ffi.mjs", "initializeInputSystems")
fn initialize_input_systems(canvas: renderer.DomElement) -> Nil

@external(javascript, "./tiramisu.ffi.mjs", "applyInitialScene")
fn apply_initial_scene(scene: Scene, nodes: List(scene.SceneNode)) -> Nil

@external(javascript, "./tiramisu.ffi.mjs", "startLoop")
fn start_loop(
  state: state,
  prev_nodes: List(scene.SceneNode),
  effect: effect.Effect(msg),
  context: Context,
  scene: Scene,
  renderer: renderer.WebGLRenderer,
  update: fn(state, msg, Context) -> #(state, effect.Effect(msg)),
  view: fn(state) -> List(scene.SceneNode),
) -> Nil
