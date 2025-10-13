//// Tiramisu game engine main module - immutable game loop with effect system.
////
//// This module provides the core game loop following the Model-View-Update (MVU) architecture,
//// inspired by Lustre. Your game state is immutable, and updates return new state along with effects.
////
//// ## Quick Example
////
//// ```gleam
//// import gleam/option
//// import tiramisu
//// import tiramisu/camera
//// import tiramisu/effect
//// import tiramisu/geometry
//// import tiramisu/material
//// import tiramisu/scene
//// import tiramisu/transform
//// import vec/vec3
////
//// type Model {
////   Model(rotation: Float)
//// }
////
//// type Msg {
////   Tick
//// }
////
//// type Ids {
////   Cube
////   MainCamera
//// }
////
//// pub fn main() {
////   tiramisu.run(
////     dimensions: option.None,
////     background: tiramisu.Color(0x111111),
////     init: init,
////     update: update,
////     view: view,
////   )
//// }
////
//// fn init(_ctx: tiramisu.Context(Ids)) {
////   #(Model(rotation: 0.0), effect.tick(Tick), option.None)
//// }
////
//// fn update(model: Model, msg: Msg, ctx: tiramisu.Context(Ids)) {
////   case msg {
////     Tick -> {
////       let new_rotation = model.rotation +. ctx.delta_time
////       #(Model(rotation: new_rotation), effect.tick(Tick), option.None)
////     }
////   }
//// }
////
//// fn view(model: Model, _ctx: tiramisu.Context(Ids)) {
////   let assert Ok(cam) = camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
////   let assert Ok(cube_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
////   let assert Ok(cube_mat) =
////     material.new()
////     |> material.with_color(0xff0000)
////     |> material.build()
////
////   [
////     scene.Camera(
////       id: MainCamera,
////       camera: cam,
////       transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
////       look_at: option.None,
////       active: True,
////       viewport: option.None,
////     ),
////     scene.Mesh(
////       id: Cube,
////       geometry: cube_geo,
////       material: cube_mat,
////       transform: transform.identity
////         |> transform.rotate_y(model.rotation),
////       physics: option.None,
////     ),
////   ]
//// }
//// ```

import gleam/option.{type Option}
import tiramisu/background.{type Background}
import tiramisu/effect
import tiramisu/input
import tiramisu/internal/input_manager
import tiramisu/internal/renderer
import tiramisu/physics
import tiramisu/scene

/// Internal Three.js Scene type (opaque)
@internal
pub type Scene

/// Canvas dimensions for the game window.
///
/// Used with `tiramisu.run()` to specify the size of the game canvas.
/// If not provided (None), the game will run in fullscreen mode.
///
/// ## Example
///
/// ```gleam
/// import gleam/option.{None, Some}
/// import tiramisu
///
/// // Fullscreen mode
/// tiramisu.run(
///   dimensions: None,
///   background: tiramisu.Color(0x111111),
///   // ...
/// )
///
/// // Fixed size
/// tiramisu.run(
///   dimensions: Some(tiramisu.Dimensions(width: 800.0, height: 600.0)),
///   background: tiramisu.Color(0x111111),
///   // ...
/// )
/// ```
pub type Dimensions {
  Dimensions(width: Float, height: Float)
}

/// Game context passed to init and update functions.
///
/// Contains timing information, input state, canvas dimensions, and physics world for the current frame.
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: Context) {
///   // Move player based on delta time for smooth motion
///   let speed = 5.0
///   let new_x = model.x +. speed *. ctx.delta_time
///
///   // Convert screen coordinates to world space
///   let world_x = { screen_x -. ctx.canvas_width /. 2.0 } /. 100.0
///   let world_y = { ctx.canvas_height /. 2.0 -. screen_y } /. 100.0
///
///   // Check if space key is pressed
///   case input.is_key_down(ctx.input, "Space") {
///     True -> jump(model)
///     False -> Model(..model, x: new_x)
///   }
/// }
/// ```
pub type Context(id) {
  Context(
    delta_time: Float,
    input: input.InputState,
    canvas_width: Float,
    canvas_height: Float,
    physics_world: Option(physics.PhysicsWorld(id)),
    input_manager: input_manager.InputManager,
  )
}

/// Initialize and run the game loop.
///
/// This is the main entry point for your game. It sets up the renderer, initializes your game state,
/// and starts the game loop. The loop will call your `update` and `view` functions each frame.
///
/// ## Parameters
///
/// - `dimensions`: Canvas dimensions (width and height). Use `None` for fullscreen mode.
/// - `background`: Background as Color, Texture, or CubeTexture (see `Background` type)
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
/// import gleam/option.{None, Some}
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
///   // Fullscreen mode with color background
///   tiramisu.run(
///     dimensions: None,
///     background: tiramisu.Color(0x111111),
///     init: fn(_ctx) {
///       #(Model(rotation: 0.0), effect.tick(Tick), option.None)
///     },
///     update: fn(model, msg, ctx) {
///       case msg {
///         Tick -> #(
///           Model(rotation: model.rotation +. ctx.delta_time),
///           effect.tick(Tick),
///           option.None,
///         )
///       }
///     },
///     view: fn(model, _ctx) {
///       let assert Ok(cam) = camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
///       let assert Ok(geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
///       let assert Ok(mat) = material.new() |> material.with_color(0xff0000) |> material.build()
///
///       [
///         scene.Camera(
///           id: "main-camera",
///           camera: cam,
///           transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
///           look_at: option.None,
///           active: True,
///           viewport: option.None,
///         ),
///         scene.Mesh(
///           id: "cube",
///           geometry: geo,
///           material: mat,
///           transform: transform.identity
///             |> transform.rotate_y(model.rotation),
///           physics: option.None,
///         ),
///       ]
///     },
///   )
/// }
/// ```
pub fn run(
  dimensions dimensions: Option(Dimensions),
  background background: Background,
  init init: fn(Context(id)) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld(id))),
  update update: fn(state, msg, Context(id)) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld(id))),
  view view: fn(state, Context(id)) -> List(scene.Node(id)),
) -> Nil {
  // Create renderer state (audio manager is initialized internally)
  let renderer_state =
    renderer.create(renderer.RendererOptions(
      antialias: True,
      alpha: False,
      dimensions: dimensions
        |> option.map(fn(dimensions) {
          renderer.Dimensions(
            height: dimensions.height,
            width: dimensions.width,
          )
        }),
    ))

  // Set background on the Three.js scene
  let scene_obj = renderer.get_scene(renderer_state)
  set_background(scene_obj, background)

  // Get canvas and append to DOM first (so dimensions are available)
  let webgl_renderer = renderer.get_renderer(renderer_state)
  let canvas = renderer.get_dom_element(webgl_renderer)
  append_to_dom(canvas)

  // Now get canvas dimensions (after it's in the DOM)
  let #(initial_width, initial_height) = get_canvas_dimensions(webgl_renderer)

  // Create input manager
  let input_manager = input_manager.new(canvas)

  // Initial context with empty input (no physics_world yet)
  let initial_context =
    Context(
      delta_time: 0.0,
      input: input.new(),
      canvas_width: initial_width,
      canvas_height: initial_height,
      physics_world: option.None,
      input_manager: input_manager,
    )

  // Initialize game state
  let #(initial_state, initial_effect, physics_world) = init(initial_context)

  // Create context with physics_world for the game loop
  let context_with_physics =
    Context(
      delta_time: 0.0,
      input: input.new(),
      canvas_width: initial_width,
      canvas_height: initial_height,
      physics_world: physics_world,
      input_manager: input_manager,
    )

  // Set physics world in renderer state if provided
  let renderer_state_with_physics = case physics_world {
    option.Some(world) -> {
      renderer.set_physics_world(renderer_state, option.Some(world))
    }
    option.None -> renderer.set_physics_world(renderer_state, option.None)
  }

  // Get initial scene nodes
  let initial_nodes = view(initial_state, context_with_physics)

  // Apply initial scene using renderer.gleam
  let renderer_state_after_init =
    apply_initial_scene_gleam(renderer_state_with_physics, initial_nodes)

  // Extract updated physics world from renderer (it now has bodies created during patching)
  let updated_context = case
    renderer.get_physics_world(renderer_state_after_init)
  {
    option.Some(updated_world) -> {
      // Convert opaque physics world back to typed physics world
      Context(..context_with_physics, physics_world: option.Some(updated_world))
    }
    option.None -> context_with_physics
  }

  // Start game loop with updated context containing bodies
  start_loop(
    initial_state,
    initial_nodes,
    initial_effect,
    updated_context,
    renderer_state_after_init,
    update,
    view,
  )
}

// --- Helper Functions ---

/// Apply initial scene using renderer.gleam's patch system
fn apply_initial_scene_gleam(
  renderer_state: renderer.RendererState(id),
  nodes: List(scene.Node(id)),
) -> renderer.RendererState(id) {
  // Use diff with empty previous scene to generate proper patches
  let patches = scene.diff([], nodes)
  renderer.apply_patches(renderer_state, patches)
}

// --- FFI Declarations ---

@external(javascript, "./tiramisu.ffi.mjs", "appendToDom")
fn append_to_dom(element: renderer.DomElement) -> Nil

@external(javascript, "./tiramisu.ffi.mjs", "getCanvasDimensions")
fn get_canvas_dimensions(renderer: renderer.WebGLRenderer) -> #(Float, Float)

@external(javascript, "./tiramisu.ffi.mjs", "setBackground")
fn set_background(scene: renderer.Scene, background: Background) -> Nil

@external(javascript, "./tiramisu.ffi.mjs", "startLoop")
fn start_loop(
  state: state,
  prev_nodes: List(scene.Node(id)),
  effect: effect.Effect(msg),
  context: Context(id),
  renderer_state: renderer.RendererState(id),
  update: fn(state, msg, Context(id)) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld(id))),
  view: fn(state, Context(id)) -> List(scene.Node(id)),
) -> Nil
