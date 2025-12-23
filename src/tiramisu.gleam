////
//// This module provides the core game loop following the Model-View-Update (MVU) architecture,
//// inspired by Lustre. Your game state is immutable, and updates return new state along with effects.
////

import gleam/option.{type Option}
import gleam/time/duration.{type Duration}
import tiramisu/background.{type Background}
import tiramisu/effect
import tiramisu/input
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
pub type Dimensions {
  Dimensions(width: Float, height: Float)
}

/// Game context passed to init and update functions.
///
/// Contains timing information, input state, canvas dimensions, and physics world for the current frame.
///
/// ## Fields
///
/// - `delta_time`: Time elapsed since the last frame as a Duration. Use this for frame-rate independent movement and animations.
/// - `input`: Current input state (keyboard, mouse, touch, gamepad)
/// - `canvas_width`: Current canvas width in pixels
/// - `canvas_height`: Current canvas height in pixels
/// - `physics_world`: Optional physics world handle (if physics is enabled)
///
pub type Context {
  Context(
    /// Time elapsed since the last frame as a Duration
    delta_time: Duration,
    /// Current input state (keyboard, mouse, touch, gamepad)
    input: input.InputState,
    /// Current canvas width in pixels
    canvas_width: Float,
    /// Current canvas height in pixels
    canvas_height: Float,
    /// Physics world handle (if physics is enabled)
    physics_world: Option(physics.PhysicsWorld),
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
pub fn run(
  dimensions dimensions: Option(Dimensions),
  background background: Background,
  init init: fn(Context) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld)),
  update update: fn(state, msg, Context) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld)),
  view view: fn(state, Context) -> scene.Node,
) -> Nil {
  // Create renderer state (audio manager is initialized internally)
  let renderer_state =
    scene.create(scene.RendererOptions(
      antialias: True,
      alpha: False,
      dimensions: dimensions
        |> option.map(fn(dimensions) {
          scene.Dimensions(height: dimensions.height, width: dimensions.width)
        }),
    ))

  // Set background on the Three.js scene
  let scene_obj = scene.get_scene(renderer_state)
  set_background(scene_obj, background)

  // Get canvas and append to DOM first (so dimensions are available)
  let webgl_renderer = scene.get_renderer(renderer_state)
  let canvas = scene.get_dom_element(webgl_renderer)
  append_to_dom(canvas)

  // Now get canvas dimensions (after it's in the DOM)
  let #(initial_width, initial_height) = get_canvas_dimensions(webgl_renderer)

  // Initial context with empty input (no physics_world yet)
  let initial_context =
    Context(
      delta_time: duration.nanoseconds(0),
      input: input.new(),
      canvas_width: initial_width,
      canvas_height: initial_height,
      physics_world: option.None,
    )

  // Initialize game state
  let #(initial_state, initial_effect, physics_world) = init(initial_context)

  // Create context with physics_world for the game loop
  let context_with_physics =
    Context(
      delta_time: duration.nanoseconds(0),
      input: input.new(),
      canvas_width: initial_width,
      canvas_height: initial_height,
      physics_world: physics_world,
    )

  // Set physics world in renderer state if provided
  let renderer_state_with_physics = case physics_world {
    option.Some(world) -> {
      scene.set_physics_world(renderer_state, option.Some(world))
    }
    option.None -> scene.set_physics_world(renderer_state, option.None)
  }

  // Get initial scene root node
  let initial_root = view(initial_state, context_with_physics)

  // Apply initial scene using renderer.gleam
  let renderer_state_after_init =
    apply_initial_scene_gleam(renderer_state_with_physics, initial_root)

  // Extract updated physics world from renderer (it now has bodies created during patching)
  let updated_context = case
    scene.get_physics_world(renderer_state_after_init)
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
    initial_root,
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
  renderer_state: scene.RendererState,
  node: scene.Node,
) -> scene.RendererState {
  // Use diff with empty previous scene to generate proper patches
  // No cache available for first frame
  let #(patches, new_dict) =
    scene.diff(option.None, option.Some(node), option.None)
  let renderer_state = scene.apply_patches(renderer_state, patches)
  // Store the initial scene dict for next frame
  scene.set_cached_scene_dict(renderer_state, option.Some(new_dict))
}

// --- FFI Declarations ---

@external(javascript, "./tiramisu.ffi.mjs", "appendToDom")
fn append_to_dom(element: scene.DomElement) -> Nil

@external(javascript, "./tiramisu.ffi.mjs", "getCanvasDimensions")
fn get_canvas_dimensions(renderer: scene.WebGLRenderer) -> #(Float, Float)

@external(javascript, "./tiramisu.ffi.mjs", "setBackground")
fn set_background(scene: scene.Scene, background: Background) -> Nil

@external(javascript, "./tiramisu.ffi.mjs", "startLoop")
fn start_loop(
  state: state,
  prev_node: scene.Node,
  effect: effect.Effect(msg),
  context: Context,
  renderer_state: scene.RendererState,
  update: fn(state, msg, Context) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld)),
  view: fn(state, Context) -> scene.Node,
) -> Nil
