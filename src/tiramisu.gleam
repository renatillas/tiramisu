////
//// This module provides the core game loop following the Model-View-Update (MVU) architecture,
//// inspired by Lustre. Your game state is immutable, and updates return new state along with effects.
////

import gleam/float
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/time/duration.{type Duration}
import plinth/browser/document
import plinth/browser/element
import plinth/browser/window
import savoiardi
import tiramisu/camera
import tiramisu/effect
import tiramisu/input
import tiramisu/internal/input_init
import tiramisu/internal/input_manager
import tiramisu/physics
import tiramisu/scene

/// Canvas dimensions for the game window.
///
/// Used with `tiramisu.run()` to specify the size of the game canvas.
/// If not provided (None), the game will run in fullscreen mode.
///
pub type Dimensions {
  Dimensions(width: Float, height: Float)
}

/// Opaque type for the WebGL renderer (for performance stats)
pub type Renderer =
  scene.WebGLRenderer

/// Game context passed to init and update functions.
///
/// Contains timing information, input state, canvas dimensions, physics world, scene, and renderer for the current frame.
///
/// ## Fields
///
/// - `delta_time`: Time elapsed since the last frame as a Duration. Use this for frame-rate independent movement and animations.
/// - `input`: Current input state (keyboard, mouse, touch, gamepad)
/// - `canvas_width`: Current canvas width in pixels
/// - `canvas_height`: Current canvas height in pixels
/// - `physics_world`: Optional physics world handle (if physics is enabled)
/// - `scene`: The Three.js scene instance (for background changes, etc.)
/// - `renderer`: The WebGL renderer (for performance stats)
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
    /// The Three.js scene instance
    scene: scene.Scene,
    /// The WebGL renderer (for performance stats)
    renderer: Renderer,
  )
}

/// Initialize and run the game loop.
///
/// This is the main entry point for your game. It sets up the renderer, initializes your game state,
/// and starts the game loop. The loop will call your `update` and `view` functions each frame.
///
/// ## Parameters
///
/// - `selector`: CSS selector for the container element (e.g., "#app", ".game-container")
/// - `dimensions`: Canvas dimensions (width and height). Use `None` for fullscreen mode.
/// - `init`: Function to create initial game state and effect
/// - `update`: Function to update state based on messages
/// - `view`: Function to render your game state as scene nodes
///
/// ## Example
///
/// ```gleam
/// import tiramisu
/// 
/// pub fn main() {
///   tiramisu.run(
///     selector: "#game",
///     dimensions: None,
///     init: init,
///     update: update,
///     view: view,
///   )
/// }
/// ```
///
pub fn run(
  selector selector: String,
  dimensions dimensions: Option(Dimensions),
  init init: fn(Context) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld)),
  update update: fn(state, msg, Context) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld)),
  view view: fn(state, Context) -> scene.Node,
) -> Result(Nil, Nil) {
  // Find the container element using the selector
  use container <- result.try(document.query_selector(selector))

  // Create renderer state (audio manager is initialized internally)
  let renderer_state =
    scene.new_render_state(savoiardi.RendererOptions(
      antialias: True,
      alpha: False,
      dimensions: dimensions
        |> option.map(fn(dimensions) {
          savoiardi.Dimensions(
            height: dimensions.height,
            width: dimensions.width,
          )
        }),
    ))

  // Get canvas and append to container (so dimensions are available)
  let webgl_renderer = scene.get_renderer(renderer_state)
  let canvas = savoiardi.get_renderer_dom_element(webgl_renderer)
  element.append_child(container, canvas |> coerce)

  // Create input manager (will be initialized with listeners in game_loop)
  let input_mgr = input_manager.new()

  // Now get canvas dimensions (after it's in the DOM)
  let #(initial_width, initial_height) =
    savoiardi.get_canvas_dimensions(webgl_renderer)

  // Get the Three.js scene from renderer state
  let three_scene = scene.get_scene(renderer_state)

  // Initial context with empty input (no physics_world yet)
  let initial_context =
    Context(
      delta_time: duration.nanoseconds(0),
      input: input.new(),
      canvas_width: initial_width,
      canvas_height: initial_height,
      physics_world: option.None,
      scene: three_scene,
      renderer: webgl_renderer,
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
      scene: three_scene,
      renderer: webgl_renderer,
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

  // Start game loop (now in pure Gleam!)
  game_loop(
    state: initial_state,
    prev_node: initial_root,
    pending_effect: initial_effect,
    context: updated_context,
    renderer_state: renderer_state_after_init,
    message_queue: [],
    input_manager: input_mgr,
    canvas: canvas |> coerce,
    update: update,
    view: view,
  )

  Ok(Nil)
}

// ============================================================================
// GAME LOOP - Pure Gleam Implementation
// ============================================================================

/// Start the game loop
fn game_loop(
  state state: state,
  prev_node prev_node: scene.Node,
  pending_effect pending_effect: effect.Effect(msg),
  context context: Context,
  renderer_state renderer_state: scene.RendererState,
  message_queue message_queue: List(msg),
  input_manager input_manager: input_manager.InputManager,
  canvas canvas: element.Element,
  update update: fn(state, msg, Context) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld)),
  view view: fn(state, Context) -> scene.Node,
) -> Nil {
  // Create the runtime instance that holds all mutable state (no globals!)
  let runtime =
    create_runtime_ffi(
      state,
      prev_node,
      context,
      renderer_state,
      message_queue,
      input_manager,
    )

  // Set up input event listeners
  let _ =
    input_init.initialize(
      canvas,
      fn() { get_runtime_input_manager_ffi(runtime) },
      fn(updated_mgr) { update_runtime_input_manager_ffi(runtime, updated_mgr) },
    )

  // Run initial effect
  let dispatch = get_runtime_dispatch_ffi(runtime)
  effect.run(pending_effect, dispatch)

  // Start the animation loop
  schedule_frame_with_runtime(runtime, update, view)
}

/// Schedule the next animation frame using the runtime
fn schedule_frame_with_runtime(
  runtime: GameRuntime,
  update: fn(state, msg, Context) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld)),
  view: fn(state, Context) -> scene.Node,
) -> Nil {
  window.request_animation_frame(fn(timestamp) {
    // Get frame data from runtime (includes delta_time, input, canvas dimensions)
    let frame_data =
      process_frame_ffi(
        runtime,
        update,
        view,
        timestamp,
        input_manager.capture_state,
        input_manager.clear_frame_state,
      )

    // Update context with new delta_time, input, and canvas dimensions
    let updated_context =
      Context(
        ..frame_data.context,
        delta_time: duration.milliseconds(float.round(frame_data.delta_time_ms)),
        input: frame_data.input_state,
        canvas_width: frame_data.canvas_width,
        canvas_height: frame_data.canvas_height,
      )

    // Process all queued messages
    let #(new_state, combined_effect, final_physics_world) =
      process_messages(
        frame_data.state,
        frame_data.messages,
        updated_context,
        option.None,
        update,
      )

    // Update context with new physics world if changed
    let new_context = case final_physics_world {
      option.Some(world) ->
        Context(..updated_context, physics_world: option.Some(world))
      option.None -> updated_context
    }

    // Render the view
    let new_node = view(new_state, new_context)

    // Apply scene changes (diff and patch)
    let #(patches, new_dict) =
      scene.diff(
        option.Some(frame_data.prev_node),
        option.Some(new_node),
        frame_data.renderer_state.cached_scene_dict,
      )

    let new_renderer_state =
      frame_data.renderer_state
      |> scene.apply_patches(patches)
      |> scene.set_cached_scene_dict(option.Some(new_dict))

    // Render the scene to the canvas
    render_scene(new_renderer_state)

    // Update runtime state
    update_runtime_state_ffi(
      runtime,
      new_state,
      new_node,
      new_context,
      new_renderer_state,
    )

    // Clear input frame state (just-pressed, just-released, etc.)
    clear_runtime_input_frame_state_ffi(
      runtime,
      input_manager.clear_frame_state,
    )

    // Run the combined effect from this frame
    let dispatch = get_runtime_dispatch_ffi(runtime)
    effect.run(combined_effect, dispatch)

    // Schedule next frame
    schedule_frame_with_runtime(runtime, update, view)
  })
  Nil
}

// Opaque type for the game runtime instance
pub type GameRuntime

// Type for frame data returned from FFI
pub type FrameData(state, msg) {
  FrameData(
    messages: List(msg),
    state: state,
    prev_node: scene.Node,
    context: Context,
    renderer_state: scene.RendererState,
    delta_time_ms: Float,
    input_state: input.InputState,
    canvas_width: Float,
    canvas_height: Float,
  )
}

/// Process all queued messages through the update function
fn process_messages(
  state: state,
  messages: List(msg),
  context: Context,
  accumulated_effect: Option(effect.Effect(msg)),
  update: fn(state, msg, Context) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld)),
) -> #(state, effect.Effect(msg), Option(physics.PhysicsWorld)) {
  case messages {
    [] -> {
      let final_effect = case accumulated_effect {
        option.Some(eff) -> eff
        option.None -> effect.none()
      }
      #(state, final_effect, context.physics_world)
    }
    [msg, ..rest] -> {
      let #(new_state, new_effect, new_physics_world) =
        update(state, msg, context)

      // Update context with new physics world
      let new_context = case new_physics_world {
        option.Some(world) ->
          Context(..context, physics_world: option.Some(world))
        option.None -> context
      }

      // Batch effects together
      let combined = case accumulated_effect {
        option.Some(prev_effect) ->
          option.Some(effect.batch([prev_effect, new_effect]))
        option.None -> option.Some(new_effect)
      }

      process_messages(new_state, rest, new_context, combined, update)
    }
  }
}

// --- Helper Functions ---

/// Render the scene to the canvas
fn render_scene(renderer_state: scene.RendererState) -> Nil {
  // Get all cameras with their info
  let cameras = scene.get_all_cameras_with_info(renderer_state)

  // Split into main cameras (no viewport) and viewport cameras
  let #(main_cameras, viewport_cameras) = split_cameras(cameras)

  // Render using FFI (handles postprocessing, viewports, etc.)
  render_cameras_ffi(renderer_state, main_cameras, viewport_cameras)
}

/// Split cameras into main (fullscreen, active) and viewport cameras
/// Main cameras: no viewport AND is_active
/// Viewport cameras: have a viewport (regardless of active state)
fn split_cameras(
  cameras: List(
    #(
      String,
      savoiardi.Object3D,
      Option(camera.ViewPort),
      Option(camera.PostProcessing),
      Bool,
    ),
  ),
) -> #(
  List(
    #(
      String,
      savoiardi.Object3D,
      Option(camera.ViewPort),
      Option(camera.PostProcessing),
    ),
  ),
  List(
    #(
      String,
      savoiardi.Object3D,
      Option(camera.ViewPort),
      Option(camera.PostProcessing),
    ),
  ),
) {
  // Main cameras: no viewport AND is active
  let main =
    list.filter_map(cameras, fn(entry) {
      let #(id, cam, viewport_opt, pp_opt, is_active) = entry
      case option.is_none(viewport_opt) && is_active {
        True -> Ok(#(id, cam, viewport_opt, pp_opt))
        False -> Error(Nil)
      }
    })

  // Viewport cameras: have a viewport (render regardless of active state)
  let viewport =
    list.filter_map(cameras, fn(entry) {
      let #(id, cam, viewport_opt, pp_opt, _is_active) = entry
      case option.is_some(viewport_opt) {
        True -> Ok(#(id, cam, viewport_opt, pp_opt))
        False -> Error(Nil)
      }
    })

  #(main, viewport)
}

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

// Game runtime FFI 
@external(javascript, "./game_runtime.ffi.mjs", "createRuntime")
fn create_runtime_ffi(
  state: state,
  prev_node: scene.Node,
  context: Context,
  renderer_state: scene.RendererState,
  message_queue: List(msg),
  input_manager: input_manager.InputManager,
) -> GameRuntime

@external(javascript, "./game_runtime.ffi.mjs", "getRuntimeDispatch")
fn get_runtime_dispatch_ffi(runtime: GameRuntime) -> fn(msg) -> Nil

@external(javascript, "./game_runtime.ffi.mjs", "processFrame")
fn process_frame_ffi(
  runtime: GameRuntime,
  update: fn(state, msg, Context) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld)),
  view: fn(state, Context) -> scene.Node,
  timestamp: Float,
  capture_input_state: fn(input_manager.InputManager) -> input.InputState,
  clear_input_frame_state: fn(input_manager.InputManager) ->
    input_manager.InputManager,
) -> FrameData(state, msg)

@external(javascript, "./game_runtime.ffi.mjs", "updateRuntimeState")
fn update_runtime_state_ffi(
  runtime: GameRuntime,
  state: state,
  prev_node: scene.Node,
  context: Context,
  renderer_state: scene.RendererState,
) -> Nil

@external(javascript, "./game_runtime.ffi.mjs", "clearRuntimeInputFrameState")
fn clear_runtime_input_frame_state_ffi(
  runtime: GameRuntime,
  clear_input_frame_state: fn(input_manager.InputManager) ->
    input_manager.InputManager,
) -> Nil

@external(javascript, "./game_runtime.ffi.mjs", "updateRuntimeInputManager")
fn update_runtime_input_manager_ffi(
  runtime: GameRuntime,
  input_manager: input_manager.InputManager,
) -> Nil

@external(javascript, "./game_runtime.ffi.mjs", "getRuntimeInputManager")
fn get_runtime_input_manager_ffi(
  runtime: GameRuntime,
) -> input_manager.InputManager

@external(javascript, "./game_runtime.ffi.mjs", "renderCameras")
fn render_cameras_ffi(
  renderer_state: scene.RendererState,
  cameras: List(
    #(
      String,
      savoiardi.Object3D,
      Option(camera.ViewPort),
      Option(camera.PostProcessing),
    ),
  ),
  viewport_cameras: List(
    #(
      String,
      savoiardi.Object3D,
      Option(camera.ViewPort),
      Option(camera.PostProcessing),
    ),
  ),
) -> Nil

@external(javascript, "../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b
