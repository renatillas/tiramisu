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
import tiramisu/ui
import vec/vec2.{type Vec2}

/// Canvas dimensions type alias for the game window.
///
/// Used with `tiramisu.run()` to specify the size of the game canvas.
/// If not provided (None), the game will run in fullscreen mode.
/// Vec2(x, y) where x is width and y is height.
///
pub type Dimensions {
  FullScreen
  Window(dimensions: Vec2(Float))
}

/// Game context passed to init and update functions.
///
/// Contains timing information, input state, canvas dimensions, physics world, scene, and renderer for the current frame.
///
/// ## Fields
///
/// - `delta_time`: Time elapsed since the last frame as a Duration. Use this for frame-rate independent movement and animations.
/// - `input`: Current input state (keyboard, mouse, touch, gamepad)
/// - `canvas_size`: Current canvas size in pixels as Vec2(width, height)
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
    /// Current canvas size in pixels as Vec2(width, height)
    canvas_size: Vec2(Float),
    /// Physics world handle (if physics is enabled)
    physics_world: Option(physics.PhysicsWorld),
    /// The Three.js scene instance
    scene: scene.Scene,
    /// The WebGL renderer (for performance stats)
    renderer: scene.Renderer,
  )
}

// Opaque type for the game runtime instance
type GameRuntime

@internal
pub type FrameData(state, msg) {
  FrameData(
    messages: List(msg),
    state: state,
    prev_node: scene.Node,
    context: Context,
    renderer_state: scene.RendererState,
    delta_time_ms: Float,
    input_state: input.InputState,
    canvas_size: Vec2(Float),
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
/// - `bridge`: Optional UI bridge for Lustre integration. Use `None` for standalone games,
///   or `Some(bridge)` to enable bidirectional communication with Lustre.
/// - `init`: Function to create initial game state and effect
/// - `update`: Function to update state based on messages
/// - `view`: Function to render your game state as scene nodes
///
/// ## Example (standalone game)
///
/// ```gleam
/// import tiramisu
///
/// pub fn main() {
///   tiramisu.run(
///     selector: "#game",
///     dimensions: None,
///     bridge: None,
///     init: init,
///     update: update,
///     view: view,
///   )
/// }
/// ```
///
/// ## Example (with Lustre UI)
///
/// ```gleam
/// import tiramisu
/// import tiramisu/ui
///
/// pub fn main() {
///   let bridge = ui.new_bridge()
///
///   // Start Lustre first with bridge in flags...
///
///   tiramisu.run(
///     selector: "#game",
///     dimensions: None,
///     bridge: Some(bridge),
///     init: init,
///     update: update,
///     view: view,
///   )
/// }
/// ```
///
pub fn run(
  selector selector: String,
  dimensions dimensions: Dimensions,
  bridge bridge: Option(ui.Bridge(lustre_msg, msg)),
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
    scene.new_render_state(
      savoiardi.RendererOptions(
        antialias: True,
        alpha: False,
        dimensions: case dimensions {
          FullScreen -> option.None
          Window(dim) -> option.Some(dim)
        },
      ),
    )

  // Get canvas and append to container (so dimensions are available)
  let renderer = scene.get_renderer(renderer_state)
  let canvas = savoiardi.get_renderer_dom_element(renderer)
  element.append_child(container, canvas |> coerce)

  // Initialize CSS2D renderer for HTML overlay labels
  let renderer_state = scene.init_css2d_renderer(renderer_state, container)

  // Create input manager (will be initialized with listeners in game_loop)
  let input_mgr = input_manager.new()

  // Now get canvas dimensions (after it's in the DOM)
  let canvas_size = savoiardi.get_canvas_dimensions(renderer)

  // Get the Three.js scene from renderer state
  let scene = scene.get_scene(renderer_state)

  // Initial context with empty input (no physics_world yet)
  let initial_context =
    Context(
      delta_time: duration.nanoseconds(0),
      input: input.new(),
      canvas_size:,
      physics_world: option.None,
      scene:,
      renderer:,
    )

  // Initialize game state
  let #(initial_state, initial_effect, physics_world) = init(initial_context)

  // Create context with physics_world for the game loop
  let context_with_physics =
    Context(
      delta_time: duration.nanoseconds(0),
      input: input.new(),
      canvas_size:,
      physics_world:,
      scene:,
      renderer:,
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
    bridge: bridge |> option.map(ui.get_internal),
    state: initial_state,
    prev_node: initial_root,
    pending_effect: initial_effect,
    context: updated_context,
    renderer_state: renderer_state_after_init,
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
  bridge bridge: Option(ui.BridgeInternal),
  state state: state,
  prev_node prev_node: scene.Node,
  pending_effect pending_effect: effect.Effect(msg),
  context context: Context,
  renderer_state renderer_state: scene.RendererState,
  input_manager input_manager: input_manager.InputManager,
  canvas canvas: element.Element,
  update update: fn(state, msg, Context) ->
    #(state, effect.Effect(msg), Option(physics.PhysicsWorld)),
  view view: fn(state, Context) -> scene.Node,
) -> Nil {
  // Create the runtime instance that holds all mutable state (no globals!)
  let runtime =
    create_runtime_ffi(
      bridge,
      state,
      prev_node,
      context,
      renderer_state,
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
        canvas_size: frame_data.canvas_size,
      )

    // Resume audio context on user interaction (browser autoplay policy)
    let renderer_state_with_audio = case
      input.has_user_interaction(frame_data.input_state)
    {
      True -> scene.resume_audio_context(frame_data.renderer_state)
      False -> frame_data.renderer_state
    }

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
        renderer_state_with_audio.cached_scene_dict,
      )

    let new_renderer_state =
      renderer_state_with_audio
      |> scene.apply_patches(patches)
      |> scene.set_cached_scene_dict(option.Some(new_dict))

    // Update animation mixers
    scene.update_mixers(new_renderer_state, new_context.delta_time)

    // Render the scene to the canvas
    render_scene(new_renderer_state)

    // Sync physics world from renderer state back to context
    // This is necessary because apply_patches may create new physics bodies
    let final_context = case scene.get_physics_world(new_renderer_state) {
      option.Some(world) ->
        Context(..new_context, physics_world: option.Some(world))
      option.None -> new_context
    }

    // Update runtime state
    update_runtime_state_ffi(
      runtime,
      new_state,
      new_node,
      final_context,
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
  bridge: Option(ui.BridgeInternal),
  state: state,
  prev_node: scene.Node,
  context: Context,
  renderer_state: scene.RendererState,
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

@external(javascript, "../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b
