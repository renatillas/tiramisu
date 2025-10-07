/// Immutable game loop with effect system
/// Following Lustre's Model-View-Update architecture
import tiramisu/effect
import tiramisu/input
import tiramisu/internal/renderer
import tiramisu/scene

/// Internal Three.js Scene type (opaque)
type Scene

/// Game context passed to init and update functions
pub type Context {
  Context(delta_time: Float, input: input.InputState)
}

/// Initialize and run the game loop
///
/// The camera parameter is optional. If provided, it will initialize a default camera for
/// backwards compatibility. If not provided, you must include a Camera scene node with
/// active=True in your initial scene.
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
