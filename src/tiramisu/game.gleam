/// Immutable game loop with effect system
/// Following Lustre's Model-View-Update architecture
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/input.{type InputState}
import tiramisu/internal/renderer
import tiramisu/scene.{type SceneNode}

/// Internal Three.js Scene type (opaque)
type Scene

/// Game context passed to init and update functions
pub type GameContext {
  GameContext(camera: camera.Camera, delta_time: Float, input: InputState)
}

/// Initialize and run the game loop
pub fn run(
  width width: Int,
  height height: Int,
  background background: Int,
  camera camera: camera.Camera,
  init init: fn(GameContext) -> #(state, Effect(msg)),
  update update: fn(state, msg, GameContext) -> #(state, Effect(msg)),
  view view: fn(state) -> List(SceneNode),
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
  let camera_obj = camera

  // Initial context with empty input
  let initial_context =
    GameContext(camera: camera_obj, delta_time: 0.0, input: input.new())

  // Initialize game state
  let #(initial_state, initial_effect) = init(initial_context)

  // Get initial scene nodes
  let initial_nodes = view(initial_state)

  // Append renderer to DOM and initialize input
  let canvas = renderer.get_dom_element(renderer_obj)
  append_to_dom(canvas)
  initialize_input_systems(canvas)

  // Apply initial scene
  apply_initial_scene(scene_obj, initial_nodes)

  // Start game loop
  start_loop(
    initial_state,
    initial_nodes,
    initial_effect,
    initial_context,
    scene_obj,
    renderer_obj,
    camera_obj,
    update,
    view,
  )
}

// --- FFI Declarations ---

@external(javascript, "./ffi/game.mjs", "createScene")
fn create_scene(background: Int) -> Scene

@external(javascript, "./ffi/game.mjs", "appendToDom")
fn append_to_dom(element: renderer.DomElement) -> Nil

@external(javascript, "./ffi/game.mjs", "initializeInputSystems")
fn initialize_input_systems(canvas: renderer.DomElement) -> Nil

@external(javascript, "./ffi/game.mjs", "applyInitialScene")
fn apply_initial_scene(scene: Scene, nodes: List(SceneNode)) -> Nil

@external(javascript, "./ffi/game.mjs", "startLoop")
fn start_loop(
  state: state,
  prev_nodes: List(SceneNode),
  effect: Effect(msg),
  context: GameContext,
  scene: Scene,
  renderer: renderer.WebGLRenderer,
  camera: camera.Camera,
  update: fn(state, msg, GameContext) -> #(state, Effect(msg)),
  view: fn(state) -> List(SceneNode),
) -> Nil
