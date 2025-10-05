import tiramisu/scene.{type SceneNode}
import tiramisu/three/camera
import tiramisu/three/renderer
import tiramisu/three/scene as three_scene

/// Game context without mutable scene reference
pub type GameContext {
  GameContext(camera: camera.Camera, delta_time: Float)
}

/// Initialize and run the game loop with declarative rendering
pub fn run(
  width width: Int,
  height height: Int,
  background background: Int,
  camera camera: camera.Camera,
  init init: fn(GameContext) -> state,
  update update: fn(state, GameContext) -> state,
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

  let scene_obj = three_scene.create()
  let _scene = three_scene.set_background(scene_obj, background)

  let camera_obj = camera

  let context = GameContext(camera: camera_obj, delta_time: 0.0)

  // Initialize game state
  let initial_state = init(context)

  // Get initial scene nodes
  let initial_nodes = view(initial_state)

  // Append renderer to DOM
  let canvas = renderer.get_dom_element(renderer_obj)
  append_to_dom(canvas)

  // Apply initial scene nodes (diff from empty to initial)
  apply_initial_scene(scene_obj, initial_nodes)

  // Start declarative game loop
  start_declarative_loop(
    initial_state,
    initial_nodes,
    context,
    scene_obj,
    renderer_obj,
    camera_obj,
    update,
    view,
  )
}

@external(javascript, "./ffi/game.mjs", "appendToDom")
fn append_to_dom(element: renderer.DomElement) -> Nil

@external(javascript, "./ffi/game.mjs", "applyInitialScene")
fn apply_initial_scene(scene: three_scene.Scene, nodes: List(SceneNode)) -> Nil

@external(javascript, "./ffi/game.mjs", "startDeclarativeLoop")
fn start_declarative_loop(
  state: state,
  prev_nodes: List(SceneNode),
  context: GameContext,
  scene: three_scene.Scene,
  renderer: renderer.WebGLRenderer,
  camera: camera.Camera,
  update: fn(state, GameContext) -> state,
  view: fn(state) -> List(SceneNode),
) -> Nil
