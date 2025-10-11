import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { toList, CustomType as $CustomType } from "./gleam.mjs";
import {
  appendToDom as append_to_dom,
  getCanvasDimensions as get_canvas_dimensions,
  setBackground as set_background,
  startLoop as start_loop,
  identity as to_dynamic,
  getWindowAspectRatio as get_window_aspect_ratio,
} from "./tiramisu.ffi.mjs";
import * as $background from "./tiramisu/background.mjs";
import * as $effect from "./tiramisu/effect.mjs";
import * as $input from "./tiramisu/input.mjs";
import * as $managers from "./tiramisu/internal/managers.mjs";
import * as $renderer from "./tiramisu/internal/renderer.mjs";
import * as $physics from "./tiramisu/physics.mjs";
import * as $scene from "./tiramisu/scene.mjs";

export { get_window_aspect_ratio };

export class Dimensions extends $CustomType {
  constructor(width, height) {
    super();
    this.width = width;
    this.height = height;
  }
}

export class Context extends $CustomType {
  constructor(delta_time, input, canvas_width, canvas_height, physics_world, input_manager, audio_manager) {
    super();
    this.delta_time = delta_time;
    this.input = input;
    this.canvas_width = canvas_width;
    this.canvas_height = canvas_height;
    this.physics_world = physics_world;
    this.input_manager = input_manager;
    this.audio_manager = audio_manager;
  }
}

/**
 * Apply initial scene using renderer.gleam's patch system
 * 
 * @ignore
 */
function apply_initial_scene_gleam(renderer_state, nodes) {
  let patches = $scene.diff(toList([]), nodes);
  return $renderer.apply_patches(renderer_state, patches);
}

/**
 * Initialize and run the game loop.
 *
 * This is the main entry point for your game. It sets up the renderer, initializes your game state,
 * and starts the game loop. The loop will call your `update` and `view` functions each frame.
 *
 * ## Parameters
 *
 * - `dimensions`: Canvas dimensions (width and height). Use `None` for fullscreen mode.
 * - `background`: Background as Color, Texture, or CubeTexture (see `Background` type)
 * - `init`: Function to create initial game state and effect
 * - `update`: Function to update state based on messages
 * - `view`: Function to render your game state as scene nodes
 *
 * ## Camera Setup
 *
 * You must include a `Camera` scene node with `active: True` in your initial scene.
 * Use `effect.set_active_camera(id)` to switch between cameras at runtime.
 *
 * ## Example
 *
 * ```gleam
 * import gleam/option.{None, Some}
 * import tiramisu
 * import tiramisu/camera
 * import tiramisu/effect
 * import tiramisu/scene
 * import tiramisu/transform
 * import vec/vec3
 *
 * type Model {
 *   Model(rotation: Float)
 * }
 *
 * type Msg {
 *   Tick
 * }
 *
 * pub fn main() {
 *   // Fullscreen mode with color background
 *   tiramisu.run(
 *     dimensions: None,
 *     background: background.Color(0x111111),
 *     init: fn(_ctx) {
 *       #(Model(rotation: 0.0), effect.batch([
 *         effect.on_animation_frame(Tick),
 *       ]))
 *     },
 *     update: fn(model, msg, ctx) {
 *       case msg {
 *         Tick -> #(
 *           Model(rotation: model.rotation +. ctx.delta_time),
 *           effect.on_animation_frame(Tick),
 *         )
 *       }
 *     },
 *     view: fn(model) {
 *       [
 *         scene.Camera(
 *           id: "main-camera",
 *           camera: camera.perspective(fov: 75.0, near: 0.1, far: 1000.0),
 *           transform: transform.at(vec3.Vec3(0.0, 0.0, 5.0)),
 *           active: True,
 *           viewport: option.None,
 *         ),
 *         scene.Mesh(
 *           id: "cube",
 *           geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
 *           material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),
 *           transform: transform.identity
 *             |> transform.set_rotation(vec3.Vec3(model.rotation, model.rotation, 0.0)),
 *           physics: option.None,
 *         ),
 *       ]
 *     },
 *   )
 * }
 * ```
 */
export function run(dimensions, background, init, update, view) {
  let audio_manager = $managers.new_audio_manager();
  let renderer_state = $renderer.create(
    new $renderer.RendererOptions(
      true,
      false,
      (() => {
        let _pipe = dimensions;
        return $option.map(
          _pipe,
          (dimensions) => {
            return new $renderer.Dimensions(dimensions.width, dimensions.height);
          },
        );
      })(),
    ),
    to_dynamic(audio_manager),
  );
  let scene_obj = $renderer.get_scene(renderer_state);
  set_background(scene_obj, background);
  let webgl_renderer = $renderer.get_renderer(renderer_state);
  let $ = get_canvas_dimensions(webgl_renderer);
  let initial_width;
  let initial_height;
  initial_width = $[0];
  initial_height = $[1];
  let canvas = $renderer.get_dom_element(webgl_renderer);
  let input_manager = $managers.new_input_manager(canvas);
  let initial_context = new Context(
    0.0,
    $input.new$(),
    initial_width,
    initial_height,
    new $option.None(),
    input_manager,
    audio_manager,
  );
  let $1 = init(initial_context);
  let initial_state;
  let initial_effect;
  let physics_world;
  initial_state = $1[0];
  initial_effect = $1[1];
  physics_world = $1[2];
  let context_with_physics = new Context(
    0.0,
    $input.new$(),
    initial_width,
    initial_height,
    physics_world,
    input_manager,
    audio_manager,
  );
  let _block;
  if (physics_world instanceof $option.Some) {
    let world = physics_world[0];
    _block = $renderer.set_physics_world(
      renderer_state,
      new $option.Some(world),
    );
  } else {
    _block = $renderer.set_physics_world(renderer_state, new $option.None());
  }
  let renderer_state_with_physics = _block;
  let initial_nodes = view(initial_state, context_with_physics);
  append_to_dom(canvas);
  let renderer_state_after_init = apply_initial_scene_gleam(
    renderer_state_with_physics,
    initial_nodes,
  );
  let _block$1;
  let $2 = $renderer.get_physics_world(renderer_state_after_init);
  if ($2 instanceof $option.Some) {
    let updated_world = $2[0];
    _block$1 = new Context(
      context_with_physics.delta_time,
      context_with_physics.input,
      context_with_physics.canvas_width,
      context_with_physics.canvas_height,
      new $option.Some(updated_world),
      context_with_physics.input_manager,
      context_with_physics.audio_manager,
    );
  } else {
    _block$1 = context_with_physics;
  }
  let updated_context = _block$1;
  return start_loop(
    initial_state,
    initial_nodes,
    initial_effect,
    updated_context,
    renderer_state_after_init,
    update,
    view,
  );
}
