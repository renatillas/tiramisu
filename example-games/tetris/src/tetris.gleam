import gleam/int
import gleam/list
import gleam/set
import gleam/time/duration

import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html

import input
import tetris/game
import tetris/piece
import tetris/position
import tetris/ui

import tiramisu
import tiramisu/audio
import tiramisu/camera
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/tick
import tiramisu/transform

import vec/vec3

// Constants
const block_size = 1.0

const grid_width = 10

const grid_height = 20

// Model
pub type Model {
  Model(
    input_state: input.InputState,
    game_state: game.GameState,
    drop_timer: Float,
    last_move_time: Float,
    last_rotate_time: Float,
    // True only during the frame a player action occurred — used for one-shot audio
    player_moved: Bool,
  )
}

// Messages
pub type Msg {
  Tick(tick.TickContext)
  KeyDown(input.Key)
  KeyUp(input.Key)
  Restart
}

// Initialize
pub fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      input_state: input.new(),
      game_state: game.new(),
      drop_timer: 0.0,
      last_move_time: 0.0,
      last_rotate_time: 0.0,
      player_moved: False,
    )

  // Subscribe to the renderer's animation frame ticks
  #(model, tick.subscribe("tetris", Tick))
}

// Update
pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    KeyDown(key) -> #(
      Model(..model, input_state: input.key_down(model.input_state, key)),
      effect.none(),
    )

    KeyUp(key) -> #(
      Model(..model, input_state: input.key_up(model.input_state, key)),
      effect.none(),
    )

    Restart -> {
      let #(new_model, eff) = init(Nil)
      #(new_model, eff)
    }

    Tick(ctx) -> {
      // Reset player_moved from previous frame so audio plays only once per action
      let model = Model(..model, player_moved: False)

      let state = model.input_state
      let move_cooldown = 0.15
      let rotate_cooldown = 0.2

      let left_pressed =
        input.is_pressed(state, input.ArrowLeft)
        || input.is_pressed(state, input.A)
      let right_pressed =
        input.is_pressed(state, input.ArrowRight)
        || input.is_pressed(state, input.D)
      let rotate_pressed =
        input.is_pressed(state, input.ArrowUp)
        || input.is_pressed(state, input.W)
      let drop_pressed = input.is_just_pressed(state, input.Space)

      let can_move = model.last_move_time >=. move_cooldown
      let can_rotate = model.last_rotate_time >=. rotate_cooldown

      // Apply the first matching input with cooldown
      let #(new_game_state, new_move_time, new_rotate_time, player_moved) =
        case
          left_pressed && can_move,
          right_pressed && can_move,
          rotate_pressed && can_rotate,
          drop_pressed
        {
          True, _, _, _ -> #(
            game.move_left(model.game_state),
            0.0,
            model.last_rotate_time,
            True,
          )
          _, True, _, _ -> #(
            game.move_right(model.game_state),
            0.0,
            model.last_rotate_time,
            True,
          )
          _, _, True, _ -> #(
            game.rotate(model.game_state),
            model.last_move_time,
            0.0,
            True,
          )
          _, _, _, True -> #(
            game.hard_drop(model.game_state),
            model.last_move_time,
            model.last_rotate_time,
            True,
          )
          _, _, _, _ -> #(
            model.game_state,
            model.last_move_time,
            model.last_rotate_time,
            False,
          )
        }

      let delta_seconds = duration.to_seconds(ctx.delta_time)
      let updated_drop_timer = model.drop_timer +. delta_seconds
      let drop_interval = game.drop_interval(new_game_state.level)
      let should_auto_drop = updated_drop_timer >=. drop_interval

      let final_state = case should_auto_drop {
        True -> game.tick(new_game_state)
        False -> new_game_state
      }

      // Detect piece change (new piece spawned) to reset the drop timer
      let piece_changed =
        final_state.current_piece.piece_type
        != new_game_state.current_piece.piece_type

      let final_drop_timer = case should_auto_drop || piece_changed {
        True -> 0.0
        False -> updated_drop_timer
      }

      let updated_move_time = new_move_time +. delta_seconds
      let updated_rotate_time = new_rotate_time +. delta_seconds

      let new_model =
        Model(
          input_state: input.end_frame(state),
          game_state: final_state,
          drop_timer: final_drop_timer,
          last_move_time: updated_move_time,
          last_rotate_time: updated_rotate_time,
          player_moved:,
        )

      #(new_model, effect.none())
    }
  }
}

// View
pub fn view(model: Model) -> Element(Msg) {
  // Current piece blocks
  let current_piece_blocks =
    piece.get_piece_positions(model.game_state.current_piece)
    |> list.index_map(fn(pos, idx) {
      create_block(
        pos,
        idx,
        piece.piece_color(model.game_state.current_piece.piece_type),
      )
    })

  // Locked blocks
  let locked_blocks =
    set.to_list(model.game_state.board.locked_blocks)
    |> list.index_map(fn(pos, idx) { create_locked_block(pos, idx) })

  // Move sound — only present when the player acted this frame (one-shot)
  let move_audio_nodes = case model.player_moved {
    True -> [
      tiramisu.audio(
        "move-sfx",
        [
          attribute.attribute("src", "wav/Select-1-(Saw).wav"),
          audio.playing(True),
        ],
        [],
      ),
    ]
    False -> []
  }

  html.div(
    [
      attribute.style("position", "relative"),
      attribute.style("width", "fit-content"),
    ],
    [
      // 3D scene rendered by the tiramisu-renderer web component
      renderer.renderer(
        [
          renderer.background_color(0x1a1a2e),
          renderer.scene_id("tetris"),
          // tabindex makes the renderer focusable so keyboard events work
          attribute.attribute("tabindex", "0"),
          input.on_keydown(KeyDown),
          input.on_keyup(KeyUp),
        ],
        list.flatten([
          [
            tiramisu.camera(
              "main-camera",
              [
                camera.fov(75.0),
                tiramisu.transform(transform.at(vec3.Vec3(5.0, 10.0, 25.0))),
                camera.active(True),
              ],
              [],
            ),
            tiramisu.light(
              "ambient-light",
              [
                light.light_type("ambient"),
                tiramisu.color(0xffffff),
                light.intensity(0.5),
              ],
              [],
            ),
            tiramisu.light(
              "directional-light",
              [
                light.light_type("directional"),
                tiramisu.color(0xffffff),
                light.intensity(0.8),
                tiramisu.transform(transform.at(vec3.Vec3(10.0, 15.0, 10.0))),
              ],
              [],
            ),
            create_grid_outline(),
          ],
          current_piece_blocks,
          locked_blocks,
          move_audio_nodes,
        ]),
      ),
      // HTML overlay for score, next piece, controls, and game-over screen
      html.div(
        [
          attribute.style("position", "absolute"),
          attribute.style("top", "0"),
          attribute.style("left", "0"),
          attribute.style("width", "100%"),
          attribute.style("height", "100%"),
          attribute.style("pointer-events", "none"),
          attribute.style("font-family", "sans-serif"),
          attribute.style("color", "white"),
        ],
        [ui.view(model.game_state, Restart)],
      ),
    ],
  )
}

// Helper: Create a block for the active falling piece
fn create_block(
  pos: position.Position,
  index: Int,
  color: Int,
) -> Element(Msg) {
  let x = int.to_float(pos.x) *. block_size
  let y = int.to_float(pos.y) *. block_size

  tiramisu.mesh(
    "current-block-" <> int.to_string(index),
    [
      mesh.geometry_box(vec3.Vec3(block_size, block_size, block_size)),
      tiramisu.color(color),
      tiramisu.metalness(0.3),
      tiramisu.roughness(0.7),
      tiramisu.transform(transform.at(vec3.Vec3(x, y, 0.0))),
    ],
    [],
  )
}

// Helper: Create a locked (settled) block
fn create_locked_block(pos: position.Position, index: Int) -> Element(Msg) {
  let x = int.to_float(pos.x) *. block_size
  let y = int.to_float(pos.y) *. block_size

  tiramisu.mesh(
    "locked-block-" <> int.to_string(index),
    [
      mesh.geometry_box(vec3.Vec3(block_size, block_size, block_size)),
      tiramisu.color(0x808080),
      tiramisu.transform(transform.at(vec3.Vec3(x, y, 0.0))),
    ],
    [],
  )
}

// Helper: Dark backdrop box that frames the play field
fn create_grid_outline() -> Element(Msg) {
  tiramisu.mesh(
    "grid-outline",
    [
      mesh.geometry_box(vec3.Vec3(
        int.to_float(grid_width) *. block_size +. 0.2,
        int.to_float(grid_height) *. block_size +. 0.2,
        0.1,
      )),
      tiramisu.color(0x333333),
      tiramisu.transform(transform.at(vec3.Vec3(
        int.to_float(grid_width) /. 2.0 -. 0.5,
        int.to_float(grid_height) /. 2.0 -. 0.5,
        -0.5,
      ))),
    ],
    [],
  )
}

// Main entry point
pub fn main() -> Nil {
  // Register the tiramisu-renderer custom element before starting the app
  let assert Ok(_) = tiramisu.register()

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
