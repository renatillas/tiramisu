import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/time/duration
import lustre
import lustre/attribute
import lustre/effect as lustre_effect
import lustre/element
import lustre/element/html
import tetris/game
import tetris/piece
import tetris/position
import tetris/ui
import tiramisu
import tiramisu/audio
import tiramisu/background
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import tiramisu/ui as tiramisu_ui
import vec/vec3

// Constants
const block_size = 1.0

const grid_width = 10

const grid_height = 20

// Shared bridge messages (Game <-> UI)
pub type BridgeMsg {
  // Game -> UI
  UpdateGameState(game.GameState)
  // UI -> Game
  RestartGame
}

// Model
pub type Model {
  Model(
    bridge: tiramisu_ui.Bridge(BridgeMsg),
    player_moved: Bool,
    game_state: game.GameState,
    drop_timer: Float,
    camera: camera.Camera,
    last_move_time: Float,
    last_rotate_time: Float,
    previous_piece_shape: piece.Shape,
    move_sound: option.Option(audio.Buffer),
  )
}

// Messages
pub type Msg {
  Tick
  FromBridge(BridgeMsg)
  MoveSoundLoaded(audio.Buffer)
  AudioLoadFailed
  BackgroundSet
  BackgroundFailed
}

// Initialize
pub fn init(
  bridge: tiramisu_ui.Bridge(BridgeMsg),
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(a)) {
  // Load move sound
  let load_move_sound =
    audio.load_audio(
      url: "wav/Select-1-(Saw).wav",
      on_success: MoveSoundLoaded,
      on_error: AudioLoadFailed,
    )

  // Set background color
  let set_background =
    background.set(
      ctx.scene,
      background.Color(0x1a1a2e),
      BackgroundSet,
      BackgroundFailed,
    )

  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let initial_game = game.new()

  let model =
    Model(
      bridge:,
      player_moved: False,
      game_state: initial_game,
      drop_timer: 0.0,
      camera: cam,
      last_move_time: 0.0,
      last_rotate_time: 0.0,
      previous_piece_shape: initial_game.current_piece.piece_type,
      move_sound: option.None,
    )

  #(
    model,
    effect.batch([effect.dispatch(Tick), load_move_sound, set_background]),
    option.None,
  )
}

// Update
pub fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(a)) {
  case msg {
    MoveSoundLoaded(buffer) -> {
      #(Model(..model, move_sound: Some(buffer)), effect.none(), option.None)
    }
    AudioLoadFailed | BackgroundSet | BackgroundFailed -> {
      #(model, effect.none(), option.None)
    }
    FromBridge(RestartGame) -> {
      let #(new_model, _, _) = init(model.bridge, ctx)
      // Preserve loaded audio
      let new_model = Model(..new_model, move_sound: model.move_sound)
      #(
        new_model,
        effect.batch([
          tiramisu_ui.send_to_ui(model.bridge, UpdateGameState(
            new_model.game_state,
          )),
        ]),
        option.None,
      )
    }
    FromBridge(_) -> {
      #(model, effect.none(), option.None)
    }
    Tick -> {
      // Reset player_moved from previous frame (for audio one-shot behavior)
      let model = case model.player_moved {
        True -> Model(..model, player_moved: False)
        False -> model
      }

      let move_cooldown = 0.15
      let rotate_cooldown = 0.2

      // Handle input with cooldowns
      let left_pressed =
        input.is_key_pressed(ctx.input, input.ArrowLeft)
        || input.is_key_pressed(ctx.input, input.KeyA)
      let right_pressed =
        input.is_key_pressed(ctx.input, input.ArrowRight)
        || input.is_key_pressed(ctx.input, input.KeyD)
      let rotate_pressed =
        input.is_key_pressed(ctx.input, input.ArrowUp)
        || input.is_key_pressed(ctx.input, input.KeyW)
      let drop_pressed = input.is_key_just_pressed(ctx.input, input.Space)

      // Apply movements with cooldowns
      let can_move = model.last_move_time >=. move_cooldown
      let can_rotate = model.last_rotate_time >=. rotate_cooldown

      let #(new_state, new_move_time, new_rotate_time, player_moved) = case
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

      // Accumulate drop timer
      let updated_drop_timer =
        model.drop_timer +. duration.to_seconds(ctx.delta_time)
      let drop_interval = game.drop_interval(new_state.level)

      // Auto-drop if timer reached
      let should_auto_drop = updated_drop_timer >=. drop_interval

      let final_state = case should_auto_drop {
        True -> game.tick(new_state)
        False -> new_state
      }

      // Detect if piece type changed (new piece spawned) by comparing before and after
      let piece_changed =
        final_state.current_piece.piece_type
        != new_state.current_piece.piece_type

      // Reset timer if auto-dropped OR if new piece spawned
      let final_drop_timer = case should_auto_drop || piece_changed {
        True -> 0.0
        False -> updated_drop_timer
      }

      // Update cooldown timers
      let delta_seconds = duration.to_seconds(ctx.delta_time)
      let updated_move_time = new_move_time +. delta_seconds
      let updated_rotate_time = new_rotate_time +. delta_seconds

      let updated_model =
        Model(
          ..model,
          player_moved:,
          game_state: final_state,
          drop_timer: final_drop_timer,
          camera: model.camera,
          last_move_time: updated_move_time,
          last_rotate_time: updated_rotate_time,
          previous_piece_shape: final_state.current_piece.piece_type,
        )

      #(
        updated_model,
        effect.batch([
          effect.dispatch(Tick),
          tiramisu_ui.send_to_ui(
            model.bridge,
            UpdateGameState(updated_model.game_state),
          ),
        ]),
        option.None,
      )
    }
  }
}

// View
pub fn view(model: Model, _context: tiramisu.Context) -> scene.Node {
  // Only add audio node when player actually moved (for one-shot sound effect)
  let move_audio_node = case model.move_sound, model.player_moved {
    Some(buffer), True -> [
      scene.audio(
        id: "move-sfx",
        audio: audio.GlobalAudio(
          buffer:,
          config: audio.config() |> audio.with_state(audio.Playing),
        ),
      ),
    ]
    _, _ -> []
  }

  // Camera
  let camera_node =
    scene.camera(
      id: "main-camera",
      camera: model.camera,
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 25.0)),
      active: True,
      viewport: None,
      postprocessing: None,
    )

  // Lights
  let ambient_light =
    scene.light(
      id: "ambient-light",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    )

  let directional_light =
    scene.light(
      id: "directional-light",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.8)
        light
      },
      transform: transform.at(position: vec3.Vec3(10.0, 15.0, 10.0)),
    )

  // Create blocks for current piece
  let current_piece_blocks =
    piece.get_piece_positions(model.game_state.current_piece)
    |> list.index_map(fn(pos, idx) {
      create_block(
        pos,
        idx,
        piece.piece_color(model.game_state.current_piece.piece_type),
      )
    })

  // Create blocks for locked pieces
  let locked_blocks =
    set.to_list(model.game_state.board.locked_blocks)
    |> list.index_map(fn(pos, idx) { create_locked_block(pos, idx) })

  // Create grid outline
  let grid_outline = create_grid_outline()

  // Combine all scene nodes
  scene.empty(
    id: "scene",
    transform: transform.identity,
    children: list.flatten([
      [camera_node],
      [ambient_light],
      [directional_light],
      [grid_outline],
      current_piece_blocks,
      locked_blocks,
      move_audio_node,
    ]),
  )
}

// Helper: Create a block for the current piece
fn create_block(
  pos: position.Position,
  index: Int,
  color: Int,
) -> scene.Node {
  let assert Ok(geom) =
    geometry.box(size: vec3.Vec3(block_size, block_size, block_size))
  let assert Ok(mat) =
    material.new()
    |> material.with_color(color)
    |> material.with_metalness(0.3)
    |> material.with_roughness(0.7)
    |> material.build()

  let x = int.to_float(pos.x) *. block_size
  let y = int.to_float(pos.y) *. block_size

  scene.mesh(
    id: "current-block-" <> int.to_string(index),
    geometry: geom,
    material: mat,
    transform: transform.at(position: vec3.Vec3(x, y, 0.0)),
    physics: None,
  )
}

// Helper: Create a locked block
fn create_locked_block(pos: position.Position, index: Int) -> scene.Node {
  let assert Ok(geom) =
    geometry.box(size: vec3.Vec3(block_size, block_size, block_size))
  let assert Ok(mat) =
    material.new()
    |> material.with_color(0x808080)
    |> material.build()

  let x = int.to_float(pos.x) *. block_size
  let y = int.to_float(pos.y) *. block_size

  scene.mesh(
    id: "locked-block-" <> int.to_string(index),
    geometry: geom,
    material: mat,
    transform: transform.at(position: vec3.Vec3(x, y, 0.0)),
    physics: None,
  )
}

// Helper: Create grid outline
fn create_grid_outline() -> scene.Node {
  let assert Ok(geom) =
    geometry.box(size: vec3.Vec3(
      int.to_float(grid_width) *. block_size +. 0.2,
      int.to_float(grid_height) *. block_size +. 0.2,
      0.1,
    ))
  let assert Ok(mat) =
    material.new()
    |> material.with_color(0x333333)
    |> material.build()

  scene.mesh(
    id: "grid-outline",
    geometry: geom,
    material: mat,
    transform: transform.at(position: vec3.Vec3(
      int.to_float(grid_width) /. 2.0 -. 0.5,
      int.to_float(grid_height) /. 2.0 -. 0.5,
      -0.5,
    )),
    physics: None,
  )
}

// UI overlay state management
pub type UiModel {
  UiModel(bridge: tiramisu_ui.Bridge(BridgeMsg), game_state: game.GameState)
}

pub type UiMsg {
  FromBridgeUi(BridgeMsg)
  UiRestart
}

// Main entry point
pub fn main() {
  // Create a bridge for communication
  let bridge = tiramisu_ui.new_bridge()

  // Start the UI overlay
  let assert Ok(_) =
    lustre.application(ui_init, ui_update, ui_view)
    |> lustre.start("#app", bridge)

  // Start the game
  let assert Ok(_) =
    tiramisu.application(init(bridge, _), update, view)
    |> tiramisu.start("#game", tiramisu.FullScreen, Some(#(bridge, FromBridge)))
  Nil
}

fn ui_init(bridge) {
  #(
    UiModel(bridge:, game_state: game.new()),
    tiramisu_ui.register_lustre(bridge, FromBridgeUi),
  )
}

fn ui_update(
  model: UiModel,
  msg: UiMsg,
) -> #(UiModel, lustre_effect.Effect(UiMsg)) {
  case msg {
    FromBridgeUi(UpdateGameState(new_state)) -> #(
      UiModel(..model, game_state: new_state),
      lustre_effect.none(),
    )
    FromBridgeUi(_) -> #(model, lustre_effect.none())
    UiRestart -> #(
      UiModel(..model, game_state: game.new()),
      tiramisu_ui.send(model.bridge, RestartGame),
    )
  }
}

fn ui_view(model: UiModel) -> element.Element(UiMsg) {
  html.div(
    [
      attribute.id("ui-root"),
      attribute.class(
        "fixed top-0 left-0 w-full h-full pointer-events-none font-sans text-white",
      ),
    ],
    [ui.view(model.game_state, UiRestart)],
  )
}
