import gleam/int
import gleam/javascript/promise
import gleam/list
import gleam/option.{None, Some}
import gleam/set
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
import tiramisu/asset
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

// Model
pub type Model {
  Model(
    player_moved: Bool,
    game_state: game.GameState,
    drop_timer: Float,
    camera: camera.Camera,
    last_move_time: Float,
    last_rotate_time: Float,
    previous_piece_shape: piece.Shape,
    assets_cache: option.Option(asset.AssetCache),
  )
}

pub type Id {
  MoveSfx
  AmbientLight
  DirectionalLight
  MainCamera
  LockedBlock(Int)
  CurrentBlock(Int)
  GridOutLine
}

// Messages
pub type Msg {
  Tick
  Restart
  LoadedAssets(asset.BatchLoadResult)
}

// Initialize
pub fn init(
  _ctx: tiramisu.Context(Id),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  let assets = [
    asset.AudioAsset("wav/Select-1-(Saw).wav"),
    asset.AudioAsset("wav/Cursor-1-(Saw).wav"),
    asset.AudioAsset("wav/Error-2-(Saw).wav"),
  ]

  let effects =
    effect.batch([
      effect.from_promise(promise.map(
        asset.load_batch_simple(assets),
        LoadedAssets,
      )),
      effect.tick(Tick),
    ])

  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let initial_game = game.new()

  let model =
    Model(
      player_moved: False,
      game_state: initial_game,
      drop_timer: 0.0,
      camera: cam,
      last_move_time: 0.0,
      last_rotate_time: 0.0,
      previous_piece_shape: initial_game.current_piece.piece_type,
      assets_cache: option.None,
    )

  #(model, effects, option.None)
}

// Update
pub fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(Id),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  case msg {
    LoadedAssets(batch_result) -> {
      #(
        Model(..model, assets_cache: option.Some(batch_result.cache)),
        effect.none(),
        option.None,
      )
    }
    Restart -> {
      let #(new_model, init_effect, _) = init(ctx)
      #(
        new_model,
        effect.batch([
          init_effect,
          tiramisu_ui.dispatch_to_lustre(UiUpdateGameState(new_model.game_state)),
        ]),
        option.None,
      )
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
      let updated_drop_timer = model.drop_timer +. ctx.delta_time
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
      let updated_move_time = new_move_time +. ctx.delta_time
      let updated_rotate_time = new_rotate_time +. ctx.delta_time

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
          effect.tick(Tick),
          tiramisu_ui.dispatch_to_lustre(UiUpdateGameState(
            updated_model.game_state,
          )),
        ]),
        option.None,
      )
    }
  }
}

// View
pub fn view(
  model: Model,
  _context: tiramisu.Context(Id),
) -> List(scene.Node(Id)) {
  // Only add audio node when player actually moved (for one-shot sound effect)
  let move_audio_node = case model.assets_cache, model.player_moved {
    option.Some(cache), True ->
      case asset.get_audio(cache, "wav/Select-1-(Saw).wav") {
        Ok(buffer) -> [
          scene.Audio(
            id: MoveSfx,
            audio: audio.GlobalAudio(
              buffer:,
              config: audio.config() |> audio.with_state(audio.Playing),
            ),
          ),
        ]
        Error(_) -> []
      }
    _, _ -> []
  }
  // Camera
  let camera_node =
    scene.Camera(
      id: MainCamera,
      camera: model.camera,
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 25.0)),
      look_at: Some(vec3.Vec3(5.0, 10.0, 0.0)),
      active: True,
      viewport: None,
    )

  // Lights
  let ambient_light =
    scene.Light(
      id: AmbientLight,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    )

  let directional_light =
    scene.Light(
      id: DirectionalLight,
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
  [camera_node, ambient_light, directional_light, grid_outline]
  |> list.append(current_piece_blocks)
  |> list.append(locked_blocks)
  |> list.append(move_audio_node)
}

// Helper: Create a block for the current piece
fn create_block(
  pos: position.Position,
  index: Int,
  color: Int,
) -> scene.Node(Id) {
  let assert Ok(geometry) =
    geometry.box(width: block_size, height: block_size, depth: block_size)
  let assert Ok(material) =
    material.new()
    |> material.with_color(color)
    |> material.with_metalness(0.3)
    |> material.with_roughness(0.7)
    |> material.build()

  let x = int.to_float(pos.x) *. block_size
  let y = int.to_float(pos.y) *. block_size

  scene.Mesh(
    id: CurrentBlock(index),
    geometry: geometry,
    material: material,
    transform: transform.at(position: vec3.Vec3(x, y, 0.0)),
    physics: None,
  )
}

// Helper: Create a locked block
fn create_locked_block(pos: position.Position, index: Int) -> scene.Node(Id) {
  let assert Ok(geometry) =
    geometry.box(width: block_size, height: block_size, depth: block_size)
  let assert Ok(material) =
    material.new()
    |> material.with_color(0x808080)
    |> material.build()

  let x = int.to_float(pos.x) *. block_size
  let y = int.to_float(pos.y) *. block_size

  scene.Mesh(
    id: LockedBlock(index),
    geometry: geometry,
    material: material,
    transform: transform.at(position: vec3.Vec3(x, y, 0.0)),
    physics: None,
  )
}

// Helper: Create grid outline
fn create_grid_outline() -> scene.Node(Id) {
  let assert Ok(geometry) =
    geometry.box(
      width: int.to_float(grid_width) *. block_size +. 0.2,
      height: int.to_float(grid_height) *. block_size +. 0.2,
      depth: 0.1,
    )
  let assert Ok(material) =
    material.new()
    |> material.with_color(0x333333)
    |> material.build()

  scene.Mesh(
    id: GridOutLine,
    geometry: geometry,
    material: material,
    transform: transform.at(position: vec3.Vec3(
      int.to_float(grid_width) /. 2.0 -. 0.5,
      int.to_float(grid_height) /. 2.0 -. 0.5,
      -0.5,
    )),
    physics: None,
  )
}

// Main entry point
pub fn main() {
  // Start the UI overlay
  let assert Ok(_) =
    lustre.application(ui_init, ui_update, ui_view)
    |> lustre.start("#app", Nil)

  // Start the game
  tiramisu.run(
    dimensions: None,
    background: background.Color(0x1a1a2e),
    init: init,
    update: update,
    view: view,
  )
}

// UI overlay state management
type UiModel {
  UiModel(game_state: game.GameState)
}

type UiMsg {
  UiUpdateGameState(game.GameState)
  UiRestart
}

fn ui_init(_flags) -> #(UiModel, lustre_effect.Effect(UiMsg)) {
  #(UiModel(game_state: game.new()), tiramisu_ui.register_lustre())
}

fn ui_update(
  _model: UiModel,
  msg: UiMsg,
) -> #(UiModel, lustre_effect.Effect(UiMsg)) {
  case msg {
    UiUpdateGameState(new_state) -> #(
      UiModel(game_state: new_state),
      lustre_effect.none(),
    )
    UiRestart -> #(
      UiModel(game_state: game.new()),
      tiramisu_ui.dispatch_to_tiramisu(Restart),
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
