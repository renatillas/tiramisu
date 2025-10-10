import gleam/bool
import gleam/int
import gleam/javascript/promise
import gleam/list
import gleam/option.{None}
import lustre
import lustre/attribute.{class}
import lustre/effect as lustre_effect
import lustre/element.{type Element}
import lustre/element/html
import tiramisu
import tiramisu/asset
import tiramisu/audio.{Playing}
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import tiramisu/ui
import vec/vec3

const grid_size = 20

const cell_size = 1.0

const initial_speed = 0.2

const speed_increase = 0.0015

pub type Direction {
  Up
  Down
  Left
  Right
}

pub type Position {
  Position(x: Int, y: Int)
}

pub type Model {
  Model(
    input_bindings: input.InputBindings(Direction),
    snake: List(Position),
    direction: Direction,
    food: Position,
    score: Int,
    game_over: Bool,
    move_timer: Float,
    move_interval: Float,
    asset_cache: asset.AssetCache,
    ate_food: Bool,
  )
}

pub type Msg {
  Tick
  RestartGame
  AudioAssetsLoaded(asset.BatchLoadResult)
}

// UI Messages
pub type UIMsg {
  UpdateScore(Int)
  GameOver
  Restart
}

pub type UIModel {
  UIModel(score: Int, game_over: Bool)
}

pub fn main() -> Nil {
  // Start Lustre UI overlay
  let assert Ok(_) =
    lustre.application(ui_init, ui_update, ui_view)
    |> lustre.start("#app", Nil)

  // Start Tiramisu game in fullscreen mode
  tiramisu.run(dimensions: None, background: 0x1a1a2e, init:, update:, view:)
}

// UI Init/Update/View
fn ui_init(_flags) {
  #(UIModel(score: 0, game_over: False), ui.register_lustre())
}

fn ui_update(model: UIModel, msg: UIMsg) {
  case msg {
    UpdateScore(score) -> #(
      UIModel(..model, score: score),
      lustre_effect.none(),
    )
    GameOver -> #(UIModel(..model, game_over: True), lustre_effect.none())
    Restart -> #(UIModel(score: 0, game_over: False), lustre_effect.none())
  }
}

fn ui_view(model: UIModel) -> Element(UIMsg) {
  html.div([class("fixed top-0 left-0 w-full h-full pointer-events-none")], [
    case model.game_over {
      True -> game_over_overlay(model)
      False -> hud_overlay(model)
    },
  ])
}

fn hud_overlay(model: UIModel) -> Element(UIMsg) {
  html.div(
    [
      class(
        "absolute top-5 left-5 p-4 bg-black/70 rounded-lg text-white font-mono text-xl pointer-events-auto",
      ),
    ],
    [html.div([], [element.text("Score: " <> int.to_string(model.score))])],
  )
}

fn game_over_overlay(model: UIModel) -> Element(UIMsg) {
  html.div([], [
    hud_overlay(model),
    html.div(
      [
        class(
          "text-white absolute top-0 left-0 w-full h-full flex items-center justify-center bg-red/80 pointer-events-auto",
        ),
      ],
      [
        html.div(
          [
            class(
              "p-10 bg-[#1e1e2e]/95 rounded-xl text-white font-mono text-center",
            ),
          ],
          [
            html.h1([class("text-6xl m-0 mb-5 text-[#ff6b6b]")], [
              element.text("Game Over!"),
            ]),
            html.p([class("text-3xl my-5")], [
              element.text("Final Score: " <> int.to_string(model.score)),
            ]),
          ],
        ),
      ],
    ),
  ])
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg)) {
  let input_bindings =
    input.new_bindings()
    |> input.bind_key(input.KeyW, Up)
    |> input.bind_key(input.KeyD, Right)
    |> input.bind_key(input.KeyS, Down)
    |> input.bind_key(input.KeyA, Left)
  let sound_assets = [
    asset.AudioAsset("game-over.wav"),
    asset.AudioAsset("fruit-collect.wav"),
  ]
  let fruit_eat_sound =
    effect.from_promise(promise.map(
      asset.load_batch_simple(sound_assets),
      AudioAssetsLoaded,
    ))
  let initial_snake = [
    Position(10, 10),
    Position(10, 11),
    Position(10, 12),
  ]
  let model =
    Model(
      input_bindings:,
      snake: initial_snake,
      direction: Up,
      food: Position(15, 15),
      score: 0,
      game_over: False,
      move_timer: 0.0,
      move_interval: initial_speed,
      asset_cache: asset.new_cache(),
      ate_food: False,
    )
  #(model, effect.batch([effect.tick(Tick), fruit_eat_sound]))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg)) {
  case msg {
    AudioAssetsLoaded(audio_buffer) -> {
      let asset.BatchLoadResult(asset_cache, _) = audio_buffer
      #(Model(..model, asset_cache:), effect.none())
    }
    RestartGame -> {
      // Reset UI and restart game
      let #(new_model, game_effect) = init(ctx)
      #(
        new_model,
        effect.batch([
          game_effect,
          ui.dispatch_to_lustre(RestartGame),
          ui.dispatch_to_lustre(UpdateScore(0)),
        ]),
      )
    }
    Tick -> {
      use <- bool.guard(
        model.game_over,
        case input.is_key_pressed(ctx.input, input.KeyR) {
          True -> {
            #(model, effect.from(fn(dispatch) { dispatch(RestartGame) }))
          }
          False -> #(model, effect.tick(Tick))
        },
      )
      // Handle direction input
      let new_direction =
        get_new_direction(ctx.input, model.input_bindings, model.direction)

      // Update move timer
      let new_timer = model.move_timer +. ctx.delta_time

      // Validate direction change before moving
      let assert [head, ..tail] = model.snake
      let safe_direction = case tail {
        [neck, ..] -> {
          let potential_head = move_position(head, new_direction)
          case potential_head == neck {
            True -> model.direction
            False -> new_direction
          }
        }
        [] -> new_direction
      }

      case new_timer >=. model.move_interval {
        True -> {
          // Move snake
          let new_head = move_position(head, safe_direction)

          // Check collisions
          let hit_wall =
            new_head.x < 0
            || new_head.x >= grid_size
            || new_head.y < 0
            || new_head.y >= grid_size
          let hit_self = list.contains(tail, new_head)

          case hit_wall || hit_self {
            True -> #(
              Model(..model, game_over: True),
              effect.batch([
                effect.tick(Tick),
                ui.dispatch_to_lustre(GameOver),
              ]),
            )
            False -> {
              // Check if ate food
              let ate_food = new_head == model.food
              let new_snake = case ate_food {
                True -> [new_head, ..model.snake]
                False -> [
                  new_head,
                  ..list.take(model.snake, list.length(model.snake) - 1)
                ]
              }

              let #(new_food, new_score, new_interval) = case ate_food {
                True -> #(
                  generate_food(new_snake),
                  model.score + 1,
                  model.move_interval -. speed_increase,
                )
                False -> #(model.food, model.score, model.move_interval)
              }

              #(
                Model(
                  ..model,
                  snake: new_snake,
                  direction: safe_direction,
                  food: new_food,
                  score: new_score,
                  game_over: False,
                  move_timer: 0.0,
                  ate_food:,
                  move_interval: new_interval,
                ),
                effect.batch([
                  effect.tick(Tick),
                  ui.dispatch_to_lustre(UpdateScore(new_score)),
                ]),
              )
            }
          }
        }
        False -> #(
          Model(..model, direction: safe_direction, move_timer: new_timer),
          effect.tick(Tick),
        )
      }
    }
  }
}

fn get_new_direction(
  input_state: input.InputState,
  input_bidnings: input.InputBindings(Direction),
  current: Direction,
) -> Direction {
  case
    current,
    input.is_action_just_pressed(input_state, input_bidnings, Up),
    input.is_action_just_pressed(input_state, input_bidnings, Down),
    input.is_action_just_pressed(input_state, input_bidnings, Right),
    input.is_action_just_pressed(input_state, input_bidnings, Left)
  {
    _, False, False, False, False
    | Down, True, _, _, _
    | Up, _, True, _, _
    | Left, _, _, True, _
    | Right, _, _, _, True
    -> current
    _, True, _, _, _ -> Up
    _, _, True, _, _ -> Down
    _, _, _, True, _ -> Right
    _, _, _, _, True -> Left
  }
}

fn move_position(pos: Position, dir: Direction) -> Position {
  case dir {
    Up -> Position(pos.x, pos.y - 1)
    Down -> Position(pos.x, pos.y + 1)
    Left -> Position(pos.x - 1, pos.y)
    Right -> Position(pos.x + 1, pos.y)
  }
}

fn generate_food(snake: List(Position)) -> Position {
  // Simple pseudo-random food generation
  // In a real game, you'd want better random generation
  let assert [head, ..] = snake
  let x = { head.x * 7 + head.y * 13 + list.length(snake) } % grid_size
  let y = { head.y * 11 + head.x * 17 + list.length(snake) * 3 } % grid_size
  let new_food = Position(x, y)

  case list.contains(snake, new_food) {
    True -> Position({ x + 3 } % grid_size, { y + 5 } % grid_size)
    False -> new_food
  }
}

fn position_to_world(pos: Position) -> vec3.Vec3(Float) {
  let offset = int.to_float(grid_size) *. cell_size /. 2.0
  vec3.Vec3(
    int.to_float(pos.x) *. cell_size -. offset,
    0.0,
    int.to_float(pos.y) *. cell_size -. offset,
  )
}

fn get_head_rotation(direction: Direction) -> vec3.Vec3(Float) {
  case direction {
    Up -> vec3.Vec3(0.0, 0.0, 0.0)
    Down -> vec3.Vec3(0.0, 3.14159, 0.0)
    Left -> vec3.Vec3(0.0, 1.5708, 0.0)
    Right -> vec3.Vec3(0.0, -1.5708, 0.0)
  }
}

fn get_eye_positions(
  head_pos: vec3.Vec3(Float),
  direction: Direction,
) -> #(vec3.Vec3(Float), vec3.Vec3(Float)) {
  let eye_offset = 0.15
  let forward_offset = 0.45
  case direction {
    Up -> #(
      vec3.Vec3(
        head_pos.x -. eye_offset,
        head_pos.y,
        head_pos.z -. forward_offset,
      ),
      vec3.Vec3(
        head_pos.x +. eye_offset,
        head_pos.y,
        head_pos.z -. forward_offset,
      ),
    )
    Down -> #(
      vec3.Vec3(
        head_pos.x -. eye_offset,
        head_pos.y,
        head_pos.z +. forward_offset,
      ),
      vec3.Vec3(
        head_pos.x +. eye_offset,
        head_pos.y,
        head_pos.z +. forward_offset,
      ),
    )
    Left -> #(
      vec3.Vec3(
        head_pos.x -. forward_offset,
        head_pos.y,
        head_pos.z -. eye_offset,
      ),
      vec3.Vec3(
        head_pos.x -. forward_offset,
        head_pos.y,
        head_pos.z +. eye_offset,
      ),
    )
    Right -> #(
      vec3.Vec3(
        head_pos.x +. forward_offset,
        head_pos.y,
        head_pos.z -. eye_offset,
      ),
      vec3.Vec3(
        head_pos.x +. forward_offset,
        head_pos.y,
        head_pos.z +. eye_offset,
      ),
    )
  }
}

fn view(model: Model) -> List(scene.Node) {
  let fruit_audio = case
    asset.get_audio(model.asset_cache, "fruit-collect.wav"),
    model.ate_food
  {
    Ok(audio_buffer), True ->
      scene.Audio(
        id: "eating-sound",
        audio: audio.GlobalAudio(
          buffer: audio_buffer,
          config: audio.AudioConfig(
            state: case model.ate_food {
              True -> Playing
              False -> audio.Stopped
            },
            fade: audio.NoFade,
            group: option.Some(audio.SFX),
            on_end: option.None,
            volume: 1.0,
            loop: False,
            playback_rate: 1.0,
          ),
        ),
      )
      |> list.wrap

    _, _ -> []
  }

  let game_over_audio = case
    asset.get_audio(model.asset_cache, "game-over.wav"),
    model.game_over
  {
    Ok(audio_buffer), True -> [
      scene.Audio(
        id: "game-over-sound",
        audio: audio.GlobalAudio(
          buffer: audio_buffer,
          config: audio.AudioConfig(
            volume: 0.3,
            loop: False,
            playback_rate: 1.0,
            state: case model.game_over {
              True -> audio.Playing
              False -> audio.Stopped
            },
            fade: audio.NoFade,
            group: option.Some(audio.SFX),
            on_end: option.None,
          ),
        ),
      ),
    ]
    _, _ -> []
  }

  let assert Ok(cam) =
    camera.perspective(field_of_view: 60.0, near: 0.1, far: 1000.0)

  let camera_node =
    scene.Camera(
      id: "main_camera",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 15.0, 15.0)),
      viewport: option.None,
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      active: True,
    )
    |> list.wrap

  let lights = [
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 0.8)
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(10.0, 20.0, 10.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  // Create snake segments
  let snake_segments =
    list.index_map(model.snake, fn(pos, idx) {
      let world_pos = position_to_world(pos)
      let #(color, rotation) = case idx {
        0 -> #(0x4ecdc4, get_head_rotation(model.direction))
        _ -> #(0x3aafa9, vec3.Vec3(0.0, 0.0, 0.0))
      }
      scene.Mesh(
        id: "snake_" <> int.to_string(idx),
        geometry: {
          let assert Ok(box) =
            geometry.box(
              width: cell_size *. 0.9,
              height: cell_size *. 0.9,
              depth: cell_size *. 0.9,
            )
          box
        },
        material: {
          let assert Ok(material) =
            material.new() |> material.with_color(color) |> material.build()
          material
        },
        transform: transform.Transform(
          position: world_pos,
          rotation: rotation,
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
        physics: option.None,
      )
    })

  // Create eyes for the snake head
  let eyes = case model.snake {
    [head, ..] -> {
      let head_world = position_to_world(head)
      let #(left_eye_pos, right_eye_pos) =
        get_eye_positions(head_world, model.direction)

      [
        scene.Mesh(
          id: "left_eye",
          geometry: {
            let assert Ok(sphere) =
              geometry.sphere(
                radius: 0.12,
                width_segments: 16,
                height_segments: 12,
              )
            sphere
          },
          material: {
            let assert Ok(material) =
              material.new()
              |> material.with_color(0xffffff)
              |> material.build()
            material
          },
          transform: transform.Transform(
            position: left_eye_pos,
            rotation: vec3.Vec3(0.0, 0.0, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          physics: option.None,
        ),
        scene.Mesh(
          id: "right_eye",
          geometry: {
            let assert Ok(sphere) =
              geometry.sphere(
                radius: 0.12,
                width_segments: 16,
                height_segments: 12,
              )
            sphere
          },
          material: {
            let assert Ok(material) =
              material.new()
              |> material.with_color(0xffffff)
              |> material.build()
            material
          },
          transform: transform.Transform(
            position: right_eye_pos,
            rotation: vec3.Vec3(0.0, 0.0, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          physics: option.None,
        ),
      ]
    }
    [] -> []
  }

  // Create food
  let food_world = position_to_world(model.food)
  let food_node =
    scene.Mesh(
      id: "food",
      geometry: {
        let assert Ok(box) =
          geometry.box(
            width: cell_size *. 0.8,
            height: cell_size *. 0.8,
            depth: cell_size *. 0.8,
          )
        box
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(0xff6b6b)
          |> material.build()
        material
      },
      transform: transform.Transform(
        position: food_world,
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    )
    |> list.wrap

  // Visual feedback for game over - make ground red
  let ground_color = case model.game_over {
    True -> 0x661111
    False -> 0x2d2d44
  }

  let ground_updated =
    scene.Mesh(
      id: "ground",
      geometry: {
        let assert Ok(plane) =
          geometry.plane(
            width: int.to_float(grid_size) *. cell_size,
            height: int.to_float(grid_size) *. cell_size,
          )
        plane
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(ground_color)
          |> material.build()
        material
      },
      transform: transform.Transform(
        position: vec3.Vec3(0.0, -0.5, 0.0),
        rotation: vec3.Vec3(-1.5708, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    )
    |> list.wrap

  list.flatten([
    fruit_audio,
    game_over_audio,
    camera_node,
    lights,
    ground_updated,
    snake_segments,
    eyes,
    food_node,
  ])
}
