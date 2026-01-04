import gleam/bool
import gleam/int
import gleam/javascript/promise
import gleam/list
import gleam/option.{None, Some}
import gleam/time/duration
import lustre
import lustre/attribute.{class}
import lustre/effect as lustre_effect
import lustre/element.{type Element}
import lustre/element/html
import tiramisu
import tiramisu/audio.{Playing}
import tiramisu/background
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import tiramisu/ui
import vec/vec2
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

// Shared bridge messages (Game <-> UI)
pub type BridgeMsg {
  // Game -> UI
  UpdateScore(Int)
  NotifyGameOver
  // UI -> Game
  RequestRestart
}

pub type Model {
  Model(
    bridge: ui.Bridge(BridgeMsg),
    input_bindings: input.InputBindings(Direction),
    snake: List(#(Int, Position)),
    direction: Direction,
    food: Position,
    score: Int,
    game_over: Bool,
    move_timer: Float,
    move_interval: Float,
    fruit_sound: option.Option(audio.Buffer),
    game_over_sound: option.Option(audio.Buffer),
    ate_food: Bool,
    next_segment_id: Int,
  )
}

pub type Msg {
  Tick
  FromBridge(BridgeMsg)
  FruitSoundLoaded(audio.Buffer)
  GameOverSoundLoaded(audio.Buffer)
  AudioLoadFailed
  BackgroundSet
  BackgroundFailed
}

// UI Messages
pub type UIMsg {
  FromBridgeUi(BridgeMsg)
  UiRestart
}

pub type UIModel {
  UIModel(bridge: ui.Bridge(BridgeMsg), score: Int, game_over: Bool)
}

pub fn main() -> Nil {
  // Create a bridge for communication
  let bridge = ui.new_bridge()

  // Start Lustre UI overlay
  let assert Ok(_) =
    lustre.application(ui_init, ui_update, ui_view)
    |> lustre.start("#app", bridge)

  // Start Tiramisu game in fullscreen mode
  let assert Ok(_) =
    tiramisu.application(init(bridge, _), update, view)
    |> tiramisu.start("#game", tiramisu.FullScreen, Some(#(bridge, FromBridge)))
  Nil
}

// UI Init/Update/View
fn ui_init(bridge) {
  #(
    UIModel(bridge: bridge, score: 0, game_over: False),
    ui.register_lustre(bridge, FromBridgeUi),
  )
}

fn ui_update(model: UIModel, msg: UIMsg) {
  case msg {
    FromBridgeUi(UpdateScore(score)) -> #(
      UIModel(..model, score: score),
      lustre_effect.none(),
    )
    FromBridgeUi(NotifyGameOver) -> #(
      UIModel(..model, game_over: True),
      lustre_effect.none(),
    )
    FromBridgeUi(_) -> #(model, lustre_effect.none())
    UiRestart -> #(
      UIModel(..model, score: 0, game_over: False),
      ui.send(model.bridge, RequestRestart),
    )
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

fn init(
  bridge: ui.Bridge(BridgeMsg),
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), option.Option(a)) {
  let input_bindings =
    input.new_bindings()
    |> input.bind_key(input.KeyW, Up)
    |> input.bind_key(input.KeyD, Right)
    |> input.bind_key(input.KeyS, Down)
    |> input.bind_key(input.KeyA, Left)

  // Load audio files
  let load_fruit_sound =
    audio.load_audio(
      url: "fruit-collect.wav",
      on_success: FruitSoundLoaded,
      on_error: AudioLoadFailed,
    )

  let load_game_over_sound =
    audio.load_audio(
      url: "game-over.wav",
      on_success: GameOverSoundLoaded,
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

  let initial_snake = [
    #(0, Position(10, 10)),
    #(1, Position(10, 11)),
    #(2, Position(10, 12)),
  ]
  let model =
    Model(
      bridge:,
      input_bindings:,
      snake: initial_snake,
      direction: Up,
      food: Position(15, 15),
      score: 0,
      game_over: False,
      move_timer: 0.0,
      move_interval: initial_speed,
      fruit_sound: option.None,
      game_over_sound: option.None,
      ate_food: False,
      next_segment_id: 3,
    )
  #(
    model,
    effect.batch([
      effect.dispatch(Tick),
      load_fruit_sound,
      load_game_over_sound,
      set_background,
    ]),
    option.None,
  )
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), option.Option(a)) {
  case msg {
    FruitSoundLoaded(buffer) -> {
      #(Model(..model, fruit_sound: Some(buffer)), effect.none(), option.None)
    }
    GameOverSoundLoaded(buffer) -> {
      #(
        Model(..model, game_over_sound: Some(buffer)),
        effect.none(),
        option.None,
      )
    }
    AudioLoadFailed | BackgroundSet | BackgroundFailed -> {
      #(model, effect.none(), option.None)
    }
    FromBridge(RequestRestart) -> {
      // Reset UI and restart game
      let #(new_model, game_effect, _) = init(model.bridge, ctx)
      // Preserve loaded audio
      let new_model =
        Model(
          ..new_model,
          fruit_sound: model.fruit_sound,
          game_over_sound: model.game_over_sound,
        )
      #(
        new_model,
        effect.batch([
          game_effect,
          ui.send_to_ui(model.bridge, UpdateScore(0)),
        ]),
        option.None,
      )
    }
    FromBridge(_) -> #(model, effect.none(), option.None)
    Tick -> {
      use <- bool.guard(
        model.game_over,
        case input.is_key_pressed(ctx.input, input.KeyR) {
          True -> {
            #(
              model,
              effect.from(fn(dispatch) { dispatch(FromBridge(RequestRestart)) }),
              option.None,
            )
          }
          False -> #(model, effect.dispatch(Tick), option.None)
        },
      )
      // Handle direction input
      let new_direction =
        get_new_direction(ctx.input, model.input_bindings, model.direction)

      // Update move timer
      let new_timer = model.move_timer +. duration.to_seconds(ctx.delta_time)

      // Validate direction change before moving
      let assert [#(_, head_pos), ..tail] = model.snake
      let safe_direction = case tail {
        [#(_, neck_pos), ..] -> {
          let potential_head = move_position(head_pos, new_direction)
          case potential_head == neck_pos {
            True -> model.direction
            False -> new_direction
          }
        }
        [] -> new_direction
      }

      case new_timer >=. model.move_interval {
        True -> {
          // Move snake
          let new_head_pos = move_position(head_pos, safe_direction)

          // Check collisions
          let hit_wall =
            new_head_pos.x < 0
            || new_head_pos.x >= grid_size
            || new_head_pos.y < 0
            || new_head_pos.y >= grid_size
          let tail_positions = list.map(tail, fn(segment) { segment.1 })
          let hit_self = list.contains(tail_positions, new_head_pos)

          case hit_wall || hit_self {
            True -> #(
              Model(..model, game_over: True),
              effect.batch([
                effect.dispatch(Tick),
                ui.send_to_ui(model.bridge, NotifyGameOver),
              ]),
              option.None,
            )
            False -> {
              // Check if ate food
              let ate_food = new_head_pos == model.food
              let #(new_snake, next_id) = case ate_food {
                True -> #(
                  [#(model.next_segment_id, new_head_pos), ..model.snake],
                  model.next_segment_id + 1,
                )
                False -> #(
                  [
                    #(model.next_segment_id, new_head_pos),
                    ..list.take(model.snake, list.length(model.snake) - 1)
                  ],
                  model.next_segment_id + 1,
                )
              }

              let #(new_food, new_score, new_interval) = case ate_food {
                True -> {
                  let food_snake_positions = list.map(new_snake, fn(s) { s.1 })
                  #(
                    generate_food(food_snake_positions),
                    model.score + 1,
                    model.move_interval -. speed_increase,
                  )
                }
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
                  next_segment_id: next_id,
                ),
                effect.batch([
                  effect.dispatch(Tick),
                  ui.send_to_ui(model.bridge, UpdateScore(new_score)),
                ]),
                option.None,
              )
            }
          }
        }
        False -> #(
          Model(..model, direction: safe_direction, move_timer: new_timer),
          effect.dispatch(Tick),
          option.None,
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

fn view(model: Model, _context: tiramisu.Context) -> scene.Node {
  let fruit_audio = case model.fruit_sound, model.ate_food {
    Some(buffer), True -> [
      scene.audio(
        id: "eating-sound",
        audio: audio.GlobalAudio(
          buffer:,
          config: audio.config()
            |> audio.with_state(Playing)
            |> audio.with_group(audio.SFX),
        ),
      ),
    ]
    _, _ -> []
  }

  let game_over_audio = case model.game_over_sound, model.game_over {
    Some(buffer), True -> [
      scene.audio(
        id: "game-over-sound",
        audio: audio.GlobalAudio(
          buffer:,
          config: audio.config()
            |> audio.with_state(Playing)
            |> audio.with_volume(0.3)
            |> audio.with_group(audio.SFX),
        ),
      ),
    ]
    _, _ -> []
  }

  let assert Ok(cam) =
    camera.perspective(field_of_view: 60.0, near: 0.1, far: 1000.0)

  let camera_node =
    scene.camera(
      id: "main-camera",
      camera: cam,
      transform: transform.look_at(
        from: transform.at(vec3.Vec3(0.0, 15.0, 15.0)),
        to: transform.identity,
        up: option.None,
      ),
      viewport: option.None,
      active: True,
      postprocessing: option.None,
    )
    |> list.wrap

  let lights = [
    scene.light(
      id: "ambient-light",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    ),
    scene.light(
      id: "directional-light",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 0.8)
        light
      },
      transform: transform.at(position: vec3.Vec3(10.0, 20.0, 10.0)),
    ),
  ]

  // Create snake segments
  // Each segment has a persistent ID that follows it through its lifetime
  let snake_segments =
    list.index_map(model.snake, fn(segment, idx) {
      let #(segment_id, pos) = segment
      let world_pos = position_to_world(pos)
      let #(color, rotation) = case idx {
        0 -> #(0x4ecdc4, get_head_rotation(model.direction))
        _ -> #(0x3aafa9, vec3.Vec3(0.0, 0.0, 0.0))
      }
      scene.mesh(
        id: "snake-" <> int.to_string(segment_id),
        geometry: {
          let size = cell_size *. 0.9
          let assert Ok(box) = geometry.box(size: vec3.Vec3(size, size, size))
          box
        },
        material: {
          let assert Ok(material) =
            material.new() |> material.with_color(color) |> material.build()
          material
        },
        transform: transform.at(position: world_pos)
          |> transform.with_euler_rotation(rotation),
        physics: option.None,
      )
    })

  // Create eyes for the snake head
  let eyes = case model.snake {
    [#(_, head_pos), ..] -> {
      let head_world = position_to_world(head_pos)
      let #(left_eye_pos, right_eye_pos) =
        get_eye_positions(head_world, model.direction)

      [
        scene.mesh(
          id: "left-eye",
          geometry: {
            let assert Ok(sphere) =
              geometry.sphere(radius: 0.12, segments: vec2.Vec2(16, 12))
            sphere
          },
          material: {
            let assert Ok(material) =
              material.new()
              |> material.with_color(0xffffff)
              |> material.build()
            material
          },
          transform: transform.at(position: left_eye_pos),
          physics: option.None,
        ),
        scene.mesh(
          id: "right-eye",
          geometry: {
            let assert Ok(sphere) =
              geometry.sphere(radius: 0.12, segments: vec2.Vec2(16, 12))
            sphere
          },
          material: {
            let assert Ok(material) =
              material.new()
              |> material.with_color(0xffffff)
              |> material.build()
            material
          },
          transform: transform.at(position: right_eye_pos),
          physics: option.None,
        ),
      ]
    }
    [] -> []
  }

  // Create food
  let food_world = position_to_world(model.food)
  let food_node =
    scene.mesh(
      id: "food",
      geometry: {
        let size = cell_size *. 0.8
        let assert Ok(box) = geometry.box(size: vec3.Vec3(size, size, size))
        box
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(0xff6b6b)
          |> material.build()
        material
      },
      transform: transform.at(position: food_world),
      physics: option.None,
    )
    |> list.wrap

  // Visual feedback for game over - make ground red
  let ground_color = case model.game_over {
    True -> 0x661111
    False -> 0x2d2d44
  }

  let ground_updated =
    scene.mesh(
      id: "ground",
      geometry: {
        let size = int.to_float(grid_size) *. cell_size
        let assert Ok(plane) = geometry.plane(size: vec2.Vec2(size, size))
        plane
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(ground_color)
          |> material.build()
        material
      },
      transform: transform.at(position: vec3.Vec3(0.0, -0.5, 0.0))
        |> transform.with_euler_rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
      physics: option.None,
    )
    |> list.wrap

  scene.empty(
    id: "scene",
    transform: transform.identity,
    children: list.flatten([
      fruit_audio,
      game_over_audio,
      camera_node,
      lights,
      ground_updated,
      snake_segments,
      eyes,
      food_node,
    ]),
  )
}
