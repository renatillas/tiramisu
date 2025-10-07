/// Example: Lustre UI Overlay with Tiramisu
///
/// This example demonstrates how to build UI overlays using Lustre
/// on top of a Tiramisu 3D game. Both systems run independently:
/// - Tiramisu manages the 3D game loop and rendering
/// - Lustre manages the UI overlay state
/// - Communication happens through tiramisu/ui module
///
/// Controls:
/// - Click "Start Game" to begin
/// - WASD: Move cube (increases score)
/// - Space: Pause game
import gleam/float
import gleam/int
import gleam/option.{None}
import lustre
import lustre/attribute.{attribute, class}
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import tiramisu
import tiramisu/camera
import tiramisu/effect as game_effect
import tiramisu/input
import tiramisu/scene
import tiramisu/transform
import tiramisu/ui
import vec/vec3

// --- State ---

pub type GameState {
  Menu
  Playing
  Paused
}

pub type UIModel {
  UIModel(state: GameState, score: Int, health: Float)
}

pub type UIMsg {
  StartGame
  PauseGame
  ResumeGame
  UpdateScore(Int)
  UpdateHealth(Float)
}

// --- Main ---

pub fn main() {
  // Start Lustre UI overlay
  let assert Ok(_) =
    lustre.application(init, update, view)
    |> lustre.start("#app", Nil)

  // Start Tiramisu game (in paused state initially)
  start_game()
}

// --- Init/Update/View ---

fn init(_flags) {
  // Register with Tiramisu to receive game state updates
  #(UIModel(Menu, 0, 100.0), ui.register())
}

fn update(model: UIModel, msg: UIMsg) {
  case msg {
    StartGame -> #(UIModel(..model, state: Playing), ui.send_to_game(Resume))
    PauseGame -> #(UIModel(..model, state: Paused), ui.send_to_game(Pause))
    ResumeGame -> #(UIModel(..model, state: Playing), ui.send_to_game(Resume))
    UpdateScore(score) -> #(UIModel(..model, score: score), effect.none())
    UpdateHealth(health) -> #(UIModel(..model, health: health), effect.none())
  }
}

fn view(model: UIModel) -> Element(UIMsg) {
  // UI overlay - positioned fixed to cover entire viewport and overlay Tiramisu canvas
  html.div(
    [class("fixed top-0 left-0 w-full h-full pointer-events-none")],
    [
      case model.state {
        Menu -> menu_overlay()
        Playing -> hud_overlay(model)
        Paused -> pause_overlay(model)
      },
    ],
  )
}

// --- UI Components ---

fn menu_overlay() -> Element(UIMsg) {
  html.div(
    [
      class(
        "absolute top-0 left-0 w-full h-full flex items-center justify-center bg-black/80 pointer-events-auto",
      ),
    ],
    [
      html.div(
        [
          class(
            "p-10 bg-[#1e1e2e]/95 rounded-[10px] text-white font-sans text-center",
          ),
        ],
        [
          html.h1([class("m-0 mb-5")], [element.text("Lustre + Tiramisu")]),
          html.p([class("my-2.5")], [element.text("UI Overlay Demo")]),
          html.ul([class("text-left my-5 pl-5")], [
            html.li([], [element.text("WASD: Move cube")]),
            html.li([], [element.text("Space: Pause")]),
          ]),
          html.button(
            [
              event.on_click(StartGame),
              class(
                "px-7 py-4 text-lg cursor-pointer bg-[#4ecdc4] text-white border-none rounded-[5px]",
              ),
            ],
            [element.text("Start Game")],
          ),
        ],
      ),
    ],
  )
}

fn hud_overlay(model: UIModel) -> Element(UIMsg) {
  html.div(
    [
      class(
        "absolute top-5 left-5 p-4 bg-black/60 rounded-[5px] text-white font-sans pointer-events-auto",
      ),
    ],
    [
      html.div([class("mb-2.5")], [
        element.text("Score: " <> int.to_string(model.score)),
      ]),
      html.div([], [
        html.div([class("mb-1")], [element.text("Health")]),
        progress_bar(model.health, 100.0, health_color(model.health)),
      ]),
    ],
  )
}

fn pause_overlay(model: UIModel) -> Element(UIMsg) {
  html.div([], [
    hud_overlay(model),
    html.div(
      [
        class(
          "absolute top-0 left-0 w-full h-full flex items-center justify-center bg-black/70 pointer-events-auto",
        ),
      ],
      [
        html.div(
          [
            class(
              "p-10 bg-[#1e1e2e]/95 rounded-[10px] text-white font-sans text-center",
            ),
          ],
          [
            html.h2([], [element.text("Paused")]),
            html.button(
              [
                event.on_click(ResumeGame),
                class(
                  "px-7 py-4 text-lg cursor-pointer bg-[#4ecdc4] text-white border-none rounded-[5px] mt-5",
                ),
              ],
              [element.text("Resume")],
            ),
          ],
        ),
      ],
    ),
  ])
}

// --- Helpers ---

fn progress_bar(current: Float, max: Float, color: String) -> Element(UIMsg) {
  let percentage = { current /. max } *. 100.0
  let percentage_str = float.to_string(percentage)

  html.div(
    [class("w-[150px] h-5 bg-white/20 rounded-[3px] overflow-hidden")],
    [
      html.div(
        [
          class("h-full transition-[width] duration-300"),
          attribute(
            "style",
            "width: " <> percentage_str <> "%; background: " <> color,
          ),
        ],
        [],
      ),
    ],
  )
}

fn health_color(health: Float) -> String {
  case health {
    h if h >. 60.0 -> "#4ecdc4"
    h if h >. 30.0 -> "#ffe66d"
    _ -> "#ff6b6b"
  }
}

// --- Tiramisu Game ---

pub type GameModel {
  GameModel(
    rotation: Float,
    position: vec3.Vec3(Float),
    score: Int,
    health: Float,
    paused: Bool,
  )
}

pub type GameMsg {
  Tick
  Pause
  Resume
}

fn start_game() -> Nil {
  tiramisu.run(
    width: 800,
    height: 600,
    background: 0x1a1a2e,
    init: game_init,
    update: game_update,
    view: game_view,
  )
}

fn game_init(_ctx: tiramisu.Context) {
  #(
    GameModel(
      rotation: 0.0,
      position: vec3.Vec3(0.0, 0.0, 0.0),
      score: 0,
      health: 100.0,
      paused: True,
    ),
    game_effect.tick(Tick),
  )
}

fn game_update(model: GameModel, msg: GameMsg, ctx: tiramisu.Context) {
  case msg {
    Pause -> #(GameModel(..model, paused: True), game_effect.none())

    Resume -> #(GameModel(..model, paused: False), game_effect.none())

    Tick -> {
      // Skip updates if paused
      case model.paused {
        True -> #(model, game_effect.tick(Tick))
        False -> {
          // Update rotation
          let new_rotation = model.rotation +. ctx.delta_time

          // Handle WASD movement
      let move_speed = 3.0
      let dx = case
        input.is_key_pressed(ctx.input, input.KeyD),
        input.is_key_pressed(ctx.input, input.KeyA)
      {
        True, False -> move_speed *. ctx.delta_time
        False, True -> 0.0 -. move_speed *. ctx.delta_time
        _, _ -> 0.0
      }

      let dy = case
        input.is_key_pressed(ctx.input, input.KeyW),
        input.is_key_pressed(ctx.input, input.KeyS)
      {
        True, False -> move_speed *. ctx.delta_time
        False, True -> 0.0 -. move_speed *. ctx.delta_time
        _, _ -> 0.0
      }

      let new_position =
        vec3.Vec3(
          model.position.x +. dx,
          model.position.y +. dy,
          model.position.z,
        )

      // Increase score when moving
      let new_score = case dx, dy {
        0.0, 0.0 -> model.score
        _, _ -> model.score + 1
      }

      // Check for pause key
      let should_pause = input.is_key_just_pressed(ctx.input, input.Space)

          let new_model =
            GameModel(
              rotation: new_rotation,
              position: new_position,
              score: new_score,
              health: model.health,
              paused: False,
            )

          // Send updates to Lustre UI
          let effects = case should_pause {
            True -> [
              game_effect.tick(Tick),
              ui.dispatch(UpdateScore(new_score)),
              ui.dispatch(UpdateHealth(model.health)),
              ui.dispatch(PauseGame),
            ]
            False -> [
              game_effect.tick(Tick),
              ui.dispatch(UpdateScore(new_score)),
              ui.dispatch(UpdateHealth(model.health)),
            ]
          }

          #(new_model, game_effect.batch(effects))
        }
      }
    }
  }
}

fn game_view(model: GameModel) {
  [
    // Camera
    scene.Camera(
      id: "main-camera",
      camera: {
        let assert Ok(cam) =
          camera.perspective(
            field_of_view: 75.0,
            aspect: 800.0 /. 600.0,
            near: 0.1,
            far: 1000.0,
          )
        cam
        |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))
        |> camera.set_position(vec3.Vec3(0.0, 0.0, 20.0))
      },
      transform: transform.at(vec3.Vec3(0.0, 0.0, 0.0)),
      active: True,
      viewport: None,
    ),
    // Lights
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(0xffffff, 0.5),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(0xffffff, 0.8),
      transform: transform.at(vec3.Vec3(5.0, 5.0, 5.0)),
    ),
    // Rotating cube
    scene.Mesh(
      id: "cube",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(0x4ecdc4, 0.5, 0.3, None, None),
      transform: transform.identity()
        |> transform.set_position(model.position)
        |> transform.set_rotation(vec3.Vec3(
          model.rotation,
          model.rotation *. 0.7,
          0.0,
        )),
      physics: None,
    ),
  ]
}
