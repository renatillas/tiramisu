import gleam/float
import gleam/int
import gleam/option.{None, Some}
import gleam/time/duration
import lustre
import lustre/attribute.{attribute, class}
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import tiramisu
import tiramisu/camera
import tiramisu/effect as game_effect
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import tiramisu/ui
import vec/vec3

// =============================================================================
// Lustre UI Types
// =============================================================================

pub type GameState {
  Menu
  Playing
  Paused
}

/// Lustre model - stores UI state and the bridge
pub type UIModel {
  UIModel(
    bridge: ui.Bridge(UIMsg, GameMsg),
    state: GameState,
    score: Int,
    health: Float,
  )
}

/// Messages for the Lustre UI
pub type UIMsg {
  StartGame
  PauseGame
  ResumeGame
  UpdateScore(Int)
  UpdateHealth(Float)
}

// =============================================================================
// Tiramisu Game Types
// =============================================================================

pub type GameModel {
  GameModel(
    bridge: ui.Bridge(UIMsg, GameMsg),
    rotation: Float,
    position: vec3.Vec3(Float),
    score: Int,
    health: Float,
    paused: Bool,
  )
}

/// Messages for the Tiramisu game
pub type GameMsg {
  Tick
  Pause
  Resume
}

// =============================================================================
// Main Entry Point
// =============================================================================

pub fn main() {
  // 1. Create a bridge for Tiramisu-Lustre communication
  let bridge = ui.new_bridge()

  // 2. Start Lustre UI with the bridge in flags
  let assert Ok(_) =
    lustre.application(init, update, view)
    |> lustre.start("#app", bridge)

  // 3. Start Tiramisu game with the same bridge
  let assert Ok(_) =
    tiramisu.run(
      selector: "#game",
      dimensions: None,
      bridge: Some(bridge),
      init: game_init(bridge, _),
      update: game_update,
      view: game_view,
    )
}

// =============================================================================
// Lustre Init/Update/View
// =============================================================================

fn init(bridge: ui.Bridge(UIMsg, GameMsg)) -> #(UIModel, effect.Effect(UIMsg)) {
  #(
    UIModel(bridge: bridge, state: Menu, score: 0, health: 100.0),
    // Register Lustre's dispatch with the bridge
    ui.register_lustre(bridge),
  )
}

fn update(model: UIModel, msg: UIMsg) {
  case msg {
    StartGame -> #(
      UIModel(..model, state: Playing),
      ui.to_tiramisu(model.bridge, Resume),
    )
    PauseGame -> #(
      UIModel(..model, state: Paused),
      ui.to_tiramisu(model.bridge, Pause),
    )
    ResumeGame -> #(
      UIModel(..model, state: Playing),
      ui.to_tiramisu(model.bridge, Resume),
    )
    UpdateScore(score) -> #(UIModel(..model, score:), effect.none())
    UpdateHealth(health) -> #(UIModel(..model, health:), effect.none())
  }
}

fn view(model: UIModel) -> Element(UIMsg) {
  html.div([class("fixed top-0 left-0 w-full h-full pointer-events-none")], [
    case model.state {
      Menu -> menu_overlay()
      Playing -> hud_overlay(model)
      Paused -> pause_overlay(model)
    },
  ])
}

// =============================================================================
// UI Components
// =============================================================================

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

fn progress_bar(current: Float, max: Float, color: String) -> Element(UIMsg) {
  let percentage = { current /. max } *. 100.0
  let percentage_str = float.to_string(percentage)

  html.div([class("w-[150px] h-5 bg-white/20 rounded-[3px] overflow-hidden")], [
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
  ])
}

fn health_color(health: Float) -> String {
  case health {
    h if h >. 60.0 -> "#4ecdc4"
    h if h >. 30.0 -> "#ffe66d"
    _ -> "#ff6b6b"
  }
}

// =============================================================================
// Tiramisu Game Init/Update/View
// =============================================================================

fn game_init(bridge: ui.Bridge(UIMsg, GameMsg), _ctx: tiramisu.Context) {
  #(
    GameModel(
      bridge:,
      rotation: 0.0,
      position: vec3.Vec3(0.0, 0.0, 0.0),
      score: 0,
      health: 100.0,
      paused: True,
    ),
    game_effect.tick(Tick),
    None,
  )
}

fn game_update(model: GameModel, msg: GameMsg, ctx: tiramisu.Context) {
  case msg {
    Pause -> #(GameModel(..model, paused: True), game_effect.none(), None)

    Resume -> #(GameModel(..model, paused: False), game_effect.none(), None)

    Tick -> {
      case model.paused {
        True -> #(model, game_effect.tick(Tick), None)
        False -> {
          let delta_seconds = duration.to_seconds(ctx.delta_time)

          // Update rotation
          let new_rotation = model.rotation +. delta_seconds

          // Handle WASD movement
          let move_speed = 3.0
          let dx = case
            input.is_key_pressed(ctx.input, input.KeyD),
            input.is_key_pressed(ctx.input, input.KeyA)
          {
            True, False -> move_speed *. delta_seconds
            False, True -> 0.0 -. move_speed *. delta_seconds
            _, _ -> 0.0
          }

          let dy = case
            input.is_key_pressed(ctx.input, input.KeyW),
            input.is_key_pressed(ctx.input, input.KeyS)
          {
            True, False -> move_speed *. delta_seconds
            False, True -> 0.0 -. move_speed *. delta_seconds
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
              ..model,
              rotation: new_rotation,
              position: new_position,
              score: new_score,
            )

          // Build effects - always tick, always update UI, optionally pause
          let effects = case should_pause {
            True -> [
              game_effect.tick(Tick),
              ui.to_lustre(model.bridge, UpdateScore(new_score)),
              ui.to_lustre(model.bridge, UpdateHealth(model.health)),
              ui.to_lustre(model.bridge, PauseGame),
            ]
            False -> [
              game_effect.tick(Tick),
              ui.to_lustre(model.bridge, UpdateScore(new_score)),
              ui.to_lustre(model.bridge, UpdateHealth(model.health)),
            ]
          }

          #(new_model, game_effect.batch(effects), None)
        }
      }
    }
  }
}

fn game_view(model: GameModel, _ctx: tiramisu.Context) -> scene.Node {
  scene.empty(id: "root", transform: transform.identity, children: [
    // Camera
    scene.camera(
      id: "main-camera",
      camera: {
        let assert Ok(cam) =
          camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
        cam
      },
      transform: transform.at(vec3.Vec3(0.0, 0.0, 20.0)),
      active: True,
      look_at: None,
      viewport: None,
      postprocessing: None,
    ),
    // Lights
    scene.light(
      id: "ambient",
      light: {
        let assert Ok(l) = light.ambient(color: 0xffffff, intensity: 0.5)
        l
      },
      transform: transform.identity,
    ),
    scene.light(
      id: "directional",
      light: {
        let assert Ok(l) = light.directional(color: 0xffffff, intensity: 0.8)
        l
      },
      transform: transform.at(vec3.Vec3(5.0, 5.0, 5.0)),
    ),
    // Rotating cube
    scene.mesh(
      id: "cube",
      geometry: {
        let assert Ok(g) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
        g
      },
      material: {
        let assert Ok(m) =
          material.new()
          |> material.with_color(0x4ecdc4)
          |> material.build()
        m
      },
      transform: transform.identity
        |> transform.with_position(model.position)
        |> transform.with_euler_rotation(vec3.Vec3(
          model.rotation,
          model.rotation *. 0.7,
          0.0,
        )),
      physics: None,
    ),
  ])
}
