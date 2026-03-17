import gleam/time/duration
import gleam_community/maths
import lustre
import lustre/attribute
import lustre/effect
import lustre/element/html
import lustre/event
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/primitive
import tiramisu/renderer
import tiramisu/scene
import tiramisu/transform
import vec/vec2
import vec/vec3

pub fn component() -> lustre.App(Nil, Model, Msg) {
  lustre.application(init, update, view)
}

pub type Model {
  Model(
    preset: PresetKind,
    cube_x: Float,
    bob: Float,
    rotation: Float,
    velocity: Float,
  )
}

pub type PresetKind {
  Warm
  Cool
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(preset: Warm, cube_x: 0.0, bob: 0.0, rotation: 0.0, velocity: 1.8),
    effect.none(),
  )
}

pub type Msg {
  Tick(renderer.Tick)
  ChoseWarm
  ChoseCool
  MoveLeft
  MoveRight
  Pause
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      let next_x = model.cube_x +. model.velocity *. dt
      let max_x = 2.6
      let min_x = 0.0 -. max_x
      let #(cube_x, velocity) = case next_x >. max_x {
        True -> #(max_x, -1.0 *. absolute(model.velocity))
        False ->
          case next_x <. min_x {
            True -> #(min_x, absolute(model.velocity))
            False -> #(next_x, model.velocity)
          }
      }

      #(
        Model(
          ..model,
          cube_x:,
          bob: model.bob +. dt *. 2.2,
          rotation: model.rotation +. dt *. 1.4,
          velocity:,
        ),
        effect.none(),
      )
    }
    ChoseWarm -> #(Model(..model, preset: Warm), effect.none())
    ChoseCool -> #(Model(..model, preset: Cool), effect.none())
    MoveLeft -> #(
      Model(..model, velocity: -1.0 *. base_speed(model.velocity)),
      effect.none(),
    )
    MoveRight -> #(
      Model(..model, velocity: base_speed(model.velocity)),
      effect.none(),
    )
    Pause -> #(Model(..model, velocity: 0.0), effect.none())
  }
}

fn view(model: Model) {
  let preset = current_preset(model.preset)
  let movement_label = case model.velocity >. 0.0 {
    True -> "Moving right"
    False ->
      case model.velocity <. 0.0 {
        True -> "Moving left"
        False -> "Paused"
      }
  }
  let cube_y = maths.sin(model.bob) *. 0.45

  html.div([attribute.styles([#("display", "grid"), #("gap", "1rem")])], [
    html.div(
      [
        attribute.styles([
          #("display", "flex"),
          #("flex-wrap", "wrap"),
          #("gap", "0.75rem"),
          #("align-items", "center"),
        ]),
      ],
      [
        html.p(
          [attribute.styles([#("margin", "0"), #("font-size", "0.95rem")])],
          [
            html.text(
              "Server state: " <> preset.name <> " preset, " <> movement_label,
            ),
          ],
        ),
        preset_button("Warm", model.preset == Warm, ChoseWarm),
        preset_button("Cool", model.preset == Cool, ChoseCool),
        preset_button("Left", model.velocity <. 0.0, MoveLeft),
        preset_button("Right", model.velocity >. 0.0, MoveRight),
        preset_button("Pause", model.velocity == 0.0, Pause),
      ],
    ),
    html.div(
      [
        attribute.styles([
          #("padding", "1rem"),
          #("border-radius", "20px"),
          #("background", "#ffffff"),
          #("box-shadow", "0 18px 40px rgba(15, 23, 42, 0.10)"),
        ]),
      ],
      [
        tiramisu.renderer(
          "server-renderer",
          [
            renderer.width(720),
            renderer.height(420),
            renderer.on_tick(Tick),
          ],
          [
            tiramisu.scene(
              "server-scene",
              [
                scene.background_color(preset.background),
              ],
              [
                tiramisu.camera(
                  "main-camera",
                  [
                    camera.active(True),
                    transform.position(vec3.Vec3(0.0, 1.4, 7.2)),
                  ],
                  [],
                ),
                tiramisu.light(
                  "ambient",
                  [light.ambient(), light.intensity(0.4)],
                  [],
                ),
                tiramisu.light(
                  "accent",
                  [
                    light.point(),
                    light.intensity(18.0),
                    light.distance(20.0),
                    light.color(preset.light),
                    transform.position(vec3.Vec3(3.5, 4.5, 3.0)),
                  ],
                  [],
                ),
                tiramisu.primitive(
                  "floor",
                  [
                    primitive.plane(vec2.Vec2(9.0, 6.0)),
                    material.basic(),
                    material.color(preset.floor),
                    transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
                    transform.position(vec3.Vec3(0.0, -1.4, 0.0)),
                  ],
                  [],
                ),
                tiramisu.primitive(
                  "cube",
                  [
                    primitive.box(vec3.Vec3(1.4, 1.4, 1.4)),
                    material.basic(),
                    material.color(preset.accent),
                    transform.position(vec3.Vec3(model.cube_x, cube_y, 0.0)),
                    transform.rotation(vec3.Vec3(0.35, model.rotation, 0.15)),
                  ],
                  [],
                ),
              ],
            ),
          ],
        ),
      ],
    ),
  ])
}

type Preset {
  Preset(
    name: String,
    background: Int,
    accent: Int,
    floor: Int,
    track: Int,
    light: Int,
  )
}

fn current_preset(preset: PresetKind) -> Preset {
  case preset {
    Warm ->
      Preset(
        name: "Warm",
        background: 0x2b1c1d,
        accent: 0xfb7185,
        floor: 0xfecdd3,
        track: 0x7c2d12,
        light: 0xfdba74,
      )
    Cool ->
      Preset(
        name: "Cool",
        background: 0x172554,
        accent: 0x38bdf8,
        floor: 0xbfdbfe,
        track: 0x1d4ed8,
        light: 0x67e8f9,
      )
  }
}

fn absolute(value: Float) -> Float {
  case value <. 0.0 {
    True -> 0.0 -. value
    False -> value
  }
}

fn base_speed(value: Float) -> Float {
  case value == 0.0 {
    True -> 1.8
    False -> absolute(value)
  }
}

fn preset_button(label: String, selected: Bool, message: Msg) {
  let background = case selected {
    True -> "#111827"
    False -> "#e5e7eb"
  }
  let color_value = case selected {
    True -> "#ffffff"
    False -> "#111827"
  }

  html.button(
    [
      event.on_click(message),
      attribute.styles([
        #("border", "0"),
        #("border-radius", "999px"),
        #("padding", "0.6rem 1rem"),
        #("background", background),
        #("color", color_value),
        #("cursor", "pointer"),
      ]),
    ],
    [html.text(label)],
  )
}
