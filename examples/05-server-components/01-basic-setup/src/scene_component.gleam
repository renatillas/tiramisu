import lustre
import lustre/attribute
import lustre/element/html
import lustre/event
import tiramisu
import tiramisu/camera
import tiramisu/material
import tiramisu/primitive
import tiramisu/renderer
import tiramisu/scene
import tiramisu/transform
import vec/vec2
import vec/vec3

pub fn component() -> lustre.App(Nil, Model, Msg) {
  lustre.simple(init, update, view)
}

pub type Model {
  Warm
  Cool
}

fn init(_) -> Model {
  Warm
}

pub type Msg {
  ChoseWarm
  ChoseCool
}

fn update(_model: Model, msg: Msg) -> Model {
  case msg {
    ChoseWarm -> Warm
    ChoseCool -> Cool
  }
}

fn view(model: Model) {
  let preset = current_preset(model)

  html.div(
    [attribute.styles([#("display", "grid"), #("gap", "1rem")])],
    [
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
            [html.text("Server state: " <> preset.name <> " preset")],
          ),
          preset_button("Warm", model == Warm, ChoseWarm),
          preset_button("Cool", model == Cool, ChoseCool),
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
            [renderer.width(720), renderer.height(420)],
            [
              tiramisu.scene("server-scene", [scene.background_color(preset.background)], [
                tiramisu.camera(
                  "main-camera",
                  [
                    camera.active(True),
                    transform.position(vec3.Vec3(0.0, 1.25, 5.5)),
                  ],
                  [],
                ),
                tiramisu.primitive(
                  "floor",
                  [
                    primitive.plane(vec2.Vec2(8.0, 8.0)),
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
                    primitive.box(vec3.Vec3(1.8, 1.8, 1.8)),
                    material.basic(),
                    material.color(preset.accent),
                    transform.rotation(vec3.Vec3(0.45, 0.65, 0.0)),
                    transform.position(vec3.Vec3(0.0, 0.0, 0.0)),
                  ],
                  [],
                ),
              ]),
            ],
          ),
        ],
      ),
    ],
  )
}

type Preset {
  Preset(name: String, background: Int, accent: Int, floor: Int)
}

fn current_preset(model: Model) -> Preset {
  case model {
    Warm ->
      Preset(
        name: "Warm",
        background: 0x2b1c1d,
        accent: 0xfb7185,
        floor: 0xfecdd3,
      )
    Cool ->
      Preset(
        name: "Cool",
        background: 0x172554,
        accent: 0x38bdf8,
        floor: 0xbfdbfe,
      )
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
