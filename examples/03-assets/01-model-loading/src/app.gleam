import gleam/time/timestamp
import gleam_community/colour
import lustre
import lustre/effect
import lustre/element/html
import tiramisu/material
import tiramisu/scene

import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/transform
import vec/vec3

const model_url = "https://raw.githubusercontent.com/ginkogruen/lucy-openscad/df1607a929b26d53bcf90e7257426e33ea670db4/lucy.stl"

const background_texture = "background.jpg"

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

type Model {
  Model(timestamp: timestamp.Timestamp)
}

type Msg {
  Tick(scene.Tick)
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(Model(timestamp: timestamp.system_time()), effect.none())
}

fn update(_model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick(scene.Tick(timestamp:, ..)) -> #(Model(timestamp:), effect.none())
  }
}

fn view(model: Model) {
  let rotation = model.timestamp |> timestamp.to_unix_seconds
  html.div([], [
    tiramisu.renderer(
      "renderer",
      [
        renderer.width(900),
        renderer.height(540),
      ],
      [
        tiramisu.scene(
          "scene",
          [
            scene.on_tick(Tick),
            scene.background_texture(background_texture),
            scene.background_color_space_srgb(),
          ],
          [
            tiramisu.camera(
              "camera",
              [
                camera.active(True),
                camera.fov(45.0),
                transform.position(vec3.Vec3(0.0, 1.0, 10.0)),
              ],
              [],
            ),
            tiramisu.mesh(
              "lucy",
              [
                mesh.src(model_url),
                mesh.center(True),
                transform.rotation(vec3.Vec3(-1.5708, 0.0, rotation)),
                transform.scale(vec3.splat(0.004)),
                material.color(colour.pink |> colour.to_rgb_hex),
              ],
              [],
            ),
            tiramisu.light(
              "ambient",
              [
                light.ambient(),
                light.intensity(0.5),
              ],
              [],
            ),
            tiramisu.light(
              "sun",
              [
                light.directional(),
                light.intensity(1.2),
                transform.position(vec3.Vec3(5.0, 8.0, 5.0)),
              ],
              [],
            ),
          ],
        ),
      ],
    ),
  ])
}
