import gleam/time/duration
import lustre
import lustre/effect
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/primitive
import tiramisu/renderer
import tiramisu/tick
import tiramisu/transform
import vec/vec3

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

type Model {
  Model(rotation: Float)
}

type Msg {
  Tick(tick.TickContext)
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(Model(rotation: 0.0), effect.none())
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      #(Model(rotation: model.rotation +. dt), effect.none())
    }
  }
}

fn view(model: Model) {
  tiramisu.renderer(
    "renderer",
    [
      renderer.width(800),
      renderer.height(500),
      renderer.background_color(0x020617),
    ],
    [
      tiramisu.scene("scene", [tiramisu.on_tick(Tick)], [
        tiramisu.camera(
          "camera",
          [
            camera.active(True),
            transform.position(vec3.Vec3(0.0, 1.0, 5.0)),
          ],
          [],
        ),
        tiramisu.light(
          "light",
          [
            transform.position(vec3.Vec3(0.0, 5.0, 0.0)),
            light.ambient(),
          ],
          [],
        ),
        tiramisu.primitive(
          "cube",
          [
            primitive.box(vec3.Vec3(1.8, 1.8, 1.8)),
            material.color(0xe879f9),
            transform.rotation(vec3.Vec3(0.3, model.rotation, 0.1)),
          ],
          [],
        ),
      ]),
    ],
  )
}
