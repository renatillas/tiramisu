import gleam/time/duration
import gleam_community/maths
import lustre
import lustre/effect
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/primitive
import tiramisu/renderer
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

type Model {
  Model(rotation: Float, bob: Float)
}

type Msg {
  Tick(renderer.Tick)
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(Model(rotation: 0.0, bob: 0.0), effect.none())
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      #(
        Model(rotation: model.rotation +. dt, bob: model.bob +. dt *. 1.7),
        effect.none(),
      )
    }
  }
}

fn view(model: Model) {
  tiramisu.renderer(
    "renderer",
    [renderer.on_tick(Tick), renderer.width(800), renderer.height(500)],
    [
      tiramisu.scene("scene", [scene.background_color(0x020617)], [
        tiramisu.camera(
          "camera",
          [camera.active(True), transform.position(vec3.Vec3(0.0, 0.0, 5.0))],
          [],
        ),
        tiramisu.light("ambient", [light.ambient(), light.intensity(0.25)], []),
        tiramisu.light(
          "accent",
          [
            light.point(),
            light.intensity(18.0),
            light.distance(16.0),
            light.color(0x38bdf8),
            transform.position(vec3.Vec3(2.5, 3.5, 2.0)),
          ],
          [],
        ),
        tiramisu.primitive(
          "cube",
          [
            primitive.box(vec3.Vec3(1.8, 1.8, 1.8)),
            material.color(0xe879f9),
            transform.position(vec3.Vec3(0.0, maths.sin(model.bob) *. 0.45, 0.0)),
            transform.rotation(vec3.Vec3(0.3, model.rotation, 0.1)),
          ],
          [],
        ),
        tiramisu.primitive(
          "ring",
          [
            primitive.torus(
              radius: 2.2,
              tube: 0.12,
              radial_segments: 24,
              tubular_segments: 64,
            ),
            material.color(0x38bdf8),
            material.wireframe(True),
            transform.rotation(vec3.Vec3(model.rotation *. 0.6, 0.0, 0.0)),
          ],
          [],
        ),
      ]),
    ],
  )
}
