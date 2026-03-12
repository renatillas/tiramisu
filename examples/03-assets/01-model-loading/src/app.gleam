import gleam_community/colour
import lustre
import lustre/attribute
import lustre/effect
import lustre/element/html
import tiramisu/material

import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/transform
import vec/vec3

const model_url = "Lucy.stl"

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

type Model {
  Model(status: String)
}

type Msg {
  ModelLoaded(String)
  ModelErrored(String)
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(Model("Loading model..."), effect.none())
}

fn update(_model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    ModelLoaded(id) -> #(Model("Loaded model: " <> id), effect.none())
    ModelErrored(id) -> #(Model("Failed to load model: " <> id), effect.none())
  }
}

fn view(model: Model) {
  html.div([], [
    tiramisu.renderer(
      "renderer",
      [
        renderer.width(900),
        renderer.height(540),
      ],
      [
        tiramisu.scene("scene", [], [
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
              mesh.cast_shadow(True),
              mesh.center(True),
              //transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
              transform.scale(vec3.splat(0.004)),
              material.color(colour.pink |> colour.to_rgb_hex),
              mesh.on_model_load(ModelLoaded),
              mesh.on_model_error(ModelErrored),
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
        ]),
      ],
    ),
    html.p([attribute.style("text-align", "center")], [html.text(model.status)]),
  ])
}
