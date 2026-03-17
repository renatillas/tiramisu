import lustre
import lustre/attribute
import lustre/effect
import lustre/element/html
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/scene
import tiramisu/transform
import vec/vec3

const good_model_url = "https://raw.githubusercontent.com/ginkogruen/lucy-openscad/df1607a929b26d53bcf90e7257426e33ea670db4/lucy.stl"

const bad_model_url = "https://example.com/not-found.stl"

const background_texture = "https://images.unsplash.com/photo-1519681393784-d120267933ba?auto=format&fit=crop&w=1200&q=80"

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

type Model {
  Model(good: String, bad: String)
}

type Msg {
  GoodLoaded(String)
  BadFailed(String)
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(Model(good: "", bad: ""), effect.none())
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    GoodLoaded(id) -> #(Model(..model, good: id), effect.none())
    BadFailed(id) -> #(Model(..model, bad: id), effect.none())
  }
}

fn view(model: Model) {
  html.div(
    [attribute.style("display", "grid"), attribute.style("gap", "1rem")],
    [
      status_banner(model),
      tiramisu.renderer(
        "renderer",
        [renderer.width(900), renderer.height(540)],
        [
          tiramisu.scene(
            "scene",
            [
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
                "lucy-mesh",
                [
                  mesh.src(good_model_url),
                  mesh.center(True),
                  // This doesn't work for now
                  mesh.on_model_load(GoodLoaded),
                  mesh.cast_shadow(True),
                  transform.scale(vec3.splat(0.004)),
                  transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
                  material.color(0xf472b6),
                ],
                [],
              ),
              tiramisu.mesh(
                "broken-texture-mesh",
                [
                  mesh.src(bad_model_url),
                  // This doesn't work for now
                  mesh.on_model_error(BadFailed),
                  mesh.hidden(True),
                ],
                [],
              ),
              tiramisu.light(
                "ambient",
                [light.ambient(), light.intensity(0.5)],
                [],
              ),
              tiramisu.light(
                "sun",
                [
                  light.directional(),
                  light.intensity(0.4),
                  transform.position(vec3.Vec3(5.0, 8.0, 10.0)),
                ],
                [],
              ),
              tiramisu.light(
                "sun",
                [
                  light.directional(),
                  light.intensity(1.2),
                  transform.position(vec3.Vec3(-5.0, 8.0, 10.0)),
                ],
                [],
              ),
            ],
          ),
        ],
      ),
    ],
  )
}

fn status_banner(model: Model) {
  let text =
    "Textures loaded: " <> model.good <> ". Textures failed: " <> model.bad

  html.p(
    [
      attribute.styles([
        #("margin", "0"),
        #("padding", "0.75rem 1rem"),
        #("border-radius", "12px"),
        #("background", "#e2e8f0"),
        #("color", "#0f172a"),
        #("font-family", "Iosevka, Menlo, monospace"),
      ]),
    ],
    [html.text(text)],
  )
}
