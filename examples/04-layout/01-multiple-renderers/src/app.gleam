import lustre
import lustre/attribute
import lustre/element/html
import tiramisu
import tiramisu/camera
import tiramisu/material
import tiramisu/primitive
import tiramisu/renderer
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
  let assert Ok(_) = lustre.start(lustre.element(view()), "#app", Nil)
  Nil
}

fn view() {
  html.div(
    [
      attribute.styles([
        #("display", "grid"),
        #("gap", "1rem"),
        #("grid-template-columns", "repeat(auto-fit, minmax(320px, 1fr))"),
        #("padding", "1rem"),
      ]),
    ],
    [
      scene_card("Warm scene", 0x3f1d1d, 0xfb7185, "warm"),
      scene_card("Cool scene", 0x172554, 0x38bdf8, "cool"),
    ],
  )
}

fn scene_card(title: String, background: Int, color: Int, prefix: String) {
  html.div([], [
    html.h2([attribute.style("text-align", "center")], [html.text(title)]),
    tiramisu.renderer(
      prefix <> "-renderer",
      [
        renderer.width(420),
        renderer.height(320),
      ],
      [
        tiramisu.scene(prefix <> "-scene", [scene.background_color(background)], [
          tiramisu.camera(
            prefix <> "-camera",
            [
              camera.active(True),
              transform.position(vec3.Vec3(0.0, 1.0, 5.0)),
            ],
            [],
          ),
          tiramisu.primitive(
            prefix <> "-cube",
            [
              primitive.box(vec3.Vec3(1.8, 1.8, 1.8)),
              material.color(color),
              transform.rotation(vec3.Vec3(0.4, 0.6, 0.0)),
            ],
            [],
          ),
        ]),
      ],
    ),
  ])
}
