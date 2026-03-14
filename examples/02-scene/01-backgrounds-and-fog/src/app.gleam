import lustre
import lustre/attribute
import lustre/element/html
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

const background = "https://images.unsplash.com/photo-1506744038136-46273834b3fb?auto=format&fit=crop&w=1200&q=80"

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
        #("grid-template-columns", "repeat(auto-fit, minmax(360px, 1fr))"),
        #("padding", "1rem"),
      ]),
    ],
    [
      scene_card("Texture background", [
        scene.background_texture(background),
        scene.background_color_space_srgb(),
      ]),
      scene_card("Equirectangular background", [
        scene.background_equirectangular(background),
        scene.background_color_space_srgb(),
      ]),
      scene_card("Fog", [
        scene.background_color(0x000000),
        scene.fog_exp2(color: 0x0f172a, density: 0.2),
      ]),
      scene_card("Fog", [scene.clear_background()]),
    ],
  )
}

fn scene_card(title: String, scene_attrs) {
  html.div([], [
    html.h2(
      [
        attribute.styles([
          #("margin", "0 0 0.75rem 0"),
          #("font-size", "1rem"),
        ]),
      ],
      [html.text(title)],
    ),
    tiramisu.renderer(
      title,
      [
        renderer.alpha(True),
        renderer.antialias(True),
        renderer.width(420),
        renderer.height(280),
      ],
      [
        tiramisu.scene(title <> "-scene", scene_attrs, [
          tiramisu.camera(
            title <> "-camera",
            [camera.active(True), transform.position(vec3.Vec3(0.0, 1.6, 6.0))],
            [],
          ),
          tiramisu.light(
            title <> "-ambient",
            [light.ambient(), light.intensity(0.65)],
            [],
          ),
          tiramisu.light(
            title <> "-sun",
            [
              light.directional(),
              light.intensity(1.0),
              transform.position(vec3.Vec3(5.0, 6.0, 4.0)),
            ],
            [],
          ),
          tiramisu.primitive(
            title <> "-front",
            [
              primitive.box(vec3.Vec3(1.4, 1.4, 1.4)),
              material.color(0x38bdf8),
              transform.position(vec3.Vec3(-1.6, 0.7, 1.4)),
            ],
            [],
          ),
          tiramisu.primitive(
            title <> "-mid",
            [
              primitive.sphere(radius: 0.9, segments: vec2.Vec2(32, 16)),
              material.color(0xf97316),
              transform.position(vec3.Vec3(0.0, 0.9, 0.0)),
            ],
            [],
          ),
          tiramisu.primitive(
            title <> "-back",
            [
              primitive.torus(
                radius: 0.8,
                tube: 0.25,
                radial_segments: 24,
                tubular_segments: 48,
              ),
              material.color(0x22c55e),
              transform.position(vec3.Vec3(1.6, 1.0, -1.8)),
              transform.rotation(vec3.Vec3(0.7, 0.3, 0.0)),
            ],
            [],
          ),
        ]),
      ],
    ),
  ])
}
