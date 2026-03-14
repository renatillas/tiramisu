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
        #("background", "#020617"),
        #("color", "#e2e8f0"),
      ]),
    ],
    [
      camera_card("Perspective camera", perspective_scene("perspective")),
      camera_card("Orthographic camera", orthographic_scene("orthographic")),
    ],
  )
}

fn camera_card(title: String, content) {
  html.div([], [
    html.h2([attribute.style("margin", "0 0 0.75rem 0")], [html.text(title)]),
    content,
  ])
}

fn perspective_scene(prefix: String) {
  scene_renderer(
    prefix,
    tiramisu.camera(
      prefix <> "-camera",
      [
        camera.active(True),
        camera.perspective(),
        camera.fov(80.0),
        camera.near(0.1),
        camera.far(100.0),
        transform.position(vec3.Vec3(0.0, 2.0, 7.0)),
      ],
      [],
    ),
  )
}

fn orthographic_scene(prefix: String) {
  scene_renderer(
    prefix,
    tiramisu.camera(
      prefix <> "-camera",
      [
        camera.active(True),
        camera.orthographic(),
        camera.left(-4.0),
        camera.right(4.0),
        camera.top(3.0),
        camera.bottom(-3.0),
        camera.near(0.1),
        camera.far(100.0),
        transform.position(vec3.Vec3(-0.5, 2.0, 7.0)),
        transform.rotation(vec3.Vec3(-0.2, -0.1, 0.0)),
      ],
      [],
    ),
  )
}

fn scene_renderer(prefix: String, camera_node) {
  tiramisu.renderer(
    prefix <> "-renderer",
    [renderer.width(460), renderer.height(320)],
    [
      tiramisu.scene(prefix <> "-scene", [scene.background_color(0x111827)], [
        camera_node,
        tiramisu.light(
          prefix <> "-positional-light",
          [transform.position(vec3.Vec3(1.0, 2.0, 1.0)), light.intensity(2.0)],
          [],
        ),
        tiramisu.light(
          prefix <> "-ambient-light",
          [light.ambient(), light.intensity(0.3)],
          [],
        ),
        tiramisu.primitive(
          prefix <> "-floor",
          [
            primitive.plane(vec2.Vec2(8.0, 8.0)),
            material.color(0x334155),
            transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
          ],
          [],
        ),
        tiramisu.primitive(
          prefix <> "-front",
          [
            primitive.box(vec3.Vec3(1.2, 1.2, 1.2)),
            material.color(0x38bdf8),
            transform.position(vec3.Vec3(-1.6, 0.6, 1.4)),
          ],
          [],
        ),
        tiramisu.primitive(
          prefix <> "-middle",
          [
            primitive.sphere(radius: 0.8, segments: vec2.Vec2(32, 16)),
            material.color(0xf97316),
            transform.position(vec3.Vec3(0.0, 0.8, 0.0)),
          ],
          [],
        ),
        tiramisu.primitive(
          prefix <> "-back",
          [
            primitive.torus(
              radius: 0.8,
              tube: 0.28,
              radial_segments: 24,
              tubular_segments: 48,
            ),
            material.color(0x22c55e),
            transform.position(vec3.Vec3(1.7, 0.9, -1.6)),
            transform.rotation(vec3.Vec3(0.8, 0.2, 0.0)),
          ],
          [],
        ),
      ]),
    ],
  )
}
