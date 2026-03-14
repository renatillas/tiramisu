import lustre
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
  tiramisu.renderer(
    "renderer",
    [renderer.width(900), renderer.height(540)],
    [
      tiramisu.scene("scene", [scene.background_color(0x0f172a)], [
        tiramisu.camera(
          "camera",
          [
            camera.active(True),
            camera.fov(55.0),
            transform.position(vec3.Vec3(0.0, 1.8, 6.0)),
          ],
          [],
        ),
        tiramisu.light("ambient", [light.ambient(), light.intensity(0.45)], []),
        tiramisu.light(
          "sun",
          [
            light.directional(),
            light.intensity(1.1),
            transform.position(vec3.Vec3(4.0, 6.0, 3.0)),
          ],
          [],
        ),
        tiramisu.primitive(
          "hero",
          [
            primitive.sphere(radius: 1.0, segments: vec2.Vec2(48, 24)),
            material.color(0x38bdf8),
            material.metalness(0.25),
            material.roughness(0.3),
            transform.position(vec3.Vec3(0.0, 0.4, 0.0)),
          ],
          [],
        ),
        tiramisu.primitive(
          "ground",
          [
            primitive.plane(vec2.Vec2(12.0, 12.0)),
            material.color(0x1e293b),
            transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
            transform.position(vec3.Vec3(0.0, -1.0, 0.0)),
          ],
          [],
        ),
      ]),
    ],
  )
}
