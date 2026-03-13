import lustre
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
  tiramisu.renderer(
    "renderer",
    [
      renderer.width(800),
      renderer.height(480),
    ],
    [
      tiramisu.scene("scene", [scene.background_color(0x0f172a)], [
        tiramisu.camera(
          "camera",
          [
            camera.active(True),
            camera.fov(60.0),
            transform.position(vec3.Vec3(0.0, 1.5, 6.0)),
          ],
          [],
        ),
        tiramisu.primitive(
          "cube",
          [
            primitive.box(vec3.Vec3(2.0, 2.0, 2.0)),
            material.color(0x38bdf8),
            transform.position(vec3.Vec3(0.0, 0.0, 0.0)),
            transform.rotation(vec3.Vec3(0.5, 0.7, 0.0)),
          ],
          [],
        ),
      ]),
    ],
  )
}
