import lustre
import tiramisu
import tiramisu/camera
import tiramisu/renderer
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
      renderer.background_color(0x111827),
    ],
    [
      tiramisu.scene("scene", [], [
        tiramisu.camera(
          "camera",
          [
            camera.active(True),
            transform.position(vec3.Vec3(0.0, 0.0, 6.0)),
          ],
          [],
        ),
      ]),
    ],
  )
}
