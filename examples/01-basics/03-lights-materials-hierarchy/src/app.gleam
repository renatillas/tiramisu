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
    [
      renderer.width(900),
      renderer.height(540),
    ],
    [
      tiramisu.scene("scene", [scene.background_color(0x111827)], [
        tiramisu.camera(
          "camera",
          [
            camera.active(True),
            camera.fov(55.0),
            transform.position(vec3.Vec3(0.0, 3.0, 8.0)),
          ],
          [],
        ),
        tiramisu.empty(
          "cluster",
          [transform.position(vec3.Vec3(0.0, 1.0, 0.0))],
          [
            tiramisu.primitive(
              "cube",
              [
                primitive.box(vec3.Vec3(1.5, 1.5, 1.5)),
                material.color(0xf97316),
                material.metalness(0.4),
                material.roughness(0.35),
                transform.position(vec3.Vec3(-1.8, 0.0, 0.0)),
              ],
              [],
            ),
            tiramisu.primitive(
              "sphere",
              [
                primitive.sphere(radius: 1.0, segments: vec2.Vec2(32, 16)),
                material.color(0x22c55e),
                material.roughness(0.6),
                transform.position(vec3.Vec3(1.8, 0.0, 0.0)),
              ],
              [],
            ),
          ],
        ),
        tiramisu.primitive(
          "ground",
          [
            primitive.plane(vec2.Vec2(20.0, 20.0)),
            material.color(0x334155),
            transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
            primitive.receive_shadow(True),
          ],
          [],
        ),
        tiramisu.light(
          "ambient",
          [
            light.ambient(),
            light.intensity(0.35),
            light.color(0xffffff),
          ],
          [],
        ),
        tiramisu.light(
          "sun",
          [
            light.directional(),
            light.intensity(1.2),
            light.color(0xfff7ed),
            light.cast_shadow(True),
            transform.position(vec3.Vec3(4.0, 8.0, 6.0)),
          ],
          [],
        ),
      ]),
    ],
  )
}
