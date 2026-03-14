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
    [renderer.width(960), renderer.height(560)],
    [
      tiramisu.scene("scene", [scene.background_color(0x111827)], [
        tiramisu.camera(
          "camera",
          [camera.active(True), transform.position(vec3.Vec3(0.0, 2.5, 8.5))],
          [],
        ),
        tiramisu.light("ambient", [light.ambient(), light.intensity(0.35)], []),
        tiramisu.light(
          "rim",
          [
            light.directional(),
            light.intensity(1.2),
            transform.position(vec3.Vec3(5.0, 7.0, 4.0)),
          ],
          [],
        ),
        tiramisu.empty(
          "rig",
          [
            transform.position(vec3.Vec3(0.0, 1.2, 0.0)),
            transform.rotation(vec3.Vec3(0.0, 0.6, 0.0)),
          ],
          [
            tiramisu.primitive(
              "center",
              [
                primitive.box(vec3.Vec3(1.3, 1.3, 1.3)),
                material.color(0xf97316),
                material.roughness(0.35),
              ],
              [],
            ),
            tiramisu.primitive(
              "left",
              [
                primitive.sphere(radius: 0.7, segments: vec2.Vec2(36, 18)),
                material.color(0x22c55e),
                transform.position(vec3.Vec3(-2.2, 0.0, 0.0)),
              ],
              [],
            ),
            tiramisu.empty(
              "right-arm",
              [
                transform.position(vec3.Vec3(2.4, 0.4, 0.0)),
                transform.rotation(vec3.Vec3(0.0, 0.0, 0.5)),
              ],
              [
                tiramisu.primitive(
                  "arm",
                  [
                    primitive.cylinder(
                      radius_top: 0.25,
                      radius_bottom: 0.25,
                      height: 2.2,
                      segments: 24,
                    ),
                    material.color(0xf8fafc),
                    transform.rotation(vec3.Vec3(0.0, 0.0, 1.5708)),
                  ],
                  [],
                ),
                tiramisu.primitive(
                  "arm-tip",
                  [
                    primitive.cone(radius: 0.45, height: 0.9, segments: 24),
                    material.color(0xe879f9),
                    transform.position(vec3.Vec3(1.5, 0.0, 0.0)),
                    transform.rotation(vec3.Vec3(0.0, 0.0, -1.5708)),
                  ],
                  [],
                ),
              ],
            ),
          ],
        ),
        tiramisu.primitive(
          "floor",
          [
            primitive.plane(vec2.Vec2(16.0, 16.0)),
            material.color(0x1f2937),
            transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
          ],
          [],
        ),
      ]),
    ],
  )
}
