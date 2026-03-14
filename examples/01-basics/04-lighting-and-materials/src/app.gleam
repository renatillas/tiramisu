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
    [renderer.width(1100), renderer.height(620)],
    [
      tiramisu.scene("scene", [scene.background_color(0x0f172a)], [
        tiramisu.camera(
          "camera",
          [
            camera.active(True),
            camera.fov(45.0),
            transform.position(vec3.Vec3(0.0, 3.5, 10.0)),
          ],
          [],
        ),
        tiramisu.light(
          "ambient",
          [light.ambient(), light.intensity(0.2), light.color(0xf8fafc)],
          [],
        ),
        tiramisu.light(
          "sun",
          [
            light.directional(),
            light.intensity(1.1),
            light.color(0xfffbeb),
            transform.position(vec3.Vec3(6.0, 8.0, 5.0)),
          ],
          [],
        ),
        tiramisu.light(
          "fill",
          [
            light.point(),
            light.intensity(20.0),
            light.distance(18.0),
            light.color(0x38bdf8),
            transform.position(vec3.Vec3(-4.5, 2.5, 2.0)),
          ],
          [],
        ),
        tiramisu.light(
          "accent",
          [
            light.spot(),
            light.intensity(35.0),
            light.distance(20.0),
            light.angle(0.45),
            light.penumbra(0.5),
            light.color(0xf97316),
            transform.position(vec3.Vec3(4.5, 6.0, 3.5)),
          ],
          [],
        ),
        tiramisu.primitive(
          "standard-sphere",
          [
            primitive.sphere(radius: 1.0, segments: vec2.Vec2(40, 20)),
            material.standard(),
            material.color(0xe2e8f0),
            material.metalness(0.75),
            material.roughness(0.18),
            transform.position(vec3.Vec3(-3.6, 1.0, 0.0)),
          ],
          [],
        ),
        tiramisu.primitive(
          "phong-sphere",
          [
            primitive.sphere(radius: 1.0, segments: vec2.Vec2(40, 20)),
            material.phong(),
            material.color(0x38bdf8),
            material.shininess(90.0),
            transform.position(vec3.Vec3(-1.2, 1.0, 0.0)),
          ],
          [],
        ),
        tiramisu.primitive(
          "toon-sphere",
          [
            primitive.sphere(radius: 1.0, segments: vec2.Vec2(40, 20)),
            material.toon(),
            material.color(0x22c55e),
            transform.position(vec3.Vec3(1.2, 1.0, 0.0)),
          ],
          [],
        ),
        tiramisu.primitive(
          "basic-sphere",
          [
            primitive.sphere(radius: 1.0, segments: vec2.Vec2(40, 20)),
            material.basic(),
            material.color(0xf97316),
            transform.position(vec3.Vec3(3.6, 1.0, 0.0)),
          ],
          [],
        ),
        tiramisu.primitive(
          "floor",
          [
            primitive.plane(vec2.Vec2(16.0, 10.0)),
            material.standard(),
            material.color(0x1e293b),
            material.roughness(0.95),
            transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
          ],
          [],
        ),
      ]),
    ],
  )
}
