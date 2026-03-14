import gleam/list
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
  tiramisu.renderer("renderer", [renderer.width(1120), renderer.height(680)], [
    tiramisu.scene("scene", [scene.background_color(0x111827)], [
      tiramisu.camera(
        "camera",
        [
          camera.active(True),
          camera.fov(45.0),
          transform.position(vec3.Vec3(0.0, 4.0, 12.0)),
          transform.rotation(vec3.Vec3(-0.3, 0.0, 0.0)),
        ],
        [],
      ),
      tiramisu.light("ambient", [light.ambient(), light.intensity(0.5)], []),
      tiramisu.light(
        "sun",
        [
          light.directional(),
          light.intensity(1.2),
          light.cast_shadow(True),
          transform.position(vec3.Vec3(6.0, 8.0, 5.0)),
        ],
        [],
      ),
      primitive_card("box", vec3.Vec3(-4.5, 1.0, 2.5), [
        primitive.box(vec3.Vec3(1.6, 1.6, 1.6)),
        material.color(0x38bdf8),
        primitive.cast_shadow(True),
      ]),
      primitive_card("sphere", vec3.Vec3(-1.5, 1.0, 2.5), [
        primitive.sphere(radius: 0.95, segments: vec2.Vec2(40, 20)),
        material.color(0x22c55e),
        primitive.cast_shadow(True),
      ]),
      primitive_card("cylinder", vec3.Vec3(1.5, 1.0, 2.5), [
        primitive.cylinder(
          radius_top: 0.65,
          radius_bottom: 0.95,
          height: 1.8,
          segments: 32,
        ),
        material.color(0xf97316),
        primitive.cast_shadow(True),
      ]),
      primitive_card("cone", vec3.Vec3(4.5, 1.0, 2.5), [
        primitive.cone(radius: 0.95, height: 1.9, segments: 32),
        material.color(0xe879f9),
        primitive.cast_shadow(True),
      ]),
      primitive_card("torus", vec3.Vec3(-1.5, 1.0, -1.8), [
        primitive.torus(
          radius: 0.9,
          tube: 0.28,
          radial_segments: 24,
          tubular_segments: 48,
        ),
        material.color(0xfacc15),
        primitive.cast_shadow(True),
      ]),
      primitive_card("hidden", vec3.Vec3(1.5, 1.0, -1.8), [
        primitive.box(vec3.Vec3(1.4, 1.4, 1.4)),
        material.color(0xffffff),
        primitive.hidden(True),
      ]),
      tiramisu.primitive(
        "floor",
        [
          primitive.plane(vec2.Vec2(18.0, 12.0)),
          material.color(0x334155),
          primitive.receive_shadow(True),
          transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
        ],
        [],
      ),
    ]),
  ])
}

fn primitive_card(id: String, position: vec3.Vec3(Float), attrs) {
  let attrs = list.append(attrs, [transform.position(position)])
  tiramisu.primitive(id, attrs, [])
}
