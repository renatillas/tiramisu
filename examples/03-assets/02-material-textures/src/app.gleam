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

const brick_base = "https://threejs.org/examples/textures/brick_diffuse.jpg"

const uv_grid = "https://threejs.org/examples/textures/uv_grid_opengl.jpg"

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
  let assert Ok(_) = lustre.start(lustre.element(view()), "#app", Nil)
  Nil
}

fn view() {
  tiramisu.renderer("renderer", [renderer.width(980), renderer.height(580)], [
    tiramisu.scene("scene", [scene.background_color(0x0f172a)], [
      tiramisu.camera(
        "camera",
        [
          camera.active(True),
          camera.fov(48.0),
          transform.position(vec3.Vec3(0.0, 2.4, 8.0)),
        ],
        [],
      ),
      tiramisu.light("ambient", [light.ambient(), light.intensity(0.45)], []),
      tiramisu.light(
        "sun",
        [
          light.directional(),
          light.intensity(1.2),
          transform.position(vec3.Vec3(5.0, 7.0, -4.0)),
        ],
        [],
      ),
      tiramisu.primitive(
        "brick-wall",
        [
          primitive.box(vec3.Vec3(2.2, 2.2, 2.2)),
          material.standard(),
          material.color_map(brick_base),
          transform.position(vec3.Vec3(-2.4, 1.2, 0.0)),
        ],
        [],
      ),
      tiramisu.primitive(
        "transparent-card",
        [
          primitive.plane(vec2.Vec2(3.0, 3.0)),
          material.basic(),
          material.color_map(uv_grid),
          material.transparent(True),
          material.opacity(0.1),
          material.side(material.Double),
          transform.position(vec3.Vec3(2.5, 1.6, 0.0)),
          transform.rotation(vec3.Vec3(0.0, -0.5, 0.0)),
        ],
        [],
      ),
      tiramisu.primitive(
        "ground",
        [
          primitive.plane(vec2.Vec2(14.0, 10.0)),
          material.standard(),
          material.color(0x1e293b),
          material.roughness(1.0),
          transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
        ],
        [],
      ),
    ]),
  ])
}
