//// Basic Scene Example
////
//// Demonstrates the basic usage of Tiramisu web components:
//// - Creating a renderer
//// - Adding a camera
//// - Adding a mesh with geometry and material
//// - Adding lights

import lustre
import lustre/attribute
import lustre/element/html
import quaternion
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/transform
import vec/vec2
import vec/vec3

pub fn main() -> Nil {
  // Register all Tiramisu web components
  let assert Ok(_) = tiramisu.register()

  // Start a simple Lustre app
  let app = lustre.element(view())
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

fn view() {
  html.div([attribute.id("app")], [
    renderer.renderer(
      [
        renderer.width(800),
        renderer.height(600),
        renderer.background("#1a1a2e"),
      ],
      [
        // Camera looking at the scene
        camera.camera(
          "main",
          [
            camera.fov(75.0),
            camera.transform(transform.at(vec3.Vec3(0.0, 2.0, 5.0))),
            camera.active(True),
          ],
          [],
        ),
        // A red cube
        mesh.mesh(
          "cube",
          [
            mesh.geometry_box(vec3.Vec3(2.0, 2.0, 2.0)),
            mesh.color(0xff6b6b),
            mesh.metalness(0.3),
            mesh.roughness(0.7),
            mesh.transform(transform.at(vec3.Vec3(0.0, 1.0, 0.0))),
          ],
          [],
        ),
        // A green sphere
        mesh.mesh(
          "sphere",
          [
            mesh.geometry_sphere_simple(0.8),
            mesh.color(0x4ecdc4),
            mesh.metalness(0.8),
            mesh.roughness(0.2),
            mesh.transform(transform.at(vec3.Vec3(-3.0, 0.8, 0.0))),
          ],
          [],
        ),
        // A blue cylinder
        mesh.mesh(
          "cylinder",
          [
            mesh.geometry_cylinder_simple(0.5, 2.0),
            mesh.color(0x45b7d1),
            mesh.metalness(0.5),
            mesh.roughness(0.5),
            mesh.transform(transform.at(vec3.Vec3(3.0, 1.0, 0.0))),
          ],
          [],
        ),
        // Ground plane
        mesh.mesh(
          "ground",
          [
            mesh.geometry_plane(vec2.Vec2(20.0, 20.0)),
            mesh.color(0x2d3436),
            mesh.transform(
              transform.at(vec3.Vec3(0.0, 0.0, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
              ),
            ),
          ],
          [],
        ),
        // Ambient light for base illumination
        light.light(
          "ambient",
          [
            light.light_type("ambient"),
            light.color(0xffffff),
            light.intensity(0.4),
          ],
          [],
        ),
        // Directional light for shadows and highlights
        light.light(
          "sun",
          [
            light.light_type("directional"),
            light.color(0xffffff),
            light.intensity(1.0),
            light.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
            light.cast_shadow(True),
          ],
          [],
        ),
      ],
    ),
  ])
}
