//// Physics Demo Example
////
//// Demonstrates the Tiramisu physics integration with Rapier:
//// - Creating a physics world with gravity
//// - Adding dynamic rigidbodies that fall
//// - Adding static rigidbodies for the ground
//// - Physics simulation synced with rendering

import cocoa
import cocoa/physics_world
import cocoa/rigidbody
import lustre
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/transform
import vec/vec3

pub fn main() -> Nil {
  // Register all Tiramisu web components
  let assert Ok(_) = tiramisu.register()
  // Register physics components from cocoa
  let assert Ok(_) = cocoa.register()

  // Start a simple Lustre app
  let app = lustre.element(view())
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

fn view() {
  renderer.renderer(
    [renderer.width(800), renderer.height(600), renderer.background("#1a1a2e")],
    [
      // Camera looking at the scene
      camera.camera(
        "main",
        [
          camera.fov(75.0),
          camera.transform(transform.at(vec3.Vec3(0.0, 5.0, 10.0))),
          camera.active(True),
        ],
        [],
      ),
      // Physics world with gravity
      physics_world.physics_world([physics_world.gravity(0.0, -9.81, 0.0)], [
        // Dynamic falling box — rigidbody is a child of mesh
        mesh.mesh(
          "box",
          [
            mesh.geometry_box(1.0, 1.0, 1.0),
            mesh.color(0xff6b6b),
            mesh.metalness(0.3),
            mesh.roughness(0.7),
            mesh.transform(transform.at(vec3.Vec3(0.0, 5.0, 0.0))),
          ],
          [
            rigidbody.rigidbody(
              "box_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_cuboid(0.5, 0.5, 0.5),
                rigidbody.restitution(0.5),
              ],
              [],
            ),
          ],
        ),
        // Another dynamic box, offset
        mesh.mesh(
          "box2",
          [
            mesh.geometry_box(1.0, 1.0, 1.0),
            mesh.color(0x4ecdc4),
            mesh.metalness(0.5),
            mesh.roughness(0.5),
            mesh.transform(transform.at(vec3.Vec3(0.3, 8.0, 0.0))),
          ],
          [
            rigidbody.rigidbody(
              "box2_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_cuboid(0.5, 0.5, 0.5),
                rigidbody.restitution(0.3),
              ],
              [],
            ),
          ],
        ),
        // Dynamic sphere
        mesh.mesh(
          "sphere",
          [
            mesh.geometry_sphere_simple(0.5),
            mesh.color(0xfeca57),
            mesh.metalness(0.8),
            mesh.roughness(0.2),
            mesh.transform(transform.at(vec3.Vec3(-1.0, 6.0, 0.5))),
          ],
          [
            rigidbody.rigidbody(
              "sphere_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_ball(0.5),
                rigidbody.restitution(0.8),
              ],
              [],
            ),
          ],
        ),
        // Static ground
        mesh.mesh(
          "ground",
          [
            mesh.geometry_box(20.0, 1.0, 20.0),
            mesh.color(0x2d3436),
            mesh.metalness(0.1),
            mesh.roughness(0.9),
            mesh.transform(transform.at(vec3.Vec3(0.0, -0.5, 0.0))),
          ],
          [
            rigidbody.rigidbody(
              "ground_body",
              [
                rigidbody.body_type(rigidbody.Fixed),
                rigidbody.collider_cuboid(10.0, 0.5, 10.0),
              ],
              [],
            ),
          ],
        ),
      ]),
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
  )
}
