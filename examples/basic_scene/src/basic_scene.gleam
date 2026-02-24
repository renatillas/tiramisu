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
import tiramisu/scene

import quaternion

import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
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
    tiramisu.scene(
      "main",
      [
        attribute.width(800),
        attribute.height(600),
        scene.background_color(0x1a1a2e),
      ],
      [
        // Camera looking at the scene
        tiramisu.camera(
          "main",
          [
            camera.fov(75.0),
            camera.active(True),
            transform.transform(transform.at(vec3.Vec3(0.0, 2.0, 5.0))),
          ],
          [],
        ),
        // A red cube
        tiramisu.mesh(
          "cube",
          [
            mesh.geometry_box(vec3.Vec3(2.0, 2.0, 2.0)),
            mesh.color(0xff6b6b),
            material.metalness(0.3),
            material.roughness(0.7),
            transform.transform(transform.at(vec3.Vec3(0.0, 1.0, 0.0))),
          ],
          [],
        ),
        // A green sphere
        tiramisu.mesh(
          "sphere",
          [
            mesh.sphere(radius: 0.8, segments: vec2.Vec2(32, 16)),
            mesh.color(0x4ecdc4),
            material.metalness(0.8),
            material.roughness(0.2),
            transform.transform(transform.at(vec3.Vec3(-3.0, 0.8, 0.0))),
          ],
          [],
        ),
        // A blue cylinder
        tiramisu.mesh(
          "cylinder",
          [
            mesh.cylinder(
              radius_top: 0.5,
              radius_bottom: 0.5,
              height: 2.0,
              segments: 32,
            ),
            mesh.color(0x45b7d1),
            material.metalness(0.5),
            material.roughness(0.5),
            transform.transform(transform.at(vec3.Vec3(3.0, 1.0, 0.0))),
          ],
          [],
        ),
        // Ground plane
        tiramisu.mesh(
          "ground",
          [
            mesh.plane(vec2.Vec2(20.0, 20.0)),
            mesh.color(0x2d3436),
            transform.transform(
              transform.at(vec3.Vec3(0.0, 0.0, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
              ),
            ),
          ],
          [],
        ),
        // Ambient light for base illumination
        tiramisu.light(
          "ambient",
          [
            light.kind(light.Ambient),
            light.color(0xffffff),
            light.intensity(0.4),
          ],
          [],
        ),
        // Directional light for shadows and highlights
        tiramisu.light(
          "sun",
          [
            light.kind(light.Directional),
            light.color(0xffffff),
            light.intensity(1.0),
            transform.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
            material.cast_shadow(True),
          ],
          [],
        ),
      ],
    ),
  ])
}
