//// Basic Scene Example
////
//// Demonstrates the basic usage of Tiramisu web components:
//// - Creating a renderer
//// - Adding a camera
//// - Adding a mesh with geometry and material
//// - Adding lights

import lustre
import lustre/attribute

import quaternion

import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/primitive
import tiramisu/scene
import tiramisu/transform

import vec/vec2
import vec/vec3

pub fn main() -> Nil {
  // Register the Tiramisu web component
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())

  // Start a simple Lustre app
  let app = lustre.element(view())
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

fn view() {
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
          camera.transform(transform.at(vec3.Vec3(0.0, 2.0, 5.0))),
        ],
        [],
      ),
      // A red cube
      tiramisu.primitive(
        "cube",
        [
          primitive.box(vec3.Vec3(2.0, 2.0, 2.0)),
          primitive.transform(transform.at(vec3.Vec3(0.0, 1.0, 0.0))),
          material.color(0xff6b6b),
        ],
        [],
      ),
      // A green sphere
      tiramisu.primitive(
        "sphere",
        [
          primitive.sphere(radius: 0.8, segments: vec2.Vec2(32, 16)),
          primitive.transform(transform.at(vec3.Vec3(-3.0, 0.8, 0.0))),
          material.color(0x4ecdc4),
          material.metalness(0.8),
          material.roughness(0.2),
        ],
        [],
      ),
      // A blue cylinder
      tiramisu.primitive(
        "cylinder",
        [
          primitive.cylinder(
            radius_top: 0.5,
            radius_bottom: 0.5,
            height: 2.0,
            segments: 32,
          ),
          material.color(0x45b7d1),
          material.metalness(0.5),
          material.roughness(0.5),
          primitive.transform(transform.at(vec3.Vec3(3.0, 1.0, 0.0))),
        ],
        [],
      ),
      // Ground plane
      tiramisu.primitive(
        "ground",
        [
          primitive.plane(vec2.Vec2(20.0, 20.0)),
          material.color(0x2d3436),
          primitive.transform(
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
          light.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
          light.cast_shadow(True),
        ],
        [],
      ),
    ],
  )
}
