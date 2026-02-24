//// Multi-Renderer Example
////
//// Tests that multiple <tiramisu-renderer> instances on the same page
//// work independently — each with its own scene, camera, render loop,
//// and registry. Verifies that the RenderLoop refactor correctly
//// isolates per-instance mutable state.

import gleam/time/duration
import gleam_community/maths
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import quaternion
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
import tiramisu/scene
import tiramisu/tick
import tiramisu/transform
import vec/vec2
import vec/vec3
import vec/vec3f

// MODEL -----------------------------------------------------------------------

type Model {
  Model(time: Float)
}

type Msg {
  Tick1(tick.TickContext)
  Tick2(tick.TickContext)
  Tick3(tick.TickContext)
  Tick4(tick.TickContext)
}

// MAIN ------------------------------------------------------------------------

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())

  let app = lustre.application(init:, update:, view:)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  // Subscribe to ticks from all scenes (empty string = global)
  #(
    Model(time: 0.0),
    effect.batch([
      tick.subscribe("warm", Tick1),
      tick.subscribe("cool", Tick2),
      tick.subscribe("wire", Tick3),
      tick.subscribe("mini", Tick4),
    ]),
  )
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick1(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      #(Model(time: model.time +. dt), effect.none())
    }
    Tick2(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      #(Model(time: model.time +. dt), effect.none())
    }
    Tick3(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      #(Model(time: model.time +. dt), effect.none())
    }
    Tick4(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      #(Model(time: model.time +. dt), effect.none())
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div([], [
    html.h1([], [element.text("Multi-Renderer Test")]),
    html.div([attribute.class("grid")], [
      // Renderer 1: Red/warm scene
      html.div([], [
        html.h2([], [element.text("Scene A — Warm Colors")]),
        warm_scene(model),
      ]),
      // Renderer 2: Blue/cool scene
      html.div([], [
        html.h2([], [element.text("Scene B — Cool Colors")]),
        cool_scene(model),
      ]),
      // Renderer 3: Wireframe scene
      html.div([], [
        html.h2([], [element.text("Scene C — Wireframes")]),
        wireframe_scene(model),
      ]),
    ]),
  ])
}

// SCENE A — warm colors with multiple objects ---------------------------------

fn warm_scene(model: Model) -> Element(Msg) {
  // Orbit: radius 6, speed 0.5 rad/s
  let cx = maths.sin(model.time *. 0.5) *. 6.0
  let cz = maths.cos(model.time *. 0.5) *. 6.0
  tiramisu.scene(
    "warm",
    [
      attribute.width(480),
      attribute.height(360),
      scene.background_color(0x2d1b00),
    ],
    [
      tiramisu.camera(
        "warm-cam",
        [
          camera.fov(60.0),
          transform.transform(
            transform.at(vec3.Vec3(cx, 3.0, cz))
            |> transform.with_look_at(vec3f.zero),
          ),
          camera.active(True),
        ],
        [],
      ),
      // Red cube
      tiramisu.mesh(
        "warm-cube",
        [
          mesh.geometry_box(vec3.Vec3(1.5, 1.5, 1.5)),
          mesh.color(0xff4444),
          material.metalness(0.3),
          material.roughness(0.7),
          transform.transform(transform.at(vec3.Vec3(-2.0, 0.75, 0.0))),
        ],
        [],
      ),
      // Orange sphere
      tiramisu.mesh(
        "warm-sphere",
        [
          mesh.sphere(1.0, segments: vec2.Vec2(32, 16)),
          mesh.color(0xff8800),
          material.metalness(0.6),
          material.roughness(0.3),
          transform.transform(transform.at(vec3.Vec3(0.0, 1.0, 0.0))),
        ],
        [],
      ),
      // Yellow cone
      tiramisu.mesh(
        "warm-cone",
        [
          mesh.cone(radius: 0.8, height: 2.0, segments: 32),
          mesh.color(0xffcc00),
          material.metalness(0.4),
          material.roughness(0.5),
          transform.transform(transform.at(vec3.Vec3(2.0, 1.0, 0.0))),
        ],
        [],
      ),
      // Ground
      tiramisu.mesh(
        "warm-ground",
        [
          mesh.plane(vec2.Vec2(12.0, 12.0)),
          mesh.color(0x3d2b1f),
          transform.transform(
            transform.at(vec3.Vec3(0.0, 0.0, 0.0))
            |> transform.with_rotation(
              quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
            ),
          ),
          material.receive_shadow(True),
        ],
        [],
      ),
      tiramisu.light(
        "warm-ambient",
        [
          light.kind(light.Ambient),
          light.color(0xffddaa),
          light.intensity(0.3),
        ],
        [],
      ),
      tiramisu.light(
        "warm-sun",
        [
          light.kind(light.Directional),
          light.color(0xffaa44),
          light.intensity(1.2),
          transform.transform(transform.at(vec3.Vec3(3.0, 8.0, 5.0))),
          light.cast_shadow(True),
        ],
        [],
      ),
    ],
  )
}

// SCENE B — cool colors -------------------------------------------------------

fn cool_scene(model: Model) -> Element(Msg) {
  // Orbit: radius 8, speed 0.3 rad/s, opposite direction
  let cx = maths.sin(model.time *. -0.3) *. 8.0
  let cz = maths.cos(model.time *. -0.3) *. 8.0
  tiramisu.scene(
    "cool",
    [
      attribute.width(480),
      attribute.height(360),
      scene.background_color(0x001122),
    ],
    [
      tiramisu.camera(
        "cool-cam",
        [
          camera.fov(50.0),
          transform.transform(
            transform.at(vec3.Vec3(cx, 4.0, cz))
            |> transform.with_look_at(vec3f.zero),
          ),
          camera.active(True),
        ],
        [],
      ),
      // Blue torus
      tiramisu.mesh(
        "cool-torus",
        [
          mesh.torus(
            radius: 1.2,
            tube: 0.4,
            radial_segments: 32,
            tubular_segments: 16,
          ),
          mesh.color(0x4488ff),
          material.metalness(0.8),
          material.roughness(0.1),
          transform.transform(transform.at(vec3.Vec3(-2.0, 1.5, 0.0))),
        ],
        [],
      ),
      // Cyan cylinder
      tiramisu.mesh(
        "cool-cyl",
        [
          mesh.cylinder(
            radius_top: 0.6,
            radius_bottom: 2.5,
            height: 0.5,
            segments: 32,
          ),
          mesh.color(0x00cccc),
          material.metalness(0.5),
          material.roughness(0.4),
          transform.transform(transform.at(vec3.Vec3(2.0, 1.25, 0.0))),
        ],
        [],
      ),
      // Purple box
      tiramisu.mesh(
        "cool-box",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x8844ff),
          material.metalness(0.7),
          material.roughness(0.2),
          transform.transform(
            transform.at(vec3.Vec3(0.0, 0.8, 1.5))
            |> transform.with_rotation(
              quaternion.from_euler(vec3.Vec3(0.3, 0.7, 0.0)),
            ),
          ),
        ],
        [],
      ),
      // Ground
      tiramisu.mesh(
        "cool-ground",
        [
          mesh.plane(vec2.Vec2(16.0, 16.0)),
          mesh.color(0x112233),
          transform.transform(
            transform.at(vec3.Vec3(0.0, 0.0, 0.0))
            |> transform.with_rotation(
              quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
            ),
          ),
          material.receive_shadow(True),
        ],
        [],
      ),
      tiramisu.light(
        "cool-ambient",
        [
          light.kind(light.Ambient),
          light.color(0x6688cc),
          light.intensity(0.4),
        ],
        [],
      ),
      tiramisu.light(
        "cool-sun",
        [
          light.kind(light.Directional),
          light.color(0xaaccff),
          light.intensity(1.0),
          transform.transform(transform.at(vec3.Vec3(-4.0, 10.0, 6.0))),
          light.cast_shadow(True),
        ],
        [],
      ),
    ],
  )
}

// SCENE C — wireframes --------------------------------------------------------

fn wireframe_scene(model: Model) -> Element(Msg) {
  // Orbit: radius 8, speed 0.3 rad/s, opposite direction
  let cx = maths.sin(model.time *. -0.3) *. 8.0
  let cz = maths.cos(model.time *. -0.3) *. 8.0
  tiramisu.scene(
    "wire",
    [
      attribute.width(480),
      attribute.height(360),
      scene.background_color(0x0a0a0a),
    ],
    [
      tiramisu.camera(
        "wire-cam",
        [
          camera.fov(70.0),
          transform.transform(
            transform.at(vec3.Vec3(cx, 4.0, cz))
            |> transform.with_look_at(vec3f.zero),
          ),
          camera.active(True),
        ],
        [],
      ),
      // Group of wireframe objects
      tiramisu.empty(
        "wire-group",
        [
          transform.transform(transform.at(vec3.Vec3(0.0, 0.0, 0.0))),
        ],
        [
          tiramisu.mesh(
            "wire-box",
            [
              mesh.geometry_box(vec3.Vec3(1.5, 1.5, 1.5)),
              mesh.color(0x00ff88),
              transform.transform(transform.at(vec3.Vec3(-2.0, 1.0, 0.0))),
              material.wireframe(True),
            ],
            [],
          ),
          tiramisu.mesh(
            "wire-sphere",
            [
              mesh.sphere(radius: 1.0, segments: vec2.Vec2(32, 16)),
              mesh.color(0xff0088),
              transform.transform(transform.at(vec3.Vec3(0.0, 1.0, 0.0))),
              material.wireframe(True),
            ],
            [],
          ),
        ],
      ),
      tiramisu.light(
        "wire-ambient",
        [
          light.kind(light.Ambient),
          light.color(0xffffff),
          light.intensity(1.0),
        ],
        [],
      ),
    ],
  )
}
