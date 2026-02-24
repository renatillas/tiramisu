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
import tiramisu/renderer
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
  let assert Ok(_) = tiramisu.register()

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
      // Renderer 4: Minimal scene (just camera + light + sphere)
      html.div([], [
        html.h2([], [element.text("Scene D — Minimal")]),
        minimal_scene(model),
      ]),
    ]),
  ])
}

// SCENE A — warm colors with multiple objects ---------------------------------

fn warm_scene(model: Model) -> Element(Msg) {
  // Orbit: radius 6, speed 0.5 rad/s
  let cx = maths.sin(model.time *. 0.5) *. 6.0
  let cz = maths.cos(model.time *. 0.5) *. 6.0
  renderer.renderer(
    [
      renderer.width(480),
      renderer.height(360),
      renderer.background("#2d1b00"),
      renderer.scene_id("warm"),
    ],
    [
      tiramisu.camera(
        "warm-cam",
        [
          camera.fov(60.0),
          camera.transform(
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
          mesh.transform(transform.at(vec3.Vec3(-2.0, 0.75, 0.0))),
        ],
        [],
      ),
      // Orange sphere
      tiramisu.mesh(
        "warm-sphere",
        [
          mesh.sphere_simple(1.0),
          mesh.color(0xff8800),
          material.metalness(0.6),
          material.roughness(0.3),
          mesh.transform(transform.at(vec3.Vec3(0.0, 1.0, 0.0))),
        ],
        [],
      ),
      // Yellow cone
      tiramisu.mesh(
        "warm-cone",
        [
          mesh.cone_simple(0.8, 2.0),
          mesh.color(0xffcc00),
          material.metalness(0.4),
          material.roughness(0.5),
          mesh.transform(transform.at(vec3.Vec3(2.0, 1.0, 0.0))),
        ],
        [],
      ),
      // Ground
      tiramisu.mesh(
        "warm-ground",
        [
          mesh.plane(vec2.Vec2(12.0, 12.0)),
          mesh.color(0x3d2b1f),
          mesh.transform(
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
          light.type_(light.Ambient),
          light.color(0xffddaa),
          light.intensity(0.3),
        ],
        [],
      ),
      tiramisu.light(
        "warm-sun",
        [
          light.type_(light.Directional),
          light.color(0xffaa44),
          light.intensity(1.2),
          light.transform(transform.at(vec3.Vec3(3.0, 8.0, 5.0))),
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
  renderer.renderer(
    [
      renderer.width(480),
      renderer.height(360),
      renderer.background("#001122"),
      renderer.scene_id("cool"),
    ],
    [
      tiramisu.camera(
        "cool-cam",
        [
          camera.fov(50.0),
          camera.transform(
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
          mesh.torus_simple(1.2, 0.4),
          mesh.color(0x4488ff),
          material.metalness(0.8),
          material.roughness(0.1),
          mesh.transform(transform.at(vec3.Vec3(-2.0, 1.5, 0.0))),
        ],
        [],
      ),
      // Cyan cylinder
      tiramisu.mesh(
        "cool-cyl",
        [
          mesh.cylinder_simple(0.6, 2.5),
          mesh.color(0x00cccc),
          material.metalness(0.5),
          material.roughness(0.4),
          mesh.transform(transform.at(vec3.Vec3(2.0, 1.25, 0.0))),
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
          mesh.transform(
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
          mesh.transform(
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
          light.type_(light.Ambient),
          light.color(0x6688cc),
          light.intensity(0.4),
        ],
        [],
      ),
      tiramisu.light(
        "cool-sun",
        [
          light.type_(light.Directional),
          light.color(0xaaccff),
          light.intensity(1.0),
          light.transform(transform.at(vec3.Vec3(-4.0, 10.0, 6.0))),
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
  renderer.renderer(
    [
      renderer.width(480),
      renderer.height(360),
      renderer.background("#0a0a0a"),
      renderer.scene_id("wire"),
    ],
    [
      tiramisu.camera(
        "wire-cam",
        [
          camera.fov(70.0),
          camera.transform(
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
          camera.transform(transform.at(vec3.Vec3(0.0, 0.0, 0.0))),
        ],
        [
          tiramisu.mesh(
            "wire-box",
            [
              mesh.geometry_box(vec3.Vec3(1.5, 1.5, 1.5)),
              mesh.color(0x00ff88),
              mesh.transform(transform.at(vec3.Vec3(-2.0, 1.0, 0.0))),
              material.wireframe(True),
            ],
            [],
          ),
          tiramisu.mesh(
            "wire-sphere",
            [
              mesh.sphere_simple(1.0),
              mesh.color(0xff0088),
              mesh.transform(transform.at(vec3.Vec3(0.0, 1.0, 0.0))),
              material.wireframe(True),
            ],
            [],
          ),
          tiramisu.mesh(
            "wire-torus",
            [
              mesh.torus_simple(0.8, 0.3),
              mesh.color(0x8800ff),
              material.wireframe(True),
              mesh.transform(transform.at(vec3.Vec3(2.0, 1.0, 0.0))),
            ],
            [],
          ),
        ],
      ),
      tiramisu.light(
        "wire-ambient",
        [
          light.type_(light.Ambient),
          light.color(0xffffff),
          light.intensity(1.0),
        ],
        [],
      ),
    ],
  )
}

// SCENE D — minimal -----------------------------------------------------------

fn minimal_scene(model: Model) -> Element(Msg) {
  let cx = maths.sin(model.time *. -0.3) *. 8.0
  let cz = maths.cos(model.time *. -0.3) *. 8.0
  renderer.renderer(
    [
      renderer.width(480),
      renderer.height(360),
      renderer.background("#1a0033"),
      renderer.scene_id("mini"),
    ],
    [
      tiramisu.camera(
        "mini-cam",
        [
          camera.fov(45.0),
          camera.transform(
            transform.at(vec3.Vec3(cx, 0.0, cz))
            |> transform.with_look_at(vec3f.zero),
          ),
          camera.active(True),
        ],
        [],
      ),
      // Single sphere
      tiramisu.mesh(
        "mini-sphere",
        [
          mesh.sphere_simple(1.5),
          mesh.color(0xee66ff),
          material.metalness(0.9),
          material.roughness(0.05),
        ],
        [],
      ),
      tiramisu.light(
        "mini-ambient",
        [
          light.type_(light.Ambient),
          light.color(0x332244),
          light.intensity(0.5),
        ],
        [],
      ),
      tiramisu.light(
        "mini-point",
        [
          light.type_(light.Point),
          light.color(0xffffff),
          light.intensity(2.0),
          light.transform(transform.at(vec3.Vec3(3.0, 3.0, 3.0))),
        ],
        [],
      ),
    ],
  )
}
