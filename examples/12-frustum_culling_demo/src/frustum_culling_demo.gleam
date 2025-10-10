import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam_community/maths
import tiramisu
import tiramisu/camera
import tiramisu/debug
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Model {
  Model(show_performance: Bool, time: Float)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  tiramisu.run(
    dimensions: option.None,
    background: 0x000510,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg)) {
  io.println("=== Frustum Culling Demo ===")
  io.println("")
  io.println("This demo shows automatic frustum culling:")
  io.println("- 1000 cubes spread across a large grid")
  io.println("- Camera rotates automatically")
  io.println("- Watch draw calls change as cubes move in/out of view!")
  io.println("")
  io.println("Controls:")
  io.println("  P - Toggle performance stats")
  io.println("")

  #(Model(show_performance: True, time: 0.0), effect.tick(Tick))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      // Toggle performance stats
      let p_pressed = input.is_key_just_pressed(ctx.input, input.Custom("KeyP"))
      let show_performance = case p_pressed {
        True -> {
          io.println(
            "Performance stats: "
            <> case !model.show_performance {
              True -> "ON"
              False -> "OFF"
            },
          )
          !model.show_performance
        }
        False -> model.show_performance
      }

      // Show performance stats
      case show_performance {
        True -> {
          let stats = debug.get_performance_stats()
          io.println(
            "FPS: "
            <> float.to_string(stats.fps)
            <> " | Frame: "
            <> float.to_string(stats.frame_time)
            <> "ms"
            <> " | Draw Calls: "
            <> int.to_string(stats.draw_calls)
            <> " | Tris: "
            <> int.to_string(stats.triangles),
          )
        }
        False -> Nil
      }

      #(
        Model(
          show_performance: show_performance,
          time: model.time +. ctx.delta_time,
        ),
        effect.tick(Tick),
      )
    }
  }
}

fn view(model: Model) -> List(scene.Node) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 200.0)
    |> result.map(fn(cam) {
      cam
      |> scene.Camera(
        id: "main-camera",
        camera: _,
        transform: transform.at(position: vec3.Vec3(0.0, 20.0, 80.0)),
        look_at: option.None,
        active: True,
        viewport: option.None,
      )
      |> list.wrap
    })
  let lights = [
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.4)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 0.6)
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(50.0, 50.0, 50.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  // Create a large grid of cubes spread across 100x100 units
  // Grid: 10x10x10 = 1000 cubes
  // Spacing: 10 units apart
  // Cubes orbit around center to demonstrate frustum culling
  let cubes =
    list.range(0, 999)
    |> list.map(fn(i) {
      let x = i % 10
      let y = { i / 10 } % 10
      let z = i / 100

      // Base position in grid
      let fx = int.to_float(x) *. 10.0 -. 45.0
      let fy = int.to_float(y) *. 10.0 -. 45.0
      let fz = int.to_float(z) *. 10.0 -. 45.0

      // Orbit around center - cubes move in/out of frustum
      let orbit_angle = model.time *. 0.3 +. int.to_float(i) *. 0.01
      let orbit_radius = 60.0
      let orbit_x = fx +. orbit_radius *. maths.cos(orbit_angle)
      let orbit_z = fz +. orbit_radius *. maths.sin(orbit_angle)

      // Slight rotation animation
      let rotation = model.time *. 0.5

      scene.Mesh(
        id: "cube_" <> int.to_string(i),
        geometry: {
          let assert Ok(geometry) =
            geometry.box(width: 2.0, height: 2.0, depth: 2.0)
          geometry
        },
        material: {
          let assert Ok(material) =
            material.standard(
              color: 0x4a9eff,
              metalness: 0.3,
              roughness: 0.7,
              map: option.None,
              normal_map: option.None,
              ambient_oclusion_map: option.None,
              roughness_map: option.None,
              metalness_map: option.None,
            )
          material
        },
        transform: transform.Transform(
          position: vec3.Vec3(orbit_x, fy, orbit_z),
          rotation: vec3.Vec3(rotation, rotation *. 0.7, rotation *. 0.3),
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
        physics: option.None,
      )
    })

  // Add a ground plane for reference
  let ground =
    scene.Mesh(
      id: "ground",
      geometry: {
        let assert Ok(geometry) = geometry.plane(width: 200.0, height: 200.0)
        geometry
      },
      material: {
        let assert Ok(material) =
          material.standard(
            color: 0x1a1a2e,
            metalness: 0.0,
            roughness: 1.0,
            map: option.None,
            normal_map: option.None,
            ambient_oclusion_map: option.None,
            roughness_map: option.None,
            metalness_map: option.None,
          )
        material
      },
      transform: transform.Transform(
        position: vec3.Vec3(0.0, -50.0, 0.0),
        rotation: vec3.Vec3(-1.5708, 0.0, 0.0),
        // Rotate 90° to make horizontal
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    )
    |> list.wrap

  list.flatten([camera, lights, cubes, ground])
}
