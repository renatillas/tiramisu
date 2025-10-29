import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import tiramisu
import tiramisu/background
import tiramisu/camera
import tiramisu/debug
import tiramisu/effect
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Id {
  MainCamera
  Ambient
  Directional
  Ground
  High(Int)
  Medium(Int)
  Low(Int)
  Billboard(Int)
  LOD(Int)
}

pub type Model {
  Model(camera_position: vec3.Vec3(Float), show_performance: Bool)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x0a0a1a),
    init: init,
    update: update,
    view: view,
  )
}

fn init(
  _ctx: tiramisu.Context(Id),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  io.println("=== Level of Detail (LOD) Demo ===")
  io.println("")
  io.println("LOD automatically switches mesh detail based on distance:")
  io.println("- Close (0-20 units): High detail (2562 tris)")
  io.println("- Medium (20-50 units): Medium detail (642 tris)")
  io.println("- Far (50-100 units): Low detail (162 tris)")
  io.println("- Very far (100+ units): Billboard (2 tris)")
  io.println("")
  io.println("")
  io.println("Controls:")
  io.println("  W/S - Move camera forward/backward")
  io.println("  P - Toggle performance stats")
  io.println("")
  io.println("Watch triangle count drop as you move away!")
  io.println("")

  #(
    Model(camera_position: vec3.Vec3(0.0, 20.0, 30.0), show_performance: True),
    effect.tick(Tick),
    option.None,
  )
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(Id),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      // Camera movement (W/S)
      let move_speed = 0.5
      let vec3.Vec3(x, y, z) = model.camera_position

      let new_z = case input.is_key_pressed(ctx.input, input.Custom("KeyW")) {
        True -> z -. move_speed
        False -> z
      }

      let new_z = case input.is_key_pressed(ctx.input, input.Custom("KeyS")) {
        True -> new_z +. move_speed
        False -> new_z
      }

      let new_camera_position = vec3.Vec3(x, y, new_z)

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
            <> int.to_string(stats.triangles)
            <> " | Camera Z: "
            <> float.to_string(new_z),
          )
        }
        False -> Nil
      }

      #(
        Model(
          camera_position: new_camera_position,
          show_performance: show_performance,
        ),
        effect.tick(Tick),
        option.None,
      )
    }
  }
}

fn view(model: Model, _ctx: tiramisu.Context(Id)) -> List(scene.Node(Id)) {
  // Camera setup
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 500.0)

  let camera_node =
    scene.camera(
      id: MainCamera,
      camera:,
      transform: transform.at(model.camera_position),
      look_at: option.None,
      active: True,
      viewport: option.None,
      postprocessing: option.None,
    )

  let lights = [
    scene.light(
      id: Ambient,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    ),
    scene.light(
      id: Directional,
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 0.7)
        light
      },
      transform: transform.at(position: vec3.Vec3(50.0, 50.0, 50.0)),
    ),
  ]

  // Create LOD objects at various distances
  // Place objects from -150 to 150 units, every 30 units
  let lod_objects =
    list.range(0, 10)
    |> list.map(fn(i) {
      let fz = int.to_float(i) *. 30.0 -. 150.0

      // Create different detail level meshes
      let high_detail =
        scene.mesh(
          id: High(i),
          geometry: {
            let assert Ok(geometry) =
              geometry.icosahedron(radius: 5.0, detail: 3)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.new()
              |> material.with_color(0x00ff00)
              |> material.with_metalness(0.3)
              |> material.with_roughness(0.6)
              |> material.build()
            material
          },
          transform: transform.identity,
          physics: option.None,
        )

      let medium_detail =
        scene.mesh(
          id: Medium(i),
          geometry: {
            let assert Ok(geometry) =
              geometry.icosahedron(radius: 5.0, detail: 2)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.new()
              |> material.with_color(0xffff00)
              |> material.with_metalness(0.3)
              |> material.with_roughness(0.6)
              |> material.build()
            material
          },
          transform: transform.identity,
          physics: option.None,
        )

      let low_detail =
        scene.mesh(
          id: Low(i),
          geometry: {
            let assert Ok(geometry) =
              geometry.icosahedron(radius: 5.0, detail: 1)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.new()
              |> material.with_color(0xff8800)
              |> material.with_metalness(0.3)
              |> material.with_roughness(0.6)
              |> material.build()
            material
          },
          transform: transform.identity,
          physics: option.None,
        )

      let billboard =
        scene.mesh(
          id: Billboard(i),
          geometry: {
            let assert Ok(geometry) = geometry.plane(width: 8.0, height: 8.0)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.basic(
                color: 0xff0000,
                transparent: False,
                opacity: 1.0,
                map: option.None,
              )
            material
          },
          transform: transform.identity,
          physics: option.None,
        )

      // Create LOD with distance thresholds
      scene.lod(
        id: LOD(i),
        levels: [
          scene.LODLevel(distance: 0.0, node: high_detail),
          // 0-20 units
          scene.LODLevel(distance: 20.0, node: medium_detail),
          // 20-50 units
          scene.LODLevel(distance: 50.0, node: low_detail),
          // 50-100 units
          scene.LODLevel(distance: 100.0, node: billboard),
          // 100+ units
        ],
        transform: transform.at(position: vec3.Vec3(0.0, 0.0, fz)),
      )
    })

  // Add ground grid for reference
  let ground =
    scene.debug_grid(id: Ground, size: 400.0, divisions: 40, color: 0x444444)

  list.flatten([[camera_node], lights, lod_objects, [ground]])
}
