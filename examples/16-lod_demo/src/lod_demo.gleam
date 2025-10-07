import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import tiramisu/camera
import tiramisu/debug
import tiramisu/effect
import tiramisu/game.{type GameContext}
import tiramisu/input
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub type Model {
  Model(
    camera_position: vec3.Vec3,
    camera_target: vec3.Vec3,
    show_performance: Bool,
  )
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  game.run(
    width: 1280,
    height: 720,
    background: 0x0a0a1a,
    camera: option.None,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: GameContext) -> #(Model, effect.Effect(Msg)) {
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
    Model(
      camera_position: vec3.Vec3(0.0, 20.0, 30.0),
      camera_target: vec3.Vec3(0.0, 0.0, 0.0),
      show_performance: True,
    ),
    effect.tick(Tick),
  )
}

fn update(
  model: Model,
  msg: Msg,
  ctx: GameContext,
) -> #(Model, effect.Effect(Msg)) {
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
          camera_target: model.camera_target,
          show_performance: show_performance,
        ),
        effect.tick(Tick),
      )
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  // Camera setup
  let assert Ok(cam) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 1280.0 /. 720.0,
      near: 0.1,
      far: 500.0,
    )

  let cam =
    cam
    |> camera.set_position(model.camera_position)
    |> camera.look(at: model.camera_target)

  let camera_node =
    scene.Camera(
      id: "main_camera",
      camera_type: cam,
      transform: transform.identity(),
      active: True,
      viewport: option.None,
    )

  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.5),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.7),
      transform: transform.Transform(
        position: vec3.Vec3(50.0, 50.0, 50.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
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
        scene.Mesh(
          id: "high_" <> int.to_string(i),
          geometry: scene.IcosahedronGeometry(5.0, 3),
          material: scene.StandardMaterial(
            color: 0x00ff00,
            // Green for high detail
            metalness: 0.3,
            roughness: 0.6,
            map: option.None,
            normal_map: option.None,
          ),
          transform: transform.identity(),
          physics: option.None,
        )

      let medium_detail =
        scene.Mesh(
          id: "medium_" <> int.to_string(i),
          geometry: scene.IcosahedronGeometry(5.0, 2),
          material: scene.StandardMaterial(
            color: 0xffff00,
            // Yellow for medium detail
            metalness: 0.3,
            roughness: 0.6,
            map: option.None,
            normal_map: option.None,
          ),
          transform: transform.identity(),
          physics: option.None,
        )

      let low_detail =
        scene.Mesh(
          id: "low_" <> int.to_string(i),
          geometry: scene.IcosahedronGeometry(5.0, 1),
          material: scene.StandardMaterial(
            color: 0xff8800,
            // Orange for low detail
            metalness: 0.3,
            roughness: 0.6,
            map: option.None,
            normal_map: option.None,
          ),
          transform: transform.identity(),
          physics: option.None,
        )

      let billboard =
        scene.Mesh(
          id: "billboard_" <> int.to_string(i),
          geometry: scene.PlaneGeometry(8.0, 8.0),
          material: scene.BasicMaterial(
            color: 0xff0000,
            // Red for billboard
            transparent: False,
            opacity: 1.0,
            map: option.None,
          ),
          transform: transform.identity(),
          physics: option.None,
        )

      // Create LOD with distance thresholds
      scene.LOD(
        id: "lod_" <> int.to_string(i),
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
        transform: transform.Transform(
          position: vec3.Vec3(0.0, 0.0, fz),
          rotation: vec3.Vec3(0.0, 0.0, 0.0),
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
      )
    })

  // Add ground grid for reference
  let ground =
    scene.DebugGrid(id: "ground", size: 400.0, divisions: 40, color: 0x444444)

  list.flatten([[camera_node], lights, lod_objects, [ground]])
}
