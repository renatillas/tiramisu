/// Stress Test: 1000 Nodes
///
/// Performance benchmark with 1000 animated cubes
///
/// Controls:
/// - SPACE: Toggle animation (all nodes update vs static scene)
/// - P: Toggle performance stats in console
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import tiramisu/camera
import tiramisu/debug
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/input
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub type Model {
  Model(
    animate: Bool,
    time: Float,
    show_performance: Bool,
    cached_instances: List(scene.InstanceTransform),
  )
}

pub type Msg {
  Tick
  ToggleAnimation
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

fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  io.println("=== Stress Test: 1000 Nodes ===")
  io.println("Controls:")
  io.println("  SPACE - Toggle animation (test worst/best case)")
  io.println("  P     - Toggle performance stats")
  io.println("")

  // Pre-compute initial instances (will be cached when animation is off)
  let initial_instances = compute_instances(0.0)

  #(
    Model(
      animate: True,
      time: 0.0,
      show_performance: True,
      cached_instances: initial_instances,
    ),
    effect.tick(Tick),
  )
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      // Check for key presses
      let space_pressed = input.is_key_just_pressed(ctx.input, input.Space)
      let p_pressed = input.is_key_just_pressed(ctx.input, input.Custom("KeyP"))

      // Toggle animation state
      let new_animate = case space_pressed {
        True -> {
          io.println(
            "Animation: "
            <> case !model.animate {
              True -> "ON (all 1000 nodes changing)"
              False -> "OFF (static scene)"
            },
          )
          !model.animate
        }
        False -> model.animate
      }

      // Toggle performance stats
      let new_show_perf = case p_pressed {
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

      // Update time and cached instances
      let #(new_time, new_cached_instances) = case new_animate {
        True -> #(
          model.time +. ctx.delta_time,
          compute_instances(model.time +. ctx.delta_time),
        )
        False -> {
          // When animation is OFF, freeze time and reuse cached instances
          // This makes prev == curr in scene diff, avoiding expensive list comparison
          #(model.time, model.cached_instances)
        }
      }

      // Show performance stats every frame if enabled
      case new_show_perf {
        True -> {
          let stats = debug.get_performance_stats()
          io.println(
            "FPS: "
            <> float.to_string(stats.fps)
            <> " | Frame: "
            <> float.to_string(stats.frame_time)
            <> "ms | Draw Calls: "
            <> int.to_string(stats.draw_calls)
            <> " | Tris: "
            <> int.to_string(stats.triangles),
          )
        }
        False -> Nil
      }

      #(
        Model(
          animate: new_animate,
          time: new_time,
          show_performance: new_show_perf,
          cached_instances: new_cached_instances,
        ),
        effect.tick(Tick),
      )
    }

    ToggleAnimation -> {
      #(Model(..model, animate: !model.animate), effect.tick(Tick))
    }
  }
}

// Compute instances list for given time - extracted to be reusable
fn compute_instances(time: Float) -> List(scene.InstanceTransform) {
  list.range(0, 20_000)
  |> list.map(fn(i) {
    // Calculate grid position
    let x = i % 10
    let y = { i / 10 } % 10
    let z = i / 100

    let fx = int.to_float(x) *. 3.0 -. 13.5
    let fy = int.to_float(y) *. 3.0 -. 13.5
    let fz = int.to_float(z) *. 3.0 -. 13.5

    // Base rotation offset per cube
    let base_rotation = int.to_float(i) *. 0.01

    // Apply time-based rotation
    let rotation = time +. base_rotation

    scene.instance(
      position: vec3.Vec3(fx, fy, fz),
      rotation: vec3.Vec3(rotation *. 0.5, rotation, rotation *. 0.3),
      scale: vec3.Vec3(1.0, 1.0, 1.0),
    )
  })
}

fn view(model: Model) -> List(scene.SceneNode) {
  // Camera setup
  let assert Ok(cam) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 1280.0 /. 720.0,
      near: 0.1,
      far: 1000.0,
    )

  let cam =
    cam
    |> camera.set_position(vec3.Vec3(0.0, 0.0, 60.0))
    |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

  let camera_node =
    scene.Camera(
      id: "main_camera",
      camera_type: cam,
      transform: transform.identity(),
      active: True,
    )

  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.3),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.5),
      transform: transform.Transform(
        position: vec3.Vec3(50.0, 50.0, 50.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  // Use cached instances - when animation is OFF, this is the same list reference
  // making scene diff fast-path (prev == curr) succeed
  let instances = model.cached_instances

  let instanced_cubes =
    scene.InstancedMesh(
      id: "cubes_instanced",
      geometry: scene.BoxGeometry(0.8, 0.8, 0.8),
      material: scene.StandardMaterial(
        color: 0x4ecdc4,
        metalness: 0.2,
        roughness: 0.6,
        map: option.None,
        normal_map: option.None,
      ),
      instances: instances,
    )

  list.flatten([[camera_node], lights, [instanced_cubes]])
}
