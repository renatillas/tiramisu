import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam_community/maths
import tiramisu
import tiramisu/camera
import tiramisu/debug
import tiramisu/effect
import tiramisu/input
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type CameraView {
  TopDown
  Side
  FirstPerson
  Orbiting
}

pub type Model {
  Model(current_view: CameraView, rotation: Float, show_performance: Bool)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  tiramisu.run(
    width: 1280,
    height: 720,
    background: 0x1a1a2e,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg)) {
  io.println("=== Multi-Camera Demo ===")
  io.println("")
  io.println("This demo shows multiple cameras viewing the same scene.")
  io.println("Switch between different camera perspectives in real-time!")
  io.println("")
  io.println("Features:")
  io.println("- Picture-in-picture overlay (bottom-right, always visible)")
  io.println("- Dynamic camera switching")
  io.println("")
  io.println("Controls:")
  io.println("  1 - Top-down view (bird's eye)")
  io.println("  2 - Side view (profile)")
  io.println("  3 - First-person view (ground level)")
  io.println("  4 - Orbiting view (rotating around scene)")
  io.println("  P - Toggle performance stats")
  io.println("")

  #(
    Model(current_view: TopDown, rotation: 0.0, show_performance: False),
    effect.tick(Tick),
  )
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick -> {
      // Camera switching
      let new_view = model.current_view

      let new_view = case
        input.is_key_just_pressed(ctx.input, input.Custom("Digit1"))
      {
        True -> {
          TopDown
        }
        False -> new_view
      }

      let new_view = case
        input.is_key_just_pressed(ctx.input, input.Custom("Digit2"))
      {
        True -> Side
        False -> new_view
      }

      let new_view = case
        input.is_key_just_pressed(ctx.input, input.Custom("Digit3"))
      {
        True -> {
          io.println("Switched to: First-person camera")
          FirstPerson
        }
        False -> new_view
      }

      let new_view = case
        input.is_key_just_pressed(ctx.input, input.Custom("Digit4"))
      {
        True -> {
          io.println("Switched to: Orbiting camera")
          Orbiting
        }
        False -> new_view
      }

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
            <> " | Camera: "
            <> camera_view_to_string(new_view),
          )
        }
        False -> Nil
      }

      // Update rotation for orbiting camera
      let new_rotation = model.rotation +. ctx.delta_time

      #(
        Model(
          current_view: new_view,
          rotation: new_rotation,
          show_performance: show_performance,
        ),
        effect.tick(Tick),
      )
    }
  }
}

fn camera_view_to_string(view: CameraView) -> String {
  case view {
    TopDown -> "Top-down"
    Side -> "Side"
    FirstPerson -> "First-person"
    Orbiting -> "Orbiting"
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  // Create multiple cameras with different perspectives

  // 1. Top-down camera (bird's eye view)
  let assert Ok(cam_topdown) =
    camera.perspective(
      field_of_view: 60.0,
      aspect: 1280.0 /. 720.0,
      near: 0.1,
      far: 200.0,
    )

  let cam_topdown =
    cam_topdown
    |> camera.set_position(vec3.Vec3(0.0, 80.0, 0.1))
    |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

  let camera_topdown =
    scene.Camera(
      id: "camera_topdown",
      camera: cam_topdown,
      transform: transform.identity,
      active: model.current_view == TopDown,
      viewport: option.None,
    )

  // 2. Side camera (profile view from the side)
  let assert Ok(cam_side) =
    camera.perspective(
      field_of_view: 60.0,
      aspect: 1280.0 /. 720.0,
      near: 0.1,
      far: 200.0,
    )

  let cam_side =
    cam_side
    |> camera.set_position(vec3.Vec3(60.0, 10.0, 0.0))
    |> camera.look(at: vec3.Vec3(0.0, 20.0, 0.0))

  let camera_side =
    scene.Camera(
      id: "camera_side",
      camera: cam_side,
      transform: transform.identity,
      active: model.current_view == Side,
      viewport: option.None,
    )

  // 3. First-person camera (ground level, looking at rotating cube)
  let assert Ok(cam_firstperson) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 1280.0 /. 720.0,
      near: 0.1,
      far: 200.0,
    )

  let cam_firstperson =
    cam_firstperson
    |> camera.set_position(vec3.Vec3(-18.0, 3.0, 18.0))
    |> camera.look(at: vec3.Vec3(0.0, 6.0, 0.0))

  let camera_firstperson =
    scene.Camera(
      id: "camera_firstperson",
      camera: cam_firstperson,
      transform: transform.identity,
      active: model.current_view == FirstPerson,
      viewport: option.None,
    )

  // 4. Orbiting camera (rotating around scene at 45 degree angle)
  let orbit_radius = 40.0
  let orbit_x = maths.cos(model.rotation) *. orbit_radius
  let orbit_z = maths.sin(model.rotation) *. orbit_radius

  let assert Ok(cam_orbiting) =
    camera.perspective(
      field_of_view: 65.0,
      aspect: 1280.0 /. 720.0,
      near: 0.1,
      far: 200.0,
    )

  let cam_orbiting =
    cam_orbiting
    |> camera.set_position(vec3.Vec3(orbit_x, 25.0, orbit_z))
    |> camera.look(at: vec3.Vec3(0.0, 6.0, 0.0))

  let camera_orbiting =
    scene.Camera(
      id: "camera_orbiting",
      camera: cam_orbiting,
      transform: transform.identity,
      active: model.current_view == Orbiting,
      viewport: option.None,
    )

  let assert Ok(cam_overlay) =
    camera.perspective(field_of_view: 50.0, aspect: 1.0, near: 0.1, far: 200.0)

  let cam_overlay =
    cam_overlay
    |> camera.set_position(vec3.Vec3(0.0, 80.0, 0.1))
    |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

  let camera_overlay =
    scene.Camera(
      id: "camera_overlay",
      camera: cam_overlay,
      transform: transform.identity,
      active: False,
      // Overlay camera is never the "active" main camera
      // Bottom-right corner: 250x250 pixels, 30px from edges
      viewport: option.Some(#(1280 - 250 - 30, 30, 250, 250)),
    )

  // Lights
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.4),
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.8),
      transform: transform.Transform(
        position: vec3.Vec3(30.0, 40.0, 30.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  // Create a scene with various objects to view from different angles

  // Central rotating cube
  let rotating_cube =
    scene.Mesh(
      id: "rotating_cube",
      geometry: scene.BoxGeometry(4.0, 4.0, 4.0),
      material: scene.StandardMaterial(
        color: 0xff6b6b,
        metalness: 0.3,
        roughness: 0.4,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 6.0, 0.0),
        rotation: vec3.Vec3(model.rotation, model.rotation *. 0.7, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    )

  // Ground plane
  let ground =
    scene.Mesh(
      id: "ground",
      geometry: scene.PlaneGeometry(100.0, 100.0),
      material: scene.StandardMaterial(
        color: 0x2d3561,
        metalness: 0.0,
        roughness: 0.8,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 0.0, 0.0),
        rotation: vec3.Vec3(-1.5708, 0.0, 0.0),
        // -90 degrees to make it horizontal
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    )

  // Ring of spheres around the center
  let spheres =
    list.range(0, 7)
    |> list.map(fn(i) {
      let angle = int.to_float(i) *. 0.785398
      // 45 degrees
      let radius = 15.0
      let x = maths.cos(angle) *. radius
      let z = maths.sin(angle) *. radius

      scene.Mesh(
        id: "sphere_" <> int.to_string(i),
        geometry: scene.SphereGeometry(2.0, 16, 16),
        material: scene.StandardMaterial(
          color: 0x4ecdc4,
          metalness: 0.5,
          roughness: 0.3,
          map: option.None,
          normal_map: option.None,
        ),
        transform: transform.Transform(
          position: vec3.Vec3(x, 3.0, z),
          rotation: vec3.Vec3(0.0, 0.0, 0.0),
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
        physics: option.None,
      )
    })

  // Tall pillars at corners
  let pillars = [
    scene.Mesh(
      id: "pillar_1",
      geometry: scene.CylinderGeometry(1.0, 1.0, 12.0, 8),
      material: scene.StandardMaterial(
        color: 0xf9ca24,
        metalness: 0.2,
        roughness: 0.6,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(25.0, 6.0, 25.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "pillar_2",
      geometry: scene.CylinderGeometry(1.0, 1.0, 12.0, 8),
      material: scene.StandardMaterial(
        color: 0xf9ca24,
        metalness: 0.2,
        roughness: 0.6,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(-25.0, 6.0, 25.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "pillar_3",
      geometry: scene.CylinderGeometry(1.0, 1.0, 12.0, 8),
      material: scene.StandardMaterial(
        color: 0xf9ca24,
        metalness: 0.2,
        roughness: 0.6,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(25.0, 6.0, -25.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "pillar_4",
      geometry: scene.CylinderGeometry(1.0, 1.0, 12.0, 8),
      material: scene.StandardMaterial(
        color: 0xf9ca24,
        metalness: 0.2,
        roughness: 0.6,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(-25.0, 6.0, -25.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]

  // Grid for reference
  let grid =
    scene.DebugGrid(id: "grid", size: 100.0, divisions: 20, color: 0x444466)

  // Combine all cameras and scene objects
  list.flatten([
    [
      camera_topdown,
      camera_side,
      camera_firstperson,
      camera_orbiting,
      camera_overlay,
    ],
    lights,
    [rotating_cube, ground, grid],
    spheres,
    pillars,
  ])
}
