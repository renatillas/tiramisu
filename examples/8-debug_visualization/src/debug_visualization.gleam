import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam_community/maths
import tiramisu
import tiramisu/camera
import tiramisu/debug
import tiramisu/effect.{type Effect}
import tiramisu/input
import tiramisu/scene
import tiramisu/transform
import vec/vec3
import vec/vec3f

pub type Model {
  Model(
    rotation: Float,
    debug_enabled: Bool,
    show_performance: Bool,
    animated_position: vec3.Vec3(Float),
    path_points: List(vec3.Vec3(Float)),
  )
}

pub type Msg {
  Tick
  ToggleDebug
  TogglePerformance
}

pub fn main() -> Nil {
  tiramisu.run(
    width: 1200,
    height: 800,
    background: 0x0a0a1a,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg)) {
  io.println("Debug Visualization Example")
  io.println("Controls:")
  io.println("  D - Toggle debug visualizations")
  io.println("  P - Toggle performance stats (console)")

  let model =
    Model(
      rotation: 0.0,
      debug_enabled: True,
      show_performance: True,
      animated_position: vec3.Vec3(0.0, 3.0, 0.0),
      path_points: [],
    )

  #(model, effect.tick(Tick))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      // Animate rotation
      let new_rotation = model.rotation +. ctx.delta_time *. 0.5

      // Animate a moving point in a circle
      let time = new_rotation *. 2.0
      let radius = 3.0
      let x = maths.cos(time)
      let z = maths.sin(time)
      let new_position = vec3.Vec3(x *. radius, 3.0, z *. radius)

      // Build path by recording positions
      let new_path = case list.length(model.path_points) > 50 {
        True -> [new_position, ..list.take(model.path_points, 49)]
        False -> [new_position, ..model.path_points]
      }

      // Check for input
      let debug_toggled =
        input.is_key_just_pressed(ctx.input, input.Custom("KeyD"))
      let perf_toggled =
        input.is_key_just_pressed(ctx.input, input.Custom("KeyP"))

      let new_debug = case debug_toggled {
        True -> !model.debug_enabled
        False -> model.debug_enabled
      }

      let new_perf = case perf_toggled {
        True -> !model.show_performance
        False -> model.show_performance
      }

      // Show performance stats
      case new_perf {
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
          rotation: new_rotation,
          debug_enabled: new_debug,
          show_performance: new_perf,
          animated_position: new_position,
          path_points: new_path,
        ),
        effect.tick(Tick),
      )
    }

    ToggleDebug -> {
      io.println(
        "Debug: "
        <> case !model.debug_enabled {
          True -> "ON"
          False -> "OFF"
        },
      )
      #(Model(..model, debug_enabled: !model.debug_enabled), effect.none())
    }

    TogglePerformance -> {
      #(
        Model(..model, show_performance: !model.show_performance),
        effect.none(),
      )
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let assert Ok(camera) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 1200.0 /. 800.0,
      near: 0.1,
      far: 1000.0,
    )
  let camera =
    camera
    |> scene.Camera(
      id: "main_camera",
      camera: _,
      transform: transform.at(position: vec3.Vec3(0.0, 10.0, 15.0)),
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      active: True,
      viewport: option.None,
    )
    |> list.wrap
  let lights = [
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) =
          scene.ambient_light(color: 0xffffff, intensity: 0.6)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light: {
        let assert Ok(light) =
          scene.directional_light(color: 0xffffff, intensity: 1.0)
        light
      },
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 5.0)),
    ),
  ]

  // Some example objects to debug
  let objects = [
    scene.Mesh(
      id: "cube1",
      geometry: {
        let assert Ok(geometry) = scene.box(width: 2.0, height: 2.0, depth: 2.0)
        geometry
      },
      material: {
        let assert Ok(material) =
          scene.standard_material(
            color: 0x4a90e2,
            metalness: 0.3,
            roughness: 0.7,
            map: option.None,
            normal_map: option.None,
            ao_map: option.None,
            roughness_map: option.None,
            metalness_map: option.None,
          )
        material
      },
      transform: transform.Transform(
        position: vec3.Vec3(-4.0, 1.0, 0.0),
        rotation: vec3.Vec3(model.rotation, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "sphere1",
      geometry: {
        let assert Ok(geometry) =
          scene.sphere(radius: 1.5, width_segments: 32, height_segments: 32)
        geometry
      },
      material: {
        let assert Ok(material) =
          scene.standard_material(
            color: 0xe24a4a,
            metalness: 0.5,
            roughness: 0.5,
            map: option.None,
            normal_map: option.None,
            ao_map: option.None,
            roughness_map: option.None,
            metalness_map: option.None,
          )
        material
      },
      transform: transform.Transform(
        position: vec3.Vec3(4.0, 1.5, 0.0),
        rotation: vec3f.zero,
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]

  // Debug visualizations
  let debug_nodes = case model.debug_enabled {
    False -> []
    True -> {
      // Grid at ground level
      let grid = debug.grid("grid", 20.0, 20, debug.color_white)

      // Coordinate axes at origin
      let axes_origin = debug.axes("axes_origin", vec3f.zero, 2.0)

      // Bounding box around cube
      let cube_bbox =
        debug.bounding_box(
          "cube_bbox",
          vec3.Vec3(-5.0, 0.0, -1.0),
          vec3.Vec3(-3.0, 2.0, 1.0),
          debug.color_green,
        )

      // Wireframe sphere around actual sphere
      let sphere_debug =
        debug.sphere(
          "sphere_debug",
          vec3.Vec3(4.0, 1.5, 0.0),
          2.0,
          debug.color_red,
        )

      // Line between objects
      let connection_line =
        debug.line(
          "connection",
          vec3.Vec3(-4.0, 1.0, 0.0),
          vec3.Vec3(4.0, 1.5, 0.0),
          debug.color_cyan,
        )

      // Ray from origin pointing up
      let ray_up =
        debug.ray(
          "ray_up",
          vec3f.zero,
          vec3.Vec3(0.0, 1.0, 0.0),
          5.0,
          debug.color_yellow,
        )

      // Animated point following circular path
      let moving_point =
        debug.point(
          "moving_point",
          model.animated_position,
          0.3,
          debug.color_magenta,
        )

      // Cross marker at moving point
      let cross_marker =
        debug.cross("cross", model.animated_position, 0.8, debug.color_orange)

      // Axes at animated position
      let axes_moving = debug.axes("axes_moving", model.animated_position, 1.0)

      // Path showing trail
      let trail = debug.path("trail", model.path_points, debug.color_blue)

      // Corner markers
      let corners = [
        debug.point(
          "corner1",
          vec3.Vec3(8.0, 0.0, 8.0),
          0.2,
          debug.color_purple,
        ),
        debug.point(
          "corner2",
          vec3.Vec3(-8.0, 0.0, 8.0),
          0.2,
          debug.color_purple,
        ),
        debug.point(
          "corner3",
          vec3.Vec3(8.0, 0.0, -8.0),
          0.2,
          debug.color_purple,
        ),
        debug.point(
          "corner4",
          vec3.Vec3(-8.0, 0.0, -8.0),
          0.2,
          debug.color_purple,
        ),
      ]

      // Box from transform
      let transform_box =
        transform.Transform(
          position: vec3.Vec3(0.0, 5.0, 0.0),
          rotation: vec3f.zero,
          scale: vec3.Vec3(3.0, 1.0, 3.0),
        )
      let box_from_transform =
        debug.box_from_transform(
          "transform_box",
          transform_box,
          debug.color_yellow,
        )

      list.flatten([
        [grid, axes_origin, cube_bbox, sphere_debug, connection_line],
        [ray_up, moving_point, axes_moving, box_from_transform],
        cross_marker,
        trail,
        corners,
      ])
    }
  }

  list.flatten([camera, lights, objects, debug_nodes])
}
