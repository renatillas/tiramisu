import gleam/io
import gleam/list
import gleam/option
import gleam/time/duration
import gleam_community/colour
import gleam_community/maths
import tiramisu
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec2
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
  let assert Ok(Nil) =
    tiramisu.application(init, update, view)
    |> tiramisu.start("body", tiramisu.FullScreen, option.None)
  Nil
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg), option.Option(_)) {
  let model =
    Model(
      rotation: 0.0,
      debug_enabled: True,
      show_performance: True,
      animated_position: vec3.Vec3(0.0, 3.0, 0.0),
      path_points: [],
    )

  #(model, effect.dispatch(Tick), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      // Animate rotation
      let new_rotation = model.rotation +. duration.to_seconds(ctx.delta_time)

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

      #(
        Model(
          rotation: new_rotation,
          debug_enabled: new_debug,
          show_performance: new_perf,
          animated_position: new_position,
          path_points: new_path,
        ),
        effect.dispatch(Tick),
        option.None,
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
      #(
        Model(..model, debug_enabled: !model.debug_enabled),
        effect.none(),
        option.None,
      )
    }

    TogglePerformance -> {
      #(
        Model(..model, show_performance: !model.show_performance),
        effect.none(),
        option.None,
      )
    }
  }
}

fn view(model: Model, _) -> scene.Node {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
  let camera =
    camera
    |> scene.camera(
      id: "main-camera",
      camera: _,
      transform: transform.at(position: vec3.Vec3(0.0, 10.0, 15.0)),
      active: True,
      viewport: option.None,
      postprocessing: option.None,
    )
    |> list.wrap
  let lights = [
    scene.light(
      id: "ambient-light",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.6)
        light
      },
      transform: transform.identity,
    ),
    scene.light(
      id: "directional-light",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 1.0)
        light
      },
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 5.0)),
    ),
  ]

  // Some example objects to debug
  let objects = [
    scene.mesh(
      id: "cube-1",
      geometry: {
        let assert Ok(geometry) = geometry.box(size: vec3f.one |> vec3f.scale(2.0))
        geometry
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(0x4a90e2)
          |> material.with_metalness(0.3)
          |> material.with_roughness(0.7)
          |> material.build()
        material
      },
      transform: transform.at(position: vec3.Vec3(-4.0, 1.0, 0.0))
        |> transform.with_euler_rotation(vec3.Vec3(
          model.rotation,
          model.rotation,
          0.0,
        )),
      physics: option.None,
    ),
    scene.mesh(
      id: "sphere-1",
      geometry: {
        let assert Ok(geometry) =
          geometry.sphere(radius: 1.5, segments: vec2.Vec2(32, 32))
        geometry
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(0xe24a4a)
          |> material.with_metalness(0.5)
          |> material.with_roughness(0.5)
          |> material.build()
        material
      },
      transform: transform.at(position: vec3.Vec3(4.0, 1.5, 0.0)),
      physics: option.None,
    ),
  ]

  // Debug visualizations
  let debug_nodes = case model.debug_enabled {
    False -> []
    True -> {
      // Grid at ground level
      let grid =
        scene.debug_grid("grid", 20.0, 20, colour.white |> colour.to_rgb_hex())

      // Coordinate axes at origin
      let axes_origin = scene.debug_axes("axes-origin", vec3f.zero, 2.0)

      // Wireframe sphere around actual sphere
      let sphere_debug =
        scene.debug_sphere(
          "sphere-debug",
          vec3.Vec3(4.0, 1.5, 0.0),
          2.0,
          colour.red |> colour.to_rgb_hex(),
        )

      // Line between objects
      let connection_line =
        scene.debug_line(
          "connection-line",
          vec3.Vec3(-4.0, 1.0, 0.0),
          vec3.Vec3(4.0, 1.5, 0.0),
          colour.light_blue |> colour.to_rgb_hex(),
        )

      // Animated point following circular path
      let moving_point =
        scene.debug_point(
          "moving-point",
          model.animated_position,
          0.3,
          colour.light_red |> colour.to_rgb_hex(),
        )

      // Axes at animated position
      let axes_moving =
        scene.debug_axes("axes-moving", model.animated_position, 1.0)

      [
        grid,
        axes_origin,
        sphere_debug,
        connection_line,
        moving_point,
        axes_moving,
      ]
    }
  }

  scene.empty(
    id: "scene",
    transform: transform.identity,
    children: list.flatten([camera, lights, objects, debug_nodes]),
  )
}
