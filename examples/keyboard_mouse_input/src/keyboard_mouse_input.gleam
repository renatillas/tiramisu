/// Example demonstrating keyboard and mouse input
///
/// Controls:
/// - WASD: Move the cube
/// - Mouse: Look around (moves cube based on mouse position)
/// - Left Click: Change cube color to red
/// - Right Click: Change cube color to blue
/// - Mouse Wheel: Scale the cube up/down
import gleam/option
import gleam/time/duration
import tiramisu
import tiramisu/camera
import tiramisu/effect
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
    position: vec3.Vec3(Float),
    rotation: vec3.Vec3(Float),
    scale: Float,
    color: Int,
  )
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  let assert Ok(_) =
    tiramisu.application(init, update, view)
    |> tiramisu.start("body", tiramisu.FullScreen, option.None)
  Nil
}

fn init(
  _ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  let model =
    Model(
      position: vec3.Vec3(0.0, 0.0, -5.0),
      rotation: vec3.Vec3(0.0, 0.0, 0.0),
      scale: 1.0,
      color: 0x4ecdc4,
    )
  #(model, effect.dispatch(Tick), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      // Keyboard movement (WASD)
      let move_speed = 5.0
      let dx = case
        input.is_key_pressed(ctx.input, input.KeyD),
        input.is_key_pressed(ctx.input, input.KeyA)
      {
        True, False -> move_speed *. duration.to_seconds(ctx.delta_time)
        False, True -> 0.0 -. move_speed *. duration.to_seconds(ctx.delta_time)
        _, _ -> 0.0
      }

      let dy = case
        input.is_key_pressed(ctx.input, input.KeyW),
        input.is_key_pressed(ctx.input, input.KeyS)
      {
        True, False -> move_speed *. duration.to_seconds(ctx.delta_time)
        False, True -> 0.0 -. move_speed *. duration.to_seconds(ctx.delta_time)
        _, _ -> 0.0
      }

      let new_position =
        vec3.Vec3(
          model.position.x +. dx,
          model.position.y +. dy,
          model.position.z,
        )

      // Mouse position affects rotation
      let vec2.Vec2(mouse_x, mouse_y) = input.mouse_position(ctx.input)
      let rotation_y = { mouse_x -. 400.0 } /. 100.0
      let rotation_x = { mouse_y -. 300.0 } /. 100.0

      // Mouse buttons change color
      let color = case
        input.is_left_button_pressed(ctx.input),
        input.is_right_button_pressed(ctx.input)
      {
        True, _ -> 0xff0000
        _, True -> 0x0000ff
        _, _ -> 0x4ecdc4
      }

      // Mouse wheel changes scale
      let wheel = input.mouse_wheel_delta(ctx.input)
      let scale_change = wheel /. 1000.0
      let new_scale = case model.scale -. scale_change {
        s if s >. 0.1 && s <. 3.0 -> s
        _ -> model.scale
      }

      let new_model =
        Model(
          position: new_position,
          rotation: vec3.Vec3(rotation_x, rotation_y, 0.0),
          scale: new_scale,
          color: color,
        )

      #(new_model, effect.dispatch(Tick), option.None)
    }
  }
}

fn view(model: Model, _) -> scene.Node {
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
  scene.empty(id: "scene", transform: transform.identity, children: [
    scene.camera(
      id: "main_camera",
      camera: cam,
      transform: transform.identity,
      viewport: option.None,
      active: True,
      postprocessing: option.None,
    ),
    scene.light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.6)
        light
      },
      transform: transform.identity,
    ),
    scene.light(
      id: "directional",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 0.8)
        light
      },
      transform: transform.at(position: vec3.Vec3(5.0, 5.0, 5.0)),
    ),
    scene.mesh(
      id: "cube",
      geometry: {
        let assert Ok(box) = geometry.box(vec3f.one)
        box
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(model.color)
          |> material.build()
        material
      },
      transform: transform.at(position: model.position)
        |> transform.with_euler_rotation(model.rotation)
        |> transform.scale_uniform(model.scale),
      physics: option.None,
    ),
  ])
}
