/// Example demonstrating keyboard and mouse input
///
/// Controls:
/// - WASD: Move the cube
/// - Mouse: Look around (moves cube based on mouse position)
/// - Left Click: Change cube color to red
/// - Right Click: Change cube color to blue
/// - Mouse Wheel: Scale the cube up/down
import gleam/option
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/input
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub type Model {
  Model(position: vec3.Vec3, rotation: vec3.Vec3, scale: Float, color: Int)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  let assert Ok(cam) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 800.0 /. 600.0,
      near: 0.1,
      far: 1000.0,
    )

  game.run(
    width: 800,
    height: 600,
    background: 0x1a1a2e,
    camera: option.Some(cam),
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      position: vec3.Vec3(0.0, 0.0, -5.0),
      rotation: vec3.Vec3(0.0, 0.0, 0.0),
      scale: 1.0,
      color: 0x4ecdc4,
    )
  #(model, effect.tick(Tick))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      // Keyboard movement (WASD)
      let move_speed = 5.0
      let dx = case
        input.is_key_pressed(ctx.input, input.KeyD),
        input.is_key_pressed(ctx.input, input.KeyA)
      {
        True, False -> move_speed *. ctx.delta_time
        False, True -> 0.0 -. move_speed *. ctx.delta_time
        _, _ -> 0.0
      }

      let dy = case
        input.is_key_pressed(ctx.input, input.KeyW),
        input.is_key_pressed(ctx.input, input.KeyS)
      {
        True, False -> move_speed *. ctx.delta_time
        False, True -> 0.0 -. move_speed *. ctx.delta_time
        _, _ -> 0.0
      }

      let new_position =
        vec3.Vec3(
          model.position.x +. dx,
          model.position.y +. dy,
          model.position.z,
        )

      // Mouse position affects rotation
      let #(mouse_x, mouse_y) = input.mouse_position(ctx.input)
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

      #(new_model, effect.tick(Tick))
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.6),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.8),
      transform: transform.Transform(
        position: vec3.Vec3(5.0, 5.0, 5.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
    scene.Mesh(
      id: "cube",
      geometry: scene.BoxGeometry(width: 2.0, height: 2.0, depth: 2.0),
      material: scene.StandardMaterial(
        color: model.color,
        metalness: 0.3,
        roughness: 0.4,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.Transform(
        position: model.position,
        rotation: model.rotation,
        scale: vec3.Vec3(model.scale, model.scale, model.scale),
      ),
      physics: option.None,
    ),
  ]
}
