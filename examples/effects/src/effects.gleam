import gleam/option
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/input
import tiramisu/input/keyboard
import tiramisu/math/vec3
import tiramisu/scene
import tiramisu/three/camera

// --- Model ---

pub type Model {
  Model(cube_position: vec3.Vec3, cube_rotation: vec3.Vec3, time: Float)
}

// --- Msg ---

pub type Msg {
  Tick
}

// --- Init ---

pub fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      cube_position: vec3.Vec3(0.0, 0.0, 0.0),
      cube_rotation: vec3.Vec3(0.0, 0.0, 0.0),
      time: 0.0,
    )

  #(model, effect.tick(Tick))
}

// --- Update ---

pub fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_time = model.time +. ctx.delta_time

      let new_rotation =
        vec3.Vec3(
          model.cube_rotation.x +. ctx.delta_time,
          model.cube_rotation.y +. ctx.delta_time *. 0.7,
          model.cube_rotation.z,
        )

      let move_speed = 10.0
      let dx = case
        input.is_key_pressed(ctx.input, keyboard.ArrowRight),
        input.is_key_pressed(ctx.input, keyboard.ArrowLeft)
      {
        True, False -> move_speed *. ctx.delta_time
        False, True -> -1.0 *. move_speed *. ctx.delta_time
        _, _ -> 0.0
      }

      let dy = case
        input.is_key_pressed(ctx.input, keyboard.ArrowUp),
        input.is_key_pressed(ctx.input, keyboard.ArrowDown)
      {
        True, False -> move_speed *. ctx.delta_time
        False, True -> -1.0 *. move_speed *. ctx.delta_time
        _, _ -> 0.0
      }

      let new_position =
        vec3.Vec3(
          model.cube_position.x +. dx,
          model.cube_position.y +. dy,
          model.cube_position.z,
        )

      #(
        Model(
          cube_position: new_position,
          cube_rotation: new_rotation,
          time: new_time,
        ),
        effect.tick(Tick),
      )
    }
  }
}

// --- View ---

pub fn view(model: Model) -> List(scene.SceneNode) {
  [
    scene.Mesh(
      id: "cube",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        color: 0x00aa99,
        metalness: 0.3,
        roughness: 0.4,
        map: option.None,
      ),
      transform: scene.Transform(
        position: model.cube_position,
        rotation: model.cube_rotation,
        scale: vec3.one(),
      ),
    ),
    scene.Mesh(
      id: "ground",
      geometry: scene.PlaneGeometry(10.0, 10.0),
      material: scene.StandardMaterial(
        color: 0x808080,
        metalness: 0.0,
        roughness: 1.0,
        map: option.None,
      ),
      transform: scene.Transform(
        position: vec3.Vec3(0.0, -2.0, 0.0),
        rotation: vec3.Vec3(-1.5708, 0.0, 0.0),
        scale: vec3.one(),
      ),
    ),
    scene.Light(
      id: "sun",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 1.0),
      transform: scene.transform_at(5.0, 10.0, 7.5),
    ),
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.4),
      transform: scene.identity_transform(),
    ),
  ]
}

// --- Main Entry Point ---

pub fn main() {
  let assert Ok(camera) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 16.0 /. 9.0,
      near: 0.1,
      far: 1000.0,
    )

  let camera =
    camera
    |> camera.set_position(0.0, 3.0, 8.0)
    |> camera.look_at(0.0, 0.0, 0.0)

  game.run(
    width: 800,
    height: 600,
    background: 0x1a1a2e,
    camera:,
    init:,
    update:,
    view:,
  )
}
