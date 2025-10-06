import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/input
import tiramisu/math/vec3
import tiramisu/scene

pub type Model {
  Model(
    cube_position: vec3.Vec3,
    cube_scale: Float,
    touch_spheres: List(TouchSphere),
  )
}

pub type TouchSphere {
  TouchSphere(id: Int, x: Float, y: Float)
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
    camera: cam,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      cube_position: vec3.Vec3(0.0, 0.0, -5.0),
      cube_scale: 1.0,
      touch_spheres: [],
    )
  #(model, effect.tick(Tick))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let touches = input.touches(ctx.input)

      // Convert touches to visual spheres
      let spheres =
        list.map(touches, fn(touch) {
          TouchSphere(id: touch.id, x: touch.x, y: touch.y)
        })

      // Single touch: move cube to touch position
      let new_position = case touches {
        [first, ..] -> {
          // Map touch coordinates to 3D space
          let x = { first.x -. 400.0 } /. 100.0
          let y = { 300.0 -. first.y } /. 100.0
          vec3.Vec3(x, y, -5.0)
        }
        [] -> model.cube_position
      }

      // Multi-touch: calculate pinch distance for scaling
      let new_scale = case touches {
        [first, second, ..] -> {
          let dx = first.x -. second.x
          let dy = first.y -. second.y
          let distance =
            float.square_root({ dx *. dx } +. { dy *. dy })
            |> result.unwrap(200.0)

          // Map distance to scale (200px = 1.0 scale)
          let scale = distance /. 200.0
          case scale {
            s if s >. 0.2 && s <. 3.0 -> s
            _ -> model.cube_scale
          }
        }
        _ -> model.cube_scale
      }

      let new_model =
        Model(
          cube_position: new_position,
          cube_scale: new_scale,
          touch_spheres: spheres,
        )

      #(new_model, effect.tick(Tick))
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.6),
      transform: scene.identity_transform(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.8),
      transform: scene.Transform(
        position: vec3.Vec3(5.0, 5.0, 5.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  let main_cube = [
    scene.Mesh(
      id: "cube",
      geometry: scene.BoxGeometry(width: 2.0, height: 2.0, depth: 2.0),
      material: scene.StandardMaterial(
        color: 0x4ecdc4,
        metalness: 0.3,
        roughness: 0.4,
        map: option.None,
      ),
      transform: scene.Transform(
        position: model.cube_position,
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(model.cube_scale, model.cube_scale, model.cube_scale),
      ),
      physics: option.None,
    ),
  ]

  // Create visual indicators for each touch point
  let touch_indicators =
    list.map(model.touch_spheres, fn(touch) {
      let x = { touch.x -. 400.0 } /. 100.0
      let y = { 300.0 -. touch.y } /. 100.0

      scene.Mesh(
        id: "touch-" <> int.to_string(touch.id),
        geometry: scene.SphereGeometry(
          radius: 0.3,
          width_segments: 16,
          height_segments: 16,
        ),
        material: scene.BasicMaterial(
          color: 0xff00ff,
          transparent: True,
          opacity: 0.7,
          map: option.None,
        ),
        transform: scene.Transform(
          position: vec3.Vec3(x, y, -3.0),
          rotation: vec3.Vec3(0.0, 0.0, 0.0),
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
        physics: option.None,
      )
    })

  list.flatten([lights, main_cube, touch_indicators])
}
