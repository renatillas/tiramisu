import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import tiramisu
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Model {
  Model(
    cube_position: vec3.Vec3(Float),
    cube_scale: Float,
    touch_spheres: List(TouchSphere),
    canvas_width: Float,
    canvas_height: Float,
  )
}

pub type TouchSphere {
  TouchSphere(id: Int, x: Float, y: Float)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  tiramisu.run(
    dimensions: option.None,
    background: 0x1a1a2e,
    init: init,
    update: update,
    view: view,
  )
}

fn init(ctx: tiramisu.Context) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      cube_position: vec3.Vec3(0.0, 0.0, -5.0),
      cube_scale: 1.0,
      touch_spheres: [],
      canvas_width: ctx.canvas_width,
      canvas_height: ctx.canvas_height,
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
      let touches = input.touches(ctx.input)

      // Convert touches to visual spheres
      let spheres =
        list.map(touches, fn(touch) {
          TouchSphere(id: touch.id, x: touch.x, y: touch.y)
        })

      // Single touch: move cube to touch position
      let new_position = case touches {
        [first, ..] -> {
          // Map touch coordinates to 3D space using actual canvas dimensions
          let x = { first.x -. ctx.canvas_width /. 2.0 } /. 100.0
          let y = { ctx.canvas_height /. 2.0 -. first.y } /. 100.0
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
          canvas_width: ctx.canvas_width,
          canvas_height: ctx.canvas_height,
        )

      #(new_model, effect.tick(Tick))
    }
  }
}

fn view(model: Model) -> List(scene.Node) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let camera =
    scene.Camera(
      id: "main",
      camera:,
      transform: transform.identity,
      active: True,
      look_at: option.None,
      viewport: option.None,
    )

  let lights = [
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.6)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 0.8)
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(5.0, 5.0, 5.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  let main_cube = [
    scene.Mesh(
      id: "cube",
      geometry: {
        let assert Ok(box) = geometry.box(width: 2.0, height: 2.0, depth: 2.0)
        box
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(0x4ecdc4)
          |> material.build
        material
      },
      transform: transform.Transform(
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
      let x = { touch.x -. model.canvas_width /. 2.0 } /. 100.0
      let y = { model.canvas_height /. 2.0 -. touch.y } /. 100.0

      scene.Mesh(
        id: "touch-" <> int.to_string(touch.id),
        geometry: {
          let assert Ok(sphere) =
            geometry.sphere(
              radius: 0.3,
              width_segments: 16,
              height_segments: 16,
            )
          sphere
        },
        material: {
          let assert Ok(material) =
            material.basic(
              color: 0xff00ff,
              transparent: True,
              opacity: 0.7,
              map: option.None,
              normal_map: option.None,
            )
          material
        },
        transform: transform.Transform(
          position: vec3.Vec3(x, y, -3.0),
          rotation: vec3.Vec3(0.0, 0.0, 0.0),
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
        physics: option.None,
      )
    })

  list.flatten([[camera], lights, main_cube, touch_indicators])
}
