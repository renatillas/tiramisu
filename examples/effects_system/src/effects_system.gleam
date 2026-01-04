import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/time/duration
import tiramisu
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3
import vec/vec3f

pub type Model {
  Model(cubes: List(Cube), next_id: Int)
}

pub type Cube {
  Cube(
    id: Int,
    position: vec3.Vec3(Float),
    velocity: vec3.Vec3(Float),
    color: Int,
  )
}

pub type Msg {
  Tick
  AddCube
}

pub fn main() {
  let assert Ok(_) =
    tiramisu.application(init, update, view)
    |> tiramisu.start("body", tiramisu.FullScreen, option.None)
}

fn init(
  _ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  #(
    Model(cubes: [], next_id: 0),
    effect.batch([
      effect.dispatch(Tick),
      effect.delay(duration.milliseconds(500), AddCube),
    ]),
    option.None,
  )
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      // Update cube positions based on velocity
      let updated_cubes =
        list.map(model.cubes, fn(cube) {
          let new_pos =
            vec3.Vec3(
              cube.position.x
                +. cube.velocity.x
                *. duration.to_seconds(ctx.delta_time),
              cube.position.y
                +. cube.velocity.y
                *. duration.to_seconds(ctx.delta_time),
              cube.position.z
                +. cube.velocity.z
                *. duration.to_seconds(ctx.delta_time),
            )
          Cube(..cube, position: new_pos)
        })

      let filtered_cubes =
        list.filter(updated_cubes, fn(cube) { cube.position.y >. -10.0 })

      #(
        Model(..model, cubes: filtered_cubes),
        effect.dispatch(Tick),
        option.None,
      )
    }

    AddCube -> {
      let new_cube =
        Cube(
          id: model.next_id,
          position: vec3.Vec3(0.0, 5.0, 0.0),
          velocity: vec3.Vec3(
            float.random() *. 2.0 -. 1.0,
            -2.0,
            float.random() *. 2.0 -. 1.0,
          ),
          color: random_color(),
        )

      #(
        Model(cubes: [new_cube, ..model.cubes], next_id: model.next_id + 1),
        effect.delay(duration.milliseconds(500), AddCube),
        option.None,
      )
    }
  }
}

// Schedule adding a cube after 500ms

fn random_color() -> Int {
  let colors = [
    0xff6b6b, 0x4ecdc4, 0xffe66d, 0x95e1d3, 0xf38181, 0xa8e6cf, 0xdcedc1,
    0xffd3b6, 0xffaaa5,
  ]
  let index = float.floor(float.random() *. 9.0)
  let assert Ok(color) = list_at(colors, float.truncate(index))
  color
}

fn list_at(list: List(a), index: Int) -> Result(a, Nil) {
  case list, index {
    [first, ..], 0 -> Ok(first)
    [_, ..rest], n if n > 0 -> list_at(rest, n - 1)
    _, _ -> Error(Nil)
  }
}

fn view(model: Model, _) -> scene.Node {
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let assert Ok(box_geometry) = geometry.box(size: vec3f.one)

  let camera_node =
    scene.camera(
      id: "main-camera",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 5.0, 20.0)),
      active: True,
      viewport: option.None,
      postprocessing: option.None,
    )

  let lights = [
    scene.light(
      id: "ambient-light",
      light: {
        let assert Ok(light) = light.ambient(intensity: 0.6, color: 0xffffff)
        light
      },
      transform: transform.identity,
    ),
    scene.light(
      id: "directional-light",
      light: {
        let assert Ok(light) =
          light.directional(intensity: 0.8, color: 0xffffff)
        light
      },
      transform: transform.at(position: vec3.Vec3(10.0, 10.0, 10.0)),
    ),
  ]

  let cubes =
    list.map(model.cubes, fn(cube) {
      let assert Ok(cube_material) =
        material.new() |> material.with_color(cube.color) |> material.build

      scene.mesh(
        id: int.to_string(cube.id),
        geometry: box_geometry,
        material: cube_material,
        transform: transform.at(position: cube.position)
          |> transform.with_euler_rotation(vec3.Vec3(
            cube.position.y *. 0.5,
            cube.position.x *. 0.5,
            cube.position.z *. 0.5,
          ))
          |> transform.with_scale(vec3.Vec3(1.0, 1.0, 1.0)),
        physics: option.None,
      )
    })

  scene.empty(id: "scene", transform: transform.identity, children: [
    camera_node,
    ..list.append(lights, cubes)
  ])
}
