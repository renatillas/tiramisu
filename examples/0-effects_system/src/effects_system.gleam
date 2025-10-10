/// Effects System Example
///
/// Demonstrates the effect system with tick and custom effects
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import plinth/javascript/global
import tiramisu
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

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

pub fn main() -> Nil {
  tiramisu.run(
    dimensions: option.None,
    background: 0x1a1a2e,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(cubes: [], next_id: 0),
    effect.batch([effect.tick(Tick), schedule_add_cube()]),
  )
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick -> {
      // Update cube positions based on velocity
      let updated_cubes =
        list.map(model.cubes, fn(cube) {
          let new_pos =
            vec3.Vec3(
              cube.position.x +. cube.velocity.x *. ctx.delta_time,
              cube.position.y +. cube.velocity.y *. ctx.delta_time,
              cube.position.z +. cube.velocity.z *. ctx.delta_time,
            )
          Cube(..cube, position: new_pos)
        })

      let filtered_cubes =
        list.filter(updated_cubes, fn(cube) { cube.position.y >. -10.0 })

      #(Model(..model, cubes: filtered_cubes), effect.tick(Tick))
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
        schedule_add_cube(),
      )
    }
  }
}

// Schedule adding a cube after 500ms
fn schedule_add_cube() -> effect.Effect(Msg) {
  effect.from(fn(dispatch) {
    global.set_timeout(500, fn() { dispatch(AddCube) })
    Nil
  })
}

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

fn view(model: Model) -> List(scene.Node) {
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let assert Ok(box_geometry) =
    geometry.box(width: 1.0, height: 1.0, depth: 1.0)

  let camera_node =
    scene.Camera(
      id: "main_camera",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 5.0, 20.0)),
      look_at: option.None,
      active: True,
      viewport: option.None,
    )
    |> list.wrap

  let lights = [
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(intensity: 0.6, color: 0xffffff)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light: {
        let assert Ok(light) =
          light.directional(intensity: 0.8, color: 0xffffff)
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(10.0, 10.0, 10.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  let cubes =
    list.map(model.cubes, fn(cube) {
      let assert Ok(cube_material) =
        material.new() |> material.with_color(cube.color) |> material.build

      scene.Mesh(
        id: "cube-" <> int.to_string(cube.id),
        geometry: box_geometry,
        material: cube_material,
        transform: transform.Transform(
          position: cube.position,
          rotation: vec3.Vec3(
            cube.position.y *. 0.5,
            cube.position.x *. 0.5,
            0.0,
          ),
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
        physics: option.None,
      )
    })

  list.flatten([camera_node, lights, cubes])
}
