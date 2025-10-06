/// Effects System Example
///
/// Demonstrates the effect system with tick and custom effects
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import plinth/javascript/global
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub type Model {
  Model(cubes: List(Cube), next_id: Int)
}

pub type Cube {
  Cube(id: Int, position: vec3.Vec3, velocity: vec3.Vec3, color: Int)
}

pub type Msg {
  Tick
  AddCube
}

pub fn main() -> Nil {
  let assert Ok(cam) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 1200.0 /. 800.0,
      near: 0.1,
      far: 1000.0,
    )

  let cam =
    cam
    |> camera.set_position(vec3.Vec3(0.0, 5.0, 20.0))
    |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

  game.run(
    width: 1200,
    height: 800,
    background: 0x1a1a2e,
    camera: cam,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  #(
    Model(cubes: [], next_id: 0),
    effect.batch([effect.tick(Tick), schedule_add_cube()]),
  )
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
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

      // Remove cubes that fall too far
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
fn schedule_add_cube() -> Effect(Msg) {
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

fn view(model: Model) -> List(scene.SceneNode) {
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.6),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.8),
      transform: transform.Transform(
        position: vec3.Vec3(10.0, 10.0, 10.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  let cubes =
    list.map(model.cubes, fn(cube) {
      scene.Mesh(
        id: "cube-" <> int.to_string(cube.id),
        geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
        material: scene.StandardMaterial(
          color: cube.color,
          metalness: 0.3,
          roughness: 0.5,
          map: option.None,
          normal_map: option.None,
        ),
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

  list.flatten([lights, cubes])
}
