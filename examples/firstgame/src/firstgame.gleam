import gleam/option
import tiramisu
import tiramisu/camera
import tiramisu/effect
import tiramisu/scene
import tiramisu/transform
import vec/vec3

type Model {
  Model(rotation: Float)
}

type Msg {
  Frame
}

pub fn main() {
  tiramisu.run(
    width: 800,
    height: 600,
    background: 0x111111,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) {
  #(Model(rotation: 0.0), effect.none())
}

fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  case msg {
    Frame -> {
      let new_rotation = model.rotation +. ctx.delta_time
      #(Model(rotation: new_rotation), effect.none())
    }
  }
}

fn view(model: Model) {
  let assert Ok(camera) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 800.0 /. 600.0,
      near: 0.1,
      far: 1000.0,
    )
  [
    scene.Camera(
      id: "camera",
      camera: camera,
      transform: transform.at(vec3.Vec3(0.0, 0.0, 10.0)),
      active: True,
      viewport: option.None,
    ),
    scene.Mesh(
      id: "cube",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        color: 0x00ff00,
        metalness: 1.0,
        roughness: 1.0,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.identity
        |> transform.set_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
      physics: option.None,
    ),
  ]
}
