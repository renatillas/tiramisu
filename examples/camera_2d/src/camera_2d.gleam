/// Camera 2D Example
///
/// Demonstrates 2D camera modes and orthographic projection
import gleam/option
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/math/vec3
import tiramisu/scene

pub type Model {
  Model(time: Float)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  // Use 2D camera centered at origin
  let cam = camera.camera_2d(800, 600)

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
  #(Model(time: 0.0), effect.tick(Tick))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_time = model.time +. ctx.delta_time
      #(Model(time: new_time), effect.tick(Tick))
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 1.0),
      transform: scene.identity_transform(),
    ),
    // Center square
    scene.Mesh(
      id: "center",
      geometry: scene.PlaneGeometry(50.0, 50.0),
      material: scene.BasicMaterial(
        color: 0x4ecdc4,
        transparent: False,
        opacity: 1.0,
        map: option.None,
      ),
      transform: scene.Transform(
        position: vec3.Vec3(0.0, 0.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    // Grid of squares in 2D space
    scene.Mesh(
      id: "top-left",
      geometry: scene.PlaneGeometry(30.0, 30.0),
      material: scene.BasicMaterial(
        color: 0xff6b6b,
        transparent: False,
        opacity: 1.0,
        map: option.None,
      ),
      transform: scene.Transform(
        position: vec3.Vec3(-150.0, 150.0, -1.0),
        rotation: vec3.Vec3(0.0, 0.0, model.time),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "top-right",
      geometry: scene.PlaneGeometry(30.0, 30.0),
      material: scene.BasicMaterial(
        color: 0xffe66d,
        transparent: False,
        opacity: 1.0,
        map: option.None,
      ),
      transform: scene.Transform(
        position: vec3.Vec3(150.0, 150.0, -1.0),
        rotation: vec3.Vec3(0.0, 0.0, model.time),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "bottom-left",
      geometry: scene.PlaneGeometry(30.0, 30.0),
      material: scene.BasicMaterial(
        color: 0x95e1d3,
        transparent: False,
        opacity: 1.0,
        map: option.None,
      ),
      transform: scene.Transform(
        position: vec3.Vec3(-150.0, -150.0, -1.0),
        rotation: vec3.Vec3(0.0, 0.0, model.time),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "bottom-right",
      geometry: scene.PlaneGeometry(30.0, 30.0),
      material: scene.BasicMaterial(
        color: 0xf38181,
        transparent: False,
        opacity: 1.0,
        map: option.None,
      ),
      transform: scene.Transform(
        position: vec3.Vec3(150.0, -150.0, -1.0),
        rotation: vec3.Vec3(0.0, 0.0, model.time),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]
}
