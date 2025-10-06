import gleam/list
import gleam/option
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub type Model {
  Model(rotation: Float, light_intensity: Float)
}

pub type Msg {
  Tick
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
    |> camera.set_position(position: vec3.Vec3(0.0, 3.0, 15.0))
    |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

  game.run(
    width: 1200,
    height: 800,
    background: 0x0a0a0a,
    camera: option.Some(cam),
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  #(Model(rotation: 0.0, light_intensity: 1.0), effect.tick(Tick))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time
      #(Model(..model, rotation: new_rotation), effect.tick(Tick))
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0x404040, intensity: 0.3),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.5),
      transform: transform.Transform(
        position: vec3.Vec3(5.0, 10.0, 5.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
    scene.Light(
      id: "point",
      light_type: scene.PointLight(
        color: 0xff6b6b,
        intensity: 1.0,
        distance: 50.0,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(-5.0, 3.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
    scene.Light(
      id: "hemisphere",
      light_type: scene.HemisphereLight(
        sky_color: 0xffffff,
        ground_color: 0xff0000,
        intensity: 1.0,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 10.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  let materials = [
    scene.Mesh(
      id: "basic",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.BasicMaterial(
        color: 0xff6b6b,
        transparent: False,
        opacity: 1.0,
        map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(-6.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "standard",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        color: 0x4ecdc4,
        metalness: 0.8,
        roughness: 0.2,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(-3.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "phong",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.PhongMaterial(
        color: 0xffe66d,
        shininess: 100.0,
        map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "lambert",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.LambertMaterial(color: 0x95e1d3, map: option.None),
      transform: transform.Transform(
        position: vec3.Vec3(3.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "toon",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.ToonMaterial(color: 0xf38181, map: option.None),
      transform: transform.Transform(
        position: vec3.Vec3(6.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]

  let ground = [
    scene.Mesh(
      id: "ground",
      geometry: scene.PlaneGeometry(20.0, 20.0),
      material: scene.StandardMaterial(
        color: 0x6a6a6a,
        metalness: 0.0,
        roughness: 0.8,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(0.0, -2.0, 0.0),
        rotation: vec3.Vec3(-1.5708, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]

  list.flatten([lights, materials, ground])
}
