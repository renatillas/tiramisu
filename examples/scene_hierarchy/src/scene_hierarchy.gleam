/// Scene Hierarchy Example
///
/// Demonstrates Group nodes, parent-child relationships, and nested transforms
import gleam/list
import gleam/option
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub type Model {
  Model(rotation: Float, show_planets: Bool)
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
    |> camera.set_position(vec3.Vec3(0.0, 5.0, 15.0))
    |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

  game.run(
    width: 1200,
    height: 800,
    background: 0x0f0f0f,
    camera: cam,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  #(Model(rotation: 0.0, show_planets: True), effect.tick(Tick))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time
      #(Model(rotation: new_rotation, show_planets: True), effect.tick(Tick))
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0x404040, intensity: 10.0),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "point",
      light_type: scene.PointLight(
        color: 0xffffff,
        intensity: 10.5,
        distance: 100.0,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 0.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  // Solar system: sun with orbiting planets in groups
  let sun = [
    scene.Mesh(
      id: "sun",
      geometry: scene.SphereGeometry(1.5, 32, 32),
      material: scene.BasicMaterial(
        color: 0xffff00,
        transparent: False,
        opacity: 1.0,
        map: option.None,
      ),
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 0.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]

  // Planets as children of rotating groups
  let planet_system = case model.show_planets {
    True -> [
      // Planet 1 group (rotates around sun)
      scene.Group(
        id: "planet1-orbit",
        transform: transform.Transform(
          position: vec3.Vec3(0.0, 0.0, 0.0),
          rotation: vec3.Vec3(0.0, model.rotation *. 0.5, 0.0),
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
        children: [
          scene.Mesh(
            id: "planet1",
            geometry: scene.SphereGeometry(0.5, 32, 32),
            material: scene.StandardMaterial(
              color: 0x4ecdc4,
              metalness: 0.3,
              roughness: 0.7,
              map: option.None,
              normal_map: option.None,
            ),
            transform: transform.Transform(
              position: vec3.Vec3(4.0, 0.0, 0.0),
              rotation: vec3.Vec3(0.0, model.rotation *. 2.0, 0.0),
              scale: vec3.Vec3(1.0, 1.0, 1.0),
            ),
            physics: option.None,
          ),
          // Moon orbiting planet1
          scene.Group(
            id: "moon1-orbit",
            transform: transform.Transform(
              position: vec3.Vec3(4.0, 0.0, 0.0),
              rotation: vec3.Vec3(0.0, model.rotation *. 2.0, 0.0),
              scale: vec3.Vec3(1.0, 1.0, 1.0),
            ),
            children: [
              scene.Mesh(
                id: "moon1",
                geometry: scene.SphereGeometry(0.2, 16, 16),
                material: scene.BasicMaterial(
                  color: 0xcccccc,
                  transparent: False,
                  opacity: 1.0,
                  map: option.None,
                ),
                transform: transform.Transform(
                  position: vec3.Vec3(1.0, 0.0, 0.0),
                  rotation: vec3.Vec3(0.0, 0.0, 0.0),
                  scale: vec3.Vec3(1.0, 1.0, 1.0),
                ),
                physics: option.None,
              ),
            ],
          ),
        ],
      ),
      // Planet 2 group (rotates around sun)
      scene.Group(
        id: "planet2-orbit",
        transform: transform.Transform(
          position: vec3.Vec3(0.0, 0.0, 0.0),
          rotation: vec3.Vec3(0.0, model.rotation *. 0.3, 0.0),
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
        children: [
          scene.Mesh(
            id: "planet2",
            geometry: scene.SphereGeometry(0.7, 32, 32),
            material: scene.StandardMaterial(
              color: 0xff6b6b,
              metalness: 0.5,
              roughness: 0.5,
              map: option.None,
              normal_map: option.None,
            ),
            transform: transform.Transform(
              position: vec3.Vec3(7.0, 0.0, 0.0),
              rotation: vec3.Vec3(0.0, model.rotation, 0.0),
              scale: vec3.Vec3(1.0, 1.0, 1.0),
            ),
            physics: option.None,
          ),
        ],
      ),
    ]
    False -> []
  }

  list.flatten([lights, sun, planet_system])
}
