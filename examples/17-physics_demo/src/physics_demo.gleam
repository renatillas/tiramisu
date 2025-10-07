import gleam/option
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/physics
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub type Model {
  Model(rotation: Float, physics_world: physics.PhysicsWorld)
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
    |> camera.look(at: vec3.Vec3(0.0, 2.0, 0.0))

  game.run(
    width: 1200,
    height: 800,
    background: 0x1a1a2e,
    camera: option.Some(cam),
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  // Initialize physics world with gravity
  let physics_world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  let model = Model(rotation: 0.0, physics_world: physics_world)

  #(model, effect.tick(Tick))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      // Step physics simulation
      let new_physics_world = physics.step(model.physics_world, ctx.delta_time)

      let new_rotation = model.rotation +. ctx.delta_time *. 0.5

      #(
        Model(rotation: new_rotation, physics_world: new_physics_world),
        effect.tick(Tick),
      )
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.5),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 2.0),
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 7.5)),
    ),
  ]

  // Ground plane (static physics body)
  let ground =
    scene.Mesh(
      id: "ground",
      geometry: scene.BoxGeometry(20.0, 0.2, 20.0),
      material: scene.StandardMaterial(
        color: 0x808080,
        metalness: 0.3,
        roughness: 0.7,
        map: option.None,
        normal_map: option.None,
      ),
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
      physics: option.Some(
        physics.rigid_body(physics.Fixed, physics.Box(20.0, 0.2, 20.0))
        |> physics.set_restitution(0.3),
      ),
    )

  // Falling cubes (dynamic physics bodies)
  let cube1 =
    scene.Mesh(
      id: "cube1",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        color: 0xff4444,
        metalness: 0.2,
        roughness: 0.8,
        map: option.None,
        normal_map: option.None,
      ),
      transform: case physics.get_transform(model.physics_world, "cube1") {
        option.Some(t) -> t
        option.None -> transform.at(position: vec3.Vec3(-2.0, 5.0, 0.0))
      },
      physics: option.Some(
        physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
        |> physics.set_mass(1.0)
        |> physics.set_restitution(0.5)
        |> physics.set_friction(0.5),
      ),
    )

  let cube2 =
    scene.Mesh(
      id: "cube2",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        color: 0x44ff44,
        metalness: 0.2,
        roughness: 0.8,
        map: option.None,
        normal_map: option.None,
      ),
      transform: case physics.get_transform(model.physics_world, "cube2") {
        option.Some(t) -> t
        option.None -> transform.at(position: vec3.Vec3(0.0, 7.0, 0.0))
      },
      physics: option.Some(
        physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
        |> physics.set_mass(1.0)
        |> physics.set_restitution(0.6)
        |> physics.set_friction(0.3),
      ),
    )

  let cube3 =
    scene.Mesh(
      id: "cube3",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        color: 0x4444ff,
        metalness: 0.2,
        roughness: 0.8,
        map: option.None,
        normal_map: option.None,
      ),
      transform: case physics.get_transform(model.physics_world, "cube3") {
        option.Some(t) -> t
        option.None -> transform.at(position: vec3.Vec3(2.0, 9.0, 0.0))
      },
      physics: option.Some(
        physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
        |> physics.set_mass(1.0)
        |> physics.set_restitution(0.4)
        |> physics.set_friction(0.6),
      ),
    )

  [ground, cube1, cube2, cube3, ..lights]
}
