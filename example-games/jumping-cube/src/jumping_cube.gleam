import gleam/option
import tiramisu
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/physics
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Model {
  Model(physics_world: physics.PhysicsWorld)
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

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg)) {
  // Initialize physics world with gravity
  let physics_world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  let model = Model(physics_world: physics_world)

  #(model, effect.tick(Tick))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      // Step physics simulation

      let physics_world =
        case input.is_key_just_pressed(ctx.input, input.Space) {
          True ->
            physics.apply_impulse(
              model.physics_world,
              "cube1",
              vec3.Vec3(0.0, 100.0, 0.0),
            )
          False -> model.physics_world
        }
        |> physics.step(ctx.delta_time)

      #(Model(physics_world:), effect.tick(Tick))
    }
  }
}

fn view(_model: Model) -> List(scene.Node) {
  // Get the cube's physics transform (or default if not found yet)
  let cube_transform = case physics.get_transform("cube1") {
    Ok(t) -> t
    Error(_) -> transform.at(position: vec3.Vec3(0.0, 10.0, 0.0))
  }

  // Create camera that follows the cube with an offset
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let cam =
    scene.Camera(
      id: "main_camera",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 3.0, 8.0)),
      look_at: option.Some(cube_transform.position),
      active: True,
      viewport: option.None,
    )

  let lights = [
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 2.0)
        light
      },
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 7.5)),
    ),
  ]

  // Ground plane (static physics body)
  let ground =
    scene.Mesh(
      id: "ground",
      geometry: {
        let assert Ok(box) = geometry.box(20.0, 0.2, 20.0)
        box
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(0x808080)
          |> material.with_metalness(0.3)
          |> material.with_roughness(0.7)
          |> material.build()
        material
      },
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
      physics: option.Some(
        physics.rigid_body(physics.Fixed, physics.Box(20.0, 0.2, 20.0))
        |> physics.set_restitution(0.0),
      ),
    )

  // Falling cube (physics will control its position automatically)
  let cube1 = create_sphere("cube1", 0xff4444)

  [ground, cube1, cam, ..lights]
}

fn create_sphere(id: String, color: Int) -> scene.Node {
  scene.Mesh(
    id: id,
    geometry: {
      let assert Ok(sphere) = geometry.sphere(1.0, 30, 30)
      sphere
    },
    material: {
      let assert Ok(material) =
        material.new()
        |> material.with_color(color)
        |> material.with_metalness(0.2)
        |> material.with_roughness(0.8)
        |> material.build()
      material
    },
    transform: transform.at(position: vec3.Vec3(0.0, 10.0, 0.0)),
    physics: option.Some(
      physics.rigid_body(physics.Dynamic, physics.Sphere(1.0))
      |> physics.set_mass(1.0)
      |> physics.set_restitution(0.5)
      |> physics.set_friction(0.5),
    ),
  )
}
