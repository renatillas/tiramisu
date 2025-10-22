import gleam/option
import gleam/result
import tiramisu
import tiramisu/background
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

pub type Msg {
  Tick
}

pub type Id {
  Sphere
  AmbientLight
  DirectionalLight
  Ground
  MainCamera
}

pub fn main() -> Nil {
  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x1a1a2e),
    init: init,
    update: update,
    view: view,
  )
}

fn init(
  _ctx: tiramisu.Context(Id),
) -> #(Nil, Effect(Msg), option.Option(physics.PhysicsWorld(Id))) {
  // Initialize physics world with gravity
  let physics_world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))
  #(Nil, effect.tick(Tick), option.Some(physics_world))
}

fn update(
  _model: Nil,
  msg: Msg,
  ctx: tiramisu.Context(Id),
) -> #(Nil, Effect(Msg), option.Option(physics.PhysicsWorld(Id))) {
  let assert option.Some(physics_world) = ctx.physics_world
  case msg {
    Tick -> {
      // Step physics simulation

      let physics_world =
        case input.is_key_just_pressed(ctx.input, input.Space) {
          True ->
            physics.apply_impulse(
              physics_world,
              Sphere,
              vec3.Vec3(0.0, 10.0, 0.0),
            )
          False -> physics_world
        }
        |> physics.step

      #(Nil, effect.tick(Tick), option.Some(physics_world))
    }
  }
}

fn view(_model: Nil, context: tiramisu.Context(Id)) -> List(scene.Node(Id)) {
  let assert option.Some(physics_world) = context.physics_world
  let cube_transform =
    physics.get_transform(physics_world, Sphere)
    |> result.unwrap(transform.at(position: vec3.Vec3(0.0, 10.0, 0.0)))

  // Create camera that follows the cube with an offset
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let cam =
    scene.camera(
      id: MainCamera,
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 3.0, 8.0)),
      look_at: option.Some(transform.position(cube_transform)),
      active: True,
      viewport: option.None,
    )

  let lights = [
    scene.light(
      id: AmbientLight,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    ),
    scene.light(
      id: DirectionalLight,
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
    scene.mesh(
      id: Ground,
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
        physics.new_rigid_body(physics.Fixed)
        |> physics.with_collider(physics.Box(
          transform.identity,
          20.0,
          0.2,
          20.0,
        ))
        |> physics.with_restitution(0.0)
        |> physics.build(),
      ),
    )

  // Falling cube (physics will control its position automatically)
  let cube1 = create_sphere(Sphere, 0xff4444)

  [ground, cube1, cam, ..lights]
}

fn create_sphere(id: Id, color: Int) -> scene.Node(Id) {
  scene.mesh(
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
      physics.new_rigid_body(physics.Dynamic)
      |> physics.with_collider(physics.Sphere(transform.identity, 1.0))
      |> physics.with_mass(1.0)
      |> physics.with_restitution(0.5)
      |> physics.with_friction(0.5)
      |> physics.build(),
    ),
  )
}
