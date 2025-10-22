import gleam/option
import tiramisu
import tiramisu/background
import tiramisu/camera
import tiramisu/debug
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/physics
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Id {
  MainCamera
  Ambient
  Directional
  Ground
  Cube1
  Cube2
  Cube3
}

pub type Msg {
  Tick
}

pub type Model {
  Model(debug: Bool)
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

fn init(_ctx: tiramisu.Context(Id)) -> #(Model, Effect(Msg), option.Option(_)) {
  // Initialize physics world with gravity
  let physics_world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  #(Model(False), effect.tick(Tick), option.Some(physics_world))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(Id),
) -> #(Model, Effect(Msg), option.Option(_)) {
  echo debug.get_performance_stats()
  let assert option.Some(physics_world) = ctx.physics_world
  case msg {
    Tick -> {
      let new_physics_world = physics.step(physics_world)
      case input.is_key_just_pressed(ctx.input, input.KeyD) {
        True -> #(
          Model(!model.debug),
          effect.tick(Tick),
          option.Some(new_physics_world),
        )
        False -> #(model, effect.tick(Tick), option.Some(new_physics_world))
      }
    }
  }
}

fn view(model: Model, ctx: tiramisu.Context(Id)) -> List(scene.Node(Id)) {
  let assert option.Some(physics_world) = ctx.physics_world
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let camera =
    scene.camera(
      id: MainCamera,
      camera:,
      transform: transform.at(position: vec3.Vec3(0.0, 10.0, 15.0)),
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      active: True,
      viewport: option.None,
    )

  let lights = [
    scene.light(
      id: Ambient,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    ),
    scene.light(
      id: Directional,
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
        let assert Ok(geometry) =
          geometry.box(width: 20.0, height: 0.2, depth: 20.0)
        geometry
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(0x808080)
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

  // Falling cubes (dynamic physics bodies)
  let cube1 =
    scene.mesh(
      id: Cube1,
      geometry: {
        let assert Ok(geometry) =
          geometry.box(width: 1.0, height: 1.0, depth: 1.0)
        geometry
      },
      material: {
        let assert Ok(material) =
          material.new()
          |> material.with_color(0xff4444)
          |> material.build()
        material
      },
      transform: case physics.get_transform(physics_world, Cube1) {
        Ok(t) -> t
        Error(Nil) -> transform.at(position: vec3.Vec3(-2.0, 5.0, 0.0))
      },
      physics: option.Some(
        physics.new_rigid_body(physics.Dynamic)
        |> physics.with_collider(physics.Box(transform.identity, 1.0, 1.0, 1.0))
        |> physics.with_mass(1.0)
        |> physics.with_restitution(0.5)
        |> physics.with_friction(0.5)
        |> physics.build(),
      ),
    )

  let cube2 =
    scene.mesh(
      id: Cube2,
      geometry: {
        let assert Ok(geometry) =
          geometry.box(width: 1.0, height: 1.0, depth: 1.0)
        geometry
      },
      material: {
        let assert Ok(material) =
          material.new() |> material.with_color(0x44ff44) |> material.build()
        material
      },
      transform: case physics.get_transform(physics_world, Cube2) {
        Ok(t) -> t
        Error(Nil) -> transform.at(position: vec3.Vec3(0.0, 7.0, 0.0))
      },
      physics: option.Some(
        physics.new_rigid_body(physics.Dynamic)
        |> physics.with_collider(physics.Box(transform.identity, 1.0, 1.0, 1.0))
        |> physics.with_mass(1.0)
        |> physics.with_restitution(0.6)
        |> physics.with_friction(0.3)
        |> physics.build(),
      ),
    )

  let cube3 =
    scene.mesh(
      id: Cube3,
      geometry: {
        let assert Ok(geometry) =
          geometry.box(width: 1.0, height: 1.0, depth: 1.0)
        geometry
      },
      material: {
        let assert Ok(material) =
          material.new() |> material.with_color(0x4444ff) |> material.build()
        material
      },
      transform: case physics.get_transform(physics_world, Cube3) {
        Ok(t) -> t
        Error(Nil) -> transform.at(position: vec3.Vec3(2.0, 9.0, 0.0))
      },
      physics: option.Some(
        physics.new_rigid_body(physics.Dynamic)
        |> physics.with_collider(physics.Box(transform.identity, 1.0, 1.0, 1.0))
        |> physics.with_mass(1.0)
        |> physics.with_restitution(0.4)
        |> physics.with_friction(0.6)
        |> physics.build(),
      ),
    )

  // Enable/disable debug collider visualization
  case model.debug {
    True -> debug.show_collider_wireframes(physics_world, True)
    False -> debug.show_collider_wireframes(physics_world, False)
  }

  // Return scene
  [camera, ground, cube1, cube2, cube3, ..lights]
}
