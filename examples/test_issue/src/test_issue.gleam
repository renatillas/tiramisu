/// Physics Demo - Falling Cubes
/// Demonstrates physics simulation with Rapier3D
import gleam/option
import tiramisu
import tiramisu/background
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/physics
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Id {
  Camera
  Ambient
  Directional
  Ground
  Cube1
  Cube2
}

pub type Model {
  Model
}

pub type Msg {
  Tick
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
    physics.new_world(
      physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)),
    )

  #(Model, effect.tick(Tick), option.Some(physics_world))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(Id),
) -> #(Model, Effect(Msg), option.Option(_)) {
  let assert option.Some(physics_world) = ctx.physics_world
  case msg {
    Tick -> {
      let new_physics_world = physics.step(physics_world)
      #(model, effect.tick(Tick), option.Some(new_physics_world))
    }
  }
}

fn view(_model: Model, ctx: tiramisu.Context(Id)) -> List(scene.Node(Id)) {
  let assert option.Some(physics_world) = ctx.physics_world
  let assert Ok(cam) = camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let assert Ok(cube_geom) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(cube1_mat) = material.new() |> material.with_color(0xff4444) |> material.build
  let assert Ok(cube2_mat) = material.new() |> material.with_color(0x44ff44) |> material.build

  let assert Ok(ground_geom) = geometry.box(width: 20.0, height: 0.2, depth: 20.0)
  let assert Ok(ground_mat) = material.new() |> material.with_color(0x808080) |> material.build

  [
    scene.Camera(
      id: Camera,
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 10.0, 15.0)),
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      active: True,
      viewport: option.None,
    ),
    scene.Light(
      id: Ambient,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: Directional,
      light: {
        let assert Ok(light) = light.directional(color: 0xffffff, intensity: 2.0)
        light
      },
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 7.5)),
    ),
    // Ground (static physics body)
    scene.Mesh(
      id: Ground,
      geometry: ground_geom,
      material: ground_mat,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
      physics: option.Some(
        physics.new_rigid_body(physics.Fixed)
        |> physics.with_collider(physics.Box(transform.identity, 20.0, 0.2, 20.0))
        |> physics.with_restitution(0.0)
        |> physics.build(),
      ),
    ),
    // Falling cube 1 (dynamic physics body)
    scene.Mesh(
      id: Cube1,
      geometry: cube_geom,
      material: cube1_mat,
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
    ),
    // Falling cube 2 (dynamic physics body)
    scene.Mesh(
      id: Cube2,
      geometry: cube_geom,
      material: cube2_mat,
      transform: case physics.get_transform(physics_world, Cube2) {
        Ok(t) -> t
        Error(Nil) -> transform.at(position: vec3.Vec3(2.0, 7.0, 0.0))
      },
      physics: option.Some(
        physics.new_rigid_body(physics.Dynamic)
        |> physics.with_collider(physics.Box(transform.identity, 1.0, 1.0, 1.0))
        |> physics.with_mass(1.0)
        |> physics.with_restitution(0.6)
        |> physics.with_friction(0.3)
        |> physics.build(),
      ),
    ),
  ]
}
