import gleam/option
import tiramisu
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/physics
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Model {
  Model(rotation: Float, physics_world: physics.PhysicsWorld)
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

  let model = Model(rotation: 0.0, physics_world: physics_world)

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
      let new_physics_world = physics.step(model.physics_world, ctx.delta_time)

      let new_rotation = model.rotation +. ctx.delta_time *. 0.5

      #(
        Model(rotation: new_rotation, physics_world: new_physics_world),
        effect.tick(Tick),
      )
    }
  }
}

fn view(_model: Model) -> List(scene.Node) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let camera =
    scene.Camera(
      id: "main_camera",
      camera:,
      transform: transform.at(position: vec3.Vec3(0.0, 10.0, 15.0)),
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
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
        physics.rigid_body(physics.Fixed, physics.Box(20.0, 0.2, 20.0))
        |> physics.set_restitution(0.0),
      ),
    )

  // Falling cubes (dynamic physics bodies)
  let cube1 =
    scene.Mesh(
      id: "cube1",
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
      transform: case physics.get_transform("cube1") {
        Ok(t) -> t
        Error(Nil) -> transform.at(position: vec3.Vec3(-2.0, 5.0, 0.0))
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
      transform: case physics.get_transform("cube2") {
        Ok(t) -> t
        Error(Nil) -> transform.at(position: vec3.Vec3(0.0, 7.0, 0.0))
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
      transform: case physics.get_transform("cube3") {
        Ok(t) -> t
        Error(Nil) -> transform.at(position: vec3.Vec3(2.0, 9.0, 0.0))
      },
      physics: option.Some(
        physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
        |> physics.set_mass(1.0)
        |> physics.set_restitution(0.4)
        |> physics.set_friction(0.6),
      ),
    )

  [camera, ground, cube1, cube2, cube3, ..lights]
}
