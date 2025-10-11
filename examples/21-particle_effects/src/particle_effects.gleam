import gleam/option
import tiramisu
import tiramisu/background
import tiramisu/camera
import tiramisu/debug
import tiramisu/effect
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/particle_emitter
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Id {
  MainCamera
  Ambient
  Directional
  Fire
  Sparkles
  Explosion
  Smoke
  Ground
}

pub type Model {
  Model(
    fire_active: Bool,
    sparkles_active: Bool,
    explosion_active: Bool,
    smoke_active: Bool,
  )
}

pub type Msg {
  Tick
  ToggleFire
  ToggleSparkles
  ToggleExplosion
  ToggleSmoke
}

pub fn main() -> Nil {
  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x0a0a1a),
    init: init,
    update: update,
    view: view,
  )
}

fn init(
  _ctx: tiramisu.Context(Id),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  #(
    Model(
      fire_active: True,
      sparkles_active: True,
      explosion_active: False,
      smoke_active: True,
    ),
    effect.tick(Tick),
    option.None,
  )
}

fn update(
  model: Model,
  msg: Msg,
  _ctx: tiramisu.Context(Id),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> #(model, effect.tick(Tick), option.None)

    ToggleFire -> #(
      Model(..model, fire_active: !model.fire_active),
      effect.tick(Tick),
      option.None,
    )

    ToggleSparkles -> #(
      Model(..model, sparkles_active: !model.sparkles_active),
      effect.tick(Tick),
      option.None,
    )

    ToggleExplosion -> #(
      Model(..model, explosion_active: !model.explosion_active),
      effect.tick(Tick),
      option.None,
    )

    ToggleSmoke -> #(
      Model(..model, smoke_active: !model.smoke_active),
      effect.tick(Tick),
      option.None,
    )
  }
}

fn view(model: Model, _ctx: tiramisu.Context(Id)) -> List(scene.Node(Id)) {
  echo debug.get_performance_stats()
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  [
    scene.Camera(
      id: MainCamera,
      camera: camera,
      transform: transform.at(position: vec3.Vec3(0.0, 5.0, 20.0)),
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      active: True,
      viewport: option.None,
    ),
    scene.Light(
      id: Ambient,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.3)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: Directional,
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 0.6)
        light
      },
      transform: transform.at(position: vec3.Vec3(10.0, 10.0, 10.0)),
    ),
    // Fire particles (red-orange with upward velocity)
    scene.Particles(
      id: Fire,
      emitter: {
        let assert Ok(emitter) =
          particle_emitter.new()
          |> particle_emitter.rate(100.0)
          |> particle_emitter.lifetime(1.5)
          |> particle_emitter.velocity(vec3.Vec3(0.0, 3.0, 0.0))
          |> particle_emitter.velocity_variance(vec3.Vec3(0.5, 0.5, 0.5))
          |> particle_emitter.size(0.2)
          |> particle_emitter.size_variance(0.1)
          |> particle_emitter.color(0xff4500)
          |> particle_emitter.fade_to(0xffaa00)
          |> particle_emitter.gravity(-0.2)
          |> particle_emitter.max_particles(200)
          |> particle_emitter.build()
        emitter
      },
      transform: transform.at(position: vec3.Vec3(-5.0, 0.0, 0.0)),
      active: model.fire_active,
    ),
    // Sparkles (golden with slow upward drift)
    scene.Particles(
      id: Sparkles,
      emitter: {
        let assert Ok(emitter) =
          particle_emitter.new()
          |> particle_emitter.rate(50.0)
          |> particle_emitter.lifetime(2.0)
          |> particle_emitter.velocity(vec3.Vec3(0.0, 1.0, 0.0))
          |> particle_emitter.velocity_variance(vec3.Vec3(0.3, 0.3, 0.3))
          |> particle_emitter.size(0.15)
          |> particle_emitter.size_variance(0.05)
          |> particle_emitter.color(0xffd700)
          |> particle_emitter.gravity(0.0)
          |> particle_emitter.max_particles(100)
          |> particle_emitter.build()
        emitter
      },
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
      active: model.sparkles_active,
    ),
    // Explosion (fast radial burst)
    scene.Particles(
      id: Explosion,
      emitter: {
        let assert Ok(emitter) =
          particle_emitter.new()
          |> particle_emitter.rate(200.0)
          |> particle_emitter.lifetime(0.8)
          |> particle_emitter.velocity(vec3.Vec3(0.0, 5.0, 0.0))
          |> particle_emitter.velocity_variance(vec3.Vec3(3.0, 2.0, 3.0))
          |> particle_emitter.size(0.25)
          |> particle_emitter.size_variance(0.15)
          |> particle_emitter.color(0xff0000)
          |> particle_emitter.fade_to(0xffff00)
          |> particle_emitter.gravity(9.8)
          |> particle_emitter.max_particles(300)
          |> particle_emitter.build()
        emitter
      },
      transform: transform.at(position: vec3.Vec3(5.0, 0.0, 0.0)),
      active: model.explosion_active,
    ),
    // Smoke (gray with slow upward drift and spread)
    scene.Particles(
      id: Smoke,
      emitter: {
        let assert Ok(emitter) =
          particle_emitter.new()
          |> particle_emitter.rate(30.0)
          |> particle_emitter.lifetime(3.0)
          |> particle_emitter.velocity(vec3.Vec3(0.0, 0.8, 0.0))
          |> particle_emitter.velocity_variance(vec3.Vec3(0.6, 0.2, 0.6))
          |> particle_emitter.size(0.3)
          |> particle_emitter.size_variance(0.2)
          |> particle_emitter.color(0x808080)
          |> particle_emitter.fade_to(0x202020)
          |> particle_emitter.gravity(-0.1)
          |> particle_emitter.max_particles(150)
          |> particle_emitter.build()
        emitter
      },
      transform: transform.at(position: vec3.Vec3(-5.0, 0.0, 0.0)),
      active: model.smoke_active,
    ),
    // Ground plane for reference
    scene.Mesh(
      id: Ground,
      geometry: {
        let assert Ok(geom) = geometry.plane(width: 30.0, height: 30.0)
        geom
      },
      material: {
        let assert Ok(mat) =
          material.standard(
            color: 0x2a2a3a,
            metalness: 0.2,
            roughness: 0.8,
            map: option.None,
            normal_map: option.None,
            ambient_oclusion_map: option.None,
            roughness_map: option.None,
            metalness_map: option.None,
          )
        mat
      },
      transform: transform.at(position: vec3.Vec3(0.0, -0.1, 0.0))
        |> transform.set_rotation(vec3.Vec3(-1.57, 0.0, 0.0)),
      physics: option.None,
    ),
  ]
}
