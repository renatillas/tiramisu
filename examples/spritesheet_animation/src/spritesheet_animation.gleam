import gleam/option
import gleam/result
import gleam/time/duration
import tiramisu
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/spritesheet
import tiramisu/texture
import tiramisu/transform
import vec/vec2
import vec/vec3

pub type Model {
  Model(
    coin1_machine: option.Option(spritesheet.AnimationMachine(Nil)),
    coin2_machine: option.Option(spritesheet.AnimationMachine(Nil)),
    coin3_machine: option.Option(spritesheet.AnimationMachine(Nil)),
  )
}

pub type Msg {
  TextureLoadError
  Tick
  TextureLoaded(texture.Texture)
}

pub fn main() -> Nil {
  let assert Ok(Nil) =
    tiramisu.run(
      dimensions: option.None,
      selector: "body",
      bridge: option.None,
      init: init,
      update: update,
      view: view,
    )
  Nil
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg), option.Option(_)) {
  let model =
    Model(
      coin1_machine: option.None,
      coin2_machine: option.None,
      coin3_machine: option.None,
    )

  let load_effect = texture.load("MonedaD.png", TextureLoaded, TextureLoadError)

  #(model, effect.batch([effect.tick(Tick), load_effect]), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    TextureLoadError -> panic

    Tick -> {
      // Update all animation machines
      let new_coin1 = case model.coin1_machine {
        option.Some(machine) -> {
          let #(m, _) = spritesheet.update(machine, Nil, ctx.delta_time)
          option.Some(m)
        }
        option.None -> option.None
      }

      let new_coin2 = case model.coin2_machine {
        option.Some(machine) -> {
          let #(m, _) = spritesheet.update(machine, Nil, ctx.delta_time)
          option.Some(m)
        }
        option.None -> option.None
      }

      let new_coin3 = case model.coin3_machine {
        option.Some(machine) -> {
          let #(m, _) = spritesheet.update(machine, Nil, ctx.delta_time)
          option.Some(m)
        }
        option.None -> option.None
      }

      #(
        Model(
          coin1_machine: new_coin1,
          coin2_machine: new_coin2,
          coin3_machine: new_coin3,
        ),
        effect.tick(Tick),
        option.None,
      )
    }

    TextureLoaded(tex) -> {
      // Create animation machine with texture (5 frames horizontally)
      // The builder pattern ensures at least one animation is added before build()
      let assert Ok(coin_machine) =
        spritesheet.new(texture: tex, columns: 5, rows: 1)
        |> result.map(spritesheet.with_animation(
          _,
          name: "spin",
          frames: [0, 1, 2, 3, 4],
          frame_duration: duration.milliseconds(200),
          loop: spritesheet.Repeat,
        ))
        |> result.map(spritesheet.with_pixel_art(_, True))
        |> result.map(spritesheet.build)

      // Clone machines for each coin (they animate independently)
      #(
        Model(
          coin1_machine: option.Some(coin_machine),
          coin2_machine: option.Some(coin_machine),
          coin3_machine: option.Some(coin_machine),
        ),
        effect.none(),
        option.None,
      )
    }
  }
}

fn view(model: Model, _ctx: tiramisu.Context) -> scene.Node {
  let camera = {
    let assert Ok(cam) =
      camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    scene.camera(
      id: "main-camera",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 3.0, 10.0)),
      active: True,
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      viewport: option.None,
      postprocessing: option.None,
    )
  }

  let ambient =
    scene.light(
      id: "ambient-light",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 1.2)
        light
      },
      transform: transform.identity,
    )

  // Ground plane
  let ground =
    scene.mesh(
      id: "ground",
      geometry: {
        let assert Ok(geo) = geometry.plane(vec2.Vec2(20.0, 20.0))
        geo
      },
      material: {
        let assert Ok(mat) =
          material.new()
          |> material.with_color(0x444444)
          |> material.build()
        mat
      },
      transform: transform.at(position: vec3.Vec3(0.0, -1.0, 0.0))
        |> transform.with_euler_rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
      physics: option.None,
    )

  // Build coin sprites - only render when machines are loaded
  let coin1 = case model.coin1_machine {
    option.Some(machine) ->
      scene.animated_sprite(
        id: "coin-1",
        sprite: spritesheet.to_sprite(machine),
        size: vec2.Vec2(2.0, 2.0),
        transform: transform.at(position: vec3.Vec3(-3.0, 0.0, 0.0)),
        physics: option.None,
      )
    option.None ->
      scene.empty(
        id: "coin-1-placeholder",
        transform: transform.identity,
        children: [],
      )
  }

  let coin2 = case model.coin2_machine {
    option.Some(machine) ->
      scene.animated_sprite(
        id: "coin-2",
        sprite: spritesheet.to_sprite(machine),
        size: vec2.Vec2(2.0, 2.0),
        transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
        physics: option.None,
      )
    option.None ->
      scene.empty(
        id: "coin-2-placeholder",
        transform: transform.identity,
        children: [],
      )
  }

  let coin3 = case model.coin3_machine {
    option.Some(machine) ->
      scene.animated_sprite(
        id: "coin-3",
        sprite: spritesheet.to_sprite(machine),
        size: vec2.Vec2(2.0, 2.0),
        transform: transform.at(position: vec3.Vec3(3.0, 0.0, 0.0)),
        physics: option.None,
      )
    option.None ->
      scene.empty(
        id: "coin-3-placeholder",
        transform: transform.identity,
        children: [],
      )
  }

  scene.empty(id: "scene", transform: transform.identity, children: [
    camera,
    ambient,
    ground,
    coin1,
    coin2,
    coin3,
  ])
}
