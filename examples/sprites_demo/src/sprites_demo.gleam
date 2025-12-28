/// Sprites Demo Example
///
/// Demonstrates loading sprite textures from the internet and displaying them
import gleam/option.{type Option}
import gleam/result
import gleam/time/duration
import gleam_community/maths
import tiramisu
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/texture
import tiramisu/transform
import vec/vec2
import vec/vec3

pub type Model {
  Model(rotation: Float, texture: Option(texture.Texture))
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

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg), Option(_)) {
  let model = Model(rotation: 0.0, texture: option.None)

  let load_effect =
    texture.load("star-struck.png", TextureLoaded, TextureLoadError)

  #(model, effect.batch([effect.tick(Tick), load_effect]), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), Option(_)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. duration.to_seconds(ctx.delta_time)
      #(Model(..model, rotation: new_rotation), effect.tick(Tick), option.None)
    }

    TextureLoaded(texture) -> {
      echo "hola"
      #(
        Model(..model, texture: option.Some(texture)),
        effect.none(),
        option.None,
      )
    }
    TextureLoadError -> panic as "Should not happen"
  }
}

fn view(model: Model, _) -> scene.Node {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    |> result.map(fn(camera) {
      camera
      |> scene.camera(
        id: "main-camera",
        transform: transform.at(position: vec3.Vec3(0.0, 0.0, 10.0)),
        active: True,
        look_at: option.None,
        viewport: option.None,
        camera: _,
        postprocessing: option.None,
      )
    })

  let lights =
    scene.light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 1.5)
        light
      },
      transform: transform.identity,
    )

  // Show loading text or sprites
  let sprites =
    [
      // Emoji 1 - rotating in circle
      model.texture
        |> option.map(fn(tex) {
          let x = 3.0 *. maths.cos(model.rotation)
          let y = 3.0 *. maths.sin(model.rotation)

          scene.mesh(
            id: "sprite-1",
            geometry: {
              let assert Ok(geometry) = geometry.plane(vec2.Vec2(2.0, 2.0))
              geometry
            },
            material: {
              let assert Ok(material) =
                material.basic(
                  color: 0xffffff,
                  transparent: True,
                  opacity: 1.0,
                  map: option.Some(tex),
                )
              material
            },
            transform: transform.at(position: vec3.Vec3(x, y, 0.0)),
            physics: option.None,
          )
        }),
      // Emoji 2 - rotating opposite direction
      model.texture
        |> option.map(fn(tex) {
          let x = 3.0 *. maths.cos(0.0 -. model.rotation)
          let y = 3.0 *. maths.sin(0.0 -. model.rotation)
          scene.mesh(
            id: "sprite-2",
            geometry: {
              let assert Ok(geometry) = geometry.plane(vec2.Vec2(2.0, 2.0))
              geometry
            },
            material: {
              let assert Ok(material) =
                material.basic(
                  color: 0xffffff,
                  transparent: True,
                  opacity: 1.0,
                  map: option.Some(tex),
                )
              material
            },
            transform: transform.at(position: vec3.Vec3(x, y, 0.0)),
            physics: option.None,
          )
        }),
      // Emoji 3 - center, spinning
      model.texture
        |> option.map(fn(tex) {
          scene.mesh(
            id: "sprite-3",
            geometry: {
              let assert Ok(geometry) = geometry.plane(vec2.Vec2(1.5, 1.5))
              geometry
            },
            material: {
              let assert Ok(material) =
                material.basic(
                  color: 0xffffff,
                  transparent: True,
                  opacity: 1.0,
                  map: option.Some(tex),
                )
              material
            },
            transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
              |> transform.with_euler_rotation(vec3.Vec3(
                0.0,
                0.0,
                model.rotation *. 2.0,
              )),
            physics: option.None,
          )
        }),
      // Emoji 4 - bouncing
      model.texture
        |> option.map(fn(tex) {
          let y = maths.sin(model.rotation *. 2.0) *. 2.0
          scene.mesh(
            id: "sprite-4",
            geometry: {
              let assert Ok(geometry) = geometry.plane(vec2.Vec2(2.0, 2.0))
              geometry
            },
            material: {
              let assert Ok(material) =
                material.basic(
                  color: 0xffffff,
                  transparent: True,
                  opacity: 1.0,
                  map: option.Some(tex),
                )
              material
            },
            transform: transform.at(position: vec3.Vec3(0.0, y +. 4.0, 0.0)),
            physics: option.None,
          )
        }),
    ]
    |> option.values
    |> scene.empty(id: "sprites", transform: transform.identity, children: _)

  scene.empty(id: "scene", transform: transform.identity, children: [
    camera,
    lights,
    sprites,
  ])
}
