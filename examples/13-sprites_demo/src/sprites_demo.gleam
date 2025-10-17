/// Sprites Demo Example
///
/// Demonstrates loading sprite textures from the internet and displaying them
import gleam/dict.{type Dict}
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/result
import gleam_community/maths
import tiramisu
import tiramisu/asset
import tiramisu/background
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Id {
  MainCamera
  Ambient
  Sprite1
  Sprite2
  Sprite3
  Sprite4
}

pub type Model {
  Model(
    rotation: Float,
    textures: Dict(String, asset.Texture),
    loading_complete: Bool,
  )
}

pub type Msg {
  NoOp
  Tick
  TextureLoaded(String, asset.Texture)
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
  let model =
    Model(rotation: 0.0, textures: dict.new(), loading_complete: False)

  // Load textures from the internet
  // Using placeholder images for demonstration
  let sprite_urls = [
    #("emoji1", "./star-struck.png"),
    #("emoji2", "./star-struck.png"),
    #("emoji3", "./star-struck.png"),
    #("emoji4", "./star-struck.png"),
  ]

  let load_effects =
    list.map(sprite_urls, fn(item) {
      let #(name, url) = item
      effect.from_promise(
        promise.map(asset.load_texture(url), fn(result) {
          case result {
            Ok(tex) -> TextureLoaded(name, tex)
            Error(error) -> {
              echo error
              NoOp
            }
          }
        }),
      )
    })

  #(model, effect.batch([effect.tick(Tick), ..load_effects]), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(Id),
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    NoOp -> #(model, effect.none(), option.None)
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.5
      #(Model(..model, rotation: new_rotation), effect.tick(Tick), option.None)
    }

    TextureLoaded(name, tex) -> {
      let new_textures = dict.insert(model.textures, name, tex)
      let loading_complete = echo dict.size(new_textures) >= 4
      #(
        Model(..model, textures: new_textures, loading_complete:),
        effect.none(),
        option.None,
      )
    }
  }
}

fn view(model: Model, _) -> List(scene.Node(Id)) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    |> result.map(fn(camera) {
      camera
      |> scene.Camera(
        id: MainCamera,
        transform: transform.at(position: vec3.Vec3(0.0, 0.0, 10.0)),
        active: True,
        look_at: option.None,
        viewport: option.None,
        camera: _,
      )
      |> list.wrap
    })

  let lights =
    scene.Light(
      id: Ambient,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 1.5)
        light
      },
      transform: transform.identity,
    )
    |> list.wrap

  // Show loading text or sprites
  case model.loading_complete {
    False -> list.flatten([camera, lights])
    True -> {
      let sprites = [
        // Emoji 1 - rotating in circle
        dict.get(model.textures, "emoji1")
          |> result.map(fn(tex) {
            let x = 3.0 *. maths.cos(model.rotation)
            let y = 3.0 *. maths.sin(model.rotation)

            scene.Mesh(
              id: Sprite1,
              geometry: {
                let assert Ok(geometry) =
                  geometry.plane(width: 2.0, height: 2.0)
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
        dict.get(model.textures, "emoji2")
          |> result.map(fn(tex) {
            let x = 3.0 *. maths.cos(0.0 -. model.rotation)
            let y = 3.0 *. maths.sin(0.0 -. model.rotation)
            scene.Mesh(
              id: Sprite2,
              geometry: {
                let assert Ok(geometry) =
                  geometry.plane(width: 2.0, height: 2.0)
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
        dict.get(model.textures, "emoji3")
          |> result.map(fn(tex) {
            scene.Mesh(
              id: Sprite3,
              geometry: {
                let assert Ok(geometry) =
                  geometry.plane(width: 1.5, height: 1.5)
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
        |> transform.with_rotation(vec3.Vec3(0.0, 0.0, model.rotation *. 2.0)),
              physics: option.None,
            )
          }),
        // Emoji 4 - bouncing
        dict.get(model.textures, "emoji4")
          |> result.map(fn(tex) {
            let y = maths.sin(model.rotation *. 2.0) *. 2.0
            scene.Mesh(
              id: Sprite4,
              geometry: {
                let assert Ok(geometry) =
                  geometry.plane(width: 2.0, height: 2.0)
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

      list.flatten([
        camera,
        lights,
        result.values(sprites),
      ])
    }
  }
}
