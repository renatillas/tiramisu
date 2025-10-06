/// Sprites Demo Example
///
/// Demonstrates loading sprite textures from the internet and displaying them
import gleam/dict.{type Dict}
import gleam/javascript/promise
import gleam/list
import gleam/result
import gleam_community/maths
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/graphics/sprite
import tiramisu/math/vec3
import tiramisu/scene.{type Texture}
import tiramisu/texture
import tiramisu/transform

pub type Model {
  Model(
    rotation: Float,
    textures: Dict(String, Texture),
    loading_complete: Bool,
  )
}

pub type Msg {
  Tick
  TextureLoaded(String, Texture)
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
    |> camera.set_position(vec3.Vec3(0.0, 0.0, 10.0))
    |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

  game.run(
    width: 1200,
    height: 800,
    background: 0x1a1a2e,
    camera: cam,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
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
        promise.map(texture.load(url), fn(result) {
          case result {
            Ok(tex) -> TextureLoaded(name, tex)
            Error(_) -> echo Tick
          }
        }),
      )
    })

  #(model, effect.batch([effect.tick(Tick), ..load_effects]))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.5
      #(Model(..model, rotation: new_rotation), effect.tick(Tick))
    }

    TextureLoaded(name, tex) -> {
      let new_textures = dict.insert(model.textures, name, tex)
      let loading_complete = echo dict.size(new_textures) >= 4
      #(
        Model(..model, textures: new_textures, loading_complete:),
        effect.none(),
      )
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 1.5),
      transform: transform.identity(),
    ),
  ]

  // Show loading text or sprites
  case model.loading_complete {
    False -> lights
    True -> {
      let sprites = [
        // Emoji 1 - rotating in circle
        dict.get(model.textures, "emoji1")
          |> result.map(fn(tex) {
            let x = 3.0 *. maths.cos(model.rotation)
            let y = 3.0 *. maths.sin(model.rotation)

            sprite.mesh(id: "sprite1", texture: tex, width: 2.0, height: 2.0)
            |> with_transform(transform.at(position: vec3.Vec3(x, y, 0.0)))
          }),
        // Emoji 2 - rotating opposite direction
        dict.get(model.textures, "emoji2")
          |> result.map(fn(tex) {
            let x = 3.0 *. maths.cos(0.0 -. model.rotation)
            let y = 3.0 *. maths.sin(0.0 -. model.rotation)
            sprite.mesh(id: "sprite2", texture: tex, width: 2.0, height: 2.0)
            |> with_transform(transform.at(position: vec3.Vec3(x, y, 0.0)))
          }),
        // Emoji 3 - center, spinning
        dict.get(model.textures, "emoji3")
          |> result.map(fn(tex) {
            sprite.mesh(id: "sprite3", texture: tex, width: 1.5, height: 1.5)
            |> with_transform(transform.Transform(
              position: vec3.Vec3(0.0, 0.0, 0.0),
              rotation: vec3.Vec3(0.0, 0.0, model.rotation *. 2.0),
              scale: vec3.Vec3(1.0, 1.0, 1.0),
            ))
          }),
        // Emoji 4 - bouncing
        dict.get(model.textures, "emoji4")
          |> result.map(fn(tex) {
            let y = maths.sin(model.rotation *. 2.0) *. 2.0
            sprite.mesh(id: "sprite4", texture: tex, width: 2.0, height: 2.0)
            |> with_transform(
              transform.at(position: vec3.Vec3(0.0, y +. 4.0, 0.0)),
            )
          }),
      ]

      list.flatten([
        lights,
        result.values(sprites),
      ])
    }
  }
}

// Helper function to update a scene node's transform
fn with_transform(
  node: scene.SceneNode,
  new_transform: transform.Transform,
) -> scene.SceneNode {
  case node {
    scene.Mesh(id, geometry, material, _, physics) ->
      scene.Mesh(id, geometry, material, new_transform, physics)
    scene.Group(id, _, children) -> scene.Group(id, new_transform, children)
    scene.Light(id, light_type, _) -> scene.Light(id, light_type, new_transform)
    scene.Model3D(id, object, _, animation, physics) ->
      scene.Model3D(id, object, new_transform, animation, physics)
  }
}
