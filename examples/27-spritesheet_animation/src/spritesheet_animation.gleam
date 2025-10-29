/// Spritesheet Animation Example
///
/// Demonstrates animated sprites using spritesheets with:
/// - Multiple independent sprites animating simultaneously
/// - Different animation modes (repeat, once, ping-pong)
/// - Animation state control (play, pause, change animation)
/// - Pixel art filtering for crisp sprites
import gleam/dict.{type Dict}
import gleam/javascript/promise
import gleam/option
import tiramisu
import tiramisu/asset
import tiramisu/background
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/spritesheet
import tiramisu/transform
import vec/vec3

pub type Id {
  Scene
  MainCamera
  AmbientLight
  Coin1
  Coin2
  Coin3
  Ground
}

pub type Model {
  Model(
    textures: Dict(String, asset.Texture),
    spritesheets: Dict(String, spritesheet.Spritesheet),
    animations: Dict(String, spritesheet.Animation),
    coin1_state: spritesheet.AnimationState,
    coin2_state: spritesheet.AnimationState,
    coin3_state: spritesheet.AnimationState,
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
    background: background.Color(0x2a2a3e),
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context(Id)) -> #(Model, Effect(Msg), option.Option(_)) {
  let model =
    Model(
      textures: dict.new(),
      spritesheets: dict.new(),
      animations: dict.new(),
      coin1_state: spritesheet.initial_state("spin"),
      coin2_state: spritesheet.initial_state("spin"),
      coin3_state: spritesheet.initial_state("spin"),
      loading_complete: False,
    )

  // Load coin spritesheet texture
  // For this example, we'll use a simple colored square as a placeholder
  // In a real game, you'd load an actual spritesheet PNG
  let load_effect =
    effect.from_promise(
      promise.map(asset.load_texture("MonedaD.png"), fn(result) {
        case result {
          Ok(tex) -> TextureLoaded("coin", tex)
          Error(_) -> NoOp
        }
      }),
    )

  #(model, effect.batch([effect.tick(Tick), load_effect]), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(Id),
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    NoOp -> #(model, effect.none(), option.None)

    Tick -> {
      case model.loading_complete {
        False -> #(model, effect.tick(Tick), option.None)
        True -> {
          // Update all animation states
          let assert Ok(spin_anim) = dict.get(model.animations, "spin")

          let new_coin1 =
            spritesheet.update(
              state: model.coin1_state,
              animation: spin_anim,
              delta_time: ctx.delta_time,
            )

          let new_coin2 =
            spritesheet.update(
              state: model.coin2_state,
              animation: spin_anim,
              delta_time: ctx.delta_time,
            )

          let new_coin3 =
            spritesheet.update(
              state: model.coin3_state,
              animation: spin_anim,
              delta_time: ctx.delta_time,
            )

          #(
            Model(
              ..model,
              coin1_state: new_coin1,
              coin2_state: new_coin2,
              coin3_state: new_coin3,
            ),
            effect.tick(Tick),
            option.None,
          )
        }
      }
    }

    TextureLoaded(name, tex) -> {
      let new_textures = dict.insert(model.textures, name, tex)

      // Create spritesheet (5 frames horizontally)
      let assert Ok(coin_sheet) =
        spritesheet.from_grid(texture: tex, columns: 5, rows: 1)

      let spritesheets =
        dict.new()
        |> dict.insert("coin", coin_sheet)

      // Create animation (all 8 frames)
      let spin_anim =
        spritesheet.animation(
          name: "spin",
          frames: [0, 1, 2, 3, 4],
          frame_duration: 200.0,
          loop: spritesheet.Repeat,
        )

      let animations =
        dict.new()
        |> dict.insert("spin", spin_anim)

      #(
        Model(
          ..model,
          textures: new_textures,
          spritesheets: spritesheets,
          animations: animations,
          loading_complete: True,
        ),
        effect.none(),
        option.None,
      )
    }
  }
}

fn view(model: Model, _ctx: tiramisu.Context(Id)) -> scene.Node(Id) {
  let camera = {
    let assert Ok(cam) =
      camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    scene.camera(
      id: MainCamera,
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
      id: AmbientLight,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 1.2)
        light
      },
      transform: transform.identity,
    )

  // Ground plane
  let ground =
    scene.mesh(
      id: Ground,
      geometry: {
        let assert Ok(geo) = geometry.plane(width: 20.0, height: 20.0)
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

  case model.loading_complete {
    False ->
      scene.empty(id: Scene, transform: transform.identity, children: [
        camera,
        ambient,
        ground,
      ])
    True -> {
      let assert Ok(coin_sheet) = dict.get(model.spritesheets, "coin")
      let assert Ok(spin_anim) = dict.get(model.animations, "spin")

      scene.empty(id: Scene, transform: transform.identity, children: [
        camera,
        ambient,
        ground,
        // Coin 1 - Left
        scene.animated_sprite(
          id: Coin1,
          spritesheet: coin_sheet,
          animation: spin_anim,
          state: model.coin1_state,
          width: 2.0,
          height: 2.0,
          transform: transform.at(position: vec3.Vec3(-3.0, 0.0, 0.0)),
          pixel_art: True,
          physics: option.None,
        ),
        // Coin 2 - Center
        scene.animated_sprite(
          id: Coin2,
          spritesheet: coin_sheet,
          animation: spin_anim,
          state: model.coin2_state,
          width: 2.0,
          height: 2.0,
          transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
          pixel_art: True,
          physics: option.None,
        ),
        // Coin 3 - Right
        scene.animated_sprite(
          id: Coin3,
          spritesheet: coin_sheet,
          animation: spin_anim,
          state: model.coin3_state,
          width: 2.0,
          height: 2.0,
          transform: transform.at(position: vec3.Vec3(3.0, 0.0, 0.0)),
          pixel_art: True,
          physics: option.None,
        ),
      ])
    }
  }
}
