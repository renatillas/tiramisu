import gleam/int
import gleam/io
import gleam/javascript/promise
import gleam/list
import gleam/option
import tiramisu/assets
import tiramisu/audio
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/math/vec3
import tiramisu/scene
import tiramisu/transform

pub type LoadState {
  Loading(progress: Int, total: Int, current_url: String)
  Loaded(cache: assets.AssetCache)
  Failed(errors: List(assets.AssetError))
}

pub type Model {
  Model(rotation: Float, load_state: LoadState)
}

pub type Msg {
  Tick
  LoadProgress(assets.LoadProgress)
  AssetsLoaded(assets.BatchLoadResult)
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
    |> camera.set_position(vec3.Vec3(5.0, 5.0, 10.0))
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
  let model = Model(rotation: 0.0, load_state: Loading(0, 0, "Starting..."))

  // Define assets to load (example URLs - replace with real assets)
  let assets_to_load = [
    assets.TextureAsset("metal-color.png"),
    assets.TextureAsset("metal-normal.png"),
    // assets.ModelAsset("model.glb"),  // Uncomment with real model
    assets.AudioAsset(
      "https://actions.google.com/sounds/v1/alarms/beep_short.ogg",
    ),
  ]

  // Start batch loading with progress callback
  let load_effect =
    effect.from_promise(promise.map(
      assets.load_batch(assets_to_load, fn(progress) {
        // This callback is called for each asset loaded
        io.println(
          "Loading progress: "
          <> int.to_string(progress.loaded)
          <> "/"
          <> int.to_string(progress.total)
          <> " - "
          <> progress.current_url,
        )
      }),
      AssetsLoaded,
    ))

  #(model, effect.batch([effect.tick(Tick), load_effect]))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.5
      #(
        Model(rotation: new_rotation, load_state: model.load_state),
        effect.tick(Tick),
      )
    }

    LoadProgress(progress) -> {
      #(
        Model(
          ..model,
          load_state: Loading(
            progress.loaded,
            progress.total,
            progress.current_url,
          ),
        ),
        effect.none(),
      )
    }

    AssetsLoaded(result) -> {
      #(
        Model(..model, load_state: case result.errors {
          [] -> Loaded(result.cache)
          errors -> Failed(errors)
        }),
        effect.none(),
      )
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.6),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 1.5),
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 7.5)),
    ),
  ]

  case model.load_state {
    Loading(_, _, _) -> {
      // Show a spinning cube while loading
      let loading_cube =
        scene.Mesh(
          id: "loading",
          geometry: scene.BoxGeometry(2.0, 2.0, 2.0),
          material: scene.StandardMaterial(
            color: 0x4a90e2,
            metalness: 0.3,
            roughness: 0.7,
            map: option.None,
            normal_map: option.None,
          ),
          transform: transform.Transform(
            position: vec3.Vec3(0.0, 0.0, 0.0),
            rotation: vec3.Vec3(model.rotation, model.rotation *. 1.5, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          physics: option.None,
        )

      [loading_cube, ..lights]
    }

    Loaded(cache) -> {
      // Assets loaded! Show the scene with loaded content
      io.println("✓ Displaying scene with loaded assets!")

      // Try to get audio (may fail if texture-only loading)
      let audio_node = case
        assets.get_audio(
          cache,
          "https://actions.google.com/sounds/v1/alarms/beep_short.ogg",
        )
      {
        Ok(audio_buffer) -> {
          [
            scene.Audio(
              id: "beep-sound",
              buffer: audio_buffer,
              config: audio.AudioConfig(
                volume: 0.3,
                loop: True,
                playback_rate: 1.0,
                autoplay: True,
              ),
              audio_type: audio.GlobalAudio,
            ),
          ]
        }
        Error(_) -> {
          []
        }
      }

      // Try to get textures (may fail with example URLs)
      let cube_nodes = case
        assets.get_texture(cache, "metal-color.png"),
        assets.get_texture(cache, "metal-normal.png")
      {
        Ok(metal_color), Ok(metal_normal) -> {
          [
            scene.Mesh(
              id: "cube1",
              geometry: scene.BoxGeometry(2.0, 2.0, 2.0),
              material: scene.StandardMaterial(
                color: 0x4ecdc4,
                metalness: 1.0,
                roughness: 0.5,
                map: option.Some(metal_color),
                normal_map: option.Some(metal_normal),
              ),
              transform: transform.Transform(
                position: vec3.Vec3(-3.0, 0.0, 0.0),
                rotation: vec3.Vec3(model.rotation, 0.0, 0.0),
                scale: vec3.Vec3(1.0, 1.0, 1.0),
              ),
              physics: option.None,
            ),
          ]
        }
        _, _ -> {
          [
            scene.Mesh(
              id: "cube1",
              geometry: scene.BoxGeometry(2.0, 2.0, 2.0),
              material: scene.StandardMaterial(
                color: 0x4ecdc4,
                metalness: 0.5,
                roughness: 0.5,
                map: option.None,
                normal_map: option.None,
              ),
              transform: transform.Transform(
                position: vec3.Vec3(-3.0, 0.0, 0.0),
                rotation: vec3.Vec3(model.rotation, 0.0, 0.0),
                scale: vec3.Vec3(1.0, 1.0, 1.0),
              ),
              physics: option.None,
            ),
          ]
        }
      }

      list.flatten([audio_node, cube_nodes, lights])
    }

    Failed(_errors) -> {
      lights
    }
  }
}
