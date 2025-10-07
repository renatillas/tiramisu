import gleam/int
import gleam/io
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/result
import tiramisu
import tiramisu/asset
import tiramisu/audio
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type LoadState {
  Loading(progress: Int, total: Int, current_url: String)
  Loaded(cache: asset.AssetCache)
  Failed(errors: List(asset.AssetError))
}

pub type Model {
  Model(rotation: Float, load_state: LoadState)
}

pub type Msg {
  Tick
  LoadProgress(asset.LoadProgress)
  AssetsLoaded(asset.BatchLoadResult)
}

pub fn main() -> Nil {
  tiramisu.run(
    width: 1200,
    height: 800,
    background: 0x1a1a2e,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg)) {
  let model = Model(rotation: 0.0, load_state: Loading(0, 0, "Starting..."))

  // Define asset to load (example URLs - replace with real asset)
  let asset_to_load = [
    asset.TextureAsset("metal-color.png"),
    asset.TextureAsset("metal-normal.png"),
    asset.AudioAsset(
      "https://actions.google.com/sounds/v1/alarms/beep_short.ogg",
    ),
  ]

  // Start batch loading with progress callback
  let load_effect =
    effect.from_promise(promise.map(
      asset.load_batch(asset_to_load, fn(progress) {
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

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg)) {
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
  let assert Ok(camera) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 1200.0 /. 800.0,
      near: 0.1,
      far: 1000.0,
    )
    |> result.map(fn(camera) {
      camera
      |> camera.set_position(vec3.Vec3(5.0, 5.0, 10.0))
      |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))
      |> scene.Camera(
        id: "main-camera",
        camera: _,
        transform: transform.identity,
        active: True,
        viewport: option.None,
      )
      |> list.wrap
    })

  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.6),
      transform: transform.identity,
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
        |> list.wrap

      list.flatten([camera, loading_cube, lights])
    }

    Loaded(cache) -> {
      let audio_node = case
        echo asset.get_audio(
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
        asset.get_texture(cache, "metal-color.png"),
        asset.get_texture(cache, "metal-normal.png")
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

      list.flatten([camera, audio_node, cube_nodes, lights])
    }

    Failed(_errors) -> {
      list.flatten([camera, lights])
    }
  }
}
