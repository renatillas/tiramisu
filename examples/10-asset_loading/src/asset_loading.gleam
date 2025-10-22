import gleam/int
import gleam/io
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/result
import tiramisu
import tiramisu/asset
import tiramisu/audio
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
  Directional
  LoadingCube
  Cube1
  BeepSound
}

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
    dimensions: option.None,
    background: background.Color(0x1a1a2e),
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context(Id)) -> #(Model, Effect(Msg), option.Option(_)) {
  let model = Model(rotation: 0.0, load_state: Loading(0, 0, "Starting..."))

  // Define assets to load
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

  #(model, effect.batch([effect.tick(Tick), load_effect]), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(Id),
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.0005
      #(
        Model(rotation: new_rotation, load_state: model.load_state),
        effect.tick(Tick),
        option.None,
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
        option.None,
      )
    }

    AssetsLoaded(result) -> {
      #(
        Model(..model, load_state: case result.errors {
          [] -> Loaded(result.cache)
          errors -> Failed(errors)
        }),
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
      |> scene.camera(
        id: MainCamera,
        camera: _,
        transform: transform.at(position: vec3.Vec3(0.0, 0.0, 10.0)),
        look_at: option.None,
        active: True,
        viewport: option.None,
      )
      |> list.wrap
    })

  let lights = [
    scene.light(
      id: Ambient,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.6)
        light
      },
      transform: transform.identity,
    ),
    scene.light(
      id: Directional,
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 1.5)
        light
      },
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 7.5)),
    ),
  ]

  case model.load_state {
    Loading(_, _, _) -> {
      // Show a spinning cube while loading
      let loading_cube =
        scene.mesh(
          id: LoadingCube,
          geometry: {
            let assert Ok(geometry) =
              geometry.box(width: 2.0, height: 2.0, depth: 2.0)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.new()
              |> material.with_color(0x4a90e2)
              |> material.build
            material
          },
          transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
            |> transform.with_euler_rotation(vec3.Vec3(
              model.rotation,
              model.rotation *. 1.5,
              0.0,
            )),
          physics: option.None,
        )
        |> list.wrap

      list.flatten([camera, loading_cube, lights])
    }

    Loaded(cache) -> {
      let audio_node = case
        asset.get_audio(
          cache,
          "https://actions.google.com/sounds/v1/alarms/beep_short.ogg",
        )
      {
        Ok(audio_buffer) -> {
          [
            scene.audio(
              id: BeepSound,
              audio: audio.global(
                audio_buffer,
                audio.playing()
                  |> audio.with_volume(1.0)
                  |> audio.with_loop(True)
                  |> audio.with_playback_rate(1.0),
              ),
            ),
          ]
        }
        Error(_) -> {
          []
        }
      }

      // Try to get textures
      let cube_nodes = case
        asset.get_texture(cache, "metal-color.png"),
        asset.get_texture(cache, "metal-normal.png")
      {
        Ok(metal_color), Ok(metal_normal) -> {
          [
            scene.mesh(
              id: Cube1,
              geometry: {
                let assert Ok(geometry) =
                  geometry.box(width: 2.0, height: 2.0, depth: 2.0)
                geometry
              },
              material: {
                let assert Ok(material) =
                  material.new()
                  |> material.with_color(0x4ecdc4)
                  |> material.with_metalness(1.0)
                  |> material.with_roughness(0.5)
                  |> material.with_color_map(metal_color)
                  |> material.with_normal_map(metal_normal)
                  |> material.build()
                material
              },
              transform: transform.at(position: vec3.Vec3(-3.0, 0.0, 0.0))
                |> transform.with_euler_rotation(vec3.Vec3(
                  model.rotation,
                  0.0,
                  0.0,
                )),
              physics: option.None,
            ),
          ]
        }
        _, _ -> {
          [
            scene.mesh(
              id: Cube1,
              geometry: {
                let assert Ok(geometry) =
                  geometry.box(width: 2.0, height: 2.0, depth: 2.0)
                geometry
              },
              material: {
                let assert Ok(material) =
                  material.new()
                  |> material.with_color(0x4ecdc4)
                  |> material.build()
                material
              },
              transform: transform.at(position: vec3.Vec3(-3.0, 0.0, 0.0))
                |> transform.with_euler_rotation(vec3.Vec3(
                  model.rotation,
                  0.0,
                  0.0,
                )),
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
